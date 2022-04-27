---
layout: post
title: "Better Errors Through HPC Traces"
date: 2022-04-26 22:49:00 +0200
categories: posts
image: /assets/img/logo.png
tags: programming haskell GHC
---

I'm currently implementing a cute idea I had: getting a trace of recently
evaluated expressions from a program.

The Problem
---
The insight is that if you have an error in say

```haskell
module Main where

go :: Int -> [Int]
go 2 = []
go n = go (n+1)

main :: IO ()
main = print (head $ go 0)
```

it will crash when trying to take the head of the empty list, `[]`.
But the problem isn't really with the `head` function here, it comes from
the fact that the input value `go 0` accidentally evaluates to `[]`.

I.e.: the error is not in the *consumer*, but in the *producer*.

But GHC will complain with

```text
*** Exception: Prelude.head: empty list
```

and even with a stack trace it doesn't really tell you much. To find the bug it would be more useful to know: what was evaluated
right before the error occurred?

### Background

With Haskell Program Coverage (HPC), GHC allows us to track which
expressions were evaluated during the course of the program: whenever an
expression is evaluated, it bumps a number in an array (a "tick"). At
the end of execution you can then read out how many times each expression was evaluated.

That doesn't really help us here though, we might encounter the
error after a long execution, but we want to know what happened immediately prior.

### The Idea

The idea is to extend HPC with a "trace" data structure that's simply an
array that keeps track of which expression was evaluated, and an running index
that goes in circles around the array. This way we can track e.g. the 20
(configureable) most recently evaluated expressions, and when the crash
happens we dump this trace into a file. We can then read the resulting and see, a-ha!, `go` was being evaluated! This can help us
track down the source of the error more easily than just keeping track
of everything that was evaluated, and can be very helpful for tools like
[PropR](https://github.com/Tritlo/PropR).

The Implementation
---

If were doing this from scratch, we'd have to annotate every single expression
with a tick and figure out how to emit the correct code and handle all of it
ourselves. Fortunately, the good folks over at Galois have already done most
of the heavy work when implementing HPC, so all *we* need to to is to
piggyback on top of their implementation.

To keep as much in line with HPC, we simply add two arrays, 
`HpcTraceLabel` to keep track of the data and a `HpcTraceInfoLabel` which contains
two fields: the next index in `HpcTraceLabel` we want to update, and the size
of the trace (so we can loop around when we've reached the end). 

Of particular interest are the `mkTraceBox` and `mkTraceBump` functions that do
the actual bumping and wrapping around of the index:

```haskell
mkTraceBox :: Platform -> Module -> Int -> CmmAGraph
mkTraceBox platform mod n
  = mkStore box_to_update (CmmLit (CmmInt (fromIntegral n) W64))
  where
    index_box = cmmIndex platform W64
                  (CmmLit $ CmmLabel $ mkHpcTraceInfoLabel $ mod) 0
    box_to_update = cmmIndexExpr platform W64
                     (CmmLit $ CmmLabel $ mkHpcTraceLabel $ mod)
        -- Note: I messed this up originally an forgot the crucial CmmLoad
        -- here, meaning it would use `ptr` instead of `&ptr` and segfault.
                     (CmmLoad index_box b64 NaturallyAligned)

mkTraceBump :: Platform -> Module -> CmmAGraph
mkTraceBump platform mod
  = mkStore index_box rem
  where
    add1 = (CmmMachOp (MO_Add W64)
             [ CmmLoad index_box b64 NaturallyAligned
             , CmmLit (CmmInt 1 W64) ])
    rem = (CmmMachOp (MO_S_Rem W64)
             [ add1
    -- Note: I had the same issue here, a missing load. Took me longer to
    -- figure out though, since the operation would run just fine, except it
    -- was modulus some large number representing the pointer!
             , CmmLoad size_box b64 NaturallyAligned])
    infoLabel = cmmIndex platform W64
                (CmmLit $ CmmLabel $ mkHpcTraceInfoLabel $ mod)
    index_box = infoLabel 0
    size_box = infoLabel 1
```

The Results
----

My go to program when implementing this was a simple "hello, world":

```haskell
main = putStrLn "hello, world!"
```

Using the freshly-built GHC and running:

```shell
$ _build/stage1/bin/ghc -o hello Hello.hs -fhpc -hpc-trace5
$ ./hello && cat hello.tix
```
we get the expected `"hello, world!"` as output, but if we look at the `.tix`
file:

```text
Tix [ TixModule "Main" 2820138512 3 [1,1,1] [3,5] [2,1,0,0,0]]
```

We see that the trace is there! If we then look at the `.mix` file:

```haskell
Mix "Hello.hs" 2022-04-27 01:15:00.969319804 UTC 1551044087 8
    [(3:17-3:31,ExpBox False), (3:8-3:31,ExpBox False),
     (3:1-3:31,TopLevelBox ["main"])]
```

We see that this corresponds to evaluating `main` (3:1-31), which demands
evaluation of `putStrLn "hello, world!"` (3:8-31), which in turn requires
evaluating `"hello, world!"` (3:17-31). Exactly what we wanted!


### Error cases
But what about our original purpose to find where errors occur? Let's try it!

Here's our error program from before:

```haskell
go :: Int -> [Int]
go 2 = []
go n = go (n+1)

main = print (head $ go 0)
```

Now, we run it with our improved trace, and get (as before) an error that just
points out the *consumer*:

```text
error: Prelude.head: empty list
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/List.hs:1643:3 in base:GHC.List
  errorEmptyList, called at libraries/base/GHC/List.hs:82:11 in base:GHC.List
  badHead, called at libraries/base/GHC/List.hs:78:28 in base:GHC.List
  head, called at Error.hs:7:15 in main:Main
```

But if we look at the `.tix` file:

```haskell
Tix [ TixModule "Main" 1224276158 12
      [1,2,2,2,2,3,1,1,1,1,1,1] [18,20]
      [11,10,9,6,8,5,7,4,5,3,1,2,4,5,3,1,2,0,0,0]]
    --                                       ^ 18
```

It works! We can see that it was evaluating the `go` function *right before*
the error, with the last thing being evaluated being the empty list!
A lot more useful to know the *producer* of the bad data than just knowing how many times each expression was evaluated, eh?

### Addendum
There is some weird behavior when running it multiple times: it will reload the
old data before starting up, giving (for the `./hello` case`):

```text
Tix [ TixModule "Main" 2820138512 3 [2,2,2] [1,5] [0,1,0,2,1]]
```

But this is consistent with HPCs continuously bumping the ticks (see the first
part of the data).

Conclusion
---

### The Experience

All-in-all, implementing this feature was a fun experience, and gave me a few
insights into how code generation and the RTS works in general. I hope there's
nterest in adding this to mainline GHC, but it might be hard to sell.
We're modifying the runtime semantics when HPC is enabled, i.e. evaluating an
expression now not only bumps the tick but also this list. But I know of a few
actual use cases including fault-localization for
[PropR](https://github.com/Tritlo/PropR) and other tools that need to know how
things happened rather than just what happend. 

### Future work

The proof-of-concept works, but there's some cleaning up to be done.
I need to make the HPC libraries play nicely with the new data-structure,
and there's also some potential issues with multi-threading (but I think that
holds for HPC as well).  If you want to play around with it, have a
look at the [branch on GitHub](https://github.com/Tritlo/ghc/tree/extended-ticks).
Enjoy!

