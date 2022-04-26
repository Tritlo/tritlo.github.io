---
layout: post
title: "HPC Traces"
date: 2022-04-26 22:49:00 +0200
tags: programming haskell GHC
---

I'm currently trying to implement a cute idea I had: getting a trace of recently evaluated expressions from a program.

The Problem
---
The insight is that if you have an error in say

```haskell
xs :: [Int]
xs = go 0
  where  go n | n <= 0 = []
         go n = (n : go (n-1))

main :: IO ()
main = print (head xs)
```

it will crash when trying to take the head of the empty list, `[]`.
But the problem isn't really with the `head` function here, it comes from
the fact that the input value `xs` accidentally evaluates to `[]`.

I.e.: the error is not in the *consumer*, but in the *producer*.

But GHC will complain with

```text
*** Exception: Prelude.head: empty list
```

and even with a stack trace it doesn't really tell you anything!

However, to find the bug it would be really useful to know: what was evaluated
right before the error occurred?

The Background
---

With Haskell Program Coverage (HPC), GHC allows us to track which
expressions were evaluated during the course of the program: whenever an
expression is evaluated, it bumps a number in an array (a "tick") and at
the end you can read out how many times each expression was evaluated.

That doesn't really help us so much here though, we might encounter the
error after a long execution, and we really only want to know what happened
immediately prior.

The Idea
---

The idea is to extend HPC with a "trace" data structure that's simply an
array that keeps track of which expression was evaluated, and an index
that goes in circles around the array. This way we can track e.g. the 100
(configureable) most recently evaluated expressions, and when the crash)
happens we make sure to dump this trace into a file. We can then read
the dump and see, a-ha!, `xs` was being evaluated! This can help us
track down the source of the error more easily than just keeping track
of what was evaluated, and can be very helpful for tools like [PropR](https://github.com/Tritlo/PropR).

The Progress
---
Unfortunately, I'm still hacking it together and don't have any results to show
yet (running a compiled program with the change gives a segmentation fault 😅😅😅),
but hopefully I'll get it working soon. Until next time!
