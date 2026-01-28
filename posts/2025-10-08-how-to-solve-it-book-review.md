---
title: How to Solve it [Book Review]
date: 2025-10-08
description: ""
tags: en, book, math, code
---


Written in 1945 by George Polya - a Hungarian-American mathematician, the book is not about any specific mathematic topic but on the general methology that helps you to solve any problem. According to George Polya, the framework presented here is not novel, every person who is serious about problem solving will inevitably go through a similar process. Writing down the process explicitly helps us to form a better understanding of the mental model to solve problems.

The book suggests to split problem-solving into four big steps, we can ask a set of questions on each step to solicit right mental activities. First, we have to understand the problem, only by understanding the problem we will be able to see the goal. With a clear goal, we can then device a plan. A plan is a complete blueprint of what needs to be done, this avoids jumping into details too early. With the plan plot out, we can then work out the solution. If the step fails, we devise a different plan, or see if we can solve a simpler problem. Once we get the solution, we need to review our work to integrate the problem into our problem-solving arsenal.

I will list all the questions later, but just listing them is not very interesting. The book gives some very good explainations on why you want to ask certain questions, and I think that's the more valuable part. Here are some interesting concepts that I find useful.

### Think about a similar problem

When tring to understand the problem, Polya suggests us to think about a similar problem. e.g when solving the diagnoal of a parallelepipe, we realize that pythagorean theorem will be useful.

You can't do this for some problems. e.g finding out the number of pi. Archimedes figured that we can use simpler shapes to estimate a circle. By making the simple shape smaller and smaller, the circumference becomes more and more accurate. The difficulty of solving this problem is a lot harder than finding the diagnoal of parallelpipe if we already known pythagorean theorem, because it requires a great deal of creativity.

Lucky for us, most problems are related to other problems. Mathematics is done in an axiomatic system, new knowledge are built on top of old one. as long as we are not dealing with fundamental things like pi, we can almost certainly relate it to something we already known. Otherwise, we might need some phylosophical thinking.

### Auxiliary problems

Sometimes a problem is so difficult that we have no clue where to start. Instead of staring at the problem indefinitely, it's much better if we can find a simpler but related problem and try to solve that instead. By working on auxiliary problems, you implicitly divided the big problem into smaller ones. A simpler problem gives you insights that are hard to see when the problem is too complex, sometimes the insight turns into the key of the original problem. If one related problem is not enough, try more, try to find related problems in different angles. Enough effort will built you a specialized toolkit. At some point you will have collected enough insights you need.

### Analysis and sythesis

Problem solving can be splited into two phrases: analysis and sythesis. We started from the top of the problem and exam what is required. We take the required for granted and draw consequences from it. We keep walking down the problem tree until we reach a point that no more analysis can be done. Then, we solve the terminal problems, get the solution, plugin the solution in to get another solution. We keep doing it until we are back to the original problem, hence the final solution. This process is called synthesis.

This is how structure programming work, when solving a problem, we can write the top level function with helper functions without even defining those helper functions. We assume those functions are defined, then carry out our plan base on that assumption.

It also shows that the solution of a problem is a tree, we naturally perform analysis to work down to the leaf then synthesis the entire tree back. Each sub solution requires a good deal of creativity, but the synthesis is pretty mechanical. This is one of the idea that powers theorem prover.


### Review to create a network of problems

After we solved the problem, we want to go the extra mile to really internalize the solution.

Before jump into a problem, we always want to think about related problems. Most problems require you to know something to proceed, the more you are able to recall, the more likely you devise a plan. To build up a repository of related problem, we have to put into the hard work to solve some real problems.

In grade school, we treat math problems as tasks -- the sooner we complete it the sooner we call it a day. To really train our problem solving muscle, we need to change the atitute. It takes tremendous amount of effort to solve a problem, we definitely want to optimize the utility of it. We want to review the problem, recall how did we solve the problem, what's the key of the solution, and how can the solution been used in other problems.

Knowledge is not just about knowing one thing, it's about knowing many things and the relationship among them. Our end goal is to build a network of problems. When we are solving a new problem, we can first simply navigating the network we already built. If we really can't find something related, it's time to expand the network.


### Solve a problem in different ways

The solution itself is not the end, it's a mean for us to expand the knowledge network. This is why solving a problem in different ways is interesting. We are not interested in the solution, because the solution is already found. We want to see if other idea can lead to a solutoin as well, and how does that other idea relate to other things we know.

Each problem is a note on the network, by finding different ways to relate it to other knowledge, we are increasing the surface area of this problem. The more connected the node is, the more useful it is to solve the next problem.

### Subconsious work

If we tried everything but stil can't find a solution, do not fret. As long as you understood the problem, it will be stored in the back of your head, and the subconcious will take over. Sometimes all you need to do is to go to sleep, and you know you are not wasting time, because your brain is working on it when you are sleeping.

### Question set

Here are the set of question suggested by Polya. We can evoke the right kind of mind set by asking the right question at the right time.


#### Understand the problem

- What's the `unknown`?
- What are the `data`?
- What are the `conditions`?
- Can you `restate the problem` in your own words?
- Can you `rephrase the problem` in another way?
- Can you `draw a figure`?
- What's the `suitable notation`?
- What's the `goal` exactly ?
- Is the `condition sufficient`? Or is something `missing`?
- Do you understand all the `words` used in the problem?


##### Devising a plan

- Have you seen a `similar problem` before?
- Do you know a `related problem`?
- Can you solve a `simpler version`?
- Can you `generalize` the problem?
- Can you `specialize` the problem?
- Can you make the problem `concrete`?
- Can you use an `analogy`?
- Can you solve `part of the problem`?
- Can you think of a `formula or theorem` that applies
- Can you `work backwards`?
- Can you `introduce an auxiliary goal`?
- Can you `change the variable by substitution`?
- Can you `draw a diagram`?


#### Carrying out a plan

- Can you `check each step`?
- Can you `prove` what you are doing is correct?
- Are you using `all the data`?
- Is every transformation `valid`?
- Can you see clearly the `next step`?


#### Looking back

- Can you `check the result`?
- Can you `check the argument`?
- Can you get the result `in a different way`?
- Can you `generalize` the result?
- Can you `specialize` the result?
- Can you applythe solution to `other problems`?
- Can you learn a `method` from this solution?
