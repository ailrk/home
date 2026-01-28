---
title: Calculus [In Brief Words]
date: 2026-01-18
description: ""
tags: en
---

Calculus is about divide and conquer. We first break a whole into infinite pieces, then solve the problem with small pieces, finally we reassemble pieces to get final answer for the whole.

Derivative and integral are two tools we use to solve problems on small pieces. Derivatives is about seeing how things are changing on a specific point. Integral is about summing infinite pieces together to get the total value.

Imagine a complex curve on 2D coordinate, by zooming it infinitely far, the local behavior looks like a straight line. It''s called linearization. $dy$ is the infinitesimal piece of y, dx is the infinitesimal piece of x. Their ratio: $\frac{dy}{dx}$ is called the derivative. By summing  the area of every single rectangle formed by infinitesimal x and y, we can find the area beneath the curve, this is called integral.

Derivative and integral seems to be solving two different types of problems, why they are inverse of each other? It's because the height of the pieces is the rate at which the total area grows.

Imagine area function $A(x)$ represents the total sum of all tiny rectangles from the start up to $x$ under the function $f$. Now if we add one more tiny rectangle of width $dx$, the area grows by $dA$, the new piece is a rectangle with width $dx$ and height $f(x)$.

> $dA = f(x) \cdot dx$
>
> $\Rightarrow  \frac{dA}{dx} = f(x)$
>
> $\Rightarrow  \frac{d}{dx} \int_{a}^{x}f(x)dt = f(x)$


This is exactly saying derivative of integral of f is f itself. This is called the Fundamental theorem of Calculus.

We know algebra and geometry, they are studying static objects. On the other hand, calculus is the math of continous change. Derivative is about instantaneous change, and the integral is about accumulative change. The physical world we live in is filled with changing, continous values: position, speed, time, etc. Calculus give us a way to describe these changes.

This is the essence of calculus. Multivariable calculus, partial differential equations, different cool integral all based on this very idea.
