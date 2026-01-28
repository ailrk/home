---
title: Y Combinator in Haskell
date: 2026-01-07
description: ""
tags: en
---


Fix point is defined like this:

```
 fix f = f (fix f)
```

But in lambda calculus, we cannot bind a function to a global name. Instead, we achieve fix point with Y combinator. It's defined like this:


```
\f -> (\x -> f (x x)) (\x -> f (x x))
```


Lambda calculus doesn't natively support recurison, but to find the fix point, you have to refer to the recursor somehow. The double application (applying `(\x -> f (x x))` twice) is needed so you can bind the function into a parameter.

The expansion looks like this:

```
1. (\f -> (\x -> f (x x)) (\x -> f (x x))) F
2. (\x -> F (x x)) (\x -> F (x x))
3. F ((\x -> F (x x)) (\x -> F (x x)))
4. F (F ((\x -> F (x x)) (\x -> F (x x))))
5. F (F (F ((\x -> F (x x)) (\x -> F (x x)))))
6. F (F (F (...)))
```

People are often too eager when they are talking about the Y combinator, but it's important to realize that Y combinator is just a hack for lambda calculus. Our real goal is to define `fix` because that's what gives us recursion. If we are allowed to bind name, Y can be defined and used like this:

```
Y = \f -> (\x -> f (x x)) (\x -> f (x x))
Y g = g (Y g)
```

which is exactly the defintion of `fix`.


### Omega ω

This is `ω`

```
ω = \x -> x x
```

If you apply `ω` to itself:

```
1. ωω
2. (\x -> x x)(\x -> x x)
3. (\x -> x x)(\x -> x x)
...
```

The first ω copies the second one, which form the same structure again. The copy goes on forever. This creates a **"knot"**.

`ω = \x -> x x` cannot be defined in haskell direclty, becasue the type of x is infinite. This is called a **equirecursive** type, which will be rejected by Hindley-Milner type checker.


### Hindley-Milner and Occurs Check

Haskell's HM type checker prevents you from implementing Y combinator!

It prevents a type variable t from unifing with a type contain t itself. This is called **Occurs check**.

Occurs check is required because the type checher need to prevent infinite types in order to make sure the type inference is decidable. But we want to study `ω = \x -> x x`, in which first has `(x :: t -> t0)`, second has `(x :: t)`. To bypass this check, we wrap the recursive x in a type.

In short, usually you can't get `x ~ x -> x1` unified. But you can if you do this:


```haskell
newtype X x = X (X x -> x)
```

Think about `(X x)` as something that can be unified with x. They are isomorphic as long as the recursion goes. This trick is often need in Haskell to work with self-referential structures.

A side note: Did you notice how `(X x)` is at the **contravariant** position? Recursion on contravariant position leads to a type consuming itself, this allows you to implement `Y` combinator.


### Equirecursive vs Isorecursive

Why occurs check is applied on type variable alone but not newtype wrapper? To answer this we need to understand different types of recursive types. We have **equirecursive** type where compiler treats the name and the definition as strictly equal, e.g `(A = B)`; and we have **isorecursive*** type where newtype wrapper is merely isomorphic to its content, the compiler don't treat them as equal. `X ~= X -> Int`.

When you write `newtype X x = X (X x -> x)`, `(X x)` and `(X x -> x)` are distinct types. To go from on to another, you must use the constructor to **wrap** or pattern match to **unwrap** (fold vs unfold). Haskell is isorecursive, hence `a ~ a -> b` is rejected, but `X a ~ X a -> b` is allowed, because X is opaque.

`newtype` introduces a `μ-like` knot that the type checker is forbidden to unfold automatically. Because of this explicit box, the compiler It sees `(X x)` and then stops. The explicit fold/unfold is what replaces the forbidden infinite unification.


### Define Omega with Isorecursive type

Here is an definition of `ω`:

```haskell
ome :: X x -> x
ome = \x@(X x') -> x' x
```

In untyped lambda calculus, `ω` is `\x -> (x x)`. Here we use isorecursive `X x` in the contravariant position, it is isomorphic to `x` but allows the HM type checker to terminate. `ome` is essentially the same as `ω`.

To define `ωω`:

```haskell
omeome :: x
omeome = ome (X ome)
```

Again, `X x` is just an `μ` wrapper.


If you run `omeome`, it will recurse indefinitely. It's a bottom value.


### Define Y combinator with Omega


Now we have understood `ωω`, we can take a step further and  reconstruct `Y` in Haskell. `ome` and `ome'` are isomorphic, we just need to fit the type.


```haskell
y :: (x -> x) -> x
y f = ome ome'
  where
    ome = \x -> f (x (X x))
    ome' = (\(X x) -> f (ome x))
```


This is how you define a real Y combinator in Haskell.
