---
title: Use free monad
date: 2024-06-05
description: building up a receipt for impure interpretation 
tags: haskell
---

![this is free monad](/images/free-monad-literatelly.jpg)

---

If you have ever used effect systems like `fused-effect` or `polysemy`, you probably saw "free monad" pops up over and over. It is a very widely used technique to manage effects. But what does it mean for a monad to be free? And how does it relate to effect systems? There are a lot of materials on free monad, a lot of them are from a category theory perspective. In this article, I want to give a more straight forward explanation that gives some intuitions of the topic.


## Effects with ADT


```haskell
class Monad m => Console m where
    readLn :: m String
    writeLn :: String -> m ()
```

```haskell
data Console a 
    = WriteLn String a
    | ReadLn (String -> a)
```

```haskell
program = WriteLn "hello" (WriteLn "world" (ReadLn $ \s -> undefined))
```

```haskell
interpret :: Console a -> IO ()
interpret (WriteLn s) = putStrLn "< " << putStrLn s
interpret (ReadLn f) = getLine >>= pure . f
```



### Define operations

### Build up effectful program

### Interpret the program with an interpreter


## Turn effects into a monad




[handle](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html)



## Explanation

### Free


First let's break down the terminology. What does "free" mean? 

The term "free" comes from `free algebra`.



```haskell
class Semigroup m where (<>) :: m -> m -> m

class Semigroup m => Monoid m where
  mempty :: m
  mappend :: m -> m -> m

```


Identity
```
x <> mempty = x
mempty <> x = x
```

Associativity
```
-- Associativity
(x <> y) <> z = x <> (y <> z)
```


-- M is a free monoid over S that satisfy the minial condition for a monoid.
-- M only implies monoid law
-- : 1. associativity
-- : 2. left and right identity
-- and nothing else
```haskell
data S

type M = [S]
```

-- addition on integer is not a free monoid
-- because addition on integer implies commutativity
--



### Monad

Just a quick reminder of monad laws.

```haskell
class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return ::   a               -> m a
```

```
return a >>= k                  =  k a
m        >>= return             =  m
m        >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
```

### Free Monad

So how do we put them together? First you need to understand that free monad is not a class, but a data structure. You define something as a free monad list you define something as a like. If you get a free monad, you have a tangible thing that youcan manipulate.


## Why Free Monad?


### Use free monad as interpreter

```haskell
data ASTF t a
  = Add t t (t -> a)
  | Input (t -> a)
  | Output t a
  deriving Functor


type FreeAST t = Free (ASTF t)

input :: FreeAST t t
input = liftF $ Input id

add :: t -> t -> FreeAST t t
add a b = liftF $ Add a b id

output :: t -> FreeAST t ()
output a = liftF $ Output a ()

program :: (Read a, Show a) => FreeAST a ()
program = do
  x <- input
  y <- input
  res <- add x y
  output res


runFreeAST :: FreeAST Int () -> IO ()
runFreeAST = foldFree interpFreeAST
  where
    interpFreeAST :: ASTF Int x -> IO x
    interpFreeAST (Add x y next) = pure $ next (x + y)
    interpFreeAST (Input next) = do
      val <- read <$> getLine
      return $ next val
    interpFreeAST (Output x next) = do
      return $ next
```

### define state monad with free monad

1. we only defines the command we need
2. the command needs to be a functor
3. use liftF to lift a command into Free to get a monad
4 The functor embeded in Free is automatically a monand
```haskell
newtype StateF s a = StateF { runStateF :: s -> (a, s) } deriving Functor


getF :: StateF s s
getF = StateF $ \s -> (s, s)

putF :: s -> StateF s ()
putF s = StateF $ const ((), s)

type State s = Free (StateF s)

get :: State s s
get = Free $ Pure <$> getF

put :: s -> State s ()
put s = Free $ Pure <$> putF s

-- at this point it's a monad


someComputation :: State Int ()
someComputation = do
  i <- get
  put $ i + 1
  pure ()


-- we need to interpret the Free monad
-- pure and >>= are moved to runState.
-- Free monad doesn't specify what does the monad means, so we need an
-- interpreter to define the semantics.
--
-- This also means we can have different interepreters for the saame
-- free monad.
runState :: State s a -> s -> (a, s)
runState (Pure x) s = (x, s)
runState (Free f) s =
  let (m, s') = runStateF f s
   in runState m s'
```


## Conclusion

A lot of features or techniques in haskell are popularized through research papers, and research papers have a tendency to use precise language. These jargons come from mathematics have their own history of evlolution. They tend to turn into a big dictionary of memes that circulated around a small demographic. This causes a intersting phenomenon that the terminology being used is very different from terminologies used by those who are benefited by the technique. Also it creates a false illusion that in order to master the technique, you need to also master the concept it comes from in math, which is far from the reality. I wasted a lot of time in the second point, and I hope this article can at least save you some time and get exactly what you need for free monad.



-- Free algebra: free means the minial structure for satisfying the condition
-- of being an algebra
--
-- the structure is minially restricted by conditions that is relavent to the law.


--------------------------------- 1.

--------------------------------- 2.
-- with the context above, a free monad is a monad that satisfy and only
-- satisfy minimal monadic laws.
--
-- for monad, we have
-- 1. (m >=> g) >=> h = m >=> (g >=> h)  associtivity
-- 2. return >=> m = m                   left identity
-- 3. m >=> return = m                   right identity
--
-- it's really a monoid




--------------------------------- 3.
-- endo functor
-- a monad is
-- 1. an endofunctor T : X -> X     (* -> *)
-- 2. has a nt mu: T x T -> T       (join :: m (m a) -> m a)
-- 3. has a nt nu: I -> T          (return :: a -> m a)
--
-- monoid law
-- 4. mu . T mu = mu . mu T
-- 5. mu . T nu = nu . mu T = 1


--------------------------------- 4.
-- define a monad without using Monad instance but only free
```haskell
data Free f a
  = Pure a
  | Free (f (Free f a))
```

-- fmap to a free means map f all the way down
```haskell
instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Free fa) = Free (fmap f <$> fa)


instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap


instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= f = f a
  Free fx >>= f = Free ((>>= f) <$> fx)

-- lift any functor value into Free monad
liftF :: Functor f => f a -> Free f a
liftF command = Free (fmap Pure command)


foldFree :: Monad m => (forall x . f x -> m x) -> Free f a -> m a
foldFree _ (Pure a)  = return a
foldFree f (Free as) = f as >>= foldFree f
```


------------------------------------------- edsl


