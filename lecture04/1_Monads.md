# [Monads](https://youtu.be/g4lvA14I-Jg?t=300)

"But of course, you need side effects to have an effect on the world, otherwise all your program does is heat up the processor."

-- Lars Brünjes on Monads and the purity of Haskell

---

Unlike most imperative programming languages, Haskell is considered to be pure in the sense that its functions normally do not produce side effects and are therefore "referentially transparent".

What this means is, given the same arguments, a Haskell function can be expected to produce the same result regardless of where or how many times it is called.

This can be contrasted with impure languages in which function results may (or may not) depend on variable mutations, state changes, or external inputs.

On one hand, referential transparency is one of the key features that makes Haskell relatively safe, predictable, and easy to test. On the other hand, a completely pure programming language devoid of side-effects would be practically useless for most real-world applications.

As such, Monads in Haskell bridge this gap by allowing us to create a kind of container in which computations with side-effects can occur. We'll look at a few examples of how that works now.

## Monad: [IO](https://youtu.be/g4lvA14I-Jg?t=660)

Haskell's IO monad provides a means of dealing with one of the most common forms of impure computation: input and output.

The IO monad -- a built-in primitive -- is a type constructor that takes one argument that represents the expected return type and is used as follows:

```haskell
foo :: IO Int
foo = ...
```

The above example uses IO to describe a function as a list of steps to produce an Int that may inolve side effects.

Notably, referential transparency is not lost when a function that uses an IO monad is evaluated. In other words, the value of that function persists as a kind of recipe until it is actually executed, which only happens in a main function at run time.

Here is an example of Hello World:

```haskell
main :: IO ()
main = putStrLn "Hello World"
```

## [Complex Actions](https://youtu.be/g4lvA14I-Jg?t=1080)

Since no amount of built-ins would be enough to to acommodate a potentially infinite number of use-cases, more complex actions can be achieved by using certain functions and combinators.

### Function: [fmap](https://youtu.be/g4lvA14I-Jg?t=1100)

Monads like IO have a `Functor` instance which has a useful method called fmap:

```haskell
fmap :: (a -> b) -> f a -> f b
```

In practice, fmap allows us to convert `f a` to `f b` given a function that accpets an `a` and returns a `b`. Below is an example of how this would be used to augment an `IO String` monad:

```haskell
import Data.Char

fmap (map toUpper) getLine
```

In the above example, the result of getLine is being transformed before being returned as `IO String` as if `map toUpper` were inserted into the very end of getLine. Notably, this would be different from trying to do the following:

```haskell
(map toUpper) . getLine
```

The above would not work because the return type of `getLine :: IO String` would not match the expected type of `(map toUpper) :: [Char] -> [Char]`

### Operator: [sequence (>>)](https://youtu.be/g4lvA14I-Jg?t=1310)

We can also chain monads like IO together into a defined sequence using the sequence operator:

```haskell
putStrLn "Hello" >> putStrLn "World"
```

In the above example, two IO actions are performed sequentially, with the result of the first being disregarded.

### Operator: [bind (>>=)](https://youtu.be/g4lvA14I-Jg?t=1382)

Similar to the sequence operator is the bind operator, which does not disregard the result of the preceding action.

```haskell
getLine >>= putStrLn
```

In the case of bind, the result of the preceding action is passed into the subsequent action. Here is a more complex example:

```haskell
bar :: IO ()
bar = getLine >>= \s ->
      getLine >>= \t ->
      putStrLn (s ++ t)
```

### Function: [return](https://youtu.be/g4lvA14I-Jg?t=1504)

The return function is useful for converting a general value into a monad, for instance:

```haskell
return "Haskell" :: IO String
```

## Monad: [Maybe](https://youtu.be/g4lvA14I-Jg?t=1730)

Monads [give you superpowers like] "the superpower to be able to fail."

-- Lars Brünjes on Maybe

---

The `Maybe` type is one of Haskell's most useful types and is defined as follows:

```haskell
data Maybe a = Nothing | Just a
```

We see above that `Maybe` has two constructors: `Nothing` and `Just a`. This indicates that `Maybe a` will either be a value of type `a` or `Nothing`. Here is an example of how it might be used:

```haskell
import Text.Read (readMaybe)

foo :: String -> String -> String -> Maybe Int
foo x y z = case readMaybe x of         -- Try to read x from a string to an Int
    Nothing -> Nothing                  -- Return Nothing if k :: Int cannot be derived from x
    Just k -> case readMaybe y of       -- Otherwise try to read y from a string to an Int
        Nothing -> Nothing              -- Return Nothing if l :: Int cannot be derived from y
        Just l -> case readMaybe z of   -- Otherwise try to read z from a string to an Int
            Nothing -> Nothing          -- Return Nothing if m :: Int cannot be derived from z
            Just m -> Just (k + l + m)  -- Otherwise return the sum of k, l, and m as Int
```

The above function accepts three strings, tries to read each as an Int, and then return their sum as an Int.

Because not all string can be read as an Int, we use Maybe as a form of exception handling to immediately return Nothing in case of failure. In other words, if any of the three supplied strings cannot be read as an Int, our function returns Nothing. We can re-factor this function to be more compact:

```haskell
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _  = Nothing
bindMaybe (Just x) f = f x

foo' :: String -> String -> String -> Maybe Int
foo' x y z = readMaybe x `bindMaybe` \k ->
             readMaybe y `bindMaybe` \l ->
             readMaybe z `bindMaybe` \m ->
             Just (k + l + m)
```

In this case, we extract our exception handling logic into a common function called `bindMaybe`, that accepts an `a` or `Nothing`, and a function that accepts the `a` and produces a `b` or `Nothing`.

If the first argument is Nothing, then `bindMaybe` returns `Nothing`. Otherwise, it returns the result of passing the `a` to the function that accepts an `a` and produces a `b` or `Nothing`.

As such, we can use `bindMaybe` to chain each string argument in `foo'` together, returning `Nothing` upon hitting an unreadable string, and returning their sum otherwise.

## Monad: [Either](https://youtu.be/g4lvA14I-Jg?t=2394)

The `Either` type also has two constructors, except that another variable type is used instead of `Nothing`:

```haskell
data Either a b = Left a | Right b
```

Within the context of error- and exception-handling, it might be helpful to think of the `Right` constructor as corresponding to `Maybe`'s `Just`, and the `Left` constructor as corresponding to a more helpful value than `Nothing` such as a `String` error message or an `Int` error code.

Here is an example of how `Either` might be used:

```haskell
import Test.Read (readMaybe)

readEither :: Read a => String -> Either String a
readEither s = case readMaybe s of
    Nothing -> Left $ "Unable to parse: " ++ s
    Just a  -> Right a
```

We could re-write our `foo'` function to produce error messages as follows:

```haskell
import Test.Read (readMaybe)

bindEither :: Either String a -> (a -> Either String b) -> Either String b
bindEither (Left err) _ = Left err
bindEither (Right x) f  = f x

foo'' :: String -> String -> String -> Either String Int
foo'' x y z = readEither x `bindEither` \k ->
              readEither y `bindEither` \l ->
              readEither z `bindEither` \m ->
              Right (k + l + m)
```

## [Writing a Writer](https://youtu.be/g4lvA14I-Jg?t=2874)

Thus far, we've looked at how we can use monads to represent computations that involve side effects as well as to handle computations that can fail.

We can further extend what we've learned about monads to represent computations that produce log output.

### Type: [Writer](https://youtu.be/g4lvA14I-Jg?t=2918)

First, we can define a custom type called `Writer`:

```haskell
data Writer a = Writer a [String]
    deriving Show
```

In this case, `Writer` takes two arguments: a result `a` and a list of log messages, and can be used to define, for example, a function that accepts an `Int` and produces a `Writer Int`:

```haskell
number :: Int -> Writer Int
number n = Writer n $ ["number: " ++ show n]
```

As before, we define a function `foo` that will in this case produce the "sum" of three `Writer Int` values:

```haskell
foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo (Writer k xs) (Writer l ys) (Writer m zs) =
    Writer (k + l + m) $ xs ++ ys ++ zs
```

The result of `foo` might look something like this:

```
Prelude > foo (number 1) (number 2) (number 3)

>> Writer 6 ["number: 1", "number: 2", "number: 3"]
```

We can go ahead and add the sum itself to our list of log messages:

```haskell
tell :: [String] -> Writer ()
tell = Writer ()

foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo' (Writer k xs) (Writer l ys) (Writer m zs) =
    let
      s = k + l + m
      Writer _ us = tell ["sum: " ++ show s]
    in
      Writer s $ xs ++ ys ++ zs ++ us
```

The above `foo'` would give rise to:

```
Prelude > foo' (number 1) (number 2) (number 3)

>> Writer 6 ["number: 1", "number: 2", "number: 3", "sum: 6"]
```

Once more, we can abstract away some of the logic into a separate bind function to make things more robust:

```haskell
bindWriter :: Writer a -> (a -> Writer b) -> Writer b
bindWriter (Writer a xs) f =
  let
    Writer b ys = f a
  in
    Writer b $ xs ++ ys

foo'' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo'' x y z = x `bindWriter` \k ->
              y `bindWriter` \l ->
              z `bindWriter` \m ->
              let s = k + l + m
              in tell ["sum: " ++ show s] `bindWriter` \_ ->
                 Writer s []
```

While not necessarily shorter than `foo'`, `foo''` can be considered more robust in the sense that:

- We no longer need to perform pattern matching on each of the three `Writer Int` arguments
- We no longer need to explicity combine their log messages, reducing potential for mistakes

In contrast to the `Maybe` and `Either` examples which demonstrate how to chain computations that account for failure, our `Writer` example demonstrates how to chain computations that produce log outputs.

## [The Anatomy of a Monad](https://youtu.be/g4lvA14I-Jg?t=3552)

At this point, we're in a good position to define what monads are based on what they're made of and what they do.

For instance, we probably noticed that each of the examples above featured a type constructor with one type parameter:

- `IO a`
- `Maybe a`
- `Either String a`
- `Writer a`

In each case, we made use of a bind function of the same shape to chain their computations together in sequence:

- `IO`: `(>>=)`
- `Maybe`: `bindMaybe`
- `Either`: `bindEither`
- `Writer`: `bindWriter`

Furthermore, we've seen how monads also allow for the possibility for computations that do not produce side effects:

- `IO`: `return`
- `Maybe`: `Just`
- `Either`: `Right`
- `Writer`: `(\a -> Writer a [])`

More concisely, we see that a Monad is described as follows:

```haskell
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```

Correspondingly, its superclass `Applicative` is (partially) described as follows:

```haskell
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b  -- Pronounced ap
```

And the superclass `Functor` is (partially) described as follows:

```haskell
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
```

Note the `fmap` method we worked with earlier.

## Monad: [Writer](https://youtu.be/g4lvA14I-Jg?t=4050)

Given the above information, we could formally define our `Writer` type as a monad:

```haskell
import Control.Monad

instance Functor Writer where
    fmap = liftM

instance Applicative Writer where
    pure  = return
    (<*>) = ap

instance Monad Writer where
    return a = Writer a []
    (>>=) = bindWriter
```

## Function: [threeInts](https://youtu.be/g4lvA14I-Jg?t=4288)

Using and defining monads can often be advantageous since having a set of common methods allows us to define common functions that work for all monads, regardless of their respective implementations.

```haskell
threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts mx my mz =
    mx >>= \k ->
    my >>= \l ->
    mz >>= \m ->
    let s = k + l + m in return s
```

### Example: [Maybe](https://youtu.be/g4lvA14I-Jg?t=4376)

```haskell
-- Original Implementation
foo :: String -> String -> String -> Maybe Int
foo x y z = readMaybe x `bindMaybe` \k ->
            readMaybe y `bindMaybe` \l ->
            readMaybe z `bindMaybe` \m ->
            Just (k + l + m)

-- Monadic Implementation
foo' :: String -> String -> String -> Maybe Int
foo' x y z = threeInts (readMaybe x) (readMaybe y) (readMaybe z)
```

### Example: [Either](https://youtu.be/g4lvA14I-Jg?t=4462)

```haskell
-- Original Implementation
foo :: String -> String -> String -> Either String Int
foo x y z = readEither x `bindEither` \k ->
            readEither y `bindEither` \l ->
            readEither z `bindEither` \m ->
            Right (k + l + m)

-- Monadic Implementation
foo' :: String -> String -> String -> Either String Int
foo' x y z = threeInts (readEither x) (readEither y) (readEither z)
```

### Example: [Writer](https://youtu.be/g4lvA14I-Jg?t=4520)

```haskell
-- Original Implementation
foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo x y z = x `bindWriter` \k ->
            y `bindWriter` \l ->
            z `bindWriter` \m ->
            let s = k + l + m
            in tell ["sum: " ++ show s] `bindWriter` \_ ->
              Writer s []

-- Monadic Implementation
foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo' x y z = threeInts x y z          >>= \s ->
             tell ["sum: " ++ show s] >>
             return s
```

## [Do Notation](https://youtu.be/g4lvA14I-Jg?t=4924)

Because binding monadic computations together with named results as above is so common, a special notation (syntactic sugar) known as "do notation" can be used to the same effect for added convenience. For isntance:

```haskell
-- Original Implementation
threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts mx my mz =
    mx >>= \k ->
    my >>= \l ->
    mz >>= \m ->
    let s = k + l + m in return s

-- Using Do Notation
threeIntsi' :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts' mx my mz = do
    k <- mx
    l <- my
    m <- mz
    let s = k + l + m
    return s
```

### Example: [Writer](https://youtu.be/g4lvA14I-Jg?t=5088)

```haskell
-- Monadic Implementation
foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo x y z = threeInts x y z          >>= \s ->
             tell ["sum: " ++ show s] >>
             return s

-- Using Do Notation
foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo' x y z = do
    s <- threeInts x y z
    tell ["sum: " ++ show s]
    return s
```
