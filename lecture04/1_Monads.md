# [Monads](https://youtu.be/g4lvA14I-Jg?t=300)

"But of course, you need side effects to have an effect on the world, otherwise all your program does is heat up the processor."

-- Lars Brünjes on Monads and the purity of Haskell

---

Unlike most imperative programming languages, Haskell is considered to be pure in the sense that its functions normally do not produce side effects and are therefore "referentially transparent".

What this means is, given the same arguments, a Haskell function can be expected to produce the same result regardless of where or how many times it is called.

This can be contrasted with impure languages in which function results may (or may not) depend on variable mutations, state changes, or external inputs.

On one hand, referential transparency is one of the key features that makes Haskell relatively safe, predictable, and easy to test. On the other hand, a complete exclusion of side-effects would make any programming language practically useless for any real-world application, such as those that deal with external input and output.

## Monad: [IO](https://youtu.be/g4lvA14I-Jg?t=660)

Haskell's IO monad provides one of the most common methods of dealing with an impure computation.

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

Monads like IO have a Functor instance which has a useful method called fmap:

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

## Type: [Maybe](https://youtu.be/g4lvA14I-Jg?t=1730)

The Maybe type is one of Haskell's most useful types and is defined as follows:

```haskell
data Maybe a = Nothing | Just a
```

We see above that Maybe has two constructors: `Nothing` and `Just a`. This indicates that `Maybe a` will either be a value of type `a` or `Nothing`. Here is an example of how it might be used:

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
