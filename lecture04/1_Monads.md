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
