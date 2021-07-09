# [Low-level, Untyped, On-chain Validation Scripts](https://youtu.be/sN3BIa3GAOc?t=194)

While off-chain processes occur in wallets and are responsible for constructing and submitting suitable transactions, on-chain processes occur on blockchain nodes and are responsible for validating these transactions.

Specifically, on-chain validation centers on three pieces of data that a Plutus script receives:

1. Datum (from the output being consumed)
2. Redeemer (from the consuming input)
3. Context (from the consuming transaction, its inputs, and outputs)

Within the scope of Plutus, these three pieces of data are represented by a concrete Haskell data type, which under the low-level implementation of Plutus, is a single shared data type called "Data." In practice, more suitable higher-level data types are used as representations.

## Type: [Data](https://youtu.be/sN3BIa3GAOc?t=432)

The low-level Data type is defined as follows:

```haskell
data Data =
    Constr Integer [Data]
  | Map [(Data, Data)]
  | List [Data]
  | I Integer
  | B BS.ByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
```

As shown above, Data has five constructors:

1. **Constr** taking an Integer and a recursive list of Data
2. **Map** taking a list of Data tuples
3. **List** taking a list of Data
4. **I** taking an Integer
5. **B** taking a ByteString

Keep in mind that each of the five constructors above is essentially a function with an arbitrary number of parameters that returns the value of type Data.

The "deriving" keyword above is used to derive the behaviour of Data from existing type classes, i.e.:

- Show
- Eq
- Ord
- Generic
- NFData

Two deriving clauses are used in conjuction with two [deriving strategies](https://typeclasses.com/ghc/deriving-strategies), which specify which deriving mechanism to use for the type classes listed in each clause.

For all intents and purposes, Data can be thought of as a blob type similar to what a JSON object would accomplish in other languages.

More information on Haskell types and typeclasses can be found [here](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#algebraic-data-types).

## [Writing a Validator](https://youtu.be/sN3BIa3GAOc?t=696)

Validators exist as Plutus Core scripts that live on the Cardano blockchain. Fortunately, these can be written in Haskell first and then compiled into Plutus Core. This involves several steps.

### [mkValidator](https://youtu.be/sN3BIa3GAOc?t=744)

First, it is necessary to define a Haskell function (mkValidator) that will represent our validator.

Recall that a Plutus validation script expects three pieces of data: datum, redeemer, and context, which are all represented by type Data at a low level. As such, a validator's function signature is as follows:

```haskell
mkValidator :: Data -> Data -> Data -> ()
```

Notably, the return type for mkValidator is [unit](https://en.wikipedia.org/wiki/Unit_type) which, while uncommon, is similar in use case to the [void type](https://en.wikipedia.org/wiki/Void_type) in other programming languages. That is to say, it carries no useful information because the real value of the function lies in the side effects it produces rather than the value it returns.

Within the context of a validator, there are two possible outcomes to consider:

1. It passes with no side effects and returns unit
2. It fails and produces an error as a side effect

Below is an example of arguably the simplest validator one could write:

```haskell
mkValidator _ _ _ = ()
```

In this case, mkValidator will pass every time because it completely ignores the datum, redeemer, and context by returning unit unconditionally.

## More Notes Soon...
