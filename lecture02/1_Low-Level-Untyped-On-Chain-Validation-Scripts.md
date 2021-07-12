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

### Constructors

As shown above, Data has five constructors:

1. **Constr** taking an Integer and a recursive list of Data
2. **Map** taking a list of Data tuples
3. **List** taking a list of Data
4. **I** taking an Integer
5. **B** taking a ByteString

Keep in mind that each of the five constructors above is essentially a function with an arbitrary number of parameters that returns the value of type Data.

### Deriving Clauses

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

### Function: [mkValidator](https://youtu.be/sN3BIa3GAOc?t=744)

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

### Function: [validator](https://youtu.be/sN3BIa3GAOc)

Once mkValidator has been defined, it needs to be converted into a validator of type Validator by compiling it into a Plutus script using Template Haskell.

```haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```

Here's what's going on under the hood:

1. The Oxford brackets used in `|| mkValidator ||` serve to "quote" the mkValidator function into its underlying syntax tree.

2. The `PlutusTx.compile` receives that syntax tree and compiles it into a Plutus Core syntax tree

3. The splice denoted by `$$` splices the Plutus Core syntax tree as a Plutus Core expression into the source code at that point

4. The `mkValidatorScript` converts the spliced Plutus Core expression into a Validator

Template Haskell is used here because it allows us to evaluate and expand an expression into source code, and then splice that generated source code into the rest of the source code at compile time.

### Pragma: [INLINABLE](https://youtu.be/sN3BIa3GAOc?t=1646)

Because Oxford brackets require all function references to be made inline, it is a necessary final step to add an [INLINABLE pragma](https://wiki.haskell.org/Inlining_and_Specialisation) directive to our mkValidator function.

This gives rise to the following final source code:


```haskell
{-# INLINABLE mkValidator #-}
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```

## [Failing a Validator](https://youtu.be/sN3BIa3GAOc?t=2794)

At this point, we've seen how to write a validator that passes unconditionally.

If we wish to cause a transaction to fail, we need to throw an error. In this case, we do not use the standard Haskell error, which has a signature of:

```haskell
error :: [Char] -> a
```

Instead, we use a custom error of type PlutusTx.Prelude.error, which has a signature of:

```haskell
PlutusTx.Prelude.error :: () -> a
```

The PlutusTx.Prelude.error can be accessed via import after disabling the Haskell implicit Prelude imports:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

module Week02.Example where

import PlutusTx.Prelude hiding (Semigroup(..), unless)
```

Below is an example of a mkValidator function that fails unconditionally:

```Haskell
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = error ()
```

Alternatively, errors can be made more insightful by using a PlutusTx.Prelude.traceError with an accompanied ByteString:

```Haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Week02.Example where

import PlutusTx.Prelude hiding (Semigroup(..), unless)

mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = traceError "Error Message"
```

## Example: [A (Slightly) More Practical Validator](https://youtu.be/sN3BIa3GAOc?t=3270)

Up until this point, we've completely ignored the datum, redeemer, and context when writing validators.

Below is an example of a validator that passes if the value of the redeemer is I 42 of type Data, and fails otherwise:

```haskell
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ r _
    | r == I 42 = ()
    | otherwise = traceError "Incorrect Redeemer"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```

## [Transforming a Validator](https://youtu.be/sN3BIa3GAOc?t=1868)

### Function: [valHash](https://youtu.be/sN3BIa3GAOc?t=1876)

A validator can be transformed into a hash of type Ledger.ValidatorHash using the Script.validatorHash utility:

```haskell
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator
```

### Function: [scrAddress](https://youtu.be/sN3BIa3GAOc?t=1894)

A validator can be also be transformed into a script address of type Ledger.Address using the scriptAdress utility:

```haskell
scrAdress :: Ledger.Address
scrAdress = scriptAdress validator
```

Keep in mind that the validator hash is a primary component of the script address.
