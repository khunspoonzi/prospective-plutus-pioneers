# [Low-level, Untyped, On-chain Validation Scripts](https://youtu.be/sN3BIa3GAOc?t=194)

While off-chain processes occur in wallets and are responsible for constructing and submitting suitable transactions, on-chain processes occur on blockchain nodes and are responsible for validating these transactions.

Specifically, on-chain validation centers on three pieces of data that a Plutus script receives:

1. Datum (located at the UTxO)
2. Redeemer (from the input being validated)
3. Context (the transaction, its inputs, and outputs)

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

More information on Haskell types and typeclasses can be found [here](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#algebraic-data-types).



## More Notes Soon...
