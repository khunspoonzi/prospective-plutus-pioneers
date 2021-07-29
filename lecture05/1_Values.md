# [Values](https://youtu.be/SsaVjSsPPcg?t=124)

When we talked about the [(E)UTxO model](../lecture01/1_The-(E)UTxO-Model.md), we learned about how each (E)UTxO has an address, value, and datum.

In previous examples, we worked with UTxOs that either had an ADA value, or an NFT value in the case of [Alice's auction contract](../lecture01/2_An-Auction-Contract-in-the-(E)UTxO-Model.md). In this section, we'll look at how such values are defined and implemented.

We'll be looking at types from [Plutus.V1.Ledger.Value](https://alpha.marlowe.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Value.html#g:4) and [Plutus.V1.Ledger.Ada](https://alpha.marlowe.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Ada.html) in `plutus-ledger-api`.

## Type: [Value](https://youtu.be/SsaVjSsPPcg?t=146)

The `Value` type has one constructor:

```haskell
newtype Value = Value { getValue :: Map.Map CurrencySymbol (Map.Map TokenName Integer) }
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON, Hashable, NFData)
    deriving newtype (Serialise, PlutusTx.IsData)
    deriving Pretty via (PrettyShow Value)
```

Notably, every native token in Cardano including ADA is identified by two pieces of data, as seen above:

1. `CurrencySymbol :: ByteString`
2. `TokenName :: ByteString`

Together, these two pieces of data form an `AssetClass :: (CurrencySymbol, TokenName)`.

Therefore, a `Value` indicates how many units of each asset class are contained within itself, and looks like this:

```haskell
Value (Map [(,Map [("", 42)]),(a8ff,Map [("ABC", 7),("XYZ", 100)])])
```

In the example above, the empty bytestrings represent the Lovelace `CurrencySymbol` and `TokenName`,  respectively.

The `a8ff` bytestring represents the `CurrencySymbol` of some made up currency, while `ABC` and `XYZ` represent the `TokenName` of two made up tokens.

Importantly, a `CurrencySymbol` must always be represented as a hexadecimal because of how tokens are minted, i.e. they represent the hash of a script or "minting policy," discussed in the next section.


Here are some helpful utility functions related to `Value`:

```haskell
-- Returns the CurrencySymbol of the ADA AssetClass
adaSymbol :: CurrencySymbol

-- Returns the ADA TokenName
adaToken :: TokenName

-- Returns the lovelace Value of a supplied Integer
lovelaceValueOf :: Integer -> Value

-- Constructs and returns a Value based on a CurrencySymbol, TokenName and amount
singleton :: CurrencySymbol -> TokenName -> Integer -> Value

-- Returns the Integer amount associated with a Value's CurrencySymbol and TokenName
-- 0 is returned if the TokenName is not found in Value
valueOf :: Value -> CurrencySymbol -> TokenName -> Integer

-- Returns a flat list of triples from the nested Value structure
flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]

```

Because `Value` is an instance of `Monoid`, we can combine values like so:

```haskell
lovelaceValueOf 123 <> lovelaceValueOf 10
```
