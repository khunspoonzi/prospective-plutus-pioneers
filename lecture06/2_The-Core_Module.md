# [The Core Module](https://youtu.be/24SHPHEc3zo?t=900)

## [On-chain Script](https://youtu.be/24SHPHEc3zo?t=950)

### Type: [Oracle](https://youtu.be/24SHPHEc3zo?t=950)

The `Oracle` type will serve as a parameter and is defined as a record type with four fields:

```haskell
data Oracle = Oracle
    { oSymbol   :: !CurrencySymbol  -- CurrencySymbol of the NFT, TokenName will be empty string
    , oOperator :: !PubKeyHash      -- Oracle owner, who can make updates
    , oFee      :: !Integer         -- Per-use fees in Lovelace
    , oAsset    :: !AssetClass      -- ADA exchange rate target, e.g. USD
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Oracle
```

### Type: [OracleRedeemer](https://youtu.be/24SHPHEc3zo?t=1040)

The `OracleRedeemer` type will establish the two operations that the oracle will support:

```haskell
data OracleRedeemer = Update | Use
    deriving Show

PlutusTx.unstableMakeIsData ''OracleRedeemer
```

### Function: [oracleTokenName](https://youtu.be/24SHPHEc3zo?t=1068)

The `oracleTokenName` function defines the empty bytestring used as the NFT `TokenName`:

```haskell
{-# INLINABLE oracleTokenName #-}
oracleTokenName :: TokenName
oracleTokenName = TokenName emptyByteString
```

### Function: [oracleAsset](https://youtu.be/24SHPHEc3zo?t=1076)

The `oracleAsset` function is used to identify the NFT asset class:

```haskell
{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)
```

**Note**: Not to be confused with the `oAsset`.

### Function: [oracleValue](https://youtu.be/24SHPHEc3zo?t=1116)

The `oracleValue` function is a helper that extracts the value of the datum, i.e. exchange rate:

```haskell
{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
oracleValue o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d
```

**Note:** Integer (rate * 1 million) is used instead of a double like in the swap example due to  serialization issues.

### Function: [mkOracleValidator](https://youtu.be/24SHPHEc3zo?t=1296)

The `mkOracleValidator` function will check that the necessary conditions are met based on the redeemer:

```haskell
{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oracle x r ctx =
    traceIfFalse "token missing from input"  inputHasToken  &&  -- Checks that input has NFT
    traceIfFalse "token missing from output" outputHasToken &&  -- Checks that output has NFT
    case r of
        Update -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) &&
                  traceIfFalse "invalid output datum"       validOutputDatum
        Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just x)              &&
                  traceIfFalse "fees not paid"              feesPaid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Get the oracle output that is being consumed
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    -- Check how often the NFT is contained in the value of the oracle output
    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1

    -- Get output going to the oracle address
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    -- Check how often the NFT is contained in the value of the oracle output
    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1

    -- Get the expected value of the output datum
    outputDatum :: Maybe Integer
    outputDatum = oracleValue ownOutput (`findDatum` info)

    -- Check that output datum is not Nothing
    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

    -- Check that a fee was paid
    -- Where value is NFT + fees
    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))
```

### More Notes Soon...
