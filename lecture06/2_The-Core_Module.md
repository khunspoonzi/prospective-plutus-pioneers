# [The Core Module](https://youtu.be/24SHPHEc3zo?t=900)

In this section, we'll explore how to define an oracle.

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
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer  -- TxOut = oracle UTxO
oracleValue o f = do
    dh      <- txOutDatum o     -- Get datum hash of oracle
    Datum d <- f dh             -- Turn datum hash into Maybe Datum
    PlutusTx.fromBuiltinData d  -- Turn Maybe Datum into Maybe Integer
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
        Nothing -> traceError "oracle input missing"  -- In case of no input (e.g. policy)
        Just i  -> txInInfoResolved i                 -- Get corresponding TxOut

    -- Check how often the NFT is contained in the value of the oracle output
    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1

    -- Get output going to the oracle address
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o  -- List of all outputs going to the same script address being validated
        _   -> traceError "expected exactly one oracle output"

    -- Check how often the NFT is contained in the value of the oracle output
    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1

    -- Get the expected value of the output datum
    outputDatum :: Maybe Integer
    outputDatum = oracleValue ownOutput (`findDatum` info)

    -- Check that output datum is Just Integer
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

### Type: [Oracling](https://youtu.be/24SHPHEc3zo?t=2000)

The `Oracling` helper type serves to combine the datum and redeemer:

```haskell
data Oracling
instance Scripts.ValidatorTypes Oracling where
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = OracleRedeemer
```

### Function: [typedOracleValidator](https://youtu.be/24SHPHEc3zo?t=2012)

The `typedOracleValidator` function compiles our validator into a typed validator with parameters:

```haskell
typedOracleValidator :: Oracle -> Scripts.TypedValidator Oracling
typedOracleValidator oracle = Scripts.mkTypedValidator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer
```

### Function: [oracleValidator](https://youtu.be/24SHPHEc3zo?t=2032)

The `oracleValidator` function converts our typed validator into a `Validator`:

```haskell
oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . typedOracleValidator
```

### Function: [oracleAddress](https://youtu.be/24SHPHEc3zo?t=2032)

The `oracleAddress` function converts our validator into a `Ledger.Address`:

```haskell
oracleAddress :: Oracle -> Ledger.Address
oracleAddress = scriptAddress . oracleValidator
```

## [Off-chain Script](https://youtu.be/24SHPHEc3zo?t=2046)

### Type: [OracleParams](https://youtu.be/24SHPHEc3zo?t=2126)

The `OracleParams` type is necessary to start the oracle:

```haskell
data OracleParams = OracleParams
    { opFees   :: !Integer
    , opSymbol :: !CurrencySymbol
    , opToken  :: !TokenName
    } deriving (Show, Generic, FromJSON, ToJSON)
```

Notice that an NFT is not included in these params.

### Function: [startOracle](https://youtu.be/24SHPHEc3zo?t=2178)

The `startOracle` function serves no other purpose than to mint the NFT used by the oracle:

```haskell
startOracle :: forall w s. OracleParams -> Contract w s Text Oracle
startOracle op = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    osc <- mapError (pack . show) (mintContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
    let cs     = Currency.currencySymbol osc
        oracle = Oracle
            { oSymbol   = cs
            , oOperator = pkh
            , oFee      = opFees op
            , oAsset    = AssetClass (opSymbol op, opToken op)
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle
```

Here, `mintContract` is used as a general function (which isn't limited to minting NFTs) to mint our NFT. This in turn will create a currency symbol that depends on a unique UTxO.

One small problem that arises from using `mintContract` is that it does not implement the desired error type, i.e. `Text`. To get around this, we use `mapError` that will allow us to convert a contract with error type `e` to a contract with error type `e'`. In this case, we convert `CurrencyError` into a `String` using `show`, and then convert the `String` into `Text` using `pack`.

An exchange rate value is not specified here as the minting process can take several slots at which point an exchange rate might become outdated.

### Function: [updateOracle](https://youtu.be/24SHPHEc3zo?t=2546)

The `updateOracle` helps us deal with two cases:

1. Update an existing oracle value
2. Create an oracle UTxO from a newly started oracle

```haskell
updateOracle :: forall w s. Oracle -> Integer -> Contract w s Text ()
updateOracle oracle x = do
    m <- findOracle oracle  -- Looks up an existing oracle UTxO if exists
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1
    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (typedOracleValidator oracle) c
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show x
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.typedValidatorLookups (typedOracleValidator oracle) <>
                          Constraints.otherScript (oracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show x
```

The above function utilizes the `findOracle` helper defined as follows:

```haskell
findOracle :: forall w s. Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
-- Where TxOutRef is UTxO identifier, TxOutTx is the UTxO itself, and Integer is the exchange rate
findOracle oracle = do

    -- Get all UTxOs sitting at oracle address and filter by those that have th NFT
    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle)

    -- Ensure that we have one UTxO with the desired NFT
    return $ case Map.toList utxos of

        -- Case of one element
        [(oref, o)] -> do

            -- Get the datum value
            x <- oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o

            -- Return the triple
            return (oref, o, x)

        -- Case of no elements
        _           -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1
```

### More Notes Soon...
