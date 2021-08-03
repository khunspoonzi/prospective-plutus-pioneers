# [The Swap Module](https://youtu.be/24SHPHEc3zo?t=3672)

In this section, we’ll explore how to define a swap contract that uses an oracle.

## [On-chain Script](https://youtu.be/24SHPHEc3zo?t=3696)

### Function: [price](https://youtu.be/24SHPHEc3zo?t=3696)

The `price` function determines the USD amount to pay for the offered ADA using the oracle exchange rate.

```haskell
{-# INLINABLE price #-}
price :: Integer -> Integer -> Integer
price lovelace exchangeRate = (lovelace * exchangeRate) `divide` 1000000
```

Recall that we have established the exchange rate to be an integer representing the `double` value * 1 million.

### Function: [lovelaces](https://youtu.be/24SHPHEc3zo?t=3758)

The `lovelaces` function extracts the integer value from a Lovelace `Value`.

```haskell
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue
```

### Function: [mkSwapValidator](https://youtu.be/24SHPHEc3zo?t=3786)

The `mkSwapValidator` will check that the necessary conditions are met for the swap.

```haskell
{-# INLINABLE mkSwapValidator #-}

-- Oracle, Oracle Address, Pub Key Hash of Seller, No Redeemer
-- Oracle Address explicitly passed because function to extract it is not compatible with Plutus
mkSwapValidator :: Oracle -> Address -> PubKeyHash -> () -> ScriptContext -> Bool
mkSwapValidator oracle addr pkh () ctx =

    -- Transaction signed by seller if he wants to retrieve his assets and cancel swap, OR
    txSignedBy info pkh ||

    -- There are two script inputs: oracle and swap UTxO itself (others like fees are pub key inputs)
    -- The seller got paid
    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs &&
     traceIfFalse "price not paid"                     sellerPaid)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Find oracle input
    oracleInput :: TxOut
    oracleInput =
      let
        -- Get output for each input if the output address is the oracle address
        ins = [ o
              | i <- txInfoInputs info
              , let o = txInInfoResolved i
              , txOutAddress o == addr
              ]
      in
        case ins of

            -- Case of exactly one oracle input
            [o] -> o

            -- All other cases
            _   -> traceError "expected exactly one oracle input"

    -- Check oracle exchange rate using helper defined previously
    oracleValue' = case oracleValue oracleInput (`findDatum` info) of
        Nothing -> traceError "oracle value not found"
        Just x  -> x

    hasTwoScriptInputs :: Bool
    hasTwoScriptInputs =
      let
        -- Filter inputs --> output --> output address --> validator hash if script output --> is just
        xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
      in
        -- Ensure that script input length is 2
        length xs == 2

    minPrice :: Integer
    minPrice =
      let
        -- Get input currently being validated (swap) and get lovelace value
        lovelaceIn = case findOwnInput ctx of
            Nothing -> traceError "own input not found"
            Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
      in
        price lovelaceIn oracleValue'

    sellerPaid :: Bool
    sellerPaid =
      let
        -- Check how much he did pay
        -- Adds up all values of all the pubkey outputs that goes to this address
        -- Total value sent by this address to the seller
        -- The USD tokens sent to the seller by this transaction
        -- Allows overpaying
        pricePaid :: Integer
        pricePaid =  assetClassValueOf (valuePaidTo info pkh) (oAsset oracle)
      in
        pricePaid >= minPrice
```

We do not have to check that the NFT is present because this happens in the core module.

### Type: [Swapping](https://youtu.be/24SHPHEc3zo?t=4414)

The `Swapping` helper type serves to combine the datum and redeemer:

```haskell
data Swapping
instance Scripts.ValidatorTypes Swapping where
    type instance DatumType Swapping = PubKeyHash
    type instance RedeemerType Swapping = ()
```

### Function: [typedSwapValidator](https://youtu.be/24SHPHEc3zo?t=4430)

The `typedSwapValidator` function compiles our validator into a typed validator with parameters:

```haskell
typedSwapValidator :: Oracle -> Scripts.TypedValidator Swapping
typedSwapValidator oracle = Scripts.mkTypedValidator @Swapping
    ($$(PlutusTx.compile [|| mkSwapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode oracle
        `PlutusTx.applyCode` PlutusTx.liftCode (oracleAddress oracle))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PubKeyHash @()
```

In this case, we actually compute the oracle address from the oracle.

### Function: [swapValidator](https://youtu.be/24SHPHEc3zo?t=4470)

The `swapValidator` function converts our typed validator into a Validator:

```haskell
swapValidator :: Oracle -> Validator
swapValidator = Scripts.validatorScript . typedSwapValidator
```

### Function: [swapAddress](https://youtu.be/24SHPHEc3zo?t=4470)

The `swapAddress` function converts our validator into a Ledger.Address:

```haskell
swapAddress :: Oracle -> Ledger.Address
swapAddress = scriptAddress . swapValidator
```

### Function: [offerSwap](https://youtu.be/24SHPHEc3zo?t=4484)

The `offerSwap` function allows a seller to offer a swap for his ADA:

```haskell
offerSwap :: forall w s. Oracle -> Integer -> Contract w s Text ()
offerSwap oracle amt = do
    -- Look up seller's public key
    pkh <- pubKeyHash <$> Contract.ownPubKey

    -- Define a constraint that an amount of ADA must be paid to the script for the swap
    let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt

    -- Submit transaction, wait for confirmation, and log info
    ledgerTx <- submitTxConstraints (typedSwapValidator oracle) tx
    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"
```

### Function: [findSwaps](https://youtu.be/24SHPHEc3zo?t=4542)

The `findSwaps` function finds all swaps that satisfy a specific predicate:

```haskell
findSwaps :: Oracle -> (PubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, TxOutTx, PubKeyHash)]
findSwaps oracle p = do
    -- Get all UTxOs sitting at swap address
    utxos <- utxoAt $ swapAddress oracle

    -- Return the result of map maybe where all elements are Just x
    return $ mapMaybe g $ Map.toList utxos
  where
    f :: TxOutTx -> Maybe PubKeyHash
    f o = do

        -- Get datum hash attached to output
        dh        <- txOutDatumHash $ txOutTxOut o

        -- Get datum
        (Datum d) <- Map.lookup dh $ txData $ txOutTxTx o

        -- Deserialize datum to pubkeyhash
        PlutusTx.fromBuiltinData d

    g :: (TxOutRef, TxOutTx) -> Maybe (TxOutRef, TxOutTx, PubKeyHash)
    g (oref, o) = do

        -- Get public key hash from datum
        pkh <- f o

        -- Apply guard, which fails (returns Nothing) if a boolean is False and drops this UTxO
        guard $ p pkh

        -- Return the triple otherwise
        return (oref, o, pkh)
```

### Function: [retrieveSwaps](https://youtu.be/24SHPHEc3zo?t=4864)

The `retrieveSwaps` function enables the seller to cancel his swap offer and retrieve his unswapped ADA:

```haskell
retrieveSwaps :: Oracle -> Contract w s Text ()
retrieveSwaps oracle = do
    -- Get the seller's public key hash
    pkh <- pubKeyHash <$> ownPubKey

    -- Get UTxOs sitting at swap address that belong to seller
    xs  <- findSwaps oracle (== pkh)
    case xs of

        -- Case of no UTxOs
        [] -> logInfo @String "no swaps found"

        -- Retrieve all UTxOs
        _  -> do

            -- Create a constraint to spend each UTxO
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <>
                          Constraints.otherScript (swapValidator oracle)

                -- Concatenate list of constraints
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData () | (oref, _, _) <- xs]

                -- Submit transaction, wait for confirmation, and log info
            ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)"
```

### Function: [useSwap](https://youtu.be/24SHPHEc3zo?t=5064)

The `useSwap` function will finally make use of the oracle:

```haskell
useSwap :: forall w s. Oracle -> Contract w s Text ()
useSwap oracle = do
    -- Look up own funds (all funds in my wallet)
    funds <- ownFunds

    -- Get amount of target asset, e.g. USD
    let amt = assetClassValueOf funds $ oAsset oracle
    logInfo @String $ "available assets: " ++ show amt

    -- Find the oracle UTxO
    m <- findOracle oracle
    case m of
        -- Log message if could not find oracle
        Nothing           -> logInfo @String "oracle not found"

        -- Otherwise, if found...
        Just (oref, o, x) -> do

            -- Log message to indicate oracle found
            logInfo @String $ "found oracle, exchange rate " ++ show x

            -- Check own public key
            pkh   <- pubKeyHash <$> Contract.ownPubKey

            -- Get all swaps where we are not the owner
            swaps <- findSwaps oracle (/= pkh)

            -- Find a swap we can afford
            case find (f amt x) swaps of
                Nothing                -> logInfo @String "no suitable swap found"
                Just (oref', o', pkh') -> do

                        -- Get value already in the oracle (NFT + fees) and add my own fee
                    let v       = txOutValue (txOutTxOut o) <> lovelaceValueOf (oFee oracle)

                        -- Get price I have to pay
                        p       = assetClassValue (oAsset oracle) $ price (lovelaces $ txOutValue $ txOutTxOut o') x
                                  -- Provide validator of the oracle contract
                        lookups = Constraints.otherScript (swapValidator oracle)                     <>
                                  -- Provide the validator of the swap contract
                                  Constraints.otherScript (oracleValidator oracle)                   <>
                                  -- Provide the two UTxOs we want to consume
                                  Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])
                                  -- Must consume oracle input as input with Use redeemer
                        tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toBuiltinData Use) <>
                                  -- Consume swap input
                                  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ())  <>
                                  -- Pay v to oracle script, using existing oracle datum
                                  Constraints.mustPayToOtherScript
                                    (validatorHash $ oracleValidator oracle)
                                    (Datum $ PlutusTx.toBuiltinData x)
                                    v                                                                             <>
                                  -- Pay the seller of the ADA
                                  Constraints.mustPayToPubKey pkh' p

                    -- Submit, wait, and log info
                    ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                    awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "made swap with price " ++ show (Value.flattenValue p)
  where
    getPrice :: Integer -> TxOutTx -> Integer
    getPrice x o = price (lovelaces $ txOutValue $ txOutTxOut o) x

    -- Amount we have, assets we own, and current exchange rate
    f :: Integer -> Integer -> (TxOutRef, TxOutTx, PubKeyHash) -> Bool

    -- Return swap where the price is at most the amount we own
    -- i.e. a swap we can afford
    f amt x (_, o, _) = getPrice x o <= amt
```

### Type: [SwapSchema](https://youtu.be/24SHPHEc3zo?t=5502)

The `SwapSchema` will contain four endpoints:

```haskell
type SwapSchema =
            Endpoint "offer"    Integer  -- Offer swap                (seller)
        .\/ Endpoint "retrieve" ()       -- Retrieve swaps            (seller)
        .\/ Endpoint "use"      ()       -- Perform a swap            (buyer)
        .\/ Endpoint "funds"    ()       -- Currently available funds
```

### Function: [swap](https://youtu.be/24SHPHEc3zo?t=5524)

The `swap` function will implement these four endpoints:

```haskell
swap :: Oracle -> Contract (Last Value) SwapSchema Text ()
swap oracle = (offer `select` retrieve `select` use `select` funds) >> swap oracle
  where
    offer :: Contract (Last Value) SwapSchema Text ()
    offer = h $ do
        -- Block until amount is provided
        amt <- endpoint @"offer"
        offerSwap oracle amt

    retrieve :: Contract (Last Value) SwapSchema Text ()
    retrieve = h $ do
        -- Block until invoked
        endpoint @"retrieve"
        retrieveSwaps oracle

    use :: Contract (Last Value) SwapSchema Text ()
    use = h $ do
        -- Block until invoked
        endpoint @"use"
        useSwap oracle

    funds :: Contract (Last Value) SwapSchema Text ()
    funds = h $ do

        -- Blocks until invoked
        endpoint @"funds"

        -- Get value of own funds
        v <- ownFunds

        -- Log own funds to state
        tell $ Last $ Just v

    -- Handle errors, and log error if there is an error
    -- All is wrapped into this handler
    h :: Contract (Last Value) SwapSchema Text () -> Contract (Last Value) SwapSchema Text ()
    h = handleError logError
```

In the example above, the `select` operator will allow our contract to wait until one of the four endpoints is picked, and then execute that one.
