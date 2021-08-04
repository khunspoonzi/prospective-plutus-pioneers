# [The Test Module](https://youtu.be/24SHPHEc3zo?t=5856)

In this section, we’ll explore how to test the modules we've written using the `EmulatorTrace` monad.

## Function: [assetSymbol](https://youtu.be/24SHPHEc3zo?t=5866)

The `assetSymbol` function represents an arbitrary currency symbol to represent the target of the swap:

```haskell
assetSymbol :: CurrencySymbol
assetSymbol = "ff"
```

## Function: [assetToken](https://youtu.be/24SHPHEc3zo?t=5886)

The `assetToken` function represents the target token name to be used in the swap:

```haskell
assetToken :: TokenName
assetToken = "USDT"
```

## Function: [test](https://youtu.be/24SHPHEc3zo?t=5888)

The `test` function will run the `EmulatorTrace` variant with more fine-grained control over the environment:

```haskell
test :: IO ()
test = runEmulatorTraceIO' def emCfg def myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1 .. 10]]

    v :: Value
    v = Ada.lovelaceValueOf                    100_000_000 <>
        Value.singleton assetSymbol assetToken 100_000_000

    -- Everyone starts out with 100 ADA and 100 USDT
```

## Function: [checkOracle](https://youtu.be/24SHPHEc3zo?t=5940)

The `checkOracle` function permanently checks the oracle value and logs it:

```haskell
checkOracle :: Oracle -> Contract () Empty Text a
checkOracle oracle = do
    m <- findOracle oracle
    case m of

        -- Stop of no oracle found
        Nothing        -> return ()

        -- Log the found oracle value
        Just (_, _, x) -> Contract.logInfo $ "Oracle value: " ++ show x

    -- Wait one slot and recurse
    Contract.waitNSlots 1 >> checkOracle oracle
```

## Function: [myTrace](https://youtu.be/24SHPHEc3zo?t=5986)

The `myTrace` function defines our `EmulatorTrace`:

```haskell
myTrace :: EmulatorTrace ()
myTrace = do
    let op = OracleParams
                { opFees = 1_000_000      -- 1 ADA fee
                , opSymbol = assetSymbol  -- ff
                , opToken  = assetToken   -- USDT
                }

    -- Start oracle for wallet 1, wait one slot, and get oracle
    h1 <- activateContractWallet (Wallet 1) $ runOracle op
    void $ Emulator.waitNSlots 1
    oracle <- getOracle h1

    -- Start checkOracle for wallet 2
    void $ activateContractWallet (Wallet 2) $ checkOracle oracle

    -- Initialize oracle to 1.5 USDT per ADA and wait three slots
    callEndpoint @"update" h1 1_500_000
    void $ Emulator.waitNSlots 3

    -- Call ownFunds on wallets 1-5
    void $ activateContractWallet (Wallet 1) ownFunds'
    void $ activateContractWallet (Wallet 3) ownFunds'
    void $ activateContractWallet (Wallet 4) ownFunds'
    void $ activateContractWallet (Wallet 5) ownFunds'

    -- Start the swap contract on wallets 3, 4, 5
    h3 <- activateContractWallet (Wallet 3) $ swap oracle
    h4 <- activateContractWallet (Wallet 4) $ swap oracle
    h5 <- activateContractWallet (Wallet 5) $ swap oracle

    -- Wallet 3 offers 10 ADA, Wallet 4 offers 20 ADA, wait three slots
    callEndpoint @"offer" h3 10_000_000
    callEndpoint @"offer" h4 20_000_000
    void $ Emulator.waitNSlots 3

    -- Wallet 5 uses the swap, picking one of the two according to the exchange rate, wait three slots
    callEndpoint @"use" h5 ()
    void $ Emulator.waitNSlots 3

    -- Wallet 1 updates oracle to 1.7 USDT, wait three slots
    callEndpoint @"update" h1 1_700_000
    void $ Emulator.waitNSlots 3

    -- Wallet 5 tries again with a different exchange rate, wait three slots
    callEndpoint @"use" h5 ()
    void $ Emulator.waitNSlots 3

    -- Wallet 1 updates oracle to 1.8 USDT, collects fees, wait three slots
    callEndpoint @"update" h1 1_800_000
    void $ Emulator.waitNSlots 3

    -- Wallet 3 and 4 retrieve remaining swaps, wait three slots
    callEndpoint @"retrieve" h3 ()
    callEndpoint @"retrieve" h4 ()
    void $ Emulator.waitNSlots 3
  where
    getOracle :: ContractHandle (Last Oracle) OracleSchema Text -> EmulatorTrace Oracle
    getOracle h = do
        -- Get observable start of the runOracle contract
        l <- observableState h
        case l of
            -- Wait one slot if oracle not there yet
            Last Nothing       -> Emulator.waitNSlots 1 >> getOracle h

            -- Log the oracle and return it
            Last (Just oracle) -> Extras.logInfo (show oracle) >> return oracle
```
