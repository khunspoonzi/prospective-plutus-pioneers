# [Automatic Testing Using Emulator Traces](https://youtu.be/zW3D2iM5uVg?t=2016)

Haskell has various testing frameworks that can be used to conduct unit tests.

The testing framework that Plutus uses and builds upon is called [tasty](https://hackage.haskell.org/package/tasty), which groups tests in a `TestTree`.

Plutus test support is provided in [`Plutus.Contract.Test`](https://alpha.marlowe.iohkdev.io/doc/haddock/plutus-contract/html/Plutus-Contract-Test.html) from `plutus-contract`.

## [Checking Predicates](https://youtu.be/zW3D2iM5uVg?t=2096)

### Function: [checkPredicate](https://youtu.be/zW3D2iM5uVg?t=2106)

The `checkPredicate` function produces a tasty-compatible `TestTree`:

```haskell
checkPredicate :: String -> TracePredicate -> EmulatorTrace () -> TestTree
```

In the definition above, `checkPredicate` accepts a descriptive test name, an `EmulatorTrace` as we have used in the past, and a `TracePredicate` that defines a condition that the `EmulatorTrace` should satisfy.

In this case, a `TracePredicate` is defined as follows:

```haskell
type TracePredicate = FoldM (Eff '[Reader InitialDistribution, Error EmulatorFoldErr, Writer (Doc Void)]) EmulatorEvent Bool
```

Instances of `TracePredicate` can be manipulated with the following logical combinators:

```haskell
not :: TracePredicate -> TracePredicate                       -- Negates a predicate
(.&&.) :: TracePredicate -> TracePredicate -> TracePredicate  -- Combines two predicates
```

There are also various assertions that can be used to produce a `TracePredicate`, which can be found [here](https://alpha.marlowe.iohkdev.io/doc/haddock/plutus-contract/html/Plutus-Contract-Test.html).

### Function: [checkPredicateOptions](https://youtu.be/zW3D2iM5uVg?t=2132)

The `checkPredicateOptions` function is a variation on `checkPredicate` which allows for additional options:

```haskell
checkPredicateOptions :: CheckOptions -> String -> TracePredicate -> EmulatorTrace () -> TestTree
```

## [Test Script](https://youtu.be/zW3D2iM5uVg?t=2326)

We can use the above functions to define an emulator trace test on our state machine from the [previous section](./1_Another-State-Machine-Example:-Token-Sale.md).

The `tests` function chains three predicates that ensure the funds of each wallet have changed as expected.

```haskell
tests :: TestTree
tests = checkPredicateOptions
  (defaultCheckOptions & emulatorConfig .~ emCfg)  -- Modify emulatorConfig to have value of emCfg
  "token sale trace"
  (     walletFundsChange (Wallet 1) (Ada.lovelaceValueOf   10_000_000  <> assetClassValue token (-60))
   .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf (-20_000_000) <> assetClassValue token   20)
   .&&. walletFundsChange (Wallet 3) (Ada.lovelaceValueOf (- 5_000_000) <> assetClassValue token    5)
  )
  myTrace
```

The remaining test script would look similar to what we've done in the past:

```haskell
runMyTrace :: IO ()
runMyTrace = runEmulatorTraceIO' def emCfg myTrace

-- Give 1000 ADA and 100 tokens to all three wallets
emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(Wallet w, v) | w <- [1 .. 3]]) def def
  where
    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000 <> assetClassValue token 1000

currency :: CurrencySymbol
currency = "aa"

name :: TokenName
name = "A"

token :: AssetClass
token = AssetClass (currency, name)

myTrace :: EmulatorTrace ()
myTrace = do

    -- Activate start endpoint for wallet 1
    h <- activateContractWallet (Wallet 1) startEndpoint
    callEndpoint @"start" h (currency, name, True)

    -- Wait 5 slots for start and ask for observable state
    void $ Emulator.waitNSlots 5
    Last m <- observableState h  -- Token sale value and thread token

    case m of
        -- Case of token sale not started or error occurred
        Nothing -> Extras.logError @String "error starting token sale"

        -- Case of token sale
        Just ts -> do

          -- Log info
          Extras.logInfo $ "started token sale " ++ show ts

          -- Start useEndpoints parameterized by ts from observableState
          h1 <- activateContractWallet (Wallet 1) $ useEndpoints ts
          h2 <- activateContractWallet (Wallet 2) $ useEndpoints ts
          h3 <- activateContractWallet (Wallet 3) $ useEndpoints ts

          -- Wallet 1 sets price to 1 ADA
          callEndpoint @"set price" h1 1_000_000
          void $ Emulator.waitNSlots 5

          -- Wallet 1 adds 100 tokens to contract
          callEndpoint @"add tokens" h1 100
          void $ Emulator.waitNSlots 5

          -- Wallet 2 buys 20 tokens
          callEndpoint @"buy tokens" h2 20
          void $ Emulator.waitNSlots 5

          -- Wallet 3 buys 5 tokens
          callEndpoint @"buy tokens" h3 5
          void $ Emulator.waitNSlots 5

          -- Wallet 1 withdraws 40 tokens and 10 ADA
          callEndpoint @"withdraw" h1 (40, 10_000_000)
          void $ Emulator.waitNSlots 5
```

We can then run our automated test with the following commands:

```haskell
import Test.Tasty
defaultMain tests
```
