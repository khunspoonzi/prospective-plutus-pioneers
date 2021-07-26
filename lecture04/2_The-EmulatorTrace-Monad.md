# [The EmulatorTrace Monad](https://youtu.be/g4lvA14I-Jg?t=5340)

In case you were wondering if it is possible to test Plutus contracts without starting a playground server, the answer is yes, through the use of the so-called `EmulatorTrace` monad.

The `EmulatorTrace` monad is responsible for what we've done manually so far in the simulator tab of the Plutus Playground, such as defining the initial conditions and subsequent actions.

It is defined as part of [Plutus.Trace.Emulator](https://play.marlowe-finance.io/doc/haddock/plutus-contract/html/Plutus-Trace-Emulator.html) in the plutus-contract package.

## [Running an EmulatorTrace](https://youtu.be/g4lvA14I-Jg?t=5500)

### Function: [runEmulatorTrace](https://youtu.be/g4lvA14I-Jg?t=5500)

The `EmulatorTrace` monad can be run using the so-called `runEmulatorTrace` function:

```haskell
runEmulatorTrace :: EmulatorConfig -> FeeConfig -> EmulatorTrace () -> ([EmulatorEvent], Maybe EmulatorErr, EmulatorState)
```

Note that the lack of an IO monad in the return type implies that this  is a pure function without side effects.

The `runEmulatorTrace` function expects several custom parameters, described as follows.

### Type: [EmulatorConfig](https://youtu.be/g4lvA14I-Jg?t=5540)

```haskell
data EmulatorConfig =
    EmulatorConfig
        { _initialChainState      :: InitialChainState -- ^ State of the blockchain at the beginning of the simulation. Can be given as a map of funds to wallets, or as a block of transactions.
        } deriving (Eq, Show)

type InitialChainState = Either InitialDistribution TxPool  -- TxPool is now Block

type InitialDistribution = Map Wallet Value  -- Where value is either ADA or native tokens

type TxPool = [Tx]  -- An initial list of transactions that can be used in place of InitialDistribution
```

InitialDistribution can be specified using:

- `defaultDist` which creates 10 wallets with 100 ADA each, or
- `defaultDistFor` which takes a list of wallets and creates 100 ADA for each.

### Type: [FeeConfig](https://youtu.be/g4lvA14I-Jg?t=5770)

```haskell
data FeeConfig =
    FeeConfig
        { fcConstantFee      :: Ada    -- ^ Constant fee per transaction in lovelace
        , fcScriptsFeeFactor :: Double -- ^ Factor by which to multiply the size-dependent scripts fee
        }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)
```

Since both EmulatorConfig and FeeConfig are instances of Default, we can use `def` to pass in default arguments when calling `runEmulatorTrace`:

```haskell
runEmulatorTrace def def $ return ()  -- To signify the simplest EmulatorTrace we can write
```

As you have probably noticed, this will print a huge list of `EmulatorEvent` items to your console.

Since this is not so useful, we can use the following function instead:

```haskell
runEmulatorTraceIO :: EmulatorTrace () -> IO ()
```
... like so:

```haskell
runEmulatorTraceIO $ return ()
```

Using this function will print a much friendlier summary of the emulator to the console.

If you wish to including arguments corresponding to EmulatorConfig and FeeConfig, you can use:

```haskell
runEmulatorTraceIO' :: TraceConfig -> EmulatorConfig -> FeeConfig -> EmulatorTrace () -> IO ()

data TraceConfig = TraceConfig
  { showEvent    :: EmulatorEvent' -> Maybe String
  -- ^ Function to decide how to print the particular events.
  , outputHandle :: Handle
  -- ^ Where to print the outputs to. Default: 'System.IO.stdout'
  }
```

Note that `def` can also be used with `TraceConfig`.

## [Writing an EmulatorTrace](https://youtu.be/g4lvA14I-Jg?t=6290)

An `EmulatorTrace` can be defined in the following way:

```haskell
myTrace :: EmulatorTrace ()
myTrace = do
    -- Step 1
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints

    -- Step 2
    callEndpoint @"give" h1 $ GiveParams
        { gpBeneficiary = pubKeyHash $ walletPubKey $ Wallet 2
        , gpDeadline    = slotToBeginPOSIXTime def $ Slot 20
        , gpAmount      = 10000000
        }

    -- Step 3
    void $ waitUntilSlot 20

    -- Step 4
    callEndpoint @"grab" h2 ()

    -- Step 5
    s <- waitNSlots 1
    Extras.logInfo $ "Reached " ++ show s
```

### Step 1

We activate the contract in all wallets we're interested in using `activateContractWallet`, where `endpoints` represents the contract that we wish to activate in the context of the playground.

The `h1` and `h2` bindings represent so-called handles, references to a contract instance that is returned each time `activateContractWallet` is called.

### Step 2

We call the `give` endpoint on behalf of wallet 1 using `callEndpoint` and specifying `GiveParams` where the beneficiary = wallet 2, the deadline = slot 20, and the give amount = 10 ADA.

### Step 3

We wait until slot 20 using `waitUntilSlot` and `void` to indicate we are not interested in the slot 20 return value. Note that we would see a warning if we do not use `void` here.

### Step 4

We call the `grab` endpoint on behalf of wallet 2 again using `callEndpoint`, but with no additional parameters.

### Step 5

We wait for 1 slot using `waitNSlots`, this time binding the return value to `s` to use in a log message.

We can then run the `EmulatorTrace` like so:

```haskell
test :: IO ()
test = runEmulatorTraceIO myTrace
```
