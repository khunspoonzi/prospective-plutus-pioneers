# [The EmulatorTrace Monad](https://youtu.be/g4lvA14I-Jg?t=5340)

In case you were wondering if it is possible to test Plutus contracts without starting a playground server, the answer is yes, through the use of the so-called `EmulatorTrace` monad.

The `EmulatorTrace` monad is responsible for what we've done manually so far in the simulator tab of the Plutus Playground, such as defining the initial conditions and subsequent actions.

It is defined as part of [Plutus.Trace.Emulator](https://play.marlowe-finance.io/doc/haddock/plutus-contract/html/Plutus-Trace-Emulator.html) in the plutus-contract package.

## Function: [runEmulatorTrace](https://youtu.be/g4lvA14I-Jg?t=5500)

The `EmulatorTrace` monad can be run using the so-called `runEmulatorTrace` function:

```haskell
runEmulatorTrace :: EmulatorConfig -> EmulatorTrace () -> ([EmulatorEvent], Maybe EmulatorErr, EmulatorState)
```

Note that the lack of an IO monad in the return type implies that this  is a pure function without side effects.

The `runEmulatorTrace` function expects several custom parameters, described as follows.

### Type: [EmulatorConfig](https://youtu.be/g4lvA14I-Jg?t=5540)

```haskell
data EmulatorConfig =
    EmulatorConfig
        { _initialChainState      :: InitialChainState -- ^ State of the blockchain at the beginning of the simulation. Can be given as a map of funds to wallets, or as a block of transactions.
        } deriving (Eq, Show)
```

#### Type: [InitialChainState](https://youtu.be/g4lvA14I-Jg?t=5548)

```haskell
type InitialChainState = Either InitialDistribution TxPool
```

**Note:** `TxPool` has since been renamed to `Block`

##### Type: [InitialDistribution](https://youtu.be/g4lvA14I-Jg?t=5558)

```haskell
type InitialDistribution = Map Wallet Value
```

**Note:** Value can be either ADA or native tokens.

InitialDistribution can be specified using:

- `defaultDist` which creates 10 wallets with 100 ADA each, or
- `defaultDistFor` which takes a list of wallets and creates 100 ADA for each.

##### Type: [TxPool](https://youtu.be/g4lvA14I-Jg?t=5732)

TxPool, i.e. an initial list of transactions, can be provided instead of an InitialDistribution.

```haskell
type TxPool = [Tx]
```

### More Notes Soon...
