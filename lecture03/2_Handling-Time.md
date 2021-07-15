# [Handling Time](https://youtu.be/6_rfCCY9_gY?t=422)

We know that one of the advantages of the Cardano (E)UTxO model has over Ethereum is that transaction validation can happen off-chain in the wallet before ever being sent to the blockchain ([more info](../lecture01/1_The-(E)UTxO-Model.md#comparison-cardano-vs-ethereum)).

However, the concept of time serves as one point of uncertainty with respect to this advantage. In other words, time is one factor that might influence whether a transaction is valid, such as in the case of an [auction](../lecture01/2_An-Auction-Contract-in-the-(E)UTxO-Model.md).

It isn't immediately clear how discrepancies in time might be handled between a transaction that is validated in your wallet, compared to when it is actually sent to the blockchain. After all, we wish to ensure that a transaction validated in the wallet is guaranteed to validate in the node.

## Field: [txInfoValidRange](https://youtu.be/6_rfCCY9_gY?t=562)

The txInfoValidRange field belonging to [TxInfo](./1_Script-Contexts.md#type-txinfo) in [ScriptContext](./1_Script-Contexts.md#type-scriptcontext) is used to solve this issue of time validation.

Specifically, txInfoValidRange specifies a time interval in the transaction itself that can be used during a series of general pre-checks before any scripts are actually run. Some of these pre-checks include verifying that:

- All inputs are present
- Relevant balances add up
- Appropriate fees are included
- The current time is valid relative to the time range

If any of the above checks do not pass, then validation fails immediately and no validator scripts are run.

Off-chain validation can therefore be considered deterministic in the sense that the result of the validation script does not depend on when it is run. In other words, the effect of time is extracted so that it does not need to be accounted for in off-chain validation.

All transactions use an infinite time range by default, starting at Cardano's genesis block and continuing indefinitely.

## [POSIX and Slots](https://youtu.be/6_rfCCY9_gY?t=758)

A further time-related problem arises from the fact that Cardano's consensus protocol, [Ouroboros](https://cardano.org/ouroboros/) uses slots instead of POSIX as its native measure of time, unlike Plutus.

This is not an issue insofar as a slot's length is fixed. However, it becomes necessary to account for the possibility that the length of a slot could change in the future for whatever reason.

What this ultimately means is that slot intervals specified in transactions should not have an upper bound that extends too far into the future. In other words, the upper bound should only extend as far into the future as it is possible to know what the slot length will be, which is 36 hours, i.e. the notice period for a change in protocol.

## Type: [Interval](https://youtu.be/6_rfCCY9_gY?t=972)

The underlying type for txInfoValidRange-related types such as POSIXTimeRange and SlotRange is Interval, which is defined as follows:

```haskell
data Interval a = Interval { ivFrom :: LowerBound a, ivTo :: UpperBound a }
    deriving stock (Haskell.Eq, Haskell.Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, Serialise, Hashable, NFData)
```

Without going into the specifics of the type definition itself, it is worth taking note of some of the helpful utility functions for Interval defined [here](https://alpha.marlowe.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Interval.html#t:Interval):

```haskell
-- Checks whether a given a is included in an interval
member :: Ord a => a -> Interval a -> Bool

-- Constructs an interval from a lower and upper bound
interval :: a -> a -> Interval a

-- Constructs an interval from a lower bound that continues indefinitely
from :: a -> Interval a

-- Constructs an interval that stops at an upper bound
to :: a -> Interval a

-- Constructs the infinite interval used by default in a transaction
always :: Interval a

-- Constructs an empty interval
never :: Interval a

-- Constructs an interval with just one bound
singleton :: a -> Interval a

-- Returns the smallest interval that contains two given intervals
hull :: Ord a => Interval a -> Interval a -> Interval a

-- Returns the intersection of two intervals
intersection :: Ord a => Interval a -> Interval a -> Interval a

-- Checks whether two intervals overlap
overlaps :: Ord a => Interval a -> Interval a -> Bool

-- Checks whether one interval contains another
contains :: Ord a => Interval a -> Interval a -> Bool

-- Checks whether an interval is empty
isEmpty :: Ord a => Interval a -> Bool

-- Checks whether the given time is before an interval
before :: Ord a => a -> Interval a -> Bool

-- Checks whether the given time is after an interval
after :: Ord a => a -> Interval a -> Bool

```

### More notes soon...
