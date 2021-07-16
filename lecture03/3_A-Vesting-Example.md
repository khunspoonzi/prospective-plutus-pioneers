# [A Vesting Example](https://youtu.be/6_rfCCY9_gY?t=1350)

One real-world use case that utilizes intervals and time ranges involves the idea of vesting ADA in a validation script, so that it can be retrieved by a specific address but only after a certain point in time.

## Type: [VestingDatum](https://youtu.be/6_rfCCY9_gY?t=1430)

Within the context of the vesting example, it makes sense to provide two pieces of information in the datum:

```haskell
data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum
```

## Function: [mkValidator](https://youtu.be/6_rfCCY9_gY?t=1490)

At this point, we can use our custom-defined VestingDatum as a parameter in our mkValidator function:

```haskell
{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = traceIfFalse "Beneficiary signature missing" signedByBeneficiary &&
                         traceIfFalse "Deadline not yet reached ... " deadlineReached
  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ beneficiary dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info
```

In this case, we want to pass in an argument of dat :: VestingData as our datum, and an argument of ctx :: ScriptContext as our context. A unit will suffice for our redeemer since the time information that pertains to our vesting example is contained within ctx.

The resulting script contains two conditions which both must evaluate to True:

1. That the transaction was signed by the specified beneficiary, handled by:

```haskell
txSignedBy :: TxInfo -> PubKeyHash -> Bool
```

2. That the transaction deadline has been reached, handled by:

```haskell
contains :: Ord a => Interval a -> Interval a -> Bool
```

## Type: [Vesting](https://youtu.be/6_rfCCY9_gY?t=1866)

As before, we can define a meaningful type for our validator, in this case Vesting:

```haskell
data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = VestingDatum
    type instance RedeemerType Vesting = ()
```

### More notes soon...
