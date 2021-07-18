# [Parameterized Contracts](https://youtu.be/6_rfCCY9_gY?t=3114)

Thus far, we have seen how to make use of the redeemer and the datum to pass information into a validator.

In last section's [vesting example](./3_A-Vesting-Example.md), we saw specifically how it would be possible to pass the public key hash of a beneficiary and a deadline represented by a POSIX timestamp into a validation script.

Alternatively, we might also consider "baking" such information into the script itself through parameterization. We'll extend the vesting example to demonstrate how this works.

## Type: [VestingParam](https://youtu.be/6_rfCCY9_gY?t=3206)

The first step in converting our validator into a parameterized script is to substitute what was VestingDatum with a new type containing the same information called VestingParam that will represent our parameter:

```haskell
data VestingParam = VestingParam
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    } deriving Show
```

In this case, it is not necessary to convert VestingParam into a custom IsData instance like it was with VestingDatum.

## Function: [mkValidator](https://youtu.be/6_rfCCY9_gY?t=3220)

Now that we've defined VestingParam, we can use it to define an additional parameter in our mkValidator function:

```haskell
{-# INLINABLE mkValidator #-}
mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool
mkValidator p () () ctx = traceIfFalse "Beneficiary signature missing" signedByBeneficiary &&
                          traceIfFalse "Deadline not yet reached ... " deadlineReached
  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ beneficiary p

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline p) $ txInfoValidRange info
```

At this point, we demonstrate that the value of the new VestingParam replaces the need for that of the VestingDatum used previously. As such, the value of the script's datum can be set to unit.

## Type: [Vesting](https://youtu.be/6_rfCCY9_gY?t=3286)

We can update the corresponding Vesting type accordingly:

```haskell
data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = ()
    type instance RedeemerType Vesting = ()
```

## Function: [typedValidator](https://youtu.be/6_rfCCY9_gY?t=3294)

Because of VestingParam, the value of our typedValidator function is no longer constant but rather depends on a parameter: p. Intuitively, it would look something like the following:

```haskell
typedValidator :: VestingParam -> Scripts.TypedValidator Vesting
typedValidator p = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator p ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()
```

Importantly however, the above code **will not** compile successfully.

As discussed [previously](../lecture02/1_Low-Level-Untyped-On-Chain-Validation-Scripts.md#pragma-inlinable) concerning template Haskell, everything specified within Oxford brackets must be known at compile time.

In the example above however, the the value of mkValidator is known at compile time, but the value of p is only known at run time. As such we cannot simply apply p to mkValidator when compiling Plutus Core.

### More notes soon...
