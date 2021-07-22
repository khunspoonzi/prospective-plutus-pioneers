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

In the example above however, the the value of mkValidator is known at compile time, but the value of p is only known at run time. As such we cannot simply apply p to mkValidator when compiling Plutus Core at compile time.

Therefore, our only solution will be to compile mkValidator, and then somehow apply a compiled p to that at run time.

Fortunately, because i`p :: VestingParam` is a Data-like type, we can achieve this by using the following PlutusTx utility functions:

```haskell
makeLift :: Name -> Q [Dec]

liftCode :: (Lift uni a, Throwable uni fun, ToBuiltinMeaning uni fun) => a -> CompiledCodeIn uni fun a

applyCode :: (Closed uni, uni `Everywhere` Flat, Flat fun) => CompiledCodeIn uni fun (a -> b) -> CompiledCodeIn uni fun a -> CompiledCodeIn uni fun b
```

The above utility functions can be used as follows:

```haskell
{-# MultiParamTypeClasses #-}

PlutusTx.makeLift ''VestingParam

typedValidator :: VestingParam -> Scripts.TypedValidator Vesting
typedValidator p = Scripts.mkTypedValidator @Vesting
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()
```

## Final Validation Script

Combining the above components, our final off-chain validation script would look like this:

```haskell
{-# MultiParamTypeClasses #-}

data VestingParam = VestingParam
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

PlutusTx.makeLift ''VestingParam

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

data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = ()
    type instance RedeemerType Vesting = ()

typedValidator :: VestingParam -> Scripts.TypedValidator Vesting
typedValidator p = Scripts.mkTypedValidator @Vesting
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

validator :: VestingParam -> Validator
validator = Scripts.validatorScript . typedValidator -- Implicit p applied to composed funtion

valHash :: VestingParam -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator -- Implicit p applied to composed funtion

scrAddress :: VestingParam -> Ledger.Address
scrAddress = scriptAddress . validator -- Implicit p applied to composed funtion
```

**Note:** The above excludes the various pragmas and imports that are assumed as general to all validation scripts.
