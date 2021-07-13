# [High-level, Typed, On-chain Validation Scripts](https://youtu.be/sN3BIa3GAOc?t=3526)

In practice, it would be uncommon for anyone to write the kind of low-level validator implementation we've seen thus far. This is because the low-level implementation is overly general and unintuitive for real-world use cases, particularly when we have access to higher-level types and utilities.

## Example: [A Higher-level (Slightly) More Practical Validator](https://youtu.be/sN3BIa3GAOc?t=3592)

Below is a higher-level implementation of the [(slightly) more practical validator](./1_Low-Level-Untyped-On-Chain-Validation-Scripts.md#example-a-slightly-more-practical-validator) example we saw in the previous section:

```haskell
mkValidator :: () -> Integer -> ScriptContext -> Bool
mkValidator _ r _ = traceIfFalse "Incorrect Redeemer" $ r == 42
```

In the above implementation, we are able to provide a much stronger type definition for mkValidator's expected argument parameters, as opposed to the much more general expectation of three instances of Data.

In this case, we indicate that we expect a unit for the datum (as we wish to ignore it), an Integer type for the redeemer, and a ScriptContext type to represent the context.

Notice also that mkValidator no longer returns a unit (which is unusal in Haskell), but instead a Bool to represent whether the validation has passed or failed.

PlutusTx.Prelude.traceIfFalse is used in place of PlutusTx.Prelude.traceError, and it takes a string and a boolean as arguments, returns the boolean, and logs the string if the boolean is False, i.e. the validation fails.

## [Writing a Typed Validator](https://youtu.be/sN3BIa3GAOc?t=3736)

Unfortunately, converting a mkValidator function into a validator under a higher-level implementation requires some extra boilerplate in order to work.

### Type: [Typed](https://youtu.be/sN3BIa3GAOc?t=3748)

First of all, we need to introduce a dummy data type called Typed (in this case) that derives from Scripts.ValidatorTypes where the Typed instance of DatumType is a unit and the Typed instance of RedeemerType is an Integer, as per mkValidator.

```haskell
data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = Integer
```

### Function: [typedValidator](https://youtu.be/sN3BIa3GAOc?t=3784)

In order to compile mkValidator, Scripts.mkTypedValidator is used with Typed supplied as a type argument:

```haskell
typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @Integer
```

As before, we use PlutusTx.compile to compile mkValidator, but then also compile a wrap function using Scripts.wrapValidator that accepts datum and redeemer type arguments so that the typed validator can be interpretted as an untyped validator.

Specifically, the wrap function handles conversion of the datum, redeemer, and context types, (e.g. Unit, Integer, and ScriptContext) into Data. This is done using the PlutusTx.IsData class which houses two related functions:

1. **toData,** which converts a value of type a to Data
2. **fromData,** which converts Data to a value of type Maybe a

### Function: [validator](https://youtu.be/sN3BIa3GAOc?t=3858)

Once mkValidator has been compiled into a typed validator, it needs to be converted into an untyped validator using Scripts.validatorScript:

```haskell
validator :: Validator
validator = Scripts.validatorScript typedValidator
```

## [Transforming a Typed Validator](https://youtu.be/sN3BIa3GAOc?t=3872)

### Function: [valHash](https://youtu.be/sN3BIa3GAOc?t=3872)

A typed validator can be transformed into a hash of type Ledger.ValidatorHash using the Scripts.validatorHash utility:

```haskell
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator
```

**Note:** Scripts.validatorHash comes from a different module than in the case of an untyped validator in the previous section!

### Function: [scrAddress](https://youtu.be/sN3BIa3GAOc?t=3898)

A validator can be also be transformed into a script address of type Ledger.Address using the scriptAdress utility:

```haskell
scrAdress :: Ledger.Address
scrAdress = scriptAdress validator
```

Keep in mind that the validator hash is a primary component of the script address.

## More Notes Soon...
