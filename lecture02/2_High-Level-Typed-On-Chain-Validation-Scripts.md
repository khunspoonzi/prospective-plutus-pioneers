# [High-level, Typed, On-chain Validation Scripts](https://youtu.be/sN3BIa3GAOc?t=3526)

In practice, it would be uncommon for anyone to write the kind of low-level validator implementation we've seen thus far. This is because the low-level implementation is overly general and unintuitive for real-world use cases, particularly when we have access to higher-level types and utilities.

## Example: [A Higher-level (Slightly) More Practical Validator](https://youtu.be/sN3BIa3GAOc?t=3592)

Below is a higher-level implementation of the [(slightly) more practical validator](./1_Low-Level-Untyped-On-Chain-Validation-Scripts.md#example-a-slightly-more-practical-validator) example we saw in the previous section:

```haskell
mkValidator :: () -> Integer -> ScriptContext -> Bool
mkValidator _ r _ = traceIfFalse "Incorrect Redeemer" $ r == 42

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```

In the above implementation, we are able to provide a much stronger type definition for mkValidator's expected argument parameters, as opposed to the much more general expectation of three instances of Data.

In this case, we indicate that we expect a unit for the datum (as we wish to ignore it), an Integer type for the redeemer, and a ScriptContext type to represent the context.

Notice also that mkValidator no longer returns a unit (which is unusal in Haskell), but instead a Bool to represent whether the validation has passed or failed.

PlutusTx.Prelude.traceIfFalse is used in place of PlutusTx.Prelude.traceError, and it takes a string and a boolean as arguments, returns the boolean, and logs the string if the boolean is False, i.e. the validation fails.


## More Notes Soon...
