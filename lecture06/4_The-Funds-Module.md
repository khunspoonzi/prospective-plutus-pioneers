# [The Funds Module](https://youtu.be/24SHPHEc3zo?t=5682)

In this section, we’ll explore how to define a couple helper contracts concerned with checking funds.

## Function: [ownFunds](https://youtu.be/24SHPHEc3zo?t=5696)

The `ownFunds` function returns a sum of all values contained in one's own UTxOs:

```haskell
ownFunds :: Contract w s Text Value
ownFunds = do

    -- Get own public key
    pk    <- ownPubKey

    -- Get UTxOs at the address of own public key (UTxOs sitting in own wallet)
    utxos <- utxoAt $ pubKeyAddress pk

    -- Get sum of all values of all of own UTxOs
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut <$> utxos

    -- Log string of own funds
    logInfo @String $ "own funds: " ++ show (Value.flattenValue v)

    -- Return value of all funds
    return v
```

## Function: [ownFunds'](https://youtu.be/24SHPHEc3zo?t=5806)

The `ownFunds'` function is a variation that permanently tells the value of ownFunds rather than return it:

```haskell
ownFunds' :: Contract (Last Value) Empty Text ()
ownFunds' = do
    -- Call ownFunds and then tells the Last Just of that value, handling and logging errors
    handleError logError $ ownFunds >>= tell . Last . Just

    -- Wait one slot and recurse
    void $ Contract.waitNSlots 1
    ownFunds'
```
