# [NFTs](https://youtu.be/SsaVjSsPPcg?t=2474)

NFTs or Non-fungible Tokens are tokens that can only exist once.

Importantly, all of the examples of native tokens we've seen so far are by definition not NFTs.

In the [simple minting policy example](./2_A-Simple-Minting-Policy.md), anybody could mint new tokens as they wished, whereas in the [more realistic minting policy example](./3_A-More-Realistic-Minting-Policy.md), only the owner of a specific public key hash could mint new tokens. In both cases, there was no restriction on the amount of tokens that could be minted. This is necessary for an NFT.

### Option 1

A relatively naive approach to implementing such a restriction would be for a minting policy to look at the forge field in the related script context to ensure that the transaction is only minting one token.

This is naive because it does not prevent a given signatory from submitting multiple transactions and therefore does not gurantee that the given token will exist only once.

### Option 2

A second and relatively enforceable approach would be to utilize a deadline after which no new tokens can be minted. This would require a signatory to mint one token before the deadline and then allow the deadline to pass, thus guranteeing that it is unique.

While the second approach is already being used on the Cardano blockchain, it isn't ideal in the sense that a blockchain explorer is required in order to verify that only one token exists. In other words, nothing about the token or its currency symbol necessarily implies its unicity. As such, NFTs on the Cardano blockchain are not true NFTs.

### Option 3

Fortunately, with the introduction of Plutus, it is now possible to mint true NFTs, whereby if you know the policy script that corresponds to the currency symbol, you can be sure that only one such token exists.

This in turn involves preventing there ever being more than one minting transaction of the token in question. That is to say, the corresponding policy script must only result in `True` for one transaction and `False` for any other.

On the surface, this might seem like a significant challenge to implement particularly when trying to imagine how one would prevent a signatory from submitting the same transaction, within the same slot even.

The solution to this challenge is to identify something the policy script can reference on the Cardano blockchain that is unique and only exists in one transaction. As it happens, we can use UTxOs to achieve this.

Throughout the life of the Cardano blockchain, UTxOs are unique and can only exist once. This is true even with two or more UTxOs that go to the same address, with the same value and same datum. That is to say, they will not share the same ID in any case.

We know that UTxO's are unique because they are identified by the transaction for which they are an output, as well as their position in the ordered list of that transaction's total outputs. And we know that transactions are unique because of their UTxO inputs. In theory, it would be possible to have duplicate transactions with no inputs or outputs. In practice however, Cardano's fee system ensures that no transaction can have zero inputs.

We can therefore create true NFTs by naming a specific UTxO as a parameter in our minting policy and then checking that the transaction responsible for minting the NFT also consumes the UTxO in question. This guranttes that there can never be another transaction that consumes the same UTxO.

## [On-chain Script](https://youtu.be/SsaVjSsPPcg?t=3026)

### Function: [mkPolicy](https://youtu.be/SsaVjSsPPcg?t=3026)

In this case, we'll create double-parameterized policy where the first parameter is a reference to the transaction output UTxO and the first parameter is the token name of the NFT:

```haskell
{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed" hasUTxO             &&
                          traceIfFalse "Wrong amount minted" checkMintedAmount
  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Checks whether any input has an output reference equal to (consumes) oref
    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    -- Checks whether the flattened value has own currency symbol with correct token name and amount
    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoForge info) of
        [(cs, tn', amt)] -> cs == ownCurrencySymbol ctx && tn' == tn && amt == 1
        _                -> False

    -- Since we only exactly one token, we could rewrite the above to
    -- i.e. we know that the one currency symbol corresponds to this script
    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoForge info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _                -> False
```

Below are some useful definitions to better understand the above policy:

```haskell
txInfoInputs :: [TxInInfo]

data TxInInfo = TxInInfo
    { txInInfoOutRef   :: TxOutRef
    , txInInfoResolved :: TxOut
    } deriving (Generic)
```

In this case, we require the context so that we can check the signature using `txSignedBy` as before.

### Function: [policy](https://youtu.be/SsaVjSsPPcg?t=3454)

We will need to account for the two parameters when compiling our policy to Plutus Core:

```haskell
policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
```


### Function: [curSymbol](https://youtu.be/SsaVjSsPPcg?t=3504)

We will also need to account for the parameters when deriving the policy's currency symbol:

```haskell
curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol = oref tn = scriptCurrencySymbol $ policy oref tn
```

## [On-chain Script](https://youtu.be/SsaVjSsPPcg?t=3540)

```haskell
type NFTSchema = Endpoint "mint" TokenName

mint :: TokenName -> Contract w NFTSchema Text ()
mint tn = do
    pk    <- Contract.ownPubKey
    utxos <- utxoAt (pubKeyAddress pk)
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let val     = Value.singleton (curSymbol oref tn) tn 1
                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''NFTSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 tn
    callEndpoint @"mint" h2 tn
    void $ Emulator.waitNSlots 1
```
