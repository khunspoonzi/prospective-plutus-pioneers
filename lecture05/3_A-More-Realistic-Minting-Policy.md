# [A More Realistic Minting Policy](https://youtu.be/SsaVjSsPPcg?t=1938)

In a more realistic scenario, we might consider writing a minting policy in which minting and burning is only permitted if the related transaction is signed by the owner of a specific public key hash.

## [On-chain Script](https://youtu.be/SsaVjSsPPcg?t=1972)

### Function: [mkPolicy](https://youtu.be/SsaVjSsPPcg?t=1972)

In this case, we'll create a parameterized policy where the parameter will be the public key hash we wish to check:

```haskell
{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) pkh
```

In this case, we require the context so that we can check the signature using `txSignedBy` as before.

### Function: [policy](https://youtu.be/SsaVjSsPPcg?t=2012)

We will need to account for the new parameter when compiling our policy to Plutus Core:

```haskell
policy :: PubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])
    `PlutusTx.applyCode`
    (PlutusTx.liftCode pkh)
```

We accomplish this by again using `PlutusTx.applyCode` and `PlutusTx.liftCode`.

### Function: [curSymbol](https://youtu.be/SsaVjSsPPcg?t=2064)

We will also need to account for the new parameter when deriving the policy's currency symbol:

```haskell
curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy
```

## [Off-chain Script](https://youtu.be/SsaVjSsPPcg?t=2080)

We can test the above minting policy using the following off-chain script:

```haskell
data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type SignedSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey  -- <$> is fmap
    let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.mintingPolicy $ policy pkh
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () SignedSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 555
        }
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 444
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = -222
        }
    void $ Emulator.waitNSlots 1
```
