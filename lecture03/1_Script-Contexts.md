# [Script Contexts](https://youtu.be/6_rfCCY9_gY?t=55)

Up until this point, we've looked at how to define validators that utilize the datum and redeemer.

Below is a closer look at the data type known as ScriptContext defined in Plutus.V1.Ledger.Contexts.

## Type: [ScriptContext](https://youtu.be/6_rfCCY9_gY?t=132)

The ScriptContext type is defined as follows:

```haskell
data ScriptContext = ScriptContext
  {scriptContextTxInfo   :: TxInfo
  , scriptContextPurpose :: ScriptPurpose
  }
```

Specifically, ScriptContext is defined as a record type with two fields:

1. **scriptContextTxInfo** :: TxInfo
2. **scriptContextPurpose** :: ScriptPurpose

## Type: [TxInfo](https://youtu.be/6_rfCCY9_gY?t=246)

The TxInfo type contains the actual context of of the spending transaction and is defined as follows:

```haskell
data TxInfo = TxInfo
    { txInfoInputs      :: [TxInInfo] -- ^ Transaction inputs
    , txInfoOutputs     :: [TxOut] -- ^ Transaction outputs
    , txInfoFee         :: Value -- ^ The fee paid by this transaction.
    , TxInfo -> Value
txInfoForge             :: Value -- ^ The 'Value' forged by this transaction.
    , txInfoDCert       :: [DCert] -- ^ Digests of certificates included in this transaction
    , txInfoWdrl        :: [(StakingCredential, Integer)] -- ^ Withdrawals
    , txInfoValidRange  :: POSIXTimeRange -- ^ The valid range for the transaction.
    , txInfoSignatories :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoData        :: [(DatumHash, Datum)]
    , txInfoId          :: TxId -- ^ Hash of the pending transaction (excluding witnesses)
    } deriving (Generic)
```



## Type: [ScriptPurpose](https://youtu.be/6_rfCCY9_gY?t=172)

The ScriptPurpose type describes the purpose for which the script is run and is defined as follows:

```haskell
data ScriptPurpose
    = Minting CurrencySymbol -- ^ Describes the circumstances under which a native token can be minted or burned
    | Spending TxOutRef -- ^ Used when validating the spending UTxO input of a transaction
    | Rewarding StakingCredential -- ^ Used within the context of staking
    | Certifying DCert -- ^ Used within the context of delegation certificates
```
