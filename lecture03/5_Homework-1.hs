----------------------------------------------------------------------------------------
-- EXTENSIONS
----------------------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

----------------------------------------------------------------------------------------
-- IMPORTS
----------------------------------------------------------------------------------------

module Week03.Homework1 where

import           Control.Monad           hiding ( fmap )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Map                      as Map
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           GHC.Generics                   ( Generic )
import           Ledger                  hiding ( singleton )
import           Ledger.Ada                    as Ada
import           Ledger.Constraints            as Constraints
import qualified Ledger.Typed.Scripts          as Scripts
import           Playground.Contract            ( ToSchema
                                                , ensureKnownCurrencies
                                                , printJson
                                                , printSchemas
                                                , stage
                                                )
import           Playground.TH                  ( mkKnownCurrencies
                                                , mkSchemaDefinitions
                                                )
import           Playground.Types               ( KnownCurrency(..) )
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude        hiding ( unless )
import           Prelude                        ( IO )
import qualified Prelude                       as P
import           Text.Printf                    ( printf )

----------------------------------------------------------------------------------------
-- VESTING DATUM
----------------------------------------------------------------------------------------

data VestingDatum = VestingDatum
  { beneficiary1 :: PubKeyHash
  , beneficiary2 :: PubKeyHash
  , deadline     :: POSIXTime
  }
  deriving P.Show

PlutusTx.unstableMakeIsData ''VestingDatum

----------------------------------------------------------------------------------------
-- MK VALIDATOR
----------------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = signedByBeneficiary && beneficiaryRespectsDeadline
 where

  -- Extract tx info
  info :: TxInfo
  info = scriptContextTxInfo ctx

  -- Determine if beneficiary 1 has signed
  signedByBeneficiary1 :: Bool
  signedByBeneficiary1 = txSignedBy info $ beneficiary1 dat

  -- Determine if beneficiary 2 has signed
  signedByBeneficiary2 :: Bool
  signedByBeneficiary2 = txSignedBy info $ beneficiary2 dat

  -- Determine if either beneficiary has signed
  signedByBeneficiary :: Bool
  signedByBeneficiary = traceIfFalse
    "Beneficiary signature missing"
    (signedByBeneficiary1 || signedByBeneficiary2)

  -- Determine if deadline has passed
  deadlinePassed :: Bool
  deadlinePassed = contains (from $ (1 + deadline dat)) $ txInfoValidRange info

  -- Determine if signing beneficiary respects deadline
  beneficiaryRespectsDeadline :: Bool
  beneficiaryRespectsDeadline
    | signedByBeneficiary1
      && (traceIfFalse "Deadline has already passed" (not deadlinePassed))
    = True
    | signedByBeneficiary2
      && (traceIfFalse "Deadline has not yet passed" deadlinePassed)
    = True
    | otherwise
    = False

----------------------------------------------------------------------------------------
-- VESTING
----------------------------------------------------------------------------------------

data Vesting
instance Scripts.ValidatorTypes Vesting where
  type DatumType Vesting = VestingDatum
  type RedeemerType Vesting = ()

----------------------------------------------------------------------------------------
-- TYPED VALIDATOR
----------------------------------------------------------------------------------------

typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()

----------------------------------------------------------------------------------------
-- VALIDATOR
----------------------------------------------------------------------------------------

validator :: Validator
validator = Scripts.validatorScript typedValidator

----------------------------------------------------------------------------------------
-- VAL HASH / SRC ADDRESS
----------------------------------------------------------------------------------------

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

----------------------------------------------------------------------------------------
-- GIVE PARAMS
----------------------------------------------------------------------------------------

data GiveParams = GiveParams
  { gpBeneficiary :: !PubKeyHash
  , gpDeadline    :: !POSIXTime
  , gpAmount      :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

----------------------------------------------------------------------------------------
-- VESTING SCHEMA
----------------------------------------------------------------------------------------

type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()

----------------------------------------------------------------------------------------
-- GIVE
----------------------------------------------------------------------------------------

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
  pkh <- pubKeyHash <$> ownPubKey
  let dat = VestingDatum { beneficiary1 = gpBeneficiary gp
                         , beneficiary2 = pkh
                         , deadline     = gpDeadline gp
                         }
      tx = mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp
  ledgerTx <- submitTxConstraints typedValidator tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @P.String $ printf
    "made a gift of %d lovelace to %s with deadline %s"
    (gpAmount gp)
    (P.show $ gpBeneficiary gp)
    (P.show $ gpDeadline gp)

----------------------------------------------------------------------------------------
-- GRAB
----------------------------------------------------------------------------------------

grab :: forall w s e . AsContractError e => Contract w s e ()
grab = do
  now   <- currentTime
  pkh   <- pubKeyHash <$> ownPubKey
  utxos <- utxoAt scrAddress
  let utxos1 = Map.filter
        (isSuitable $ \dat -> beneficiary1 dat == pkh && now <= deadline dat)
        utxos
      utxos2 = Map.filter
        (isSuitable $ \dat -> beneficiary2 dat == pkh && now > deadline dat)
        utxos
  logInfo @P.String
    $ printf "found %d gift(s) to grab" (Map.size utxos1 P.+ Map.size utxos2)
  unless (Map.null utxos1) $ do
    let orefs   = fst <$> Map.toList utxos1
        lookups = Constraints.unspentOutputs utxos1
          P.<> Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx =
          mconcat
              [ mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData ()
              | oref <- orefs
              ]
            P.<> mustValidateIn (to now)
    void $ submitTxConstraintsWith @Void lookups tx
  unless (Map.null utxos2) $ do
    let orefs   = fst <$> Map.toList utxos2
        lookups = Constraints.unspentOutputs utxos2
          P.<> Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx =
          mconcat
              [ mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData ()
              | oref <- orefs
              ]
            P.<> mustValidateIn (from now)
    void $ submitTxConstraintsWith @Void lookups tx
 where
  isSuitable :: (VestingDatum -> Bool) -> TxOutTx -> Bool
  isSuitable p o = case txOutDatumHash $ txOutTxOut o of
    Nothing -> False
    Just h  -> case Map.lookup h $ txData $ txOutTxTx o of
      Nothing        -> False
      Just (Datum e) -> maybe False p $ PlutusTx.fromData e

----------------------------------------------------------------------------------------
-- ENDPOINTS
----------------------------------------------------------------------------------------

endpoints :: Contract () VestingSchema Text ()
endpoints = (give' `select` grab') >> endpoints
 where
  give' = endpoint @"give" >>= give
  grab' = endpoint @"grab" >> grab

----------------------------------------------------------------------------------------
-- MK SCHEMA DEFINITIONS
----------------------------------------------------------------------------------------

mkSchemaDefinitions ''VestingSchema

----------------------------------------------------------------------------------------
-- MK KNOWN CURRENCIES
----------------------------------------------------------------------------------------

mkKnownCurrencies []
