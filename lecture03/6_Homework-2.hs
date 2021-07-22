----------------------------------------------------------------------------------------
-- EXTENSIONS
----------------------------------------------------------------------------------------

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

----------------------------------------------------------------------------------------
-- IMPORTS
----------------------------------------------------------------------------------------

module Week03.Homework2 where

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
import           PlutusTx.Prelude        hiding ( Semigroup(..)
                                                , unless
                                                )
import           Prelude                        ( IO
                                                , Semigroup(..)
                                                , Show(..)
                                                , String
                                                , undefined
                                                )
import           Text.Printf                    ( printf )

----------------------------------------------------------------------------------------
-- MK VALIDATOR
----------------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkValidator b d () ctx =

  traceIfFalse "Beneficiary signature missing" signedByBeneficiary
    && traceIfFalse "Deadline not yet reached" deadlineReached

 where

  info :: TxInfo
  info = scriptContextTxInfo ctx

  signedByBeneficiary :: Bool
  signedByBeneficiary = txSignedBy info b

  deadlineReached :: Bool
  deadlineReached = contains (from d) $ txInfoValidRange info

----------------------------------------------------------------------------------------
-- VESTING
----------------------------------------------------------------------------------------

data Vesting
instance Scripts.ValidatorTypes Vesting where
  type DatumType Vesting = POSIXTime
  type RedeemerType Vesting = ()

----------------------------------------------------------------------------------------
-- TYPED VALIDATOR
----------------------------------------------------------------------------------------

typedValidator :: PubKeyHash -> Scripts.TypedValidator Vesting
typedValidator b = Scripts.mkTypedValidator @Vesting
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode b)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @POSIXTime @()

----------------------------------------------------------------------------------------
-- VALIDATOR
----------------------------------------------------------------------------------------

validator :: PubKeyHash -> Validator
validator = Scripts.validatorScript . typedValidator

----------------------------------------------------------------------------------------
-- SRC ADDRESS
----------------------------------------------------------------------------------------

scrAddress :: PubKeyHash -> Ledger.Address
scrAddress = scriptAddress . validator

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
  let p  = gpBeneficiary gp
      d  = gpDeadline gp
      tx = mustPayToTheScript d $ Ada.lovelaceValueOf $ gpAmount gp
  ledgerTx <- submitTxConstraints (typedValidator p) tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
                           (gpAmount gp)
                           (show $ gpBeneficiary gp)
                           (show $ gpDeadline gp)

----------------------------------------------------------------------------------------
-- GRAB
----------------------------------------------------------------------------------------

grab :: forall w s e . AsContractError e => Contract w s e ()
grab = do
  now   <- currentTime
  pkh   <- pubKeyHash <$> ownPubKey
  utxos <- Map.filter (isSuitable now) <$> utxoAt (scrAddress pkh)
  if Map.null utxos
    then logInfo @String $ "no gifts available"
    else do
      let orefs   = fst <$> Map.toList utxos
          lookups = Constraints.unspentOutputs utxos
            <> Constraints.otherScript (validator pkh)
          tx :: TxConstraints Void Void
          tx =
            mconcat
                [ mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData ()
                | oref <- orefs
                ]
              <> mustValidateIn (from now)
      ledgerTx <- submitTxConstraintsWith @Void lookups tx
      void $ awaitTxConfirmed $ txId ledgerTx
      logInfo @String $ "collected gifts"
 where
  isSuitable :: POSIXTime -> TxOutTx -> Bool
  isSuitable now o = case txOutDatumHash $ txOutTxOut o of
    Nothing -> False
    Just h  -> case Map.lookup h $ txData $ txOutTxTx o of
      Nothing        -> False
      Just (Datum e) -> case PlutusTx.fromData e of
        Nothing -> False
        Just d  -> d <= now

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
