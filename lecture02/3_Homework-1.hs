----------------------------------------------------------------------------------------
-- EXTENSIONS
----------------------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
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

module Week02.Homework1 where

import           Control.Monad           hiding ( fmap )
import           Data.Map                      as Map
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Ledger                  hiding ( singleton )
import           Ledger.Ada                    as Ada
import           Ledger.Constraints            as Constraints
import qualified Ledger.Typed.Scripts          as Scripts
import           Playground.Contract            ( ensureKnownCurrencies
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
                                                , String
                                                , undefined
                                                )
import           Text.Printf                    ( printf )

----------------------------------------------------------------------------------------
-- MK VALIDATOR
----------------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: () -> (Bool, Bool) -> ScriptContext -> Bool
mkValidator _ (a, b) _ = traceIfFalse "Booleans do not match!" $ a == b

----------------------------------------------------------------------------------------
-- TYPED
----------------------------------------------------------------------------------------

data Typed
instance Scripts.ValidatorTypes Typed where
  type DatumType Typed = ()
  type RedeemerType Typed = (Bool, Bool)

----------------------------------------------------------------------------------------
-- TYPED VALIDATOR
----------------------------------------------------------------------------------------

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @(Bool, Bool)

----------------------------------------------------------------------------------------
-- VALIDATOR
----------------------------------------------------------------------------------------

validator :: Validator
validator = Scripts.validatorScript typedValidator

----------------------------------------------------------------------------------------
-- VAL HASH / SCR ADDRESS
----------------------------------------------------------------------------------------

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

----------------------------------------------------------------------------------------
-- GIFT SCHEMA
----------------------------------------------------------------------------------------

type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" (Bool, Bool)

----------------------------------------------------------------------------------------
-- GIVE
----------------------------------------------------------------------------------------

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
  let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
  ledgerTx <- submitTxConstraints typedValidator tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @String $ printf "made a gift of %d lovelace" amount

----------------------------------------------------------------------------------------
-- GRAB
----------------------------------------------------------------------------------------

grab :: forall w s e . AsContractError e => (Bool, Bool) -> Contract w s e ()
grab bs = do
  utxos <- utxoAt scrAddress
  let orefs = fst <$> Map.toList utxos
      lookups =
        Constraints.unspentOutputs utxos <> Constraints.otherScript validator
      tx :: TxConstraints Void Void
      tx = mconcat
        [ mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData bs
        | oref <- orefs
        ]
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @String $ "collected gifts"

----------------------------------------------------------------------------------------
-- ENDPOINTS
----------------------------------------------------------------------------------------

endpoints :: Contract () GiftSchema Text ()
endpoints = (give' `select` grab') >> endpoints
 where
  give' = endpoint @"give" >>= give
  grab' = endpoint @"grab" >>= grab

----------------------------------------------------------------------------------------
-- MK SCHEMA DEFINITIONS
----------------------------------------------------------------------------------------

mkSchemaDefinitions ''GiftSchema

----------------------------------------------------------------------------------------
-- MK KNOWN CURRENCIES
----------------------------------------------------------------------------------------

mkKnownCurrencies []
