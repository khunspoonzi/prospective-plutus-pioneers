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

----------------------------------------------------------------------------------------
-- IMPORTS
----------------------------------------------------------------------------------------

module Week05.Homework2 where

import           Control.Monad           hiding ( fmap )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Ledger                  hiding ( mint
                                                , singleton
                                                )
import           Ledger.Constraints            as Constraints
import qualified Ledger.Typed.Scripts          as Scripts
import           Ledger.Value                  as Value
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
import           Plutus.Contract               as Contract
import           Plutus.Trace.Emulator         as Emulator
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
import           Wallet.Emulator.Wallet

----------------------------------------------------------------------------------------
-- TOKEN NAME
----------------------------------------------------------------------------------------

tn :: TokenName
tn = TokenName emptyByteString

----------------------------------------------------------------------------------------
-- MK POLICY
----------------------------------------------------------------------------------------

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy oref () ctx =
  traceIfFalse "UTxO not consumed" hasUTxO
    && traceIfFalse "Wrong amount minted" checkMintedAmount

 where

  -- Get transaction info
  info :: TxInfo
  info = scriptContextTxInfo ctx

  -- Check for UTxO
  hasUTxO :: Bool
  hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

  -- Check minted amount
  checkMintedAmount :: Bool
  checkMintedAmount = case flattenValue (txInfoForge info) of
    [(_, tn', amt)] -> tn' == tn && amt == 1
    _               -> False

----------------------------------------------------------------------------------------
-- POLICY
----------------------------------------------------------------------------------------

policy :: TxOutRef -> Scripts.MintingPolicy
policy oref = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref

----------------------------------------------------------------------------------------
-- CUR SYMBOL
----------------------------------------------------------------------------------------

curSymbol :: TxOutRef -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

----------------------------------------------------------------------------------------
-- NFT SCHEMA
----------------------------------------------------------------------------------------

type NFTSchema = Endpoint "mint" ()

----------------------------------------------------------------------------------------
-- MINT
----------------------------------------------------------------------------------------

mint :: Contract w NFTSchema Text ()
mint = do
  pk    <- Contract.ownPubKey
  utxos <- utxoAt (pubKeyAddress pk)
  case Map.keys utxos of
    []       -> Contract.logError @String "no utxo found"
    oref : _ -> do
      let
        val     = Value.singleton (curSymbol oref) tn 1
        lookups = Constraints.mintingPolicy (policy oref)
          <> Constraints.unspentOutputs utxos
        tx = Constraints.mustMintValue val
          <> Constraints.mustSpendPubKeyOutput oref
      ledgerTx <- submitTxConstraintsWith @Void lookups tx
      void $ awaitTxConfirmed $ txId ledgerTx
      Contract.logInfo @String $ printf "forged %s" (show val)

----------------------------------------------------------------------------------------
-- ENDPOINTS
----------------------------------------------------------------------------------------

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints where mint' = endpoint @"mint" >> mint

----------------------------------------------------------------------------------------
-- MK SCHEMA DEFINITIONS
----------------------------------------------------------------------------------------

mkSchemaDefinitions ''NFTSchema

----------------------------------------------------------------------------------------
-- MK KNOWN CURRENCIES
----------------------------------------------------------------------------------------

mkKnownCurrencies []

----------------------------------------------------------------------------------------
-- TEST
----------------------------------------------------------------------------------------

test :: IO ()
test = runEmulatorTraceIO $ do
  h1 <- activateContractWallet (Wallet 1) endpoints
  h2 <- activateContractWallet (Wallet 2) endpoints
  callEndpoint @"mint" h1 ()
  callEndpoint @"mint" h2 ()
  void $ Emulator.waitNSlots 1
