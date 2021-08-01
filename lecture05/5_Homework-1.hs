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

module Week05.Homework1 where

import           Control.Monad           hiding ( fmap )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Default                   ( Default(..) )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           GHC.Generics                   ( Generic )
import           Ledger                  hiding ( mint
                                                , singleton
                                                )
import           Ledger.Constraints            as Constraints
import           Ledger.TimeSlot
import qualified Ledger.Typed.Scripts          as Scripts
import           Ledger.Value                  as Value
import           Playground.Contract            ( ToSchema )
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
                                                )
import           Text.Printf                    ( printf )
import           Wallet.Emulator.Wallet

----------------------------------------------------------------------------------------
-- MK POLICY
----------------------------------------------------------------------------------------

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkPolicy pkh deadline () ctx =
  traceIfFalse "Signature missing" isSigned
    && traceIfFalse "Deadline passed" deadlineNotPassed
 where

    -- Get transaction info
  info :: TxInfo
  info = scriptContextTxInfo ctx

  -- Determine if signed
  isSigned :: Bool
  isSigned = txSignedBy info $ pkh

  -- Determine if deadline has not yet passed
  deadlineNotPassed :: Bool
  deadlineNotPassed = (to deadline) `contains` txInfoValidRange info

----------------------------------------------------------------------------------------
-- POLICY
----------------------------------------------------------------------------------------

policy :: PubKeyHash -> POSIXTime -> Scripts.MintingPolicy
policy pkh deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \pkh' deadline' -> Scripts.wrapMintingPolicy $ mkPolicy pkh' deadline' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
    PlutusTx.liftCode deadline

----------------------------------------------------------------------------------------
-- CUR SYMBOL
----------------------------------------------------------------------------------------

curSymbol :: PubKeyHash -> POSIXTime -> CurrencySymbol
curSymbol pkh deadline = scriptCurrencySymbol $ policy pkh deadline

----------------------------------------------------------------------------------------
-- MINT PARAMS
----------------------------------------------------------------------------------------

data MintParams = MintParams
  { mpTokenName :: !TokenName
  , mpDeadline  :: !POSIXTime
  , mpAmount    :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

----------------------------------------------------------------------------------------
-- SIGNED SCHEMA
----------------------------------------------------------------------------------------

type SignedSchema = Endpoint "mint" MintParams

----------------------------------------------------------------------------------------
-- MINT
----------------------------------------------------------------------------------------

mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  now <- Contract.currentTime
  let deadline = mpDeadline mp
  if now > deadline
    then Contract.logError @String "deadline passed"
    else do
      let
        val = Value.singleton (curSymbol pkh deadline)
                              (mpTokenName mp)
                              (mpAmount mp)
        lookups = Constraints.mintingPolicy $ policy pkh deadline
        tx      = Constraints.mustMintValue val
          <> Constraints.mustValidateIn (to $ now + 5000)
      ledgerTx <- submitTxConstraintsWith @Void lookups tx
      void $ awaitTxConfirmed $ txId ledgerTx
      Contract.logInfo @String $ printf "forged %s" (show val)

----------------------------------------------------------------------------------------
-- ENDPOINTS
----------------------------------------------------------------------------------------

endpoints :: Contract () SignedSchema Text ()
endpoints = mint' >> endpoints where mint' = endpoint @"mint" >>= mint

----------------------------------------------------------------------------------------
-- MK SCHEMA DEFINITIONS
----------------------------------------------------------------------------------------

mkSchemaDefinitions ''SignedSchema

----------------------------------------------------------------------------------------
-- MK KNOWN CURRENCIES
----------------------------------------------------------------------------------------

mkKnownCurrencies []

----------------------------------------------------------------------------------------
-- TEST
----------------------------------------------------------------------------------------

test :: IO ()
test = runEmulatorTraceIO $ do
  let tn       = "ABC"
      deadline = slotToBeginPOSIXTime def 10
  h <- activateContractWallet (Wallet 1) endpoints
  callEndpoint @"mint" h
    $ MintParams { mpTokenName = tn, mpDeadline = deadline, mpAmount = 555 }
  void $ Emulator.waitNSlots 15
  callEndpoint @"mint" h
    $ MintParams { mpTokenName = tn, mpDeadline = deadline, mpAmount = 555 }
  void $ Emulator.waitNSlots 1
