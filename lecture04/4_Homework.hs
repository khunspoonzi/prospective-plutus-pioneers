----------------------------------------------------------------------------------------
-- EXTENSIONS
----------------------------------------------------------------------------------------

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

----------------------------------------------------------------------------------------
-- IMPORTS
----------------------------------------------------------------------------------------

module Week04.Homework where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Functor                   ( void )
import           Data.Text                      ( Text, unpack )
import           GHC.Generics                   ( Generic )
import           Ledger
import           Ledger.Ada                    as Ada
import           Ledger.Constraints            as Constraints
import           Plutus.Contract               as Contract
import           Plutus.Trace.Emulator         as Emulator
import           Wallet.Emulator.Wallet

----------------------------------------------------------------------------------------
-- PAY PARAMS
----------------------------------------------------------------------------------------

data PayParams = PayParams
  { ppRecipient :: PubKeyHash
  , ppLovelace  :: Integer
  }
  deriving (Show, Generic, FromJSON, ToJSON)

----------------------------------------------------------------------------------------
-- PAY SCHEMA
----------------------------------------------------------------------------------------

type PaySchema = Endpoint "pay" PayParams

----------------------------------------------------------------------------------------
-- PAY CONTRACT
----------------------------------------------------------------------------------------

payContract :: Contract () PaySchema Text ()
payContract = do
  pp <- endpoint @"pay"
  let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
  Contract.handleError (\err -> Contract.logError $ "Caught: " ++ unpack err) $ void $ submitTx tx
  payContract

----------------------------------------------------------------------------------------
-- PAY TRACE
----------------------------------------------------------------------------------------

payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace amt1 amt2 = do

  -- Activate wallet 1
  h1 <- activateContractWallet (Wallet 1) payContract

  -- Get wallet 2 public key hash
  let w2 = pubKeyHash $ walletPubKey $ Wallet 2

  -- Define pay wallet 2 helper function
  let payWallet2 = \amt -> do
        callEndpoint @"pay" h1
          $ PayParams { ppRecipient = w2, ppLovelace = amt }
        Emulator.waitNSlots 1

  -- Pay wallet 2
  void $ payWallet2 amt1 >> payWallet2 amt2

----------------------------------------------------------------------------------------
-- PAY TEST 1
----------------------------------------------------------------------------------------

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

----------------------------------------------------------------------------------------
-- PAY TEST 2
----------------------------------------------------------------------------------------

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000
