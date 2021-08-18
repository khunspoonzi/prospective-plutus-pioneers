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

----------------------------------------------------------------------------------------
-- IMPORTS
----------------------------------------------------------------------------------------

module Week08.TokenSaleWithClose
  ( TokenSale(..)
  , TSRedeemer(..)
  , TSStartSchema
  , TSUseSchema
  , startEndpoint
  , useEndpoints
  ) where

import           Control.Monad           hiding ( fmap )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Monoid                    ( Last(..) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           GHC.Generics                   ( Generic )
import           Ledger                  hiding ( singleton )
import           Ledger.Ada                    as Ada
import           Ledger.Constraints            as Constraints
import qualified Ledger.Typed.Scripts          as Scripts
import           Ledger.Value
import           Plutus.Contract               as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude        hiding ( Semigroup(..)
                                                , check
                                                , unless
                                                )
import           Prelude                        ( Semigroup(..)
                                                , Show(..)
                                                , uncurry
                                                )
import qualified Prelude

----------------------------------------------------------------------------------------
-- TOKEN SALE
----------------------------------------------------------------------------------------

data TokenSale = TokenSale
  { tsSeller :: !PubKeyHash
  , tsToken  :: !AssetClass
  , tsTT     :: !(Maybe ThreadToken)
  }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''TokenSale

----------------------------------------------------------------------------------------
-- TS REDEEMER
----------------------------------------------------------------------------------------

data TSRedeemer =
      SetPrice Integer
    | AddTokens Integer
    | BuyTokens Integer
    | Withdraw Integer Integer
    | Close
    deriving (Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''TSRedeemer

----------------------------------------------------------------------------------------
-- LOVELACES
----------------------------------------------------------------------------------------

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

----------------------------------------------------------------------------------------
-- TRANSITION
----------------------------------------------------------------------------------------

{-# INLINABLE transition #-}
transition
  :: TokenSale
  -> State (Maybe Integer)
  -> TSRedeemer
  -> Maybe (TxConstraints Void Void, State (Maybe Integer))

transition ts s r = case (stateValue s, stateData s, r) of

  --------------------------------------------------------------------------------------
  -- SET PRICE
  --------------------------------------------------------------------------------------

  (v, Just _, SetPrice p) | p >= 0 ->
    Just (Constraints.mustBeSignedBy (tsSeller ts), State (Just p) v)

  --------------------------------------------------------------------------------------
  -- ADD TOKENS
  --------------------------------------------------------------------------------------

  (v, Just p, AddTokens n) | n > 0 ->
    Just (mempty, State (Just p) $ v <> assetClassValue (tsToken ts) n)

  --------------------------------------------------------------------------------------
  -- BUY TOKENS
  --------------------------------------------------------------------------------------

  (v, Just p, BuyTokens n) | n > 0 -> Just
    ( mempty
    , State (Just p)
    $  v
    <> assetClassValue (tsToken ts) (negate n)
    <> lovelaceValueOf (n * p)
    )

  --------------------------------------------------------------------------------------
  -- WITHDRAW
  --------------------------------------------------------------------------------------

  (v, Just p, Withdraw n l) | n >= 0 && l >= 0 -> Just
    ( Constraints.mustBeSignedBy (tsSeller ts)
    , State (Just p)
    $  v
    <> assetClassValue (tsToken ts) (negate n)
    <> lovelaceValueOf (negate l)
    )

  --------------------------------------------------------------------------------------
  -- CLOSE
  --------------------------------------------------------------------------------------

  (_, Just _, Close) ->
    Just (Constraints.mustBeSignedBy (tsSeller ts), State Nothing mempty)

  --------------------------------------------------------------------------------------
  -- INVALID ACTION
  --------------------------------------------------------------------------------------

  _ -> Nothing

----------------------------------------------------------------------------------------
-- TS STATE MACHINE
----------------------------------------------------------------------------------------

{-# INLINABLE tsStateMachine #-}
tsStateMachine :: TokenSale -> StateMachine (Maybe Integer) TSRedeemer
tsStateMachine ts = mkStateMachine (tsTT ts) (transition ts) isNothing  -- Closed 

----------------------------------------------------------------------------------------
-- MK TS VALIDATOR
----------------------------------------------------------------------------------------

{-# INLINABLE mkTSValidator #-}
mkTSValidator
  :: TokenSale -> Maybe Integer -> TSRedeemer -> ScriptContext -> Bool
mkTSValidator = mkValidator . tsStateMachine

----------------------------------------------------------------------------------------
-- TS
----------------------------------------------------------------------------------------

type TS = StateMachine (Maybe Integer) TSRedeemer

----------------------------------------------------------------------------------------
-- TS TYPED VALIDATOR
----------------------------------------------------------------------------------------

tsTypedValidator :: TokenSale -> Scripts.TypedValidator TS
tsTypedValidator ts = Scripts.mkTypedValidator @TS
    ($$(PlutusTx.compile [|| mkTSValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ts)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @(Maybe Integer) @TSRedeemer

----------------------------------------------------------------------------------------
-- TS VALIDATOR
----------------------------------------------------------------------------------------

tsValidator :: TokenSale -> Validator
tsValidator = Scripts.validatorScript . tsTypedValidator

----------------------------------------------------------------------------------------
-- TS ADDRESS
----------------------------------------------------------------------------------------

tsAddress :: TokenSale -> Ledger.Address
tsAddress = scriptAddress . tsValidator

----------------------------------------------------------------------------------------
-- TS CLIENT
----------------------------------------------------------------------------------------

tsClient :: TokenSale -> StateMachineClient (Maybe Integer) TSRedeemer
tsClient ts = mkStateMachineClient
  $ StateMachineInstance (tsStateMachine ts) (tsTypedValidator ts)

----------------------------------------------------------------------------------------
-- MAP ERROR SM
----------------------------------------------------------------------------------------

mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show

----------------------------------------------------------------------------------------
-- START TS
----------------------------------------------------------------------------------------

startTS :: AssetClass -> Bool -> Contract (Last TokenSale) s Text ()
startTS token useTT = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  tt  <- if useTT then Just <$> mapErrorSM getThreadToken else return Nothing
  let ts     = TokenSale { tsSeller = pkh, tsToken = token, tsTT = tt }
      client = tsClient ts
  void $ mapErrorSM $ runInitialise client (Just 0) mempty
  tell $ Last $ Just ts
  logInfo $ "started token sale " ++ show ts

----------------------------------------------------------------------------------------
-- SET PRICE
----------------------------------------------------------------------------------------

setPrice :: TokenSale -> Integer -> Contract w s Text ()
setPrice ts p = void $ mapErrorSM $ runStep (tsClient ts) $ SetPrice p

----------------------------------------------------------------------------------------
-- ADD TOKENS
----------------------------------------------------------------------------------------

addTokens :: TokenSale -> Integer -> Contract w s Text ()
addTokens ts n = void (mapErrorSM $ runStep (tsClient ts) $ AddTokens n)

----------------------------------------------------------------------------------------
-- BUY TOKENS
----------------------------------------------------------------------------------------

buyTokens :: TokenSale -> Integer -> Contract w s Text ()
buyTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ BuyTokens n

----------------------------------------------------------------------------------------
-- WITHDRAW
----------------------------------------------------------------------------------------

withdraw :: TokenSale -> Integer -> Integer -> Contract w s Text ()
withdraw ts n l = void $ mapErrorSM $ runStep (tsClient ts) $ Withdraw n l

----------------------------------------------------------------------------------------
-- CLOSE
----------------------------------------------------------------------------------------

close :: TokenSale -> Contract w s Text ()
close ts = void $ mapErrorSM $ runStep (tsClient ts) Close

----------------------------------------------------------------------------------------
-- TS START SCHEMA
----------------------------------------------------------------------------------------

type TSStartSchema = Endpoint "start" (CurrencySymbol, TokenName, Bool)
type TSUseSchema =
        Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "buy tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)
    .\/ Endpoint "close"      ()

----------------------------------------------------------------------------------------
-- START ENDPOINT
----------------------------------------------------------------------------------------

startEndpoint :: Contract (Last TokenSale) TSStartSchema Text ()
startEndpoint =
  forever
    $ handleError logError
    $ awaitPromise
    $ endpoint @"start"
    $ \(cs, tn, useTT) -> startTS (AssetClass (cs, tn)) useTT

----------------------------------------------------------------------------------------
-- USE ENDPOINTS
----------------------------------------------------------------------------------------

useEndpoints :: TokenSale -> Contract () TSUseSchema Text ()
useEndpoints ts =
  forever
    $        handleError logError
    $        awaitPromise
    $        setPrice'
    `select` addTokens'
    `select` buyTokens'
    `select` withdraw'
    `select` close'
 where
  setPrice'  = endpoint @"set price" $ setPrice ts
  addTokens' = endpoint @"add tokens" $ addTokens ts
  buyTokens' = endpoint @"buy tokens" $ buyTokens ts
  withdraw'  = endpoint @"withdraw" $ uncurry (withdraw ts)
  close'     = endpoint @"close" $ const $ close ts
