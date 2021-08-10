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

module Week07.RockPaperScissors
  ( Game(..)
  , GameChoice(..)
  , FirstParams(..)
  , SecondParams(..)
  , GameSchema
  , Last(..)
  , ThreadToken
  , Text
  , endpoints
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
import           Ledger.Typed.Tx
import           Playground.Contract            ( ToSchema )
import           Plutus.Contract               as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude        hiding ( Semigroup(..)
                                                , check
                                                , unless
                                                )
import           Prelude                        ( Semigroup(..)
                                                , Show(..)
                                                , String
                                                )
import qualified Prelude

----------------------------------------------------------------------------------------
-- GAME
----------------------------------------------------------------------------------------

data Game = Game
  { gFirst          :: !PubKeyHash
  , gSecond         :: !PubKeyHash
  , gStake          :: !Integer
  , gPlayDeadline   :: !POSIXTime
  , gRevealDeadline :: !POSIXTime
  , gToken          :: !ThreadToken
  }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Game

----------------------------------------------------------------------------------------
-- GAME CHOICE
----------------------------------------------------------------------------------------

data GameChoice = Rock | Paper | Scissors
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

instance Eq GameChoice where
  {-# INLINABLE (==) #-}
  Rock     == Rock     = True
  Paper    == Paper    = True
  Scissors == Scissors = True
  _        == _        = False

PlutusTx.unstableMakeIsData ''GameChoice

----------------------------------------------------------------------------------------
-- BEATS
----------------------------------------------------------------------------------------

{-# INLINABLE beats #-}
beats :: GameChoice -> GameChoice -> Bool
beats Rock     Scissors = True
beats Paper    Rock     = True
beats Scissors Paper    = True
beats _        _        = False

----------------------------------------------------------------------------------------
-- GAME DATUM
----------------------------------------------------------------------------------------

data GameDatum = GameDatum ByteString (Maybe GameChoice) | Finished
    deriving Show

instance Eq GameDatum where
  {-# INLINABLE (==) #-}
  GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')
  Finished        == Finished          = True
  _               == _                 = False

PlutusTx.unstableMakeIsData ''GameDatum

----------------------------------------------------------------------------------------
-- GAME REDEEMER
----------------------------------------------------------------------------------------

data GameRedeemer = Play GameChoice | Reveal ByteString GameChoice | ClaimFirst | ClaimSecond
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

----------------------------------------------------------------------------------------
-- LOVELACES
----------------------------------------------------------------------------------------

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

----------------------------------------------------------------------------------------
-- GAME DATUM
----------------------------------------------------------------------------------------

{-# INLINABLE gameDatum #-}
gameDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe GameDatum
gameDatum o f = do
  dh      <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d

----------------------------------------------------------------------------------------
-- TRANSITION
----------------------------------------------------------------------------------------

{-# INLINABLE transition #-}
transition
  :: Game
  -> State GameDatum
  -> GameRedeemer
  -> Maybe (TxConstraints Void Void, State GameDatum)
transition game s r = case (stateValue s, stateData s, r) of

  --------------------------------------------------------------------------------------
  -- PLAYER TWO PLAYS
  --------------------------------------------------------------------------------------

  (v, GameDatum bs Nothing, Play c) | lovelaces v == gStake game -> Just
    ( Constraints.mustBeSignedBy (gSecond game)
      <> Constraints.mustValidateIn (to $ gPlayDeadline game)
    , State (GameDatum bs $ Just c) (lovelaceValueOf $ 2 * gStake game)
    )

  --------------------------------------------------------------------------------------
  -- PLAYER ONE REVEALS A WIN OR TIE
  --------------------------------------------------------------------------------------

  (v, GameDatum _ (Just c), Reveal _ c')
    | lovelaces v == (2 * gStake game) && (c' `beats` c) -> Just
      ( Constraints.mustBeSignedBy (gFirst game)
        <> Constraints.mustValidateIn (to $ gRevealDeadline game)
      , State Finished mempty
      )
    | lovelaces v == (2 * gStake game) && (c' == c) -> Just
      ( Constraints.mustBeSignedBy (gFirst game)
      <> Constraints.mustValidateIn (to $ gRevealDeadline game)
      <> Constraints.mustPayToPubKey (gSecond game)
                                     (lovelaceValueOf $ gStake game)
      , State Finished mempty
      )

  --------------------------------------------------------------------------------------
  -- PLAYER TWO DOES NOT RESPOND AND PLAYER ONE RECLAIMS STAKE
  --------------------------------------------------------------------------------------

  (v, GameDatum _ Nothing, ClaimFirst) | lovelaces v == gStake game -> Just
    ( Constraints.mustBeSignedBy (gFirst game)
      <> Constraints.mustValidateIn (from $ 1 + gPlayDeadline game)
    , State Finished mempty
    )

  --------------------------------------------------------------------------------------
  -- PLAYER ONE DOES NOT REVEAL AND PLAYER TWO CLAIMS WINNINGS
  --------------------------------------------------------------------------------------

  (v, GameDatum _ (Just _), ClaimSecond) | lovelaces v == (2 * gStake game) ->
    Just
      ( Constraints.mustBeSignedBy (gSecond game)
        <> Constraints.mustValidateIn (from $ 1 + gRevealDeadline game)
      , State Finished mempty
      )
  _ -> Nothing

----------------------------------------------------------------------------------------
-- FINAL
----------------------------------------------------------------------------------------

{-# INLINABLE final #-}
final :: GameDatum -> Bool
final Finished = True
final _        = False

----------------------------------------------------------------------------------------
-- CHECK
----------------------------------------------------------------------------------------

{-# INLINABLE check #-}
check
  :: ByteString
  -> ByteString
  -> ByteString
  -> GameDatum
  -> GameRedeemer
  -> ScriptContext
  -> Bool
check bsRock' bsPaper' bsScissors' (GameDatum bs (Just _)) (Reveal nonce c) _ =
  sha2_256
      (nonce `concatenate` if c == Rock
        then bsRock'
        else if c == Paper then bsPaper' else bsScissors'
      )
    == bs
check _ _ _ _ _ _ = True

----------------------------------------------------------------------------------------
-- GAME STATE MACHINE
----------------------------------------------------------------------------------------

{-# INLINABLE gameStateMachine #-}
gameStateMachine
  :: Game
  -> ByteString
  -> ByteString
  -> ByteString
  -> StateMachine GameDatum GameRedeemer
gameStateMachine game bsRock' bsPaper' bsScissors' = StateMachine
  { smTransition  = transition game
  , smFinal       = final
  , smCheck       = check bsRock' bsPaper' bsScissors'
  , smThreadToken = Just $ gToken game
  }

----------------------------------------------------------------------------------------
-- MK GAME VALIDATOR
----------------------------------------------------------------------------------------

{-# INLINABLE mkGameValidator #-}
mkGameValidator
  :: Game
  -> ByteString
  -> ByteString
  -> ByteString
  -> GameDatum
  -> GameRedeemer
  -> ScriptContext
  -> Bool
mkGameValidator game bsRock' bsPaper' bsScissors' =
  mkValidator $ gameStateMachine game bsRock' bsPaper' bsScissors'

----------------------------------------------------------------------------------------
-- GAMING
----------------------------------------------------------------------------------------

type Gaming = StateMachine GameDatum GameRedeemer

----------------------------------------------------------------------------------------
-- BS ROCK, BS PAPER, BS SCISSORS
----------------------------------------------------------------------------------------

bsRock, bsPaper, bsScissors :: ByteString
bsRock = "r"
bsPaper = "p"
bsScissors = "s"

----------------------------------------------------------------------------------------
-- GAME STATE MACHINE
----------------------------------------------------------------------------------------

gameStateMachine' :: Game -> StateMachine GameDatum GameRedeemer
gameStateMachine' game = gameStateMachine game bsRock bsPaper bsScissors

----------------------------------------------------------------------------------------
-- TYPED GAME VALIDATOR
----------------------------------------------------------------------------------------

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsRock
        `PlutusTx.applyCode` PlutusTx.liftCode bsPaper
        `PlutusTx.applyCode` PlutusTx.liftCode bsScissors)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

----------------------------------------------------------------------------------------
-- GAME VALIDATOR
----------------------------------------------------------------------------------------

gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

----------------------------------------------------------------------------------------
-- GAME ADDRESS
----------------------------------------------------------------------------------------

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

----------------------------------------------------------------------------------------
-- GAME CLIENT
----------------------------------------------------------------------------------------

gameClient :: Game -> StateMachineClient GameDatum GameRedeemer
gameClient game = mkStateMachineClient
  $ StateMachineInstance (gameStateMachine' game) (typedGameValidator game)

----------------------------------------------------------------------------------------
-- FIRST PARAMS
----------------------------------------------------------------------------------------

data FirstParams = FirstParams
  { fpSecond         :: !PubKeyHash
  , fpStake          :: !Integer
  , fpPlayDeadline   :: !POSIXTime
  , fpRevealDeadline :: !POSIXTime
  , fpNonce          :: !ByteString
  , fpChoice         :: !GameChoice
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

----------------------------------------------------------------------------------------
-- MAP ERROR
----------------------------------------------------------------------------------------

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

----------------------------------------------------------------------------------------
-- WAIT UNTIL TIME HAS PASSED
----------------------------------------------------------------------------------------

waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = void $ awaitTime t >> waitNSlots 1

----------------------------------------------------------------------------------------
-- FIRST GAME
----------------------------------------------------------------------------------------

firstGame :: forall s . FirstParams -> Contract (Last ThreadToken) s Text ()
firstGame fp = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  tt  <- mapError' getThreadToken
  let game = Game { gFirst          = pkh
                  , gSecond         = fpSecond fp
                  , gStake          = fpStake fp
                  , gPlayDeadline   = fpPlayDeadline fp
                  , gRevealDeadline = fpRevealDeadline fp
                  , gToken          = tt
                  }
      client = gameClient game
      v      = lovelaceValueOf (fpStake fp)
      c      = fpChoice fp
      bs     = sha2_256 $ fpNonce fp `concatenate` if c == Rock
        then bsRock
        else if c == Paper then bsPaper else bsScissors
  void $ mapError' $ runInitialise client (GameDatum bs Nothing) v
  logInfo @String $ "made first move: " ++ show (fpChoice fp)
  tell $ Last $ Just tt

  waitUntilTimeHasPassed $ fpPlayDeadline fp

  m <- mapError' $ getOnChainState client
  case m of
    Nothing          -> throwError "game output not found"
    Just ((o, _), _) -> case tyTxOutData o of

      ----------------------------------------------------------------------------------
      -- RECLAIM STAKE
      ----------------------------------------------------------------------------------

      GameDatum _ Nothing -> do
        logInfo @String "second player did not play"
        void $ mapError' $ runStep client ClaimFirst
        logInfo @String "first player reclaimed stake"

      ----------------------------------------------------------------------------------
      -- REVEAL CHOICE
      ----------------------------------------------------------------------------------

      GameDatum _ (Just c') | c `beats` c' || c' == c -> do
        logInfo @String "second player played and lost or tied"
        void $ mapError' $ runStep client $ Reveal (fpNonce fp) c
        logInfo @String "first player revealed and won or tied"

      _ -> logInfo @String "second player played and won"

----------------------------------------------------------------------------------------
-- SECOND PARAMS
----------------------------------------------------------------------------------------

data SecondParams = SecondParams
  { spFirst          :: !PubKeyHash
  , spStake          :: !Integer
  , spPlayDeadline   :: !POSIXTime
  , spRevealDeadline :: !POSIXTime
  , spChoice         :: !GameChoice
  , spToken          :: !ThreadToken
  }
  deriving (Show, Generic, FromJSON, ToJSON)

----------------------------------------------------------------------------------------
-- SECOND GAME
----------------------------------------------------------------------------------------

secondGame :: forall w s . SecondParams -> Contract w s Text ()
secondGame sp = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  let game = Game { gFirst          = spFirst sp
                  , gSecond         = pkh
                  , gStake          = spStake sp
                  , gPlayDeadline   = spPlayDeadline sp
                  , gRevealDeadline = spRevealDeadline sp
                  , gToken          = spToken sp
                  }
      client = gameClient game
  m <- mapError' $ getOnChainState client
  case m of
    Nothing          -> logInfo @String "no running game found"
    Just ((o, _), _) -> case tyTxOutData o of
      GameDatum _ Nothing -> do

        --------------------------------------------------------------------------------
        -- PLAYER TWO RESPONDS
        --------------------------------------------------------------------------------

        logInfo @String "running game found"
        void $ mapError' $ runStep client $ Play $ spChoice sp
        logInfo @String $ "made second move: " ++ show (spChoice sp)

        waitUntilTimeHasPassed $ spRevealDeadline sp

        m' <- mapError' $ getOnChainState client
        case m' of

          ------------------------------------------------------------------------------
          -- PLAYER ONE WINS OR TIES
          ------------------------------------------------------------------------------

          Nothing -> logInfo @String "first player won or tied"

          ------------------------------------------------------------------------------
          -- PLAYER TWO WINS
          ------------------------------------------------------------------------------

          Just _  -> do
            logInfo @String "first player didn't reveal"
            void $ mapError' $ runStep client ClaimSecond
            logInfo @String "second player won"

      _ -> throwError "unexpected datum"

----------------------------------------------------------------------------------------
-- GAME SCHEMA
----------------------------------------------------------------------------------------

type GameSchema
  = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

----------------------------------------------------------------------------------------
-- ENDPOINTS
----------------------------------------------------------------------------------------

endpoints :: Contract (Last ThreadToken) GameSchema Text ()
endpoints = (first `select` second) >> endpoints
 where
  first  = endpoint @"first" >>= firstGame
  second = endpoint @"second" >>= secondGame
