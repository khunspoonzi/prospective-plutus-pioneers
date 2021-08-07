# [State Machines](https://youtu.be/uwZ903Zd0DU?t=2520)

A state machine is a system that begins with an initial state and allows for one or more transition states before arriving at a final state. In other words, a state machine is like a directed graph that ends at a final state.

Within the context of the blockchain, it is possible to think of our [Plutus implementation](./2_Implementation-Without-State-Machines.md) of [Alice and Bob's game](./1_Commit-Scemes.md) example as a kind of state machine:

- **Initial State:** Alice opens the game by committing her hash and 1 ADA
  - **Transition State:** Bob doesn't answer and Alice claims back her 1 ADA
    - **Final State:** The game ends and Alice retains her 1 ADA
  - **Transition State:**  Bob accepts the game by commiting Alice's hash, his choice, and 1 ADA
    - **Transition State:** Alice realizes that she has won and reveals her choice
        - **Final State:** The game ends and Alice ends up with 2 ADA
    - **Transition State:** Alice realizes that she has lost, simply waits for a deadline to pass, and Bob claims his winnings
        - **Final State:** The game ends and Bob ends up with 2 ADA

In this case, the state machine itself is represented by the UTxO sitting at the game UTxO's script address, while the state is represented by that UTxO's datum.

A transition is represented by a transaction that consumes the game UTxO, and is characterized by the redeemer it uses. Each transaction produces an updated datum to reflect the new state of the game.

In this section, we'll explore further how [`Plutus.Contract.StateMachine`](https://alpha.marlowe.iohkdev.io/doc/haddock/plutus-contract/html/Plutus-Contract-StateMachine.html) from `plutus-contract` analogizes this.

## Type: [StateMachine](https://youtu.be/uwZ903Zd0DU?t=2684)

The `StateMachine` type is a record type with four fields and accepts two type parameters `s` and `i` that stand for state and input, and correspond to datum and redeemer, respectively:

```haskell
-- | Specification of a state machine, consisting of a transition function that determines the
-- next state from the current state and an input, and a checking function that checks the validity
-- of the transition in the context of the current transaction.
data StateMachine s i = StateMachine {
      -- | The transition function of the state machine. 'Nothing' indicates an invalid transition from the current state.
      smTransition  :: State s -> i -> Maybe (TxConstraints Void Void, State s),

      -- | Check whether a state is the final state
      smFinal       :: s -> Bool,

      -- | The condition checking function. Can be used to perform
      --   checks on the pending transaction that aren't covered by the
      --   constraints. 'smCheck' is always run in addition to checking the
      --   constraints, so the default implementation always returns true.
      smCheck       :: s -> i -> ScriptContext -> Bool,

      -- | The 'AssetClass' of the thread token that identifies the contract
      --   instance.
      smThreadToken :: Maybe AssetClass
    }
```

### Function: [smTransition](https://youtu.be/uwZ903Zd0DU?t=2710)

The `smTransition` function defines from which states and using which transitions it is possible to change to another state:

```haskell
-- Field from StateMachine
smTransition :: State s -> i -> Maybe (TxConstraints Void Void, State s)

data State s = State { stateData :: s, stateValue :: Value }  -- The datum and value
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
```

Give the State (present datum and value of the UTxO) along with a transaction that tries to consume the UTxO using redeemer `i`, we can return `Nothing` if the transaction is not allowed, or a tuple containing a new `State` (datum and value) and additional constraints that the transaction must satisfy if it is allowed.

### Function: [smFinal](https://youtu.be/uwZ903Zd0DU?t=2800)

The `smFinal` function tells us whether we are dealing with a final state:

```haskell
-- Field from StateMachine
smFinal :: s -> Bool
```

In the case of a final state, we ensure that a value is not attached and a new UTxO is not produced.

### Function: [smCheck](https://youtu.be/uwZ903Zd0DU?t=2842)

The `smCheck` function provides an additional check on the datum, redeemer, and context not included by the additional `TxConstraints`:

```haskell
-- Field from StateMachine
smCheck :: s -> i -> ScriptContext -> Bool
```

### Function: [smThreadToken](https://youtu.be/uwZ903Zd0DU?t=2842)

The `smThreadToken` field is optional but helps to identify the correct contract instance:

```haskell
-- Field from StateMachine
smThreadToken :: Maybe AssetClass
```

Using this field will automatically take care of minting the NFT and threading it through the state transitions

---

##### Continue [watching](https://youtu.be/uwZ903Zd0DU?t=3014) for a walkthrough comparison on the [StateMachine implementation](https://github.com/input-output-hk/plutus-pioneer-program/blob/main/code/week07/src/Week07/StateMachine.hs) of our game:

```haskell
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

module Week07.StateMachine
    ( Game (..)
    , GameChoice (..)
    , FirstParams (..)
    , SecondParams (..)
    , GameSchema
    , Last (..)
    , ThreadToken
    , Text
    , endpoints
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import           Ledger.Typed.Tx
import qualified Ledger.Typed.Scripts         as Scripts
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Playground.Contract          (ToSchema)
import           Prelude                      (Semigroup (..), Show (..), String)
import qualified Prelude

data Game = Game
    { gFirst          :: !PubKeyHash
    , gSecond         :: !PubKeyHash
    , gStake          :: !Integer
    , gPlayDeadline   :: !POSIXTime
    , gRevealDeadline :: !POSIXTime
    , gToken          :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Game

data GameChoice = Zero | One
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False

PlutusTx.unstableMakeIsData ''GameChoice

data GameDatum = GameDatum ByteString (Maybe GameChoice) | Finished
    deriving Show  -- New Finished constructor represents the final state of our game

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')
    Finished        == Finished          = True  -- Account for new Finished constructor here
    _               == _                 = False

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer = Play GameChoice | Reveal ByteString | ClaimFirst | ClaimSecond
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE gameDatum #-}
gameDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe GameDatum
gameDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

-- Define transition function of the state machine
{-# INLINABLE transition #-}
transition :: Game -> State GameDatum -> GameRedeemer -> Maybe (TxConstraints Void Void, State GameDatum)
transition game s r = case (stateValue s, stateData s, r) of
    (v, GameDatum bs Nothing, Play c)
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                       Constraints.mustValidateIn (to $ gPlayDeadline game)
                                                     , State (GameDatum bs $ Just c) (lovelaceValueOf $ 2 * gStake game)
                                                     )
    (v, GameDatum _ (Just _), Reveal _)
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                       Constraints.mustValidateIn (to $ gRevealDeadline game)
                                                     , State Finished mempty
                                                     )
    (v, GameDatum _ Nothing, ClaimFirst)
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                       Constraints.mustValidateIn (from $ 1 + gPlayDeadline game)
                                                     , State Finished mempty
                                                     )
    (v, GameDatum _ (Just _), ClaimSecond)
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                       Constraints.mustValidateIn (from $ 1 + gRevealDeadline game)
                                                     , State Finished mempty
                                                     )
    _                                        -> Nothing

    -- Remember that smTransition returns Maybe a tuple of constraints + a new state
    -- In this case we don't need to check for the NFT (ThreadToken)

{-# INLINABLE final #-}
final :: GameDatum -> Bool
final Finished = True
final _        = False

{-# INLINABLE check #-}
check :: ByteString -> ByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
check bsZero' bsOne' (GameDatum bs (Just c)) (Reveal nonce) _ =
    sha2_256 (nonce `concatenate` if c == Zero then bsZero' else bsOne') == bs
check _       _      _                       _              _ = True

{-# INLINABLE gameStateMachine #-}
gameStateMachine :: Game -> ByteString -> ByteString -> StateMachine GameDatum GameRedeemer
gameStateMachine game bsZero' bsOne' = StateMachine
    { smTransition  = transition game
    , smFinal       = final
    , smCheck       = check bsZero' bsOne'
    , smThreadToken = Just $ gToken game
    }

{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> ByteString -> ByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' = mkValidator $ gameStateMachine game bsZero' bsOne'

type Gaming = StateMachine GameDatum GameRedeemer

bsZero, bsOne :: ByteString
bsZero = "0"
bsOne  = "1"

gameStateMachine' :: Game -> StateMachine GameDatum GameRedeemer
gameStateMachine' game = gameStateMachine game bsZero bsOne

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsZero
        `PlutusTx.applyCode` PlutusTx.liftCode bsOne)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

gameClient :: Game -> StateMachineClient GameDatum GameRedeemer
gameClient game = mkStateMachineClient $ StateMachineInstance (gameStateMachine' game) (typedGameValidator game)

data FirstParams = FirstParams
    { fpSecond         :: !PubKeyHash
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !POSIXTime
    , fpRevealDeadline :: !POSIXTime
    , fpNonce          :: !ByteString
    , fpChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = void $ awaitTime t >> waitNSlots 1

firstGame :: forall s. FirstParams -> Contract (Last ThreadToken) s Text ()
firstGame fp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    tt  <- mapError' getThreadToken
    let game   = Game
            { gFirst          = pkh
            , gSecond         = fpSecond fp
            , gStake          = fpStake fp
            , gPlayDeadline   = fpPlayDeadline fp
            , gRevealDeadline = fpRevealDeadline fp
            , gToken          = tt
            }
        client = gameClient game
        v      = lovelaceValueOf (fpStake fp)
        c      = fpChoice fp
        bs     = sha2_256 $ fpNonce fp `concatenate` if c == Zero then bsZero else bsOne
    void $ mapError' $ runInitialise client (GameDatum bs Nothing) v
    logInfo @String $ "made first move: " ++ show (fpChoice fp)
    tell $ Last $ Just tt

    waitUntilTimeHasPassed $ fpPlayDeadline fp

    m <- mapError' $ getOnChainState client
    case m of
        Nothing             -> throwError "game output not found"
        Just ((o, _), _) -> case tyTxOutData o of

            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                void $ mapError' $ runStep client ClaimFirst
                logInfo @String "first player reclaimed stake"

            GameDatum _ (Just c') | c' == c -> do
                logInfo @String "second player played and lost"
                void $ mapError' $ runStep client $ Reveal $ fpNonce fp
                logInfo @String "first player revealed and won"

            _ -> logInfo @String "second player played and won"

data SecondParams = SecondParams
    { spFirst          :: !PubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spChoice         :: !GameChoice
    , spToken          :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON)

secondGame :: forall w s. SecondParams -> Contract w s Text ()
secondGame sp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game   = Game
            { gFirst          = spFirst sp
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
                logInfo @String "running game found"
                void $ mapError' $ runStep client $ Play $ spChoice sp
                logInfo @String $ "made second move: " ++ show (spChoice sp)

                waitUntilTimeHasPassed $ spRevealDeadline sp

                m' <- mapError' $ getOnChainState client
                case m' of
                    Nothing -> logInfo @String "first player won"
                    Just _  -> do
                        logInfo @String "first player didn't reveal"
                        void $ mapError' $ runStep client ClaimSecond
                        logInfo @String "second player won"

            _ -> throwError "unexpected datum"

type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

endpoints :: Contract (Last ThreadToken) GameSchema Text ()
endpoints = (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  >>= firstGame
    second = endpoint @"second" >>= secondGame
```
