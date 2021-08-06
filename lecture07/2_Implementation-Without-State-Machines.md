# [Implementation Without State Machines](https://youtu.be/uwZ903Zd0DU?t=556)

In this section, we'll implement [Alice and Bob's game](./1_Commit-Scemes.md) example as a Plutus contract.

## [On-chain Script](https://youtu.be/uwZ903Zd0DU?t=588)

### Type: [Game](https://youtu.be/uwZ903Zd0DU?t=588)

The `Game` type will be used as a parameter for the contract:

```haskell
data Game = Game
    { gFirst          :: !PubKeyHash  -- Alice
    , gSecond         :: !PubKeyHash  -- Bob
    , gStake          :: !Integer     -- Number of Lovelace stake for each player
    , gPlayDeadline   :: !POSIXTime   -- Deadline for second player to accept before first player can re-claim stake
    , gRevealDeadline :: !POSIXTime   -- Deadline for first player to claim victory
    , gToken          :: !AssetClass  -- An arbitrary NFT to identify the right UTxO instance
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Game
```

In this case, the `gToken` NFT is used to keep track of the game's progress.

### Type: [GameChoice](https://youtu.be/uwZ903Zd0DU?t=678)

The `GameChoice` type defines which two choices each player can make:

```haskell
data GameChoice = Zero | One
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False

PlutusTx.unstableMakeIsData ''GameChoice
```

In this case, we manually define the `Eq` derivation clause so that it is compatible with Plutus.

### Type: [GameDatum](https://youtu.be/uwZ903Zd0DU?t=720)

The `GameDatum` type will be used to represent the game state:

```haskell
data GameDatum = GameDatum ByteString (Maybe GameChoice)
    deriving Show

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')

PlutusTx.unstableMakeIsData ''GameDatum
```

In this case, `ByteString` represents the hash submitted by the first player, while `GameChoice` represents the choice of the second player, which can be `Nothing` in case a choice has not yet been made.

Once again, the `Eq` derivation clause is manually defined.

### Type: [GameRedeemer](https://youtu.be/uwZ903Zd0DU?t=760)

The `GameRedeemer` type will be used to represent the actions that can be used to progress the game forward:

```haskell
data GameRedeemer = Play GameChoice | Reveal ByteString | ClaimFirst | ClaimSecond
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer
```

`Play` indicates when the second player accepts the game and makes a `GameChoice`.

`Reveal` indicates that the first player has won and has provided their `ByteString` nonce.

Note that we don't need to provide the choice itself because we know that player one will only `Reveal` in the case they have won, and we therefore know what the choice would be if so.

`ClaimFirst` is used in the case that player two does not accept to play, allowing player one to re-calim their stake.

`ClaimSecond` is used in the case that player one does not `Reveal` they have won because they know they have lost, allowing player two to collect their winnings.

### Function: [lovelaces](https://youtu.be/uwZ903Zd0DU?t=830)

The `lovelaces` helper function extracts the `Integer` amount of lovelaces contained in a `Value`:

```haskell
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue
```

### Function: [gameDatum](https://youtu.be/uwZ903Zd0DU?t=842)

The `gameDatum` function returns a `Maybe GameDatum` given an output of a transaction:

```haskell
{-# INLINABLE gameDatum #-}
gameDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe GameDatum
gameDatum o f = do
    dh      <- txOutDatum o     -- Get datum hash from output
    Datum d <- f dh             -- Convert hash into a Datum value
    PlutusTx.fromBuiltinData d  -- Convert Datum to GameDatum
```

### Function: [mkGameValidator](https://youtu.be/uwZ903Zd0DU?t=892)

The `mkGameValidator` function defines the core business logic of the contract:

```haskell
{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> ByteString -> ByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' dat red ctx =
    -- Check that input being validated contains NFT state token
    traceIfFalse "token missing from input" (assetClassValueOf (txOutValue ownInput) (gToken game) == 1) &&
    case (dat, red) of
        -- When second player is moving
        -- GameChoice is Nothing, and is playing choice c
        (GameDatum bs Nothing, Play c) ->
            -- Player two signs transaction
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))                                   &&
            -- Player one has stake
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            -- Second player has stake
            traceIfFalse "second player's stake missing" (lovelaces (txOutValue ownOutput) == (2 * gStake game))            &&
            -- Output datum GameChoice is Just c now instead of Nothing
            traceIfFalse "wrong output datum"            (outputDatum == GameDatum bs (Just c))                             &&
            -- Play deadline is respected
            traceIfFalse "missed deadline"               (to (gPlayDeadline game) `contains` txInfoValidRange info)         &&
            -- NFT is passed into output
            traceIfFalse "token missing from output"     (assetClassValueOf (txOutValue ownOutput) (gToken game) == 1)

        -- Player one realizes they won, reveals nonce
        (GameDatum bs (Just c), Reveal nonce) ->
            -- Player one signs transaction
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))                                    &&
            -- The nonce agrees with hash
            traceIfFalse "commit mismatch"               (checkNonce bs nonce c)                                            &&
            -- Reveal deadline is respected
            traceIfFalse "missed deadline"               (to (gRevealDeadline game) `contains` txInfoValidRange info)       &&
            -- The input contains the stake of both players
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            -- The NFT goes back to player one
            traceIfFalse "NFT must go to first player"   nftToFirst

        -- Player two hasn't accepted to play, player one wants stake back
        (GameDatum _ Nothing, ClaimFirst) ->
            -- Player one has signed transaction
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))                                    &&
            -- Play deadline has passed
            traceIfFalse "too early"                     (from (1 + gPlayDeadline game) `contains` txInfoValidRange info)   &&
            -- Player one has stake
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            -- Player one gets NFT back
            traceIfFalse "NFT must go to first player"   nftToFirst

        -- Both players have moved but player one didn't win or missed deadline
        (GameDatum _ (Just _), ClaimSecond) ->
            -- Player two signs transaction
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))                                   &&
            -- Reveal deadline has passed
            traceIfFalse "too early"                     (from (1 + gRevealDeadline game) `contains` txInfoValidRange info) &&
            -- Both players have stake
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            -- NFT goes back to player one
            traceIfFalse "NFT must go to first player"   nftToFirst

        _                              -> False
  where

    -- Get transaction info
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Get input UTxO (shouldn't fail)
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "game input missing"
        Just i  -> txInInfoResolved i

    -- Get output UTxO, for when the game isn't done yet
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one game output"

    -- Get datum from output
    outputDatum :: GameDatum
    outputDatum = case gameDatum ownOutput (`findDatum` info) of
        Nothing -> traceError "game output datum not found"
        Just d  -> d

    -- Prove that player one has won by checking nonce in hash
    -- where first argument is hash, second is nonce, third is choice of player two
    checkNonce :: ByteString -> ByteString -> GameChoice -> Bool
    checkNonce bs nonce cSecond = sha2_256 (nonce `concatenate` cFirst) == bs
      where
        cFirst :: ByteString
        cFirst = case cSecond of
            Zero -> bsZero'
            One  -> bsOne'

    -- Ensure NFT goes back to first player (as the first player needs it to start the game)
    nftToFirst :: Bool
    nftToFirst = assetClassValueOf (valuePaidTo info $ gFirst game) (gToken game) == 1
```

In the above function, the second and third `ByteString` arguments are unfortunately necessary to represent the 0 and 1 options as it is not possible to use string literals to get bytestrings in Haskell that is compiled to Plutus Core.

### Type: [Gaming](https://youtu.be/uwZ903Zd0DU?t=1404)

The `Gaming` type serves to combine the datum and redeemer:

```haskell
data Gaming
instance Scripts.ValidatorTypes Gaming where
    type instance DatumType Gaming = GameDatum
    type instance RedeemerType Gaming = GameRedeemer
```

### Function: [typedGameValidator](https://youtu.be/uwZ903Zd0DU?t=1414)

The `typedGameValidator` function compiles `mkGameValidator` to Plutus Core:

```haskell
bsZero, bsOne :: ByteString
bsZero = "0"
bsOne  = "1"

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsZero
        `PlutusTx.applyCode` PlutusTx.liftCode bsOne)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer
```

### Function: [gameValidator](https://youtu.be/uwZ903Zd0DU?t=1450)

The `gameValidator` function converts our `typedGameValidator` into a Validator:

```haskell
gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator
```

### Function: [gameAddress](https://youtu.be/uwZ903Zd0DU?t=1450)

The `gameAddress` function converts our `gameValidator` into a Ledger.Address:

```haskell
gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator
```

## [Off-chain Script](https://youtu.be/uwZ903Zd0DU?t=1454)

### Function: [findGameOutput](https://youtu.be/uwZ903Zd0DU?t=1454)

The `findGameOutput` attempts to find the NFT-bearing UTxO:

```haskell
findGameOutput :: Game -> Contract w s Text (Maybe (TxOutRef, TxOutTx, GameDatum))
findGameOutput game = do
    -- Get all UTxOs at the game address
    utxos <- utxoAt $ gameAddress game
    return $ do

        -- Use Data.List find to find the first output that contains the NFT
        (oref, o) <- find f $ Map.toList utxos

        -- Get UTxO datum
        dat       <- gameDatum (txOutTxOut o) (`Map.lookup` txData (txOutTxTx o))

        -- Return corresponding triple
        return (oref, o, dat)
  where
    f :: (TxOutRef, TxOutTx) -> Bool
    f (_, o) = assetClassValueOf (txOutValue $ txOutTxOut o) (gToken game) == 1
```

### Function: [waitUntilTimeHasPassed](https://youtu.be/uwZ903Zd0DU?t=1584)

The `waitUntilTimeHasPassed` function gets a POSIX time and waits until it has passed and we are in the next slot:

```haskell
waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = do

    -- Get and log current slot
    s1 <- currentSlot
    logInfo @String $ "current slot: " ++ show s1 ++ ", waiting until " ++ show t

    -- Waits until POSIX time has come and ensure in the next slot
    void $ awaitTime t >> waitNSlots 1

    -- Get and log current slot
    s2 <- currentSlot
    logInfo @String $ "waited until: " ++ show s2
```

### Type: [FirstParams](https://youtu.be/uwZ903Zd0DU?t=1650)

The `FirstParams` type will initiate the game by giving some key parameters:

```haskell
data FirstParams = FirstParams
    { fpSecond         :: !PubKeyHash      -- Player two
    , fpStake          :: !Integer         -- Stake amount
    , fpPlayDeadline   :: !POSIXTime       -- Play deadline
    , fpRevealDeadline :: !POSIXTime       -- Reveal deadline
    , fpNonce          :: !ByteString      -- Nonce
    , fpCurrency       :: !CurrencySymbol  -- NFT currency symbol
    , fpTokenName      :: !TokenName       -- NFT token name
    , fpChoice         :: !GameChoice      -- Choice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
```

### Function: [firstGame](https://youtu.be/uwZ903Zd0DU?t=1678)

The `firstGame` function initializes the game for player one:

```haskell
firstGame :: forall w s. FirstParams -> Contract w s Text ()
firstGame fp = do

    -- Get own public key hash
    pkh <- pubKeyHash <$> Contract.ownPubKey

    -- Initialize Game instance
    let game = Game
            { gFirst          = pkh
            , gSecond         = fpSecond fp
            , gStake          = fpStake fp
            , gPlayDeadline   = fpPlayDeadline fp
            , gRevealDeadline = fpRevealDeadline fp
            , gToken          = AssetClass (fpCurrency fp, fpTokenName fp)
            }

        -- Stake plus the NFT
        v    = lovelaceValueOf (fpStake fp) <> assetClassValue (gToken game) 1

        -- Player one choice
        c    = fpChoice fp

        -- Compute the hash using the nonce and choice
        bs   = sha2_256 $ fpNonce fp `concatenate` if c == Zero then bsZero else bsOne

        -- Produce a script output at this address with the datum that contains the hash and value
        tx   = Constraints.mustPayToTheScript (GameDatum bs Nothing) v

    -- Submit transaction, wait for confirmation, and log info
    ledgerTx <- submitTxConstraints (typedGameValidator game) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "made first move: " ++ show (fpChoice fp)

    -- Wait until play deadline has passed
    waitUntilTimeHasPassed $ fpPlayDeadline fp

    -- Find UTxO with NFT
    m   <- findGameOutput game
    now <- currentTime
    case m of

        -- Case of NFT not found
        -- Shouldn't happen because NFT should end up address again even if player two moves
        Nothing             -> throwError "game output not found"

        -- Case of found NFT
        Just (oref, o, dat) -> case dat of

            -- Deadline passed, player two hasn't moved, invoke ClaimFirst redeemer
            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"

                    -- Provide the found UTxO and the game validator
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)

                    -- Spend found UTxO with redeemer
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ClaimFirst) <>
                              Constraints.mustValidateIn (from now)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ txId ledgerTx'
                logInfo @String "reclaimed stake"

            -- Player two moved and lost
            GameDatum _ (Just c') | c' == c -> do

                logInfo @String "second player played and lost"

                    -- Provide the found UTxO and the game validator
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)

                    -- Reveal nonce with Reveal redeemer before deadline has passed
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Reveal $ fpNonce fp) <>
                              Constraints.mustValidateIn (to $ now + 1000)

                -- Submit transaction, wait for confirmation and log info
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ txId ledgerTx'
                logInfo @String "victory"

            -- Player two moved and won
            _ -> logInfo @String "second player played and won"
```

### Type: [SecondParams](https://youtu.be/uwZ903Zd0DU?t=1886)

The `SecondParams` type serves a very similary function to `FirstParams` but initiates player two's move:

```haskell
data SecondParams = SecondParams
    { spFirst          :: !PubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spCurrency       :: !CurrencySymbol
    , spTokenName      :: !TokenName
    , spChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
```


### Function: [secondGame](https://youtu.be/uwZ903Zd0DU?t=1908)

The `secondGame` function initializes the game for player one:

```haskell
secondGame :: forall w s. SecondParams -> Contract w s Text ()
secondGame sp = do

    -- Get own public key hash
    pkh <- pubKeyHash <$> Contract.ownPubKey

    -- Initialize game instance
    let game = Game
            { gFirst          = spFirst sp
            , gSecond         = pkh
            , gStake          = spStake sp
            , gPlayDeadline   = spPlayDeadline sp
            , gRevealDeadline = spRevealDeadline sp
            , gToken          = AssetClass (spCurrency sp, spTokenName sp)
            }

    -- Find NFT-bearing UTxO
    m <- findGameOutput game
    case m of

        -- Case of game found, player two choice still Nothing
        Just (oref, o, GameDatum bs Nothing) -> do
            logInfo @String "running game found"
            now <- currentTime

            -- Token is NFT
            let token   = assetClassValue (gToken game) 1

            -- The value we put in the new output at the same time
            -- Old one contains NFT and player one's stake, new one adds player two's stake
            let v       = let x = lovelaceValueOf (spStake sp) in x <> x <> token

                -- The choice
                c       = spChoice sp

                -- Provide the UTxO, game validator, and script instance because we are producing one
                lookups = Constraints.unspentOutputs (Map.singleton oref o)                                   <>
                          Constraints.otherScript (gameValidator game)                                        <>
                          Constraints.typedValidatorLookups (typedGameValidator game)

                -- Spend existing UTxO with Play redeemer and choice c
                tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Play c) <>

                -- Create new UTxO with updated datum with same hash plust player two's move and v
                          Constraints.mustPayToTheScript (GameDatum bs $ Just c) v                            <>

                -- Must do this before deadline passes
                          Constraints.mustValidateIn (to now)

            -- Submit transaction, wait for confirmation and log info
            ledgerTx <- submitTxConstraintsWith @Gaming lookups tx
            let tid = txId ledgerTx
            void $ awaitTxConfirmed tid
            logInfo @String $ "made second move: " ++ show (spChoice sp)

            -- Wait until reveal deadline has passed
            waitUntilTimeHasPassed $ spRevealDeadline sp

            -- Again try to find UTxO, which could now be a different one
            m'   <- findGameOutput game
            now' <- currentTime
            case m' of

                -- Case of no UTxO found, i.e. player one revealed and won
                Nothing             -> logInfo @String "first player won"

                -- Case of UTxO found, i.e. player one didn't reveal / lost
                Just (oref', o', _) -> do
                    logInfo @String "first player didn't reveal"
                    let lookups' = Constraints.unspentOutputs (Map.singleton oref' o')                                     <>
                                   Constraints.otherScript (gameValidator game)

                        -- Spend the UTxO
                        tx'      = Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ClaimSecond) <>

                        -- Ensure that reveal deadline has passed
                                   Constraints.mustValidateIn (from now')                                                  <>
                        -- Return NFT to player one
                                   Constraints.mustPayToPubKey (spFirst sp) token

                    -- Submit transaction, wait for confirmation, and log info
                    ledgerTx' <- submitTxConstraintsWith @Gaming lookups' tx'
                    void $ awaitTxConfirmed $ txId ledgerTx'
                    logInfo @String "second player won"

        -- Case of game not found
        _ -> logInfo @String "no running game found"
```

### Type: [GameSchema](https://youtu.be/uwZ903Zd0DU?t=2124)

The `GameSchema` type defines two endpoints for player one / `FirstParams`, and for player two / `SecondParams`:

```haskell
type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

```

### Function: [endpoints](https://youtu.be/uwZ903Zd0DU?t=2138)

The `endpoints` function offers a choice between the two defined endpoints:

```haskell
endpoints :: Contract () GameSchema Text ()
endpoints = (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  >>= firstGame
    second = endpoint @"second" >>= secondGame
```
