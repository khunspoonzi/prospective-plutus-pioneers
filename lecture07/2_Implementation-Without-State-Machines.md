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

### More Notes Soon...
