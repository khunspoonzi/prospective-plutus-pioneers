# [Property-based Testing of Plutus Contracts](https://youtu.be/zW3D2iM5uVg?t=4148)

One of the key challenges of testing Plutus contracts involves adequately accounting for side-effects.

To deal with this, we must create a model of how the real-life system should work, and then generate a random sequence of actions that can be applied to that system and its model in step.

We can then track the states of the system and model, and compare them accross each step.

In our case, the real-life system in question could be the blockchain or an emulator.

## [Test Script](https://youtu.be/zW3D2iM5uVg?t=4376)

### Type: [TSState](https://youtu.be/zW3D2iM5uVg?t=4420)

The `TSState` type represents the state of one token sale instance:

```haskell
data TSState = TSState
    { _tssPrice    :: !Integer  -- Current price
    , _tssLovelace :: !Integer  -- Current supple of Lovelace in the contract
    , _tssToken    :: !Integer  -- Current supply of tokens in the contract
    } deriving Show

makeLenses ''TSState
```

### Type: [TSModel](https://youtu.be/zW3D2iM5uVg?t=4442)

The `TSModel` type represents the model for our system as a map from a `Wallet` to `TSState`:

```haskell
newtype TSModel = TSModel {_tsModel :: Map Wallet TSState}
    deriving Show

makeLenses ''TSModel
```

The idea in this case is that there are several wallets that run their own token sale.

We declare `TSModel` as an instance of the `ContractModel` type class to define how our model should behave:

```haskell
tests :: TestTree
tests = testProperty "token sale model" prop_TS

instance ContractModel TSModel where

    -- Define an associative data type to represent an action
    data Action TSModel =
              Start Wallet                            -- Wallet starts the token sale contract
            | SetPrice Wallet Wallet Integer          -- Second wallet sets price for token sale operated by first wallet
            | AddTokens Wallet Wallet Integer         -- Second wallet adds tokens to first wallet's token sales
            | Withdraw Wallet Wallet Integer Integer  -- Second wallet withdraws tokens and ADA from first wallet's token sale
            | BuyTokens Wallet Wallet Integer         -- Second wallet buys tokens from first wallet's token sale
        deriving (Show, Eq)

    -- Define an associative data type represent a key that identifies each instance of a running contract
    -- Generalized algebraic data type format: write constructors with their type signature
    -- We need this to allow for different type parameters for the constructors
    data ContractInstanceKey TSModel w s e where
        StartKey :: Wallet           -> ContractInstanceKey TSModel (Last TokenSale) TSStartSchema Text
        UseKey   :: Wallet -> Wallet -> ContractInstanceKey TSModel ()               TSUseSchema   Text

        -- Start key for the start contract
        -- Use key for wallet that owns the token sale and wallet that runs the contract respectively

    -- Define manual implementation of instanceTag ignoring the wallet argument
    -- We already know the wallet that runs the instance because it is one of the arguments to our ContractInstanceKey
    instanceTag key _ = fromString $ "instance tag for: " ++ show key

    -- Cannot use default implementation because it only works with at most one contract instance per wallet
    -- In our case we have three: one start and two use

    -- Tell the system how to generate a random action
    -- Can ignore the model state argument as we don't need it here
    arbitraryAction _ = oneof $  -- Oneof is a QuickCheck combinator
        (Start <$> genWallet) :  -- Start wallet on random wallet using fmap to produce Gen Action
        [ SetPrice  <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ AddTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ BuyTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ Withdraw  <$> genWallet <*> genWallet <*> genNonNeg <*> genNonNeg ]

    -- Uses applicative from monads to make code more compact (e.g. <$>)
    -- This works as long as the result of one computation does not rely on another

    -- Define the initial state of our model
    -- No token sales in the beginning (empty map)
    initialState = TSModel Map.empty

    -- Define the effect of the start action
    nextState (Start w) = do
        -- Change model state, creating entry in map for wallet w
        -- $= comes from Spect monad and takes a lens from the model and sets focus of lens to entry on the right
        (tsModel . at w) $= Just (TSState 0 0 0)  -- Zooms into to entry at key w in map, and set entry
                                                  -- 0 price, 0 tokens, 0 Lovelace
        wait 1                                    -- Wait for 1 step / "block" in Spec monad
                                                  -- Would need to wait maybe two steps if we used thread token

    -- Define the effect of setting price
    nextState (SetPrice v w p) = do  -- To token sale run by wallet v, by pricesetter w, at price p
        when (v == w) $                       -- Control.Monad "when" owner is price setter
            (tsModel . ix v . tssPrice) $= p  -- ix instead of at -> traversal
                                              -- If there is an entry for wallet v, set price to p
        wait 1

    -- Define the effect of adding tokens
    nextState (AddTokens v w n) = do
        -- Check whether token sale has started
        started <- hasStarted v                                     -- has the token sale started?

        -- If has started and want to add tokens
        when (n > 0 && started) $ do

            -- Ask for the balance change to determine how much the balance has changed during the course of the simulation
            bc <- askModelState $ view $ balanceChange w  -- view is synonymous with ^.
            let token = tokens Map.! v                    -- Get token that wallet v is selling
            when (tokenAmt + assetClassValueOf bc token >= n) $ do  -- does the wallet have the tokens to give?
                withdraw w $ assetClassValue token n  -- n tokens withdrawn from wallet w
                (tsModel . ix v . tssToken) $~ (+ n)  -- n tokens are added to token sale run by wallet v

        -- Otherwise, do nothing and wait one step if not started
        wait 1

    nextState (BuyTokens v w n) = do
        when (n > 0) $ do
            m <- getTSState v
            case m of
                Just t
                    | t ^. tssToken >= n -> do
                        let p = t ^. tssPrice
                            l = p * n
                        withdraw w $ lovelaceValueOf l
                        deposit w $ assetClassValue (tokens Map.! v) n
                        (tsModel . ix v . tssLovelace) $~ (+ l)
                        (tsModel . ix v . tssToken)    $~ (+ (- n))
                _ -> return ()
        wait 1

    nextState (Withdraw v w n l) = do
        when (v == w) $ do
            m <- getTSState v
            case m of
                Just t
                    | t ^. tssToken >= n && t ^. tssLovelace >= l -> do
                        deposit w $ lovelaceValueOf l <> assetClassValue (tokens Map.! w) n
                        (tsModel . ix v . tssLovelace) $~ (+ (- l))
                        (tsModel . ix v . tssToken) $~ (+ (- n))
                _ -> return ()
        wait 1

    perform h _ cmd = case cmd of
        (Start w)          -> callEndpoint @"start"      (h $ StartKey w) (tokenCurrencies Map.! w, tokenNames Map.! w, False) >> delay 1
        (SetPrice v w p)   -> callEndpoint @"set price"  (h $ UseKey v w) p                                                    >> delay 1
        (AddTokens v w n)  -> callEndpoint @"add tokens" (h $ UseKey v w) n                                                    >> delay 1
        (BuyTokens v w n)  -> callEndpoint @"buy tokens" (h $ UseKey v w) n                                                    >> delay 1
        (Withdraw v w n l) -> callEndpoint @"withdraw"   (h $ UseKey v w) (n, l)                                               >> delay 1

    precondition s (Start w)          = isNothing $ getTSState' s w
    precondition s (SetPrice v _ _)   = isJust    $ getTSState' s v
    precondition s (AddTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (BuyTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (Withdraw v _ _ _) = isJust    $ getTSState' s v

deriving instance Eq (ContractInstanceKey TSModel w s e)
deriving instance Show (ContractInstanceKey TSModel w s e)
```

Note that `arbitraryAction` can be tested using `sample`, ignoring the model state:

Note also the difference between `at` and `ix`, where `at` will insert and set a map value if the key does not exist, whereas `ix` will not do anything if the key does not exist.

```haskell
sample (arbitraryAction undefined) :: Gen (Action TSModel))
```

### Function: [genWallet](https://youtu.be/zW3D2iM5uVg?t=5082)

The `genWallet` function uses the `Gen` monad to generate a random wallet using the QuickCheck `elements` combinator:

```haskell
genWallet :: Gen Wallet
genWallet = elements wallets
```

In this case, `elements` takes a list of values of the type to generate and picks a random element.

### Function: [wallets](https://youtu.be/zW3D2iM5uVg?t=5104o)

The `wallets` helper function returns a list containing wallet 1 and wallet 2:

```haskell
wallets :: [Wallet]
wallets = [w1, w2]
```

### Function: [genNonNeg](https://youtu.be/zW3D2iM5uVg?t=5190)

The `genNonNeg` helper function generates a non-negative `Integer` using QuickCheck's `getNonNegative`:

```haskell
genNonNeg :: Gen Integer
genNonNeg = getNonNegative <$> arbitrary
```

### Function: [hasStarted](https://youtu.be/zW3D2iM5uVg?t=6112)

The `hasStarted` helper function uses `getTSState` to determine if the token sale has started:

```haskell
hasStarted :: Wallet -> Spec TSModel Bool
hasStarted v = isJust <$> getTSState v
```

### Function: [getTSState](https://youtu.be/zW3D2iM5uVg?t=6034)

The `getTSState` helper functions return a `TSState` from a `ModelState` and `Wallet`:

```haskell
getTSState' :: ModelState TSModel -> Wallet -> Maybe TSState
getTSState' s v = s ^. contractState . tsModel . at v

getTSState :: Wallet -> Spec TSModel (Maybe TSState)
getTSState v = do
    s <- getModelState
    return $ getTSState' s v
```

In this case, `getTSState'` takes a `ModelState` and a `Wallet` and returns `Maybe TSState` by going from a `ModelState` to a the actual model `contractState`, extracting the map `tsModel`, and then giving us `Maybe` the value `at` `v`.

Meanwhile, `getTSState` does the same thing but in the `Spec` monad, given a `Wallet` by getting the `ModelState` and then calling `getTSState'` on that.

### [Token Helpers](https://youtu.be/zW3D2iM5uVg?t=6320)

The token helper functions implement the idea that wallet 1 and wallet two will sell different tokens `aa` of `A` and `bb` of `B` in their token sales, respectively. Each begin with a constant `tokenAmt`:

```haskell
tokenCurrencies :: Map Wallet CurrencySymbol
tokenCurrencies = Map.fromList $ zip wallets ["aa", "bb"]

tokenNames :: Map Wallet TokenName
tokenNames = Map.fromList $ zip wallets ["A", "B"]

tokens :: Map Wallet AssetClass
tokens = Map.fromList [(w, AssetClass (tokenCurrencies Map.! w, tokenNames Map.! w)) | w <- wallets]

tss :: Map Wallet TokenSale
tss = Map.fromList
    [ (w, TokenSale { tsSeller = pubKeyHash $ walletPubKey w
                    , tsToken  = tokens Map.! w
                    , tsTT     = Nothing
                    })
    | w <- wallets
    ]

tokenAmt :: Integer
tokenAmt = 1_000
```

### Function: [prop_TS](https://youtu.be/zW3D2iM5uVg?t=6378)

The `prop_TS` function initializes the simulation with an `InitialDistribution`:

```haskell
prop_TS :: Actions TSModel -> Property
prop_TS = withMaxSuccess 100 . propRunActionsWithOptions
    (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left d) def def)
    instanceSpec
    (const $ pure True)
  where
    d :: InitialDistribution
    d = Map.fromList $ [ ( w
                         , lovelaceValueOf 1_000_000_000 <>
                           mconcat [assetClassValue t tokenAmt | t <- Map.elems tokens])
                       | w <- wallets
                       ]
```


### More Notes Soon...
