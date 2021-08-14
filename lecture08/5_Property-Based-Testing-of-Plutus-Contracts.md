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

    instanceTag key _ = fromString $ "instance tag for: " ++ show key

    arbitraryAction _ = oneof $
        (Start <$> genWallet) :
        [ SetPrice  <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ AddTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ BuyTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ Withdraw  <$> genWallet <*> genWallet <*> genNonNeg <*> genNonNeg ]

    initialState = TSModel Map.empty

    nextState (Start w) = do
        (tsModel . at w) $= Just (TSState 0 0 0)
        wait 1

    nextState (SetPrice v w p) = do
        when (v == w) $
            (tsModel . ix v . tssPrice) $= p
        wait 1

    nextState (AddTokens v w n) = do
        started <- hasStarted v                                     -- has the token sale started?
        when (n > 0 && started) $ do
            bc <- askModelState $ view $ balanceChange w
            let token = tokens Map.! v
            when (tokenAmt + assetClassValueOf bc token >= n) $ do  -- does the wallet have the tokens to give?
                withdraw w $ assetClassValue token n
                (tsModel . ix v . tssToken) $~ (+ n)
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

### More Notes Soon...
