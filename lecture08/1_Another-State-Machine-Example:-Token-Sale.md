# [Another State Machine Example: Token Sale](https://youtu.be/zW3D2iM5uVg?t=52)

In this section, we'll look at another example of a state machine.

This time, the objective of our state machine will be to allow somebody (a seller) to sell their tokens by locking them into a contract at a given price for other people (buyers) to purchase.

In this case, we begin with the seller who owns an NFT to serve the purpose of a thread token. The seller must first create a transaction `Tx1` to lock their NFT at the token sale script address `TS` with a price datum of 0:

```
Seller (NFT) --> Tx1 --> TS (NFT | 0)
```

One of the operations the seller can perform at this point is to set the token price datum to a different value by creating another transaction `Tx2` with the current UTxO as input and the updated UTxO as output:

```
TS (NFT | 0) --> Tx2 --> TS (NFT | 6)
```

In this case, the seller has set their price to 6 ADA per token.

At this point however, the seller has not yet offered any tokens to sell. To do so, the seller must create another transaction `Tx3` that accepts the contract UTxO plus another UTxO containing some of the tokens they wish to sell:

```
TS (NFT | 6) + Seller (5T) --> Tx3 --> TS (NFT + 5T | 6)
```

As can be seen above, the output of `Tx3` will be another UTxO at the contract script address containing a value of the NFT + 5 tokens at a price of 6 ADA per token. In other words, the seller has locked 5 tokens in the contract.

If a buyer wishes to puchase two tokens from the five that are currently locked in the contract, they can do so by creating another transaction `Tx4` with two inputs and two outputs:

```
TS (NFT + 5T | 6) + Buyer (12 ADA) --> Tx4 --> TS (NFT + 3T + 12 ADA | 6) + Buyer (2T)
```

The transaction above receives the current contract UTxO along with a UTxO containing 12 ADA from the buyer, and produces a new contract UTxO with 12 ADA and 3 tokens plus another UTxO with 2 tokens igoing to the buyer.

If the seller wishes to retrieve their ADA or tokens, they must create a transaction `Tx5` with the contract UTxO as input and two outputs that reflect the amount of ADA and / or tokens the seller has withdrawn:

```
TS (NFT + 3T + 12 ADA | 6) --> Tx5 --> TS (NFT + 2T | 6) + Seller (1T + 12 ADA)
```

In this case, we see the seller withdraws all 12 ADA plus 1 token, leaving 2 remaining tokens locked in the contract.

In practice, the operations represented by each transaction can be invoked in arbitrary order.


## [On-chain Script](https://youtu.be/zW3D2iM5uVg?t=270)

### Type: [TokenSale](https://youtu.be/zW3D2iM5uVg?t=270)

The `TokenSale` type will serve as a contract parameter and is defined as a record of three fields:

```haskell
data TokenSale = TokenSale
    { tsSeller :: !PubKeyHash           -- Seller's public key hash
    , tsToken  :: !AssetClass           -- The token being sold
    , tsTT     :: !(Maybe ThreadToken)  -- The NFT identifying the token sale contract
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''TokenSale
```

Note that we use `Maybe ThreadToken` in this example for easier testing. In practice, we need the thread token NFT to identify the correct UTxO at the token sale address in the event that there cwis more than one present.

### Type: [TSRedeemer](https://youtu.be/zW3D2iM5uVg?t=340)

The `TSRedeemer` type will establish the four operations that the token sale contract will support:

```haskell
data TSRedeemer =
      SetPrice Integer          -- Set token price in Lovelace to a new value
    | AddTokens Integer         -- Add tokens to lock into contract
    | BuyTokens Integer         -- Buy tokens locked in contract
    | Withdraw Integer Integer  -- Withdraw tokens and Lovelace from contract
    deriving (Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''TSRedeemer
```

Note that the use of `Integer` arguments in the `Withdraw` operation can be considered as somewhat of an anti-pattern as it isn't clear that the first refers to tokens while the second refers to Lovelace.

### Function: [lovelaces](https://youtu.be/zW3D2iM5uVg?t=384)

The `lovelaces` helper function extracts the amount of Lovelace from a given value:

```haskell
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue
```

### Function: [transition](https://youtu.be/zW3D2iM5uVg?t=398)

The `transition` function handles the transition states of the state machine:

```haskell
{-# INLINABLE transition #-}
transition :: TokenSale -> State Integer -> TSRedeemer -> Maybe (TxConstraints Void Void, State Integer)
transition ts s r = case (stateValue s, stateData s, r) of

    -- Set price where price >= 0 and transaction is signed by seller
    (v, _, SetPrice p)   | p >= 0           -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State p v
                                                    )

    -- Add tokens to contract where amount of tokens is positive
    -- No constraint as anybody could "donate" tokens to the seller if they want
    (v, p, AddTokens n)  | n > 0            -> Just ( mempty
                                                    , State p $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) n
                                                    )

    -- Buy tokens where anybody can buy tokens at the specified price
    (v, p, BuyTokens n)  | n > 0            -> Just ( mempty
                                                    , State p $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (n * p)
                                                    )

    -- Withdraw Lovelace or tokens where each must be >= 0 and transaction signed by seller
    (v, p, Withdraw n l) | n >= 0 && l >= 0 -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State p $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (negate l)
                                                    )

    -- All other transitions that are illegal
    _                                       -> Nothing
```

Recall that the `State` contains the datum and the value.

### Function: [tsStateMachine](https://youtu.be/zW3D2iM5uVg?t=706)

The `tsStateMachine` function defines a state machine using the `mkStateMachine` smart constructor:

```haskell
{-# INLINABLE tsStateMachine #-}
tsStateMachine :: TokenSale -> StateMachine Integer TSRedeemer
tsStateMachine ts = mkStateMachine (tsTT ts) (transition ts) (const False)
```

The `mkStateMachine` smart constructor takes three arguments: thread token, transition function, and final state function, which in this case will always be `False` as we do not have a final state, i.e. the contract runs forever.

In the case of the [rock, paper, scissors example](../lecture07/3_State-Machines.md), we couldn't use the `mkStateMachine` smart constructor because the hash check for the game choice could not be expressed as a constraint.

### Function: [mkTSValidator](https://youtu.be/zW3D2iM5uVg?t=782)

The `mkTSValidator` function defines a validator using `tsStateMachine`:

```haskell
{-# INLINABLE mkTSValidator #-}
mkTSValidator :: TokenSale -> Integer -> TSRedeemer -> ScriptContext -> Bool
mkTSValidator = mkValidator . tsStateMachine
```

### Type: [StateMachine]()

The`TS` type combines the datum and redeemer using `StateMachine`:

```haskell
type TS = StateMachine Integer TSRedeemer
```

### Function: [tsTypedValidator](https://youtu.be/zW3D2iM5uVg?t=798)

The `tsTypedValidator` function compiles `mkTSValidator` to Plutus Core:

```haskell
tsTypedValidator :: TokenSale -> Scripts.TypedValidator TS
tsTypedValidator ts = Scripts.mkTypedValidator @TS
    ($$(PlutusTx.compile [|| mkTSValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ts)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @TSRedeemer
```

### Function: [tsValidator](https://youtu.be/zW3D2iM5uVg?t=802)

The `tsValidator` function converts our `tsTypedValidator` into a `Validator`:

```haskell
tsValidator :: TokenSale -> Validator
tsValidator = Scripts.validatorScript . tsTypedValidator
```

### Function: [tsAddress](https://youtu.be/zW3D2iM5uVg?t=804)

The `tsAddress` function converts our `tsValidator` into a `Ledger.Address`:

```haskell
tsAddress :: TokenSale -> Ledger.Address
tsAddress = scriptAddress . tsValidator
```

### Function: [tsClient](https://youtu.be/zW3D2iM5uVg?t=806)

The `tsClient` function defines the state machine client using `mkStateMachineClient`:

```haskell
tsClient :: TokenSale -> StateMachineClient Integer TSRedeemer
tsClient ts = mkStateMachineClient $ StateMachineInstance (tsStateMachine ts) (tsTypedValidator ts)
```

### Function: [mapErrorSM](https://youtu.be/zW3D2iM5uVg?t=826)

The `mapErrorSM` function converts our state machine error type to `Text`:

```haskell
mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show
```

## [Off-chain Script](https://youtu.be/zW3D2iM5uVg?t=854)

### Function: [startTS](https://youtu.be/zW3D2iM5uVg?t=854)

The `startTS` function starts the token sale with an `AssetClass` and a `Bool` of whether to use a thread token:

```haskell
startTS :: AssetClass -> Bool -> Contract (Last TokenSale) s Text ()
startTS token useTT = do

    -- Get seller's public key hash
    pkh <- pubKeyHash <$> Contract.ownPubKey

    -- Get thread token if necessary
    tt  <- if useTT then Just <$> mapErrorSM getThreadToken else return Nothing

    -- Define TokenSale instance
    let ts = TokenSale
            { tsSeller = pkh
            , tsToken  = token
            , tsTT     = tt
            }

        -- Get state machine client
        client = tsClient ts

    -- Create the first UTxO at the state machine address, with the client, datum, and value
    void $ mapErrorSM $ runInitialise client 0 mempty

    -- Tell the ts instance so other parties could discover the value of ts and find the state machine
    tell $ Last $ Just ts

    -- Log an info message
    logInfo $ "started token sale " ++ show ts
```


### Function: [setPrice](https://youtu.be/zW3D2iM5uVg?t=1092)

The `setPrice` function invokes `runStep` with the client and `SetPrice` redeemer:

```haskell
setPrice :: TokenSale -> Integer -> Contract w s Text ()
setPrice ts p = void $ mapErrorSM $ runStep (tsClient ts) $ SetPrice p
```

### Function: [addTokens](https://youtu.be/zW3D2iM5uVg?t=1122)

The `addTokens` function invokes `runStep` with the client and `AddTokens` redeemer:

```haskell
addTokens :: TokenSale -> Integer -> Contract w s Text ()
addTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ AddTokens n
```

### Function: [buyTokens](https://youtu.be/zW3D2iM5uVg?t=1126)

The `buyTokens` function invokes `runStep` with the client and `BuyTokens` redeemer:

```haskell
buyTokens :: TokenSale -> Integer -> Contract w s Text ()
buyTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ BuyTokens n
```

### Function: [withdraw](https://youtu.be/zW3D2iM5uVg?t=1130)

The `withdraw` function invokes `runStep` with the client and `Withdraw` redeemer:

```haskell
withdraw :: TokenSale -> Integer -> Integer -> Contract w s Text ()
withdraw ts n l = void $ mapErrorSM $ runStep (tsClient ts) $ Withdraw n l
```

### Type: [TSStartSchema](https://youtu.be/zW3D2iM5uVg?t=1168)

The `TSStartSchema` type starts the token sale contract:

```haskell
type TSStartSchema =
        Endpoint "start"      (CurrencySymbol, TokenName, Bool)
```

### Function: [TSUseSchema](https://youtu.be/zW3D2iM5uVg?t=1192)

The `TSUseSchema` type define the operations available once the contract is running:

```haskell
type TSUseSchema =
        Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "buy tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)
```

### Change: [Endpoint Signatures](https://youtu.be/zW3D2iM5uVg?t=1220)

It should be noted that the signature of endpoints has changed:

```haskell
-- Old Signature
endpoint :: forall l a w s e. (HasEndpoint l a s, AsContractError e) => Contract w s e a

-- New Signature
endpoint :: forall l a w s e b. (HasEndpoint l a s, AsContractError e, FromJSON a) => (a -> Contract w s e b) -> Promise w s e b
```

Previously, endpoints that corresponded to parameters of type `a` simply returned a contract of type `a` and would block until `a` was provided from the outside.

Now, there is an additional continuation argument, which is a function that, given `a`, returns a contract of type `b`. The `endpoint` function now returns a `Promise` using `b`.

Here, a `Promise` is defined as a "wrapper indicating that this contract starts with a waiting action. For use with `select`". In essence, a `Promise` is basically a `Contract` that first waits for external input.

In practice we will make use if the following methods:

```haskell
awaitPromise :: Promise w s e a -> Contract w s e a
select :: forall w s e a. Promise w s e a -> Promise w s e a -> Promise w s e a
```

### Function: [startEndpoint](https://youtu.be/zW3D2iM5uVg?t=1346)

The `startEndpoint` function allows the seller to start the token sale:

```haskell
startEndpoint :: Contract (Last TokenSale) TSStartSchema Text ()
startEndpoint = forever  -- Forever repeats a monadic computation

              -- Handle and log errors
              $ handleError logError

              -- Await promise
              $ awaitPromise

              -- Call endpoint with the continuation function, expecting a Promise
              $ endpoint @"start" $ \(cs, tn, useTT) -> startTS (AssetClass (cs, tn)) useTT
```

### Function: [useEndpoints](https://youtu.be/zW3D2iM5uVg?t=1346)

The `useEndpoints` function allows the token sale contract to perform operations:

```haskell
useEndpoints :: TokenSale -> Contract () TSUseSchema Text ()
useEndpoints ts = forever  -- Forever repeats a monadic computation

                -- Handle and log errors
                $ handleError logError

                -- Await promise for contract
                $ awaitPromise

                -- Apply select to operations
                $ setPrice' `select` addTokens' `select` buyTokens' `select` withdraw'
  where

    -- Define operation Promises with continuation functions
    setPrice'  = endpoint @"set price"  $ setPrice ts
    addTokens' = endpoint @"add tokens" $ addTokens ts
    buyTokens' = endpoint @"buy tokens" $ buyTokens ts
    withdraw'  = endpoint @"withdraw"   $ uncurry (withdraw ts)
```

In the promise definitions above, `uncurry` handles the expected two `Integer` arguments in `withdraw`:

```haskell
uncurry :: (a -> b -> c) -> (a, b) -> c
```

In this case, `uncurry` takes a function that takes two arguments `a` and `b` and turns it into a function that takes an `(a, b)` tuple. In other words, this is simply the inverse of currying a function.

### More Notes Soon
