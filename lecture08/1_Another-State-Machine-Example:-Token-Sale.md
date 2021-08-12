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

### More Notes Soon
