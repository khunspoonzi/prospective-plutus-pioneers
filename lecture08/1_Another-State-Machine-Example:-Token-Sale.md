# [Another State Machine Example: Token Sale](https://youtu.be/zW3D2iM5uVg?t=52)

In this section, we'll look at another example of a state machine.

This time, the objective of our state machine will be to allow somebody (a seller) to sell their tokens by locking them into a contract at a given price for other people (buyers) to purchase.

In this case, we begin with the seller who owns an NFT to serve the purpose of a thread token. The seller must first create a transaction `Tx1` to lock their NFT at the token sale script address `TS` with a price datum of 0:

```
Seller (NFT) --> Tx1 --> TS (NFT | 0)
```

One of the actions the seller can take at this point is to set the token price datum to a different value by creating another transaction `Tx2` with the current UTxO as input and the updated UTxO as output:

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

In practice, the actions represented by each transaction can be invoked in arbitrary order.

### More Notes Soon
