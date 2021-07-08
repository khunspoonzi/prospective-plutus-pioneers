# [An Auction Contract in the (E)UTxO Model](https://youtu.be/_zr3W8cgzIQ?t=1775)

Smart contracts can be applied to various real-world use cases, for example to carry out an auction.

Within the context of a (E)UTxO smart contract, an auction would be parameterized by the item being auctioned, its owner, a minimum bid, and a deadline.

## Example: [Alice Auctions An NFT](https://youtu.be/_zr3W8cgzIQ?t=460)

**NFT**: Non-fungible Token, a native token on the Cardano blockchain that exists only once and can represent anything from a work of digital art to a real-world asset.

---

Alice owns an NFT and wants to auction it off so she creates a UTxO at the script output, where the script is the auction script.

The value of the UTxO is simply the NFT, and its datum is nothing pending a highest bidder and bid.

For simplicity's sake, we allow for a UTxO that contains just a native token and ignore the fact that on the Cardano blockchain all UTxOs must have some ADA value. As such:

Alice has an auction UTxO with a value of her NFT

Bob has a UTxO of 100 ADA

Bob wishes to bid 100 ADA (the auction's minimum bid), so he creates a transaction (Tx1) with two inputs:

1. The auction UTxO for the NFT
2. Bob's bid of 100 ADA

This transaction produces one output:

1. A new auction UTxO whose value and datum now reflect the highest bidder, Bob, and his bid of 100 ADA

```Auction (NFT) + Bob (100 ADA) --> Tx1 --> Auction (NFT + 100 ADA) | (Bob, 100)```

In this case, the transaction redeemer that is used to unlock the original auction UTxO is an algebraic data type called "bid," which is checked by the script to ensure that the minimum bid requirement is met, that the bid took place before the deadline, and that the correct input and outputs are present.

## Example: [Alice Auctions An NFT Contd.](https://youtu.be/_zr3W8cgzIQ?t=1983)

Charlie wishes to outbid Bob by bidding 200 ADA so he creates a new transaction (Tx2), again with two inputs:

1. The auction UTxO produced by Tx1
2. Charlie's bid of 200 ADA

This time, the transaction produces two outputs:

1. A new auction UTxO whose value and datum now reflect the new highest bidder, Charlie, and his bid of 200 ADA
2. 100 ADA UTxO bid returned to Bob

```Auction (NFT + 100 ADA) | (Bob, 100) + Charlie (200 ADA) --> Tx2 --> Auction (NFT + 200 ADA) | (Charlie, 200) + Bob (100 ADA)```

Once again, the script will check that the auction deadline has been respected as before, as well as whether the current bid is higher than the previous highest bid. It will also ensure that the previous highest bidder, Bob, receives his bid back.

Note that the smart contract knows to return 100 ADA to Bob because this information was stored in the datum of the output UTxO from Tx1.

## Example: [Alice Auctions An NFT Contd.](https://youtu.be/_zr3W8cgzIQ?t=2092)

There are no further bids for Alice's NFT, the auction deadline passes, and the auction can be closed.

In order to close the auction, a final transaction must be created so that Alice can receive her highest bid and the highest bidder, Charlie, can receive Alice's NFT.

In theory, anyone can create this transaction. In practice, it will either be Alice or Charlie to create this transaction since they are the only two who have a real incentive to do so.

The closing transaction (Tx3) will have one input:

1. The auction UTxO produced by Tx2

It will produce two outputs:

1. Charlie's UTxO for the NFT
2. Alice's UTxO of 200 ADA

In this case, the transaction redeemer used will be a "close" redeemer instead of a "bid" redeemer used in the previous two transactions.

Finally, the script will check that the auction deadline has been respected, that the highest bidder receives the NFT, and that the auction owner receives the highest bid.

## Example: [Alice Auctions An NFT (No Bids)](https://youtu.be/_zr3W8cgzIQ?t=2168)

In an alternate reality, Alice receives no bids for her NFT.

In this case, Alice will need to create a transaction (Tx1) that uses a "close" redeemer in which her NFT is returned to herself (or anybody else if she so pleases).

```Auction (NFT) --> Tx1 --> Alice (NFT)```

## Burning Questions

### [What if a transaction to close the auction is never created?](https://youtu.be/sN3BIa3GAOc?t=7)

When considering a circumstance in which nobody can or is willing to create a closing transaction, it is important to note that UTxOs on the blockchain are passive data. As such, a closing transaction **must** be created in order to complete the auction.

Since external triggers are required in order to change the state of a blockchain, a UTxO living on that blockchain would not be able to invoke a transaction by itself. However, it would be possible to automate the creation such a transaction using a wallet script.
