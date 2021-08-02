# [Oracles](https://youtu.be/24SHPHEc3zo?t=46)

An oracle is a service or mechanism for accessing and using real-world information on the blockchain, for instance:

- Time
- Weather
- Temperatures
- Election Results
- Exchange Rates
- Stock Prices
- Randomness for Games or Lotteries

Often, such information may be necessary for determining the outcome of a smart contract.

It is worth noting that there are various ways and levels of sophistication of implementing Oracles. There are also certain challenges in doing so, including related to ensuring the trustworthiness and reliability of the data source.

Such challenges can be mitigated using various approaches inluding, for example, requiring the trusted party to provide collateral in the event of wrong or missing data, or basing results on an aggregate of multiple oracles.

In this section however, we won't worry so much about these challenges as we explore how oracles are used.

## [The Mechanics of an Oracle](https://youtu.be/24SHPHEc3zo?t=212)

As we know, for anything to happen on the blockchain, there must be a UTxO.

Therefore, an oracle must also be represented as a UTxO, which in this case will sit at the oracle's script address with a datum the contains the current value of the oracle's data feed.

We run into our first problem when considering that validation occurs only when you want to consume an output from a script address, and not when you produce an output at a script address.

This means that we cannot prevent anybody from producing arbitrary outputs at this same script address. In other words, we need a reliable mechanism for distinguising the true UTxO oracle output from other outputs sitting at the same script address. Enter NFTs.

Given that NFTs can exist exactly once, we can make the correct oracle unique by giving it an NFT in addition to the correct value in its datum. This in turn will serve to validate the oracle as an input before we use it.

The second problem we run into involves making an oracle generalizable. In other words, oracles need to be implemented in such a way that they will be usable in scripts that haven't even been written yet. To better understand how this works, we turn to a real-world use case.



## Case Study: [A Currency Swap](https://youtu.be/24SHPHEc3zo?t=388)

Consider a swap contract where a seller can deposit ADA at the swap address, and then buyer can take that ADA in exchange for the corresponding amount of USD represented by a USD native token.

In this case, we require an appropriate oracle whose value will help us to determine the exchange rate between ADA and USD. For instance, an oracle with a value of 1.75 might tell us that 100 ADA is worth 175 USD.

Because the oracle operator themselves must pay transaction fees in order to create the oracle UTxO, an incentive would be necessary for the oracle to provide its data in the first place. A realistic solution would therefore be to charge a per-use fee to the oracle user, e.g. 1 ADA to the buyer of the ADA. Thus we begin with:

```
Seller (100 ADA) --> tx0 --> Swap (100 ADA)
                             Buyer (175 USD + 1 ADA)
                             Oracle (NFT | 1.75)
```

Given that the swap validation logic will require access to the current oracle value, the oracle UTxO must be an input to the transaction `tx1`. The oracle's validator will execute validation logic according to the redeemer value, which in this case will be called "use" as we wish to use the oracle.

As a result, the oracle validator will check the following conditions:

1. The NFT is present in the consumed input to make sure the correct UTxO is used
2. There is a transaction output at the same oracle address containing the same NFT
3. The value of the oracle is not changed while it is being consumed
4. The appropriate 1 ADA fee is paid for its usage

Given the above conditions are satisfied, the transaction can proceed wherein the swap and buy outputs are consumed as additional inputs to `tx1`, giving rise to two more outputs in which the seller receives 175 USD and the buyer receives 100 ADA, which will be checked by the swap validator.

```
    Oracle (NFT | 1.75)         + Swap (100 ADA)   + Buyer (175 USD + 1 ADA) --> tx1
--> Oracle (NFT + 1 ADA | 1.75) + Seller (175 USD) + Buyer (100 ADA)
```

Obviously, an oracle with a fixed value such as in the example above may not be so useful with respect to real-world use cases. After all, exchange rates are constantly changing over time.

Given that UTxOs themselves never change, we therefore also require a mechanism that allows the oracle operator can update the oracle's current value. This can be accomplished by creating another transaction `tx2` that uses the oracle's "update" redeemer to check that:

1. The transaction is signed by the oracle operator
2. The NFT is present in the consumed input to make sure the correct UTxO is used
3. There is a transaction output at the same oracle address containing the same NFT

Having this update transaction would also allow for the operator to collect any usage fees that have acummulated:

```
    Oracle (NFT + 1 ADA | 1.75) --> tx2
--> Oracle (NFT         | 1.77) + Operator (1 ADA)
```
