# [Uniswap in the EUTxO Model](https://youtu.be/CPfcyDaDtt8?t=1252)

Uniswap is a decentralized finance (DeFi) application that allows swapping of tokens using smart contracts and liquidity pools on the blockchain instead of a central authority.

## Example: [Alice, Bob, and Charlie](https://youtu.be/CPfcyDaDtt8?t=148)

### Redeemer: [Create](https://youtu.be/CPfcyDaDtt8?t=148)

A Uniswap can be initialized by creating a transaction `Tx1` that produces a UTxO at the script address containing an NFT to represent the Uniswap factory:

```
Tx1 --> Factory (US NFT | [])
```

In this case, the factory UTxO begins with an empty list, which represents the list of all its liquidity pools.

Suppose that Alice wishes to create a liquidity pool for two tokens `A` and `B` that allows others to swap `A` against `B`, or `B` against `A`. She does so by providing some initial liquidity for the pool, in this case:

```
Alice (1000A + 2000B)
```

The liquidity that Alice provides implies how she values token `A` relative to token `B`, i.e. `1A` == `2B`.

Alice creates the `AB` liquidity pool by creating a transaction `Tx2` that accepts the Uniswap factory UTxO and her initial liquidity as inputs and produces three outputs:

```
Factory (US NFT | [  ] / Create) + Alice (1000 A + 2000 B) --> Tx2 -->
Factory (US NFT | [AB]         ) + Alice (    1415 AB    )  +  Pool AB (1000 A + 2000 B + AB NFT | 1415)
```

The above transaction produces three outputs:

1. The newly created `Pool AB` containing Alice's initial liquidity, a freshly minted `AB NFT`, and a datum representing the amount of liquidity tokens Alice receives, i.e. sqrt( 1000 * 2000 )

2. The Uniswap factory with an updated datum reflecting the addition of `Pool AB`

3. A UTxO containing the freshly minted liquidity tokens that belong to Alice, i.e. `1415 AB`

### Redeemer: [Swap](https://youtu.be/CPfcyDaDtt8?t=342)

Now that `Pool AB` has been created, Bob can use it to swap `100 A` against `B` by creating another transaction `Tx3` that accepts `Pool AB` as well as a UTxO containing the `100 A` that Bob wishes to swap:

```
Pool AB (1000 A + 2000 B + AB NFT | 1415 / Swap) + Bob (100 A) --> Tx3 -->
Pool AB (1100 A + 1819 B + AB NFT | 1415       ) + Bob (181 B)
```

In this case, `Tx3` produces two outputs:

1. The updated UTxO representing `Pool AB` with an `100 A` more and `181 B` less
2. The `181 B` that Bob receives as a result of the swap

Note that the datum of `Pool AB` has not changed since the amount of liquidity tokens minted has not changed.

Note also that the value of `181 B` is derived from the Uniswap price discovery mechanism in which the product of the two token amounts must not decrease. In this case, Bob does not receive `200 B` for two reasons:

1. The amount of tokens in the liquidity pool is never allowed to reach zero
2. The more of one token that gets depleted the more expensive it becomes, i.e. the less of it you get in return

As such, this accounts for supply and demand in the sense that demand and relative price are positively correlated. Correspondingly, if the two tokens were swapped evenly, the price ratio between them would not change.

Fees are another reason why Bob does not receive `200 B` from `Tx3`. In this case, fees provide Alice an incentive to create a liquidity pool in the first place. In other words, she profits from swaps that others make. As such, not only does the product of the two token amounts not decrease, it must increase by a certain percentage depening on how much others swap (from 0.3%).

### Redeemer: [Add](https://youtu.be/CPfcyDaDtt8?t=626)

At this point, Charlie wishes to supply `Pool AB` with an additional liquidity of `400 A` and `800 B` by creating a transaction `Tx4` that accepts `Pool AB` as well as his additional liquidity:

```
Pool AB (1100 A + 1819 B + AB NFT | 1415 / Add) + Charlie (400 A + 800 B) --> Tx4 -->
Pool AB (1500 A + 2619 B + AB NFT | 1982      ) + Charlie (567 AB)
```

In this case, `Tx4` produces two outputs:

1. The updated `Pool AB` reflecting the total liquidity token amount contributed by Alice and Charlie
2. A UTxO containing the newly minted liquidity tokens that belong to Charlie, i.e. `567 AB`

Note, the liquidity ratio that Charlie provides reflects his own valuation of `A` relative to `B`.

Note also that Charlie is rewarded for adding liquidity to the pool but does not profit on swaps made previously.

### Redeemer: [Remove](https://youtu.be/CPfcyDaDtt8?t=752)

At this point, Alice can burn all of her liquidity tokens by creating a transaction `Tx5` that accepts `Pool AB` along with the liquidity tokens that she wishes to burn:

```
Pool AB (1500 A + 2619 B + AB NFT | 1982 / Remove) + Alice (    1415 AB    ) --> Tx5 -->
Pool AB ( 430 A +  750 B + AB NFT |  567         ) + Alice (1070 A + 1869 B)
```

In this case, `Tx5` produces two outputs:

1. The updated `Pool AB`, reflecting the removed tokens
2. The tokens from the pool that she receives in return

Note that burning liquidity tokens does not change the ratio of the pool.

### Redeemer: [Close](https://youtu.be/CPfcyDaDtt8?t=864)

Since Charlie now holds the last remaining liquidity tokens in `Pool AB`, he can choose to close to close `Pool AB` by creating a transaction `Tx6` which accepts the Uniswap factory, `Pool AB`, and his remaining liquidity tokens:

```
Factory (US NFT | [AB]) + Pool AB ( 430 A +  750 B + AB NFT |  567 / Close) + Charlie (   567 AB    ) --> Tx6 -->
Factory (US NFT | [  ]) +                                                     Charlie (430 A + 750 B)
```

In this case, `Tx6` produces two outputs:

1. The updated Uniswap factory with `Pool AB` removed from the pool list
2. A UTxO containing the remaining tokens of `Pool AB` that belong to Charlie

Note that the Uniswap factory is only required when updating the list of liquidity pools to prevent duplicates.
