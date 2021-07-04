# [The (E)UTxO Model](https://www.youtube.com/watch?v=_zr3W8cgzIQ&t=402s)

The (E)UTxO models is the accounting model that Cardano uses, and it stands for:

| Letter | Word        |
| ------ | ----------- |
| E      | extended    |
| U      | unspent     |
| Tx     | transaction |
| O      | output      |

The (E)UTxO model extends the UTxO model introduced by Bitcoin.

In contrast, Ethereum uses a separate account-based model, which is more representative of the system that banks use with accounts and account balances.

The UTxO models deal with transaction outputs, which are outputs from previous transactions on the blockchain that have not yet been spent.

## Example: [Alice and Bob](https://youtu.be/_zr3W8cgzIQ?t=460)

**ADA**: The currency unit for Cardano.

**Transaction**: Something that has an arbitrary number of inputs and outputs, where each input must be a complete UTxO.

---

Alice has a UTxO of 100 **ADA**.

Bob has a UTxO of 50 **ADA**.

Alice wants to send 10 ADA to Bob, so she creates a **transaction**.

Because a transaction can only accept complete UTxOs as inputs, Alice cannot simply split her 100 ADA UTxO into two fragments of 90 ADA and 10 ADA, and then pass 10 ADA into the transaction. Instead, she must pass the full 100 ADA UTxO as an input into the transaction (Tx1).

At this point, Alice's UTxO of 100 ADA is no longer unspent, i.e. it has been spent.

In order to achieve her original objective of sending 10 ADA to Bob, Alice creates two outputs from Tx1:

1. 10 ADA UTxO to Bob
2. 90 ADA UTxO in change to herself

```
Alice (100 ADA) --> Tx1 --> Alice (90 ADA) + Bob (10 ADA)
```

In this example, the sum of the outputs is equal to the sum of the inputs. In practice, there are exceptions:

- Accounting for fees, where the sum of the inputs will be slightly greater than the sum of the outputs.
- Minting of native tokens, where the sum of the outputs will be greater than the sum of the inputs.

- Buring of native tokens, where the sum of the inputs will be greater than the sum of the outputs.

## Example: [Alice and Bob Contd.](https://youtu.be/_zr3W8cgzIQ?t=710)

At this point...

Alice has a UTxO of 90 ADA

Bob has two UTxOs:
1. His original UTxO of 50 ADA
2. A new UTxO of 10 ADA from Alice

Alice and Bob want to send 55 ADA **each** to Charlie. To do this, Alice and Bob create a transaction together.

Alice has no choice but to use her only UTxO of 90 ADA.

Bob has no choice but to use both his UTxOs since neither of his two UTxOs alone are large enough to cover the 55 ADA amount.

The resulting transaction (Tx2) takes three UTxO inputs and produces three UTxO outputs:

```
Alice (90 ADA) + Bob (50 ADA) + Bob (10 ADA) --> Tx2 -> Alice (35 ADA) + Bob (5 ADA) + Charlie (110 ADA)
```

## Summary

The effect of a transaction is to consume unspent transaction outputs (UTxO) and to produce new ones. This is the only thing that happens on a UTxO blockchain. In other words, no data belonging to an existing UTxO ever changes; the UTxO itself is simply unspent until it is spent.

## More Notes Soon...
