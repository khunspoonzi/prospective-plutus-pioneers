# [The (E)UTxO Model](https://www.youtube.com/watch?v=_zr3W8cgzIQ&t=402s)

The (E)UTxO model is the accounting model that Cardano uses, and it stands for:

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

- Burning of native tokens, where the sum of the inputs will be greater than the sum of the outputs.

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

But how do we prevent a transaction from spending arbitrary UTxOs, e.g. Bob spending Alice's money?

## [Transaction Signatures](https://youtu.be/_zr3W8cgzIQ?t=771)

Alice, Bob, and Charlie all have ADA addresses, each of which corresponds to a public key, which in turn can be verified by a digital signature.

Under the traditional UTxO model, these signatures dictate the conditions under which a transaction can spend a given UTxO. In other words, a transaction would require a signature from the owners of its given inputs.

As such, in the above example, Tx1 required only Alice's signature whereas Tx2 required both Alice and Bob's signatures.

Under the extended (E)UTxO model however, the validation process that allows a transaction to spend a UTxO becomes more general.

## [Smart Contracts: Scripts and Redeemers](https://youtu.be/_zr3W8cgzIQ?t=844)

Under the (E)UTxO model, the need for validation through transaction signatures is replaced by a script containing arbitrary logic that lives in a more general address, which does not correspond to a public key.

This arbitrary logic can, in turn, dictate the conditions under which a transaction can spend a given UTxO.

In lieu of a transaction signature, the transaction input itself is used to justify whether the transaction is allowed to consume it, along with a piece of arbitrary data called a "redeemer".

### Example: [Bitcoin](https://youtu.be/_zr3W8cgzIQ?t=1004)

Bitcoin offers a primitive implementation of smart contracts with scripts and redeemers using a programming language known as [Script](https://en.bitcoin.it/wiki/Script).

In the case of Bitcoin, a script on the UTxO side receives a redeemer from the input side, which is used to decide whether it is permitted to consume the UTxO.

More information can be given to the script if necessary, but generally speaking, Bitcoin smart contracts operate with very little context aside from the redeemer.

### Example: [Ethereum](https://youtu.be/_zr3W8cgzIQ?t=1040)

Ethereum implements smart contracts toward the opposite extreme with respect to context using [Solidity](https://en.wikipedia.org/wiki/Solidity).

In the case of Ethereum smart contracts, Solidity scripts are able to see the complete state of the blockchain thus making them much more powerful than Bitcoin smart contracts.

Importantly however, the vast number of possibilities afforded by such power can make it difficult to predict what a given script will do, thus opening the door to all sorts of security issues.

### Example: [Cardano](https://youtu.be/_zr3W8cgzIQ?t=1100)

Cardano's implementation of smart contracts using [Plutus](https://developers.cardano.org/en/programming-languages/plutus/overview/) lies somewhere in between those of Bitcoin and Ethereum.

In the case of Cardano, a script cannot see the complete state of the blockchain like in the case of Ethereum, but can see more than the redeemer of a single input like in the case of Bitcoin.

Specifically, a Plutus script in a Cardano smart contract can see the whole transaction that is being validated. In other words, it can see all other inputs and outputs of the transaction, as well as the transaction itself in deciding whether it is permitted to consume a given output.

One additional component is required in order to make Cardano smart contracts as powerful and expressive as Ethereum smart contracts: the so-called "datum," which is a piece of data that can be associated with a UTxO in addition to its value.

With respect to Plutus, the spending transaction is responsible for providing the script, redeemer, and datum. Conversely, the producing transaction is only required to provide the hashes of the script and datum, although it can include the full script and datum optionally.

### Comparison: [Cardano vs. Ethereum](https://youtu.be/_zr3W8cgzIQ?t=1190)

The datum component of Cardano smart contracts ensures that any given piece of logic that can be expressed in an Ethereum smart contract can also be expressed within the (E)UTxO model that Cardano uses.

Aside from being at least as powerful as Ethereum smart contracts, Cardano smart contracts offer a number of added benefits:

- Using Plutus, it is possible to check whether a transaction will validate in your wallet before ever sending it to the blockchain. In other words, given all expected inputs, a transaction that validates in your wallet will also validate on the Cardano blockchain.

- While it isn't possible to prevent certain failures such as a race condition between two transactions attempting to consume the same output, such failures are guranteed not to incur a fee on the Cardano blockchain. In contrast, a failed transaction on the Ethereum blockchain may still incur a gas fee.

- It is easier to analyze and improve a Plutus script from a security standpoint because it isn't necessary to consider the complete state of the blockchain but rather the much more limited scope of the spending transaction. This makes it much easier to understand what is happening as well as what could go wrong.

## Summary

The Bitcoin and Cardano blockchains both utilize UTxO accounting models.

The effect of a transaction under a UTxO model is to consume unspent transaction outputs (UTxOs) and produce new ones. This is the only thing that happens on a UTxO blockchain. In other words, no data belonging to an existing UTxO ever changes; the UTxO itself is simply unspent until it is spent.

In order for a transaction to spend a given UTxO under the traditional UTxO model, it must be signed by the UTxO's owner.

Under the (E)UTxO model, a script containing arbitrary logic replaces public keys while a redeemer replaces the transaction signature. This is what we mean when we refer to smart contracts.

In the case of Cardano, an additional component known as a "datum" is utilized to afford its smart contracts the same level of power as Ethereum smart contracts, but without the inherent risks associated with needing to consider the complete state of the blockchain within a given script.
