# [Marlowe Overview](https://youtu.be/H1WPL01qWCc?t=135)

Marlowe is a domain-specific language (DSL) for financial contracts built on top of Plutus.

The objective of such a language is to provide a specialized context that is more relevant to the language of the user rather than the language of the system. Some advantages of using a DSL include:

- Better feedback
- Clearer error messages
- More guarantees on program behavior

In this section, we'll take a closer look at why DSLs like Marlowe are important when writing financial contracts.

## [What Does a Financial Contract Do?](https://youtu.be/H1WPL01qWCc?t=296)

Some of the capabilities of a financial contract include:

- Accept payments from participants in the contract
- Perform actions depending on internal or external information
- Make outbound payments to participants in the contract
- Establishes roles that can be "owned" by participants in the contract

With regard to roles, it is worth noting that Marlowe (via Cardano) establishes roles by minting representative tokens. As such, we can use those tokens to validate whether a particular agent is authorized to perform a given action. The use of tokens to represent roles therefore enables a given role to be tradeable or transferable.

## [DSL Design for Financial Contracts](https://youtu.be/H1WPL01qWCc?t=466)

A smart contract is a program that runs on a blockchain that can potentially:

- ... run forever
- ... wait for an input forever
- ... terminate holding assets
- ... "double spend" assets

Therefore, when designing a DSL for smart contracts, we would expect to end up with a language that makes us better able to ensure that they do what they are supposed to do, and that they also do not do what they are not supposed to do. In essence, we want a language that:

- ... is as simple as it can be
- ... produces contracts that can be read and simulated
- ... allows us to explore all behavior before running a contract
- ... allows us to prove that the contracts we write are safe in various ways

In particular, safety is a key priority. Consider the following safety features that Marlowe provides by default:

| Feature                           | Mechanism                                  |
| --------------------------------- | ------------------------------------------ |
| Contracts are finite              | No recursion or loops                      |
| Contracts will terminate          | Timeouts on actions: choice, deposit, etc. |
| Contracts have a defined lifetime | Read off from timeouts                     |
| No assets are retained on close   | (Local) accounts refunded on close         |
| Conservation of value             | Underlying blockchain                      |

It is worth noting that Plutus alone does not provide these assurances out of the box.

## [The Marlowe Language](https://youtu.be/H1WPL01qWCc?t=710)

In essence, Marlowe is can be conceptualized as a Haskell data type:

```haskell
data Contract = Close
              | Pay    Party                  Payee    Value    Contract
              | If     Observation            Contract Contract
              | When   [Case Action Contract] Timeout  Contract
              | Let    ValueId                Value    Contract
              | Assert Observation            Contract
```

The constructors in `Contract` can be summarized as follows:

- `Close`: A `Contract` is closed and nothing is retained.

- `Pay`: A `Party` makes a payment to a `Payee` of a `Value` and then the continuation `Contract` is invoked.

- `If`: If an `Observation` is true, the first `Contract` is invoked, otherwise the second `Contract` is invoked.

- `When`: A specific `Contract` is invoked upon its corresponding `Action` unless a `Timeout` occurs, at which point a different `Contract` will be invoked.

## [The Marlowe Suite](https://youtu.be/H1WPL01qWCc?t=918)

The so-called "Marlowe Suite" is comprised of several components:

| Component          | Description                                           |
| ------------------ | ----------------------------------------------------- |
| marlowe-finance.io | Website                                               |
| Run                | End users obtain and run contracts distributed        |
| Market             | Contracts are uploaded and downloaded with assurances |
| Play               | Contracts can be simulated interactively              |
| Build              | Contracts built in code, visually, embedded           |

### More Notes Soon...
