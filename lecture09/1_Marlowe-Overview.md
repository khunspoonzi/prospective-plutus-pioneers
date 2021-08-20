# [Marlowe Overview](https://youtu.be/H1WPL01qWCc?t=135)

Marlowe is a domain-specific language (DSL) for financial contracts built on top of Plutus.

The objective of such a language is to provide a specialized context that is more relevant to the language of the user rather than the language of the system. Some advantages of using a DSL include:

- Better feedback
- Clearer error messages
- More guarantees on program behavior

The so-called "Marlowe Suite" is comprised of several components:

| Component          | Description                                           |
| ------------------ | ----------------------------------------------------- |
| marlowe-finance.io | Website                                               |
| Run                | End users obtain and run contracts distributed        |
| Market             | Contracts are uploaded and downloaded with assurances |
| Play               | Contracts can be simulated interactively              |
| Build              | Contracts built in code, visually, embedded           |


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

### [Executing a Marlow Contract](https://youtu.be/H1WPL01qWCc?t=1392)

To execute a Marlowe contract, Marlowe Run is used to build and sign a series of transactions off chain, which are then validated on the blockchain through the Marlowe interpreter, i.e. a Plutus contract using the (E)UTxO model.

### [Usability](https://youtu.be/H1WPL01qWCc?t=1944)

Marlowe provides various ways of authoring contracts, including by:

- Writing pure Marlowe
- Using a no-code visual editor
- Using an embedded DSL in Haskel or JavaScript editors, compiled into pure Marlowe
- Generating contracts based on initial conditions in functions that produce Marlowe code

Marlowe contracts can also be explored before they are run in a simulation using the Marlowe Playground, which combines Marlowe Play and Marlowe Build into a single interface that allows you to interactively step foward and backward through the source code.

### [Assurance](https://youtu.be/H1WPL01qWCc?t=2220)

Marlowe provides assurances by using the power of logic, specifically through static analysis and verification.

Static analysis allows us to check all execution paths through a Marlowe contract, including all choices and all choices of slots for a transaction submission. For instance, static analysis can be used to automatically check if a constructor could fail (such as `Pay`), and if so, provide an example of a path that leads to that failure.


Meanwhile verification allows us to demonstrate that the system is safe by proving its properties by hand, e.g.:

- Accounts never have a negative balance
- Money preservation, i.e. `money_in = money_in_accounts + money_out`
- Closes produce no warnings
- Static analysis is sound and complete
