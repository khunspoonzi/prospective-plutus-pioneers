# [Handling Time](https://youtu.be/6_rfCCY9_gY?t=422)

We know that one of the advantages of the Cardano (E)UTxO model has over Ethereum is that transaction validation can happen off-chain in the wallet before ever being sent to the blockchain ([more info](../lecture01/1_The-(E)UTxO-Model.md#comparison-cardano-vs-ethereum)).

However, the concept of time serves as one point of uncertainty with respect to this advantage. In other words, time is one factor that might influence whether a transaction is valid, such as in the case of an [auction](../lecture01/2_An-Auction-Contract-in-the-(E)UTxO-Model.md).

It isn't immediately clear how discrepancies in time might be handled between a transaction that is validated in your wallet, compared to when it is actually sent to the blockchain. After all, we wish to ensure that a transaction validated in the wallet is guaranteed to validate in the node.

## Field: [txInfoValidRange](https://youtu.be/6_rfCCY9_gY?t=562)

The txInfoValidRange field belonging to [TxInfo](./1_Script-Contexts.md#type-txinfo) in [ScriptContext](./1_Script-Contexts.md#type-scriptcontext) is used to solve this issue of time validation.

Specifically, txInfoValidRange specifies a time interval in the transaction itself that can be used during a series of general pre-checks before any scripts are actually run. Some of these pre-checks include verifying that:

- All inputs are present
- Relevant balances add up
- Appropriate fees are included
- The current time is valid relative to the time range

If any of the above checks do not pass, then validation fails immediately and no validator scripts are run.

Off-chain validation can therefore be considered deterministic in the sense that the result of the validation script does not depend on when it is run. In other words, the effect of time is extracted into a separate check so that it does not need to be accounted for in off-chain validation.

All transactions use an infinite time range by default, starting at Cardano's genesis block and continuing indefinitely.

### More notes soon...
