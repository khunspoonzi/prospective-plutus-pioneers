# [A Simple Minting Policy](https://youtu.be/SsaVjSsPPcg?t=794)

Under normal circumstances, transactions cannot create or destroy tokens.

Generally, everything that enters a transaction comes out of it, apart from fees which are computed by the size of the transaction in bytes, and the complexity and resource consumption of the necessary validation scripts.

However, if we want a transaction to create or burn a native token, it is necessary to provide a so-called "minting policy," which is a script that is looked up using the associated `CurrencySymbol` hash i.e. script address.

The purpose of the minting policy is to decide whether a given transaction has the right to mint or burn tokens, and is executed along with the other validation scripts in the transaction.

Recall that ADA's `CurrencySymbol` is an empty bytestring, which implies that it isn't the hash of any existing minting policy. In other words, no script exists that allows the minting or burning of ADA, the total amount of which is fixed and was supplied in the genesis transaction.

Only custom native tokens can have minting policies and can be created or burned under certain conditions.

We'll see how that works in this section.

## [Revisiting ScriptContext](https://youtu.be/SsaVjSsPPcg?t=958)

Recall from a [previous section](../lecture03/1_Script-Contexts.md) that transaction validation scripts accept a `ScriptContext` parameter containing two fields: `TxInfo` and `ScriptPurpose`.

Up until this point, we've dealt with transactions whose `ScriptPurpose` was `Spending`.

When minting or burning native tokensi however, the `TxInfo` field's `TxInfoForge` value will necessarily no longer be zero, which will cause the the `ScriptPurpose` to change to `Minting`.

More specifically, `TxInfoForge` will now contain a bag of `AssetClass` elements, in which the corresponding minting policy script will be executed for each `CurrencySymbol` with a non-zero amount.

Unlike validation scripts which accept a datum, redeemer, and context, minting policy scripts only accept a context and a redeemer provided by the transaction.

### More Notes Soon...
