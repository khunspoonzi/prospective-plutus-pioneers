# [The Contract Monad](https://youtu.be/g4lvA14I-Jg?t=6704)

Finally, we arrive at the Contract monad, which will be an essential component of our off-chain validation scripts.

Notably, the Contract monad expects the following four parameters:

| Parameter | Description                                                                                 |
| --------- | ------------------------------------------------------------------------------------------- |
| w         | A "writer" of type `w` that allows the contract to communicate between different contracts. |
| s         | Specifies what endpoints are available in the contract.                                     |
| e         | The type of the error messages that will be used in the contract.                           |
| a         | The result type of the monad, as in all monads.                                             |

## [Writing a Contract](https://youtu.be/g4lvA14I-Jg?t=6812)

A contract can be defined in the following way:

```haskell
{-# OverloadedStrings #-}
{-# TypeApplications #-}

myContract1 :: Contract () Empty Text ()
myContract1 = Contract.logInfo @String "Hello from the contract"
```

Above is an example of one of the simplest contracts we can write.

In this case, we pass a `unit` for `w` to indicate that we do not wish to write any state or log messages. We pass `Empty` for `s` to indicate that this contract has no endpoints, and Text for `e` which is more efficient than String for textual data. Finally, we pass `unit` for `a` to indicate that we are not expecting an interesting result.

All our contract actually does is log a String message using `Contract.logInfo` (unrelated to the `w` parameter).

In order to test this contract, we can create an `EmulatorTrace` and activate the `Contract`:

```haskell
myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (Wallet 1) myContract1

test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1
```

### [Throwing Errors](https://youtu.be/g4lvA14I-Jg?t=7090)

We can throw errors (exceptions) by using `Contract.throwError`:

```haskell
{-# OverloadedStrings #-}
{-# TypeApplications #-}

myContract2 :: Contract () Empty Text ()
myContract2 = do
    void $ Contract.throwError "BOOM!"
    Contract.logInfo @String "Script will never get here"
```
