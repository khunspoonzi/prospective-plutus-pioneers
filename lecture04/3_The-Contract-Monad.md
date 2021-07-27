# [The Contract Monad](https://youtu.be/g4lvA14I-Jg?t=6704)

Finally, we arrive at the Contract monad, which will be an essential component of our off-chain validation scripts.

Notably, the Contract monad expects the following four parameters:

| Parameter | Description                                                                                 |
| --------- | ------------------------------------------------------------------------------------------- |
| w         | A "writer" of type `w` that allows the contract to communicate between different contracts. |
| s         | A "schema" that specified what endpoints are available in the contract.                     |
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

### [Handling Errors](https://youtu.be/g4lvA14I-Jg?t=7208)

We can also catch errors by using `Contract.handleError` defined as follows:

```haskell
Contract.handleError :: (e -> Contract w s e' a) -> Contract w s e a -> Contract w s e' a
```

In the definition above, we see that `Contract.handleError` accepts a contract with an error type of `e`, along with a handle that accepts `e` and produces a contract with an error type of `e'`.

Specifically, `Contract.handleError` will run the inner contract and pass in its `e` to the handle if an exception was raised, at which point it will run the handle's contract.

```haskell
{-# OverloadedStrings #-}
{-# TypeApplications #-}

myContract3 :: Contract () Empty Void ()
myContract3 = Contract.handleError
    (\err -> Contract.logError $ "Caught: " ++ unpack err)
    myContract2
```

In this case, we have a contract with an error type of `Void` meaning that it cannot produce any errors. For this example, we indicate that we wish to handle errors raised from `Contract2` which has an error type of `Text`.

In other words, `e` in this case is `Text` and `e'` is `Void`. Note that `Void` is a Haskell data type defined in `Data.Void` that has no value, unlike `unit` which has exactly one value: `unit`.

The `unpack` function from `Data.Text` is simply used to convert that `Text` error into a `String` for the `Contract.logError` output.

### [Defining a Schema](https://youtu.be/g4lvA14I-Jg?t=7608)

A schema is defined using a type synonym as follows:

```haskell
{-# DataKinds #-}

MySchema = Endpoint "foo" Int
```

The above schema indicates we expect one endpoint that takes an `Int` and is called `"foo"`, where `"foo"` is a type-level string enabled by `{-# DataKinds #-}` and `Int` is a type parameter.

As such, `MySchema` can be used as follows:

```haskell
{-# OverloadedStrings #-}
{-# TypeApplications #-}

myContract4 :: Contract () MySchema Text ()
myContract4 = do
    n <- endpoint @"foo"
    Contract.logInfo n
```

The above contract calls `endpoint`, which in turn invokes our `foo` endpoint by label.

Doing so executes a monadic computation that will block contract execution and wait for `foo`'s expected value of type `Int` to be provided, which is then bound to `n` and logged.

We can test the contract like so:

```haskell
myTrace2 :: EmulatorTrace ()
myTrace2 = do
    h <- activateContractWallet (Wallet 1) myContract4
    callEndpoint @"foo" h 42
```

In this case, it is no longer sufficient to simply start the contract since the `foo` endpoint will block until it is called.

To do so, we pass the handle returned by activating the contract in Wallet 1 into `callEndpoint @"foo"` along with an expected `Int` value.

If we wish to include more than one endpoint in our schema, we can chain them together using a type operator, which takes one or more types and combines them into a new type:

```haskell
{-# DataKinds #-}
{-# TypeOperators #-}

MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String
```

Our new contract might look as follows:

```haskell
{-# OverloadedStrings #-}
{-# TypeApplications #-}

myContract5 :: Contract () MySchema Text ()
myContract5 = do

    n <- endpoint @"foo"
    Contract.logInfo n

    s <- endpoint @"bar"
    Contract.logInfo s

myTrace3 :: EmulatorTrace ()
myTrace3 = do

    h <- activateContractWallet (Wallet 1) myContract5

    callEndpoint @"foo" h 42
    callEndpoint @"bar" h "Haskell"
```

### [Defining a Writer](https://youtu.be/g4lvA14I-Jg?t=8012)

Our writer of type `w` must be an instance of the type class `Monoid`, which is partially defined as follows:

```haskell
class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
```

A prime example of a `Monoid` is `List`, where `mempty` is an empty list of type `a`, and `mappend` is concatenation.

```haskell
myContract6 :: Contract [Int] Empty Text ()
myContract6 = do

    void $ Contract.waitNSlots 10
    tell [1]

    void $ Contract.waitNSlots 10
    tell [2]

    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do

    h <- activateContractWallet (Wallet 1) myContract6

    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs
```

In the example above, `observableState` allows us to read the current state of a running contract.

Note that `tell` comes from the `MonadWriter` class of which Contract is an instance.
