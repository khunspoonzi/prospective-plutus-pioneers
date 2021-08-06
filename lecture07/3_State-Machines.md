# [State Machines](https://youtu.be/uwZ903Zd0DU?t=2520)

A state machine is a system that begins with an initial state and allows for one or more transition states before arriving at a final state. In other words, a state machine is like a directed graph that ends at a final state.

Within the context of the blockchain, it is possible to think of our [Plutus implementation](./2_Implementation-Without-State-Machines.md) of [Alice and Bob's game](./1_Commit-Scemes.md) example as a kind of state machine:

- **Initial State:** Alice opens the game by committing her hash and 1 ADA
  - **Transition State:** Bob doesn't answer and Alice claims back her 1 ADA
    - **Final State:** The game ends and Alice retains her 1 ADA
  - **Transition State:**  Bob accepts the game by commiting Alice's hash, his choice, and 1 ADA
    - **Transition State:** Alice realizes that she has won and reveals her choice
        - **Final State:** The game ends and Alice ends up with 2 ADA
    - **Transition State:** Alice realizes that she has lost, simply waits for a deadline to pass, and Bob claims his winnings
        - **Final State:** The game ends and Bob ends up with 2 ADA

In this case, the state machine itself is represented by the UTxO sitting at the game UTxO's script address, while the state is represented by that UTxO's datum.

A transition is represented by a transaction that consumes the game UTxO, and is characterized by the redeemer it uses. Each transaction produces an updated datum to reflect the new state of the game.

In this section, we'll explore further how [`Plutus.Contract.StateMachine`](https://alpha.marlowe.iohkdev.io/doc/haddock/plutus-contract/html/Plutus-Contract-StateMachine.html) from `plutus-contract` analogizes this.

## Type: [StateMachine](https://youtu.be/uwZ903Zd0DU?t=2684)

The `StateMachine` type is a record type with four fields and accepts two type parameters `s` and `i` that stand for state and input, and correspond to datum and redeemer, respectively:

```haskell
-- | Specification of a state machine, consisting of a transition function that determines the
-- next state from the current state and an input, and a checking function that checks the validity
-- of the transition in the context of the current transaction.
data StateMachine s i = StateMachine {
      -- | The transition function of the state machine. 'Nothing' indicates an invalid transition from the current state.
      smTransition  :: State s -> i -> Maybe (TxConstraints Void Void, State s),

      -- | Check whether a state is the final state
      smFinal       :: s -> Bool,

      -- | The condition checking function. Can be used to perform
      --   checks on the pending transaction that aren't covered by the
      --   constraints. 'smCheck' is always run in addition to checking the
      --   constraints, so the default implementation always returns true.
      smCheck       :: s -> i -> ScriptContext -> Bool,

      -- | The 'AssetClass' of the thread token that identifies the contract
      --   instance.
      smThreadToken :: Maybe AssetClass
    }
```

### Function: [smTransition](https://youtu.be/uwZ903Zd0DU?t=2710)

The `smTransition` function defines from which states and using which transitions it is possible to change to another state:

```haskell
-- Field from StateMachine
smTransition :: State s -> i -> Maybe (TxConstraints Void Void, State s)

data State s = State { stateData :: s, stateValue :: Value }  -- The datum and value
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
```

Give the State (present datum and value of the UTxO) along with a transaction that tries to consume the UTxO using redeemer `i`, we can return `Nothing` if the transaction is not allowed, or a tuple containing a new `State` (datum and value) and additional constraints that the transaction must satisfy if it is allowed.

### Function: [smFinal](https://youtu.be/uwZ903Zd0DU?t=2800)

The `smFinal` function tells us whether we are dealing with a final state:

```haskell
-- Field from StateMachine
smFinal :: s -> Bool
```

In the case of a final state, we ensure that a value is not attached and a new UTxO is not produced.

### Function: [smCheck](https://youtu.be/uwZ903Zd0DU?t=2842)

The `smCheck` function provides an additional check on the datum, redeemer, and context not included by the additional `TxConstraints`:

```haskell
-- Field from StateMachine
smCheck :: s -> i -> ScriptContext -> Bool
```

### Function: [smThreadToken](https://youtu.be/uwZ903Zd0DU?t=2842)

The `smThreadToken` field is optional but helps to identify the correct contract instance:

```haskell
-- Field from StateMachine
smThreadToken :: Maybe AssetClass
```

Using this field will automatically take care of minting the NFT and threading it through the state transitions
