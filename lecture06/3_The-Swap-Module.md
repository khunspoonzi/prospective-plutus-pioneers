# [The Swap Module](https://youtu.be/24SHPHEc3zo?t=3672)

In this section, we’ll explore how to define a swap contract that uses an oracle.

## [On-chain Script](https://youtu.be/24SHPHEc3zo?t=3696)

### Function: [price](https://youtu.be/24SHPHEc3zo?t=3696)

The `price` function determines the USD amount to pay for the offered ADA using the oracle exchange rate.

```haskell
{-# INLINABLE price #-}
price :: Integer -> Integer -> Integer
price lovelace exchangeRate = (lovelace * exchangeRate) `divide` 1000000
```

Recall that we have established the exchange rate to be an integer representing the `double` value * 1 million.

### Function: [lovelaces](https://youtu.be/24SHPHEc3zo?t=3758)

The `lovelaces` function extracts the integer value from a Lovelace `Value`.

```haskell
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue
```

### Function: [mkSwapValidator](https://youtu.be/24SHPHEc3zo?t=3786)

The `mkSwapValidator` will check that the necessary conditions are met for the swap.

```haskell
{-# INLINABLE mkSwapValidator #-}

-- Oracle, Oracle Address, Pub Key Hash of Seller, No Redeemer
-- Oracle Address explicitly passed because function to extract it is not compatible with Plutus
mkSwapValidator :: Oracle -> Address -> PubKeyHash -> () -> ScriptContext -> Bool
mkSwapValidator oracle addr pkh () ctx =

    -- Transaction signed by seller if he wants to retrieve his assets and cancel swap, OR
    txSignedBy info pkh ||

    -- There are two script inputs: oracle and swap UTxO itself (others like fees are pub key inputs)
    -- The seller got paid
    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs &&
     traceIfFalse "price not paid"                     sellerPaid)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Find oracle input
    oracleInput :: TxOut
    oracleInput =
      let
        -- Get output for each input if the output address is the oracle address
        ins = [ o
              | i <- txInfoInputs info
              , let o = txInInfoResolved i
              , txOutAddress o == addr
              ]
      in
        case ins of

            -- Case of exactly one oracle input
            [o] -> o

            -- All other cases
            _   -> traceError "expected exactly one oracle input"

    -- Check oracle exchange rate using helper defined previously
    oracleValue' = case oracleValue oracleInput (`findDatum` info) of
        Nothing -> traceError "oracle value not found"
        Just x  -> x

    hasTwoScriptInputs :: Bool
    hasTwoScriptInputs =
      let
        -- Filter inputs --> output --> output address --> validator hash if script output --> is just
        xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
      in
        -- Ensure that script input length is 2
        length xs == 2

    minPrice :: Integer
    minPrice =
      let
        -- Get input currently being validated (swap) and get lovelace value
        lovelaceIn = case findOwnInput ctx of
            Nothing -> traceError "own input not found"
            Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
      in
        price lovelaceIn oracleValue'

    sellerPaid :: Bool
    sellerPaid =
      let
        -- Check how much he did pay
        -- Adds up all values of all the pubkey outputs that goes to this address
        -- Total value sent by this address to the seller
        -- The USD tokens sent to the seller by this transaction
        -- Allows overpaying
        pricePaid :: Integer
        pricePaid =  assetClassValueOf (valuePaidTo info pkh) (oAsset oracle)
      in
        pricePaid >= minPrice
```

We do not have to check that the NFT is present because this happens in the core module.

### More Notes Soon...
