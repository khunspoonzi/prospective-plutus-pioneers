# [Using the PAB](https://youtu.be/24SHPHEc3zo?t=6430)

The Plutus Application Backend or "PAB" allows us package our contracts into  executable applications.

## Type: [OracleContracts](https://youtu.be/24SHPHEc3zo?t=6506)

The `OracleContracts` type verifies the contract instances we wish to run:

```haskell
data OracleContracts = Init | Oracle CurrencySymbol | Swap Oracle.Oracle
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Pretty OracleContracts where
    pretty = viaShow
```

In this case, `Init` is used to set up an environment that defines initial funds as we would for an `EmulatorTrace`.

The `Oracle` constructor corresponds to the `runOracle` contract that will start the oracle and provide the `update` endpoint, and the `CurrencySymbol` parameter will communicate the currency symbol for the USD token.

The `Swap` constructor parameterized by `Oracle.Oracle` will be used to run the swap contract.

This definition exists in a separate module because it will be used from the PAB as well as the front end.

## [Cabal File Executables](https://youtu.be/24SHPHEc3zo?t=6598)

If we check the Cabal file, we'll find several executables that correspond how to use the PAB.

### Executable: [oracle-pab](https://youtu.be/24SHPHEc3zo?t=6610)

The `oracle-pab` executable will start a simulated wallet, initialize all the contracts, and set up a web server that allows the outside world to interact with these contracts.

```
executable oracle-pab
  main-is: oracle-pab.hs
  hs-source-dirs:      app
  ghc-options:         -Wall -threaded
  build-depends:       aeson
                     , base ^>= 4.14.1.0
                     , data-default
                     , freer-extras
                     , freer-simple
                     , plutus-contract
                     , plutus-ledger
                     , plutus-pab
                     , plutus-pioneer-program-week06
                     , plutus-use-cases
                     , text
```

### Executable: [oracle-client](https://youtu.be/24SHPHEc3zo?t=6626)

The `oracle-client` executable will interact with the `runOracle` contract and fetch exchange rates from the internet and feed them into the system.

```
executable oracle-client
  main-is: oracle-client.hs
  hs-source-dirs:      app
  ghc-options:         -Wall
  build-depends:       base ^>= 4.14.1.0
                     , bytestring
                     , regex-tdfa ^>= 1.3.1.0
                     , req ^>= 3.9.0
                     , text
                     , uuid
```

### Executable: [swap-client](https://youtu.be/24SHPHEc3zo?t=6642)

The `swap-client` executable will be run by the clients that wish to make use of the swap contract.

```
executable swap-client
  main-is: swap-client.hs
  hs-source-dirs:      app
  ghc-options:         -Wall
  build-depends:       aeson
                     , base ^>= 4.14.1.0
                     , plutus-ledger
                     , plutus-pab
                     , plutus-pioneer-program-week06
                     , req ^>= 3.9.0
                     , text
                     , uuid
```

### Continue [watching](https://youtu.be/24SHPHEc3zo?t=6658) for walkthrough...
