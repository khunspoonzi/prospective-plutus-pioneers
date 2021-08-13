# [Interlude: Optics](https://youtu.be/zW3D2iM5uVg?t=2482)

Optics provide a means of "zooming" into hierarchical data types to inspect and manipulate their hidden parts.

The optics library that Plutus uses is called [lens](https://hackage.haskell.org/package/lens).

## [A Simple Example](https://youtu.be/zW3D2iM5uVg?t=2554)

```haskell
{-# LANGUAGE TemplateHaskell #-}

module Week08.Lens where

import Control.Lens

newtype Company = Company {_staff :: [Person]} deriving Show

data Person  = Person
    { _name    :: String
    , _address :: Address
    } deriving Show

newtype Address = Address {_city :: String} deriving Show

alejandro, lars :: Person
alejandro = Person
  {  _name    = "Alejandro"
  ,  _address = Address {_city = "Zacateca"}
  }
lars = Person
  {  _name    = "Lars"
  ,  _address = Address {_city = "Regensburg"}
  }

iohk :: Company
iohk = Company { _staff = [alejandro, lars] }

goTo :: String -> Company -> Company
goTo there c = c {_staff = map movePerson (_staff c)}
  where
    movePerson p = p {_address = (_address p) {_city = there}}

makeLenses ''Company
makeLenses ''Person
makeLenses ''Address

goTo' :: String -> Company -> Company
goTo' there c = c & staff . each . address . city .~ there
```

In the example above, we define a `Company` record type that whose `_staff` is a `Person` list.

We define `Person` as a record type with a `_name` of type `String` and an `_address` of type `Address`, which in turn is defined as a record type with a `_city` of type `String`.

Our `goTo` function accepts a `Company` and returns the same `Company` where the `Address` `_city` of each `_staff` `Person` has been changed to a new city name.

While this is straightforward enough to implement in pure Haskell, our `goTo'` function demonstrates how the use of lenses can make for a cleaner implementation when dealing with a hierarchical data structure.

In essence, a lenses enable implementations that are broadly comparable to dot accessors in imperative languages.

Note that the leading underscore pattern is a convention for fields when dealing with lenses.

## [Basic Usage](https://youtu.be/zW3D2iM5uVg?t=2998)

To access the `_name` and `_address` lenses of a `Person`, the following operator can be used:

```haskell
import Control.Lens

lars ^. name
lars ^. address
```

Composability is one of the most powerful features of lenses, i.e. lenses can be chained together:

```haskell
lars ^. address . city
```

Lenses can also be set in the following way:

```haskell
lars & name .~ "LARS"
```

In this case, the `&` signifies function application in reverse order, i.e. the argument comes before the function.

Once again, this operation is composable:

```haskell
lars & address . city .~ "Munich"
```

Traversal of multiple elements is also possible:

```haskell
[1 :: Int, 3, 4] & each .~ 42

iohk & staff . each . address . city .~ "Athens"
```
