# [Property-based Testing with QuickCheck](https://youtu.be/zW3D2iM5uVg?t=3354)

Property-based testing can be considered a more powerful alternative to unit testing, and is well-suited to the purity of Haskell and its immutable data structures.

One of the most prominent libraries for implementing property-based testing in Haskell is [QuickCheck](https://hackage.haskell.org/package/QuickCheck).

## Example: [QuickChecking a Sort Function](https://youtu.be/zW3D2iM5uVg?t=3498)

```haskell
module Week08.QuickCheck where

prop_simple :: Bool
prop_simple = 2 + 2 == (4 :: Int)

-- Insertion sort code:

-- | Sort a list of integers in ascending order.
--
-- >>> sort [5,1,9]
-- [1,5,9]
--
sort :: [Int] -> [Int] -- not correct
sort []     =  []
sort (x:xs) =  insert x $ sort xs

-- | Insert an integer at the right position into an /ascendingly sorted/
-- list of integers.
--
-- >>> insert 5 [1,9]
-- [1,5,9]
--
insert :: Int -> [Int] -> [Int] -- not correct
insert x []                     =  [x]
insert x (y:ys)  | x <= y       =  x : y : ys
                 | otherwise    =  y : insert x ys

isSorted :: [Int] -> Bool
isSorted []           = True
isSorted [_]          = True
isSorted (x : y : ys) = x <= y && isSorted (y : ys)

prop_sort_sorts :: [Int] -> Bool
prop_sort_sorts xs = isSorted $ sort xs

prop_sort_preserves_length :: [Int] -> Bool
prop_sort_preserves_length xs = length (sort xs) == length xs
```

In the example above, the `isSorted` function checks whether an `Int` list is sorted.

The `prop_sort_sorts` function accepts an `Int` list, applies `sort`, and checks that it is sorted with `isSorted`.

We may also want use `prop_sort_preserves_length` to check that no elements are lost when we call `sort`.

We can then run `quickCheck` on these functions to test that they work under various conditions:

```haskell
import Test.QuickCheck

quickCheck prop_sort_sorts
quickCheck prop_sort_preserves_length
```

In this case, QuickCheck is powerful because it will run a series of tests on this function and then return an example of a failing edge-case if it finds one.

This approach can be contrasted with traditional unit tests that expect you to account for edge-cases yourself.
