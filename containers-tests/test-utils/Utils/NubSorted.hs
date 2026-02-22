module Utils.NubSorted
  ( NubSorted(..)
  , NubSortedOnFst(..)
  ) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ord (comparing)
import Test.QuickCheck

newtype NubSorted a = NubSorted { getNubSorted :: [a] }
  deriving Show

instance (Ord a, Arbitrary a) => Arbitrary (NubSorted a) where
  arbitrary = NubSorted . nubSortBy compare <$> arbitrary
  shrink = map (NubSorted . nubSortBy compare) . shrink . getNubSorted

newtype NubSortedOnFst a b = NubSortedOnFst { getNubSortedOnFst :: [(a, b)] }
  deriving Show

instance (Ord a, Arbitrary a, Arbitrary b)
  => Arbitrary (NubSortedOnFst a b) where
  arbitrary = NubSortedOnFst . nubSortBy (comparing fst) <$> arbitrary
  shrink =
    map (NubSortedOnFst . nubSortBy (comparing fst)) .
    shrink .
    getNubSortedOnFst

nubSortBy :: (a -> a -> Ordering) -> [a] -> [a]
nubSortBy cmp =
  map NonEmpty.head .
  NonEmpty.groupBy (\x y -> cmp x y == EQ) .
  List.sortBy cmp
