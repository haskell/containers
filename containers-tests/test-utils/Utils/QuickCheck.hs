{-# LANGUAGE CPP #-}

-- | Useful newtypes with Arbitrary instances.
module Utils.QuickCheck
  ( NubSorted(..)
  , NubSortedOnFst(..)
  , SortedOnFst(..)
  ) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ord (comparing)
import Test.QuickCheck
#if !MIN_VERSION_QuickCheck(2,17,0)
import Data.Coerce (coerce)
import Data.Ord (Down(..))
#endif

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

newtype SortedOnFst a b = SortedOnFst { getSortedOnFst :: [(a, b)] }
  deriving Show

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (SortedOnFst a b) where
  arbitrary = SortedOnFst . List.sortBy (comparing fst) <$> arbitrary
  shrink =
    map (SortedOnFst . List.sortBy (comparing fst)) . shrink . getSortedOnFst

#if !MIN_VERSION_QuickCheck(2,17,0)
instance Arbitrary a => Arbitrary (Down a) where
  arbitrary = Down <$> arbitrary
  shrink (Down x) = coerce (shrink x)
#endif
