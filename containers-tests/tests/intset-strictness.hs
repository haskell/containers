{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Data.Coerce (coerce)
import qualified Data.Foldable as F

import Test.ChasingBottoms.IsBottom (isBottom)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Poly (B)

import Data.IntSet (IntSet)
import qualified Data.IntSet as S

import Utils.QuickCheck (NubSorted(..))
import Utils.Strictness (Bot(..), Func2, applyFunc2)

------------------------------------------------------------------------
-- * Arbitrary

instance Arbitrary IntSet where
  arbitrary = S.fromList <$> arbitrary
  shrink = map S.fromList . shrink . S.toList

------------------------------------------------------------------------
-- * Properties

{--------------------------------------------------------------------
  Folds
--------------------------------------------------------------------}

-- See Note [Testing strictness of folds] in map-strictness.hs

prop_foldr :: NubSorted Int -> Func2 Int B (Bot B) -> Bot B -> Property
prop_foldr (NubSorted xs) fun (Bot z) =
  isBottom (S.foldr f z s) ===
  isBottom (F.foldr f z xs)
  where
    s = S.fromList xs
    f = coerce (applyFunc2 fun) :: Int -> B -> B

prop_foldl :: NubSorted Int -> Func2 B Int (Bot B) -> Bot B -> Property
prop_foldl (NubSorted xs) fun (Bot z) =
  isBottom (S.foldl f z s) ===
  isBottom (F.foldl f z xs)
  where
    s = S.fromList xs
    f = coerce (applyFunc2 fun) :: B -> Int -> B

prop_foldr' :: NubSorted Int -> Func2 Int B (Bot B) -> Bot B -> Property
prop_foldr' (NubSorted xs) fun (Bot z) =
  isBottom (S.foldr' f z s) ===
  isBottom (z `seq` F.foldr' f z xs)
  where
    s = S.fromList xs
    f = coerce (applyFunc2 fun) :: Int -> B -> B

prop_foldl' :: NubSorted Int -> Func2 B Int (Bot B) -> Bot B -> Property
prop_foldl' (NubSorted xs) fun (Bot z) =
  isBottom (S.foldl' f z s) ===
  isBottom (F.foldl' f z xs)
  where
    s = S.fromList xs
    f = coerce (applyFunc2 fun) :: B -> Int -> B

------------------------------------------------------------------------
-- * Test list

tests :: TestTree
tests = testGroup "IntSet"
  [ testProperty "prop_foldr" prop_foldr
  , testProperty "prop_foldl" prop_foldl
  , testProperty "prop_foldr'" prop_foldr'
  , testProperty "prop_foldl'" prop_foldl'
  ]

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain tests
