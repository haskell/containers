{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Coerce (coerce)
import qualified Data.Foldable as F

import Test.ChasingBottoms.IsBottom (isBottom)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Poly (OrdA, B)

import Data.Set (Set)
import qualified Data.Set as S

import Utils.ArbitrarySetMap (setFromList)
import Utils.NubSorted (NubSorted(..))
import Utils.Strictness (Bot(..), Func2, applyFunc2)

------------------------------------------------------------------------
-- * Arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = do
    NubSorted xs <- arbitrary
    setFromList xs

------------------------------------------------------------------------
-- * Properties

{--------------------------------------------------------------------
  Folds
--------------------------------------------------------------------}

-- See Note [Testing strictness of folds] in map-strictness.hs

prop_foldr :: NubSorted OrdA -> Func2 OrdA B (Bot B) -> Bot B -> Property
prop_foldr (NubSorted xs) fun (Bot z) =
  isBottom (S.foldr f z s) ===
  isBottom (F.foldr f z xs)
  where
    s = S.fromList xs
    f = coerce (applyFunc2 fun) :: OrdA -> B -> B

prop_foldl :: NubSorted OrdA -> Func2 B OrdA (Bot B) -> Bot B -> Property
prop_foldl (NubSorted xs) fun (Bot z) =
  isBottom (S.foldl f z s) ===
  isBottom (F.foldl f z xs)
  where
    s = S.fromList xs
    f = coerce (applyFunc2 fun) :: B -> OrdA -> B

prop_foldr' :: NubSorted OrdA -> Func2 OrdA B (Bot B) -> Bot B -> Property
prop_foldr' (NubSorted xs) fun (Bot z) =
  isBottom (S.foldr' f z s) ===
  isBottom (z `seq` F.foldr' f z xs)
  where
    s = S.fromList xs
    f = coerce (applyFunc2 fun) :: OrdA -> B -> B

prop_foldl' :: NubSorted OrdA -> Func2 B OrdA (Bot B) -> Bot B -> Property
prop_foldl' (NubSorted xs) fun (Bot z) =
  isBottom (S.foldl' f z s) ===
  isBottom (F.foldl' f z xs)
  where
    s = S.fromList xs
    f = coerce (applyFunc2 fun) :: B -> OrdA -> B

------------------------------------------------------------------------
-- * Test list

tests :: TestTree
tests = testGroup "Set"
  [ testProperty "prop_foldr" prop_foldr
  , testProperty "prop_foldl" prop_foldl
  , testProperty "prop_foldr'" prop_foldr'
  , testProperty "prop_foldl'" prop_foldl'
  ]

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain tests
