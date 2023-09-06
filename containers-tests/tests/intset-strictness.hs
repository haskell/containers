{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Prelude hiding (foldl, foldl')

import Test.ChasingBottoms.IsBottom
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary (..))
#if __GLASGOW_HASKELL__ >= 806
import Test.Tasty.QuickCheck (Property)
#endif

import Data.IntSet

#if __GLASGOW_HASKELL__ >= 806
import Utils.NoThunks
#endif


{--------------------------------------------------------------------
  Arbitrary, reasonably balanced trees
--------------------------------------------------------------------}
instance Arbitrary IntSet where
  arbitrary = do{ xs <- arbitrary
                ; return (fromList xs)
                }

------------------------------------------------------------------------
-- * Properties

------------------------------------------------------------------------
-- ** Lazy module

pFoldlAccLazy :: Int -> Bool
pFoldlAccLazy k =
  isn'tBottom $ foldl (\_ x -> x) (bottom :: Int) (singleton k)

#if __GLASGOW_HASKELL__ >= 806
pStrictFoldr' :: IntSet -> Property
pStrictFoldr' m = whnfHasNoThunks (foldr' (:) [] m)
#endif

#if __GLASGOW_HASKELL__ >= 806
pStrictFoldl' :: IntSet -> Property
pStrictFoldl' m = whnfHasNoThunks (foldl' (flip (:)) [] m)
#endif

------------------------------------------------------------------------
-- * Test list

tests :: TestTree
tests =
    -- Basic interface
      testGroup "IntSet"
      [ testProperty "foldl is lazy in accumulator" pFoldlAccLazy
#if __GLASGOW_HASKELL__ >= 806
      , testProperty "strict foldr'" pStrictFoldr'
      , testProperty "strict foldl'" pStrictFoldl'
#endif
      ]

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain tests

------------------------------------------------------------------------
-- * Utilities

isn'tBottom :: a -> Bool
isn'tBottom = not . isBottom
