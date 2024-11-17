{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Test.ChasingBottoms.IsBottom
import Test.Tasty (TestTree, TestName, defaultMain, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, Arbitrary(arbitrary), Fun)
#if __GLASGOW_HASKELL__ >= 806
import Test.Tasty.QuickCheck (Property)
#endif
import Test.QuickCheck.Function (apply)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import qualified Data.IntMap as L
import Data.Containers.ListUtils

import Utils.IsUnit
#if __GLASGOW_HASKELL__ >= 806
import Utils.NoThunks
#endif

instance Arbitrary v => Arbitrary (IntMap v) where
    arbitrary = M.fromList `fmap` arbitrary

apply2 :: Fun (a, b) c -> a -> b -> c
apply2 f a b = apply f (a, b)

apply3 :: Fun (a, b, c) d -> a -> b -> c -> d
apply3 f a b c = apply f (a, b, c)

------------------------------------------------------------------------
-- * Properties

------------------------------------------------------------------------
-- ** Strict module

pSingletonKeyStrict :: Int -> Bool
pSingletonKeyStrict v = isBottom $ M.singleton (bottom :: Int) v

pSingletonValueStrict :: Int -> Bool
pSingletonValueStrict k = isBottom $ (M.singleton k (bottom :: Int))

pFindWithDefaultKeyStrict :: Int -> IntMap Int -> Bool
pFindWithDefaultKeyStrict def m = isBottom $ M.findWithDefault def bottom m

pFindWithDefaultValueStrict :: Int -> IntMap Int -> Bool
pFindWithDefaultValueStrict k m =
    M.member k m || (isBottom $ M.findWithDefault bottom k m)

pAdjustKeyStrict :: Fun Int Int -> IntMap Int -> Bool
pAdjustKeyStrict f m = isBottom $ M.adjust (apply f) bottom m

pAdjustValueStrict :: Int -> IntMap Int -> Bool
pAdjustValueStrict k m
    | k `M.member` m = isBottom $ M.adjust (const bottom) k m
    | otherwise       = case M.keys m of
        []     -> True
        (k':_) -> isBottom $ M.adjust (const bottom) k' m

pInsertKeyStrict :: Int -> IntMap Int -> Bool
pInsertKeyStrict v m = isBottom $ M.insert bottom v m

pInsertValueStrict :: Int -> IntMap Int -> Bool
pInsertValueStrict k m = isBottom $ M.insert k bottom m

pInsertWithKeyStrict :: Fun (Int, Int) Int -> Int -> IntMap Int -> Bool
pInsertWithKeyStrict f v m = isBottom $ M.insertWith (apply2 f) bottom v m

pInsertWithValueStrict :: Fun (Int, Int) Int -> Int -> Int -> IntMap Int
                       -> Bool
pInsertWithValueStrict f k v m
    | M.member k m = (isBottom $ M.insertWith (const2 bottom) k v m) &&
                     not (isBottom $ M.insertWith (const2 1) k bottom m)
    | otherwise    = isBottom $ M.insertWith (apply2 f) k bottom m

pInsertLookupWithKeyKeyStrict :: Fun (Int, Int, Int) Int -> Int -> IntMap Int
                              -> Bool
pInsertLookupWithKeyKeyStrict f v m = isBottom $ M.insertLookupWithKey (apply3 f) bottom v m

pInsertLookupWithKeyValueStrict :: Fun (Int, Int, Int) Int -> Int -> Int
                                -> IntMap Int -> Bool
pInsertLookupWithKeyValueStrict f k v m
    | M.member k m = (isBottom $ M.insertLookupWithKey (const3 bottom) k v m) &&
                     not (isBottom $ M.insertLookupWithKey (const3 1) k bottom m)
    | otherwise    = isBottom $ M.insertLookupWithKey (apply3 f) k bottom m

------------------------------------------------------------------------
-- test a corner case of fromAscList
--
-- If the list contains duplicate keys, then (only) the first of the
-- given values is not evaluated. This may change in the future, see
-- also https://github.com/haskell/containers/issues/473

pFromAscListLazy :: [Int] -> Bool
pFromAscListLazy ks = not . isBottom $ L.fromAscList elems
  where
    elems = [(k, v) | k <- nubInt ks, v <- [undefined, ()]]

pFromAscListStrict :: [Int] -> Bool
pFromAscListStrict ks
    | null ks   = not . isBottom $ M.fromAscList elems
    | otherwise = isBottom $ M.fromAscList elems
  where
    elems = [(k, v) | k <- nubInt ks, v <- [undefined, undefined, ()]]

#if __GLASGOW_HASKELL__ >= 806
pStrictFoldr' :: IntMap Int -> Property
pStrictFoldr' m = whnfHasNoThunks (M.foldr' (:) [] m)
#endif

#if __GLASGOW_HASKELL__ >= 806
pStrictFoldl' :: IntMap Int -> Property
pStrictFoldl' m = whnfHasNoThunks (M.foldl' (flip (:)) [] m)
#endif

------------------------------------------------------------------------
-- check for extra thunks
--
-- These tests distinguish between `()`, a fully evaluated value, and
-- things like `id ()` which are extra thunks that should be avoided
-- in most cases. An exception is `L.fromListWith const`, which cannot
-- evaluate the `const` calls.

tExtraThunksM :: TestTree
tExtraThunksM = testGroup "IntMap.Strict - extra thunks" $
    if not isUnitSupported then [] else
    -- for strict maps, all the values should be evaluated to ()
    [ check "singleton"           $ m0
    , check "insert"              $ M.insert 42 () m0
    , check "insertWith"          $ M.insertWith const 42 () m0
    , check "fromList"            $ M.fromList [(42,()),(42,())]
    , check "fromListWith"        $ M.fromListWith const [(42,()),(42,())]
    , check "fromAscList"         $ M.fromAscList [(42,()),(42,())]
    , check "fromAscListWith"     $ M.fromAscListWith const [(42,()),(42,())]
    , check "fromDistinctAscList" $ M.fromAscList [(42,())]
    ]
  where
    m0 = M.singleton 42 ()
    check :: TestName -> IntMap () -> TestTree
    check n m = testCase n $ case M.lookup 42 m of
        Just v -> assertBool msg (isUnit v)
        _      -> assertBool "key not found" False
      where
        msg = "too lazy -- expected fully evaluated ()"

tExtraThunksL :: TestTree
tExtraThunksL = testGroup "IntMap.Lazy - extra thunks" $
    if not isUnitSupported then [] else
    -- for lazy maps, the *With functions should leave `const () ()` thunks,
    -- but the other functions should produce fully evaluated ().
    [ check "singleton"       True  $ m0
    , check "insert"          True  $ L.insert 42 () m0
    , check "insertWith"      False $ L.insertWith const 42 () m0
    , check "fromList"        True  $ L.fromList [(42,()),(42,())]
    , check "fromListWith"    False $ L.fromListWith const [(42,()),(42,())]
    , check "fromAscList"     True  $ L.fromAscList [(42,()),(42,())]
    , check "fromAscListWith" False $ L.fromAscListWith const [(42,()),(42,())]
    , check "fromDistinctAscList" True $ L.fromAscList [(42,())]
    ]
  where
    m0 = L.singleton 42 ()
    check :: TestName -> Bool -> L.IntMap () -> TestTree
    check n e m = testCase n $ case L.lookup 42 m of
        Just v -> assertBool msg (e == isUnit v)
        _      -> assertBool "key not found" False
      where
        msg | e         = "too lazy -- expected fully evaluated ()"
            | otherwise = "too strict -- expected a thunk"

------------------------------------------------------------------------
-- * Test list

tests :: [TestTree]
tests =
    [
    -- Basic interface
      testGroup "IntMap.Strict"
      [ testProperty "singleton is key-strict" pSingletonKeyStrict
      , testProperty "singleton is value-strict" pSingletonValueStrict
      , testProperty "member is key-strict" $ keyStrict M.member
      , testProperty "lookup is key-strict" $ keyStrict M.lookup
      , testProperty "findWithDefault is key-strict" pFindWithDefaultKeyStrict
      , testProperty "findWithDefault is value-strict" pFindWithDefaultValueStrict
      , testProperty "! is key-strict" $ keyStrict (flip (M.!))
      , testProperty "!? is key-strict" $ keyStrict (flip (M.!?))
      , testProperty "delete is key-strict" $ keyStrict M.delete
      , testProperty "adjust is key-strict" pAdjustKeyStrict
      , testProperty "adjust is value-strict" pAdjustValueStrict
      , testProperty "insert is key-strict" pInsertKeyStrict
      , testProperty "insert is value-strict" pInsertValueStrict
      , testProperty "insertWith is key-strict" pInsertWithKeyStrict
      , testProperty "insertWith is value-strict" pInsertWithValueStrict
      , testProperty "insertLookupWithKey is key-strict"
        pInsertLookupWithKeyKeyStrict
      , testProperty "insertLookupWithKey is value-strict"
        pInsertLookupWithKeyValueStrict
      , testProperty "fromAscList is somewhat value-lazy" pFromAscListLazy
      , testProperty "fromAscList is somewhat value-strict" pFromAscListStrict
#if __GLASGOW_HASKELL__ >= 806
      , testProperty "strict foldr'" pStrictFoldr'
      , testProperty "strict foldl'" pStrictFoldl'
#endif
      ]
      , tExtraThunksM
      , tExtraThunksL
    ]

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain $ testGroup "intmap-strictness" tests

------------------------------------------------------------------------
-- * Utilities

keyStrict :: (Int -> IntMap Int -> a) -> IntMap Int -> Bool
keyStrict f m = isBottom $ f bottom m

const2 :: a -> b -> c -> a
const2 x _ _ = x

const3 :: a -> b -> c -> d -> a
const3 x _ _ _ = x
