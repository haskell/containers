{-# LANGUAGE CPP #-}
#if !defined(TESTING) && __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Containers.ListUtils
-- Copyright   :  (c) Gershom Bazerman 2018
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- This module provides efficient containers-based functions on the list type.
-----------------------------------------------------------------------------

module Data.Containers.ListUtils (
       nubOrd,
       nubOrdOn,
       nubInt,
       nubIntOn
       ) where

import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

-- | /O(n log n)/. The 'nubOrd' function removes duplicate elements from a list.
-- In particular, it keeps only the first occurrence of each element. By using a 'Set' internally
-- it has better asymptotics than the standard 'nub' function.
nubOrd :: (Ord a) => [a] -> [a]
nubOrd l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

-- | The `nubOrdOn` function behaves just like `nubOrd` except it preforms comparisons not on the
-- original datatype, but a user-specified projection from that datatype.
nubOrdOn :: (Ord b) => (a -> b) -> [a] -> [a]
nubOrdOn f l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = let fx = f x
                  in if fx `Set.member` s then go s xs
                                          else x : go (Set.insert fx s) xs

-- | /O(n min(n,W))/. The 'nubInt' function removes duplicate elements from a list.
-- In particular, it keeps only the first occurrence of each element. By using an 'IntSet' internally
-- it has better asymptotics than the standard 'nub' function.
nubInt :: [Int] -> [Int]
nubInt l = go IntSet.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `IntSet.member` s then go s xs
                                         else x : go (IntSet.insert x s) xs

-- | The `nubIntOn` function behaves just like 'nubInt' except it preforms comparisons not on the
-- original datatype, but a user-specified projection from that datatype to 'Int'.
nubIntOn :: (a -> Int) -> [a] -> [a]
nubIntOn f l = go IntSet.empty l
  where
    go _ [] = []
    go s (x:xs) = let fx = f x
                  in if fx `IntSet.member` s then go s xs
                                             else x : go (IntSet.insert fx s) xs
