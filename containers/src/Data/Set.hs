{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE Safe #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Set
-- Copyright   :  (c) Daan Leijen 2002
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
--
-- = Finite Sets
--
-- The @'Set' e@ type represents a set of elements of type @e@. Most operations
-- require that @e@ have an instance of the 'Ord' class. A 'Set' is strict in
-- its elements.
--
-- For a walkthrough of the most commonly used functions see the
-- <https://haskell-containers.readthedocs.io/en/latest/set.html sets introduction>.
--
-- This module is intended to be imported qualified, to avoid name clashes with
-- Prelude functions, e.g.
--
-- >  import Data.Set (Set)
-- >  import qualified Data.Set as Set
--
-- The @'Ord' e@ instance is expected to be lawful and define a total order.
-- Unless otherwise specified, operations expect equality to be extensional: if
-- elements @x1@ and @x2@ satisfy @x1 == x2@, they are considered identical. For
-- instance, if only one element must be retained by an operation, it is free to
-- select either.
--
--
-- == Warning
--
-- The size of the set must not exceed @maxBound::Int@. Violation of
-- this condition is not detected and if the size limit is exceeded, its
-- behaviour is undefined.
--
--
-- == Implementation
--
-- The implementation of 'Set' is based on /size balanced/ binary trees (or
-- trees of /bounded balance/) as described by:
--
--    * Stephen Adams, \"/Efficient sets—a balancing act/\",
--      Journal of Functional Programming 3(4):553-562, October 1993,
--      <https://doi.org/10.1017/S0956796800000885>,
--      <https://groups.csail.mit.edu/mac/users/adams/BB/index.html>.
--    * J. Nievergelt and E.M. Reingold,
--      \"/Binary search trees of bounded balance/\",
--      SIAM journal of computing 2(1), March 1973.
--      <https://doi.org/10.1137/0202005>.
--    * Yoichi Hirai and Kazuhiko Yamamoto,
--      \"/Balancing weight-balanced trees/\",
--      Journal of Functional Programming 21(3):287-307, 2011,
--      <https://doi.org/10.1017/S0956796811000104>
--
--  Bounds for 'union', 'intersection', and 'difference' are as given
--  by
--
--    * Guy Blelloch, Daniel Ferizovic, and Yihan Sun,
--      \"/Parallel Ordered Sets Using Join/\",
--      <https://arxiv.org/abs/1602.02120v4>.
--
--
-- == Performance information
--
-- The time complexity is given for each operation in
-- [big-O notation](http://en.wikipedia.org/wiki/Big_O_notation), with \(n\)
-- referring to the number of entries in the set.
--
-- Operations like 'member', 'insert', and 'delete' take \(O(\log n)\) time.
--
-- Binary set operations like 'union' and 'intersection' take
-- \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr)\) time, where \(m\) and \(n\)
-- are the sizes of the smaller and larger input sets respectively.
--
-----------------------------------------------------------------------------

module Data.Set (
            -- * Set type
              Set          -- instance Eq,Ord,Show,Read,Data

            -- * Construction
            , empty
            , singleton
            , fromList
            , fromAscList
            , fromDescList
            , fromDistinctAscList
            , fromDistinctDescList
            , powerSet

            -- * Insertion
            , insert

            -- * Deletion
            , delete
            , pop

            -- * Generalized insertion/deletion

            , alterF

            -- * Query
            , member
            , notMember
            , lookupLT
            , lookupGT
            , lookupLE
            , lookupGE
            , S.null
            , size
            , isSubsetOf
            , isProperSubsetOf
            , disjoint

            -- * Combine
            , union
            , unions
            , difference
            , (\\)
            , intersection
            , intersections
            , symmetricDifference
            , cartesianProduct
            , disjointUnion
            , Intersection(..)

            -- * Filter
            , S.filter
            , takeWhileAntitone
            , dropWhileAntitone
            , spanAntitone
            , mapMaybe
            , partition
            , split
            , splitMember
            , splitRoot

            -- * Indexed
            , lookupIndex
            , findIndex
            , elemAt
            , deleteAt
            , S.take
            , S.drop
            , S.splitAt

            -- * Map
            , S.map
            , mapMonotonic

            -- * Folds
            , S.foldr
            , S.foldl
            -- ** Strict folds
            , S.foldr'
            , S.foldl'
            -- ** Legacy folds
            , fold

            -- * Min\/Max
            , lookupMin
            , lookupMax
            , deleteMin
            , deleteMax
            , maxView
            , minView
            , findMin
            , findMax
            , deleteFindMin
            , deleteFindMax

            -- * Conversion

            -- ** List
            , elems
            , toList
            , toAscList
            , toDescList

            -- * Debugging
            , showTree
            , showTreeWith
            , valid
            ) where

import Data.Set.Internal as S
