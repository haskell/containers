{-# LANGUAGE CPP #-}
#if !defined(TESTING) && __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map.Lazy
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of ordered maps from keys to values
-- (dictionaries).
--
-- The 'Map' type is shared between the lazy and strict modules,
-- meaning that the same 'Map' value can be passed to functions in
-- both modules (although that is rarely needed).
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import qualified Data.Map.Lazy as Map
--
-- The implementation of 'Map' is based on /size balanced/ binary trees (or
-- trees of /bounded balance/) as described by:
--
--    * Stephen Adams, \"/Efficient sets: a balancing act/\",
--     Journal of Functional Programming 3(4):553-562, October 1993,
--     <http://www.swiss.ai.mit.edu/~adams/BB/>.
--
--    * J. Nievergelt and E.M. Reingold,
--      \"/Binary search trees of bounded balance/\",
--      SIAM journal of computing 2(1), March 1973.
--
-- Note that the implementation is /left-biased/ -- the elements of a
-- first argument are always preferred to the second, for example in
-- 'union' or 'insert'.
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation (<http://en.wikipedia.org/wiki/Big_O_notation>).
-----------------------------------------------------------------------------

module Data.Map.Lazy (
            -- * Map type
#if !defined(TESTING)
              Map              -- instance Eq,Show,Read
#else
              Map(..)          -- instance Eq,Show,Read
#endif

            -- * Operators
            , (!), (\\)

            -- * Query
            , M.null
            , size
            , member
            , notMember
            , M.lookup
            , findWithDefault

            -- * Construction
            , empty
            , singleton

            -- ** Insertion
            , insert
            , insertWith
            , insertWithKey
            , insertLookupWithKey

            -- ** Delete\/Update
            , delete
            , adjust
            , adjustWithKey
            , update
            , updateWithKey
            , updateLookupWithKey
            , alter

            -- * Combine

            -- ** Union
            , union
            , unionWith
            , unionWithKey
            , unions
            , unionsWith

            -- ** Difference
            , difference
            , differenceWith
            , differenceWithKey

            -- ** Intersection
            , intersection
            , intersectionWith
            , intersectionWithKey

            -- * Traversal
            -- ** Map
            , M.map
            , mapWithKey
            , mapAccum
            , mapAccumWithKey
            , mapAccumRWithKey
            , mapKeys
            , mapKeysWith
            , mapKeysMonotonic

            -- * Folds
            , M.foldr
            , M.foldl
            , foldrWithKey
            , foldlWithKey
            -- ** Strict folds
            , foldr'
            , foldl'
            , foldrWithKey'
            , foldlWithKey'

            -- * Conversion
            , elems
            , keys
            , keysSet
            , assocs

            -- ** Lists
            , toList
            , fromList
            , fromListWith
            , fromListWithKey

            -- ** Ordered lists
            , toAscList
            , toDescList
            , fromAscList
            , fromAscListWith
            , fromAscListWithKey
            , fromDistinctAscList

            -- * Filter
            , M.filter
            , filterWithKey
            , partition
            , partitionWithKey

            , mapMaybe
            , mapMaybeWithKey
            , mapEither
            , mapEitherWithKey

            , split
            , splitLookup

            -- * Submap
            , isSubmapOf, isSubmapOfBy
            , isProperSubmapOf, isProperSubmapOfBy

            -- * Indexed
            , lookupIndex
            , findIndex
            , elemAt
            , updateAt
            , deleteAt

            -- * Min\/Max
            , findMin
            , findMax
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax
            , updateMin
            , updateMax
            , updateMinWithKey
            , updateMaxWithKey
            , minView
            , maxView
            , minViewWithKey
            , maxViewWithKey

            -- * Debugging
            , showTree
            , showTreeWith
            , valid

#if defined(TESTING)
            -- * Internals
            , bin
            , balanced
            , join
            , merge
#endif

            ) where

import Data.Map.Base as M
