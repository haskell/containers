{-# LANGUAGE NoBangPatterns, ScopedTypeVariables #-}
#if !defined(TESTING) && __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMap
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of maps from integer keys to values.
--
-- Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.IntMap (IntMap)
-- >  import qualified Data.IntMap as IntMap
--
-- The implementation is based on /big-endian patricia trees/.  This data
-- structure performs especially well on binary operations like 'union'
-- and 'intersection'.  However, my benchmarks show that it is also
-- (much) faster on insertions and deletions when compared to a generic
-- size-balanced map implementation (see "Data.Map").
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--      Workshop on ML, September 1998, pages 77-86,
--      <http://citeseer.ist.psu.edu/okasaki98fast.html>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve
--      Information Coded In Alphanumeric/\", Journal of the ACM, 15(4),
--      October 1968, pages 514-534.
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation <http://en.wikipedia.org/wiki/Big_O_notation>.
-- Many operations have a worst-case complexity of /O(min(n,W))/.
-- This means that the operation can become linear in the number of
-- elements with a maximum of /W/ -- the number of bits in an 'Int'
--
-- This module is spine strict, but value lazy.  If you require strict
-- operations on these maps, please use "Data.IntMap.Strict".
-- (32 or 64).
-----------------------------------------------------------------------------

module Data.IntMap (
            -- * Map type
#if !defined(TESTING)
              IntMap, Key          -- instance Eq,Show
#else
              IntMap(..), Key          -- instance Eq,Show
#endif

            -- * Operators
            , (!), (\\)

            -- * Query
            , null
            , size
            , member
            , notMember
            , lookup
            , findWithDefault

            -- * Construction
            , empty
            , singleton

            -- ** Insertion
            , insert
            , insertWith
            , insertWith'
            , insertWithKey
            , insertWithKey'
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
            , map
            , mapWithKey
            , mapAccum
            , mapAccumWithKey
            , mapAccumRWithKey

            -- * Folds
            , foldr
            , foldl
            , foldrWithKey
            , foldlWithKey
            -- ** Strict folds
            , foldr'
            , foldl'
            , foldrWithKey'
            , foldlWithKey'
            -- ** Legacy folds
            , fold
            , foldWithKey

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
            , fromAscList
            , fromAscListWith
            , fromAscListWithKey
            , fromDistinctAscList

            -- * Filter
            , filter
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
            ) where

import Prelude hiding (lookup,map,filter,foldr,foldl,null)
import Data.IntMap.Lazy
import qualified Data.IntMap.Strict as S

-- | Same as 'insertWith', but the combining function is applied strictly.
-- This function is deprecated, use 'insertWith' in "Data.IntMap.Strict"
-- instead.
insertWith' :: (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWith' = S.insertWith
{-# INLINE insertWith' #-}
-- {-# DEPRECATED insertWith' "Use insertWith in Data.IntMap.Strict instead" #-}

-- | Same as 'insertWithKey', but the combining function is applied strictly.
-- This function is deprecated, use 'insertWithKey' in "Data.IntMap.Strict"
-- instead.
insertWithKey' :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWithKey' = S.insertWithKey
{-# INLINE insertWithKey' #-}
-- {-# DEPRECATED insertWithKey' "Use insertWithKey in Data.IntMap.Strict instead" #-}

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator. This function is an equivalent of 'foldr' and is present
-- for compatibility only.
--
-- /Please note that fold will be deprecated in the future and removed./
fold :: (a -> b -> b) -> b -> IntMap a -> b
fold = foldr
{-# INLINE fold #-}
-- {-# DEPRECATED fold "Use foldr instead." #-}

-- | /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator. This function is an equivalent of 'foldrWithKey' and is present
-- for compatibility only.
--
-- /Please note that foldWithKey will be deprecated in the future and removed./
foldWithKey :: (Int -> a -> b -> b) -> b -> IntMap a -> b
foldWithKey = foldrWithKey
{-# INLINE foldWithKey #-}
-- {-# DEPRECATED foldWithKey "Use foldrWithKey instead." #-}
