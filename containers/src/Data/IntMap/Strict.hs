{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Trustworthy #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMap.Strict
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
--
-- = Finite Int Maps (strict interface)
--
-- The @'IntMap' v@ type represents a finite map (sometimes called a dictionary)
-- from key of type @Int@ to values of type @v@.
--
-- Each function in this module is careful to force values before installing
-- them in an 'IntMap'. This is usually more efficient when laziness is not
-- necessary. When laziness /is/ required, use the functions in
-- "Data.IntMap.Lazy".
--
-- In particular, the functions in this module obey the following law:
--
--  - If all values stored in all maps in the arguments are in WHNF, then all
--    values stored in all maps in the results will be in WHNF once those maps
--    are evaluated.
--
-- For a walkthrough of the most commonly used functions see the
-- <https://haskell-containers.readthedocs.io/en/latest/map.html maps introduction>.
--
-- This module is intended to be imported qualified, to avoid name clashes with
-- Prelude functions:
--
-- > import Data.IntMap.Strict (IntMap)
-- > import qualified Data.IntMap.Strict as IntMap
--
-- Note that the implementation is generally /left-biased/. Functions that take
-- two maps as arguments and combine them, such as `union` and `intersection`,
-- prefer the values in the first argument to those in the second.
--
--
-- == Detailed performance information
--
-- The amortized running time is given for each operation, with /n/ referring to
-- the number of entries in the map and /W/ referring to the number of bits in
-- an 'Int' (32 or 64).
--
-- Benchmarks comparing "Data.IntMap.Strict" with other dictionary
-- implementations can be found at https://github.com/haskell-perf/dictionaries.
--
--
-- == Warning
--
-- The 'IntMap' type is shared between the lazy and strict modules, meaning that
-- the same 'IntMap' value can be passed to functions in both modules. This
-- means that the 'Functor', 'Traversable' and 'Data.Data.Data' instances are
-- the same as for the "Data.IntMap.Lazy" module, so if they are used the
-- resulting map may contain suspended values (thunks).
--
--
-- == Implementation
--
-- The implementation is based on /big-endian patricia trees/.  This data
-- structure performs especially well on binary operations like 'union' and
-- 'intersection'. Additionally, benchmarks show that it is also (much) faster
-- on insertions and deletions when compared to a generic size-balanced map
-- implementation (see "Data.Map").
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--      Workshop on ML, September 1998, pages 77-86,
--      <http://citeseer.ist.psu.edu/okasaki98fast.html>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve
--      Information Coded In Alphanumeric/\", Journal of the ACM, 15(4),
--      October 1968, pages 514-534.
--
-----------------------------------------------------------------------------

-- See the notes at the beginning of Data.IntMap.Internal.

module Data.IntMap.Strict (
    -- * Map type
#if !defined(TESTING)
    IntMap, Key          -- instance Eq,Show
#else
    IntMap(..), Key          -- instance Eq,Show
#endif

    -- * Construction
    , empty
    , singleton
    , fromSet

    -- ** From Unordered Lists
    , fromList
    , fromListWith
    , fromListWithKey

    -- ** From Ascending Lists
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromDistinctAscList

    -- * Insertion
    , insert
    , insertWith
    , insertWithKey
    , insertLookupWithKey

    -- * Deletion\/Update
    , delete
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter
    , alterF

    -- * Query
    -- ** Lookup
    , lookup
    , (!?)
    , (!)
    , findWithDefault
    , member
    , notMember
    , lookupLT
    , lookupGT
    , lookupLE
    , lookupGE

    -- ** Size
    , null
    , size

    -- * Combine

    -- ** Union
    , union
    , unionWith
    , unionWithKey
    , unions
    , unionsWith

    -- ** Difference
    , difference
    , (\\)
    , differenceWith
    , differenceWithKey

    -- ** Intersection
    , intersection
    , intersectionWith
    , intersectionWithKey

    -- ** Disjoint
    , disjoint

    -- ** Universal combining function
    , mergeWithKey

    -- * Traversal
    -- ** Map
    , map
    , mapWithKey
    , traverseWithKey
    , mapAccum
    , mapAccumWithKey
    , mapAccumRWithKey
    , mapKeys
    , mapKeysWith
    , mapKeysMonotonic

    -- * Folds
    , foldr
    , foldl
    , foldrWithKey
    , foldlWithKey
    , foldMapWithKey

    -- ** Strict folds
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'

    -- * Conversion
    , elems
    , keys
    , assocs
    , keysSet

    -- ** Lists
    , toList

-- ** Ordered lists
    , toAscList
    , toDescList

    -- * Filter
    , filter
    , filterWithKey
    , restrictKeys
    , withoutKeys
    , partition
    , partitionWithKey

    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey

    , split
    , splitLookup
    , splitRoot

    -- * Submap
    , isSubmapOf, isSubmapOfBy
    , isProperSubmapOf, isProperSubmapOfBy

    -- * Min\/Max
    , lookupMin
    , lookupMax
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

#ifdef __GLASGOW_HASKELL__
    -- * Debugging
    , showTree
    , showTreeWith
#endif
    ) where

import Data.IntMap.Strict.Internal
import Prelude ()
