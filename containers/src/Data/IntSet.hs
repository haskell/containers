{-# LANGUAGE CPP #-}
#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Safe #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntSet
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Joachim Breitner 2011
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
--
-- = Finite Int Sets
--
-- The @'IntSet'@ type represents a set of elements of type @Int@. An @IntSet@
-- is strict in its elements.
--
-- For a walkthrough of the most commonly used functions see their
-- <https://haskell-containers.readthedocs.io/en/latest/set.html sets introduction>.
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import Data.IntSet (IntSet)
-- >  import qualified Data.IntSet as IntSet
--
--
-- == Performance information
--
-- Many operations have a worst-case complexity of \(O(\min(n,W))\).
-- This means that the operation can become linear in the number of
-- elements with a maximum of \(W\) -- the number of bits in an 'Int'
-- (32 or 64).
--
--
-- == Implementation
--
-- The implementation is based on /big-endian patricia trees/.  This data
-- structure performs especially well on binary operations like 'union'
-- and 'intersection'.  However, my benchmarks show that it is also
-- (much) faster on insertions and deletions when compared to a generic
-- size-balanced set implementation (see "Data.Set").
--
--    * Chris Okasaki and Andy Gill,
--      \"/Fast Mergeable Integer Maps/\",
--      Workshop on ML, September 1998, pages 77-86,
--      <https://web.archive.org/web/20150417234429/https://ittc.ku.edu/~andygill/papers/IntMap98.pdf>.
--
--    * D.R. Morrison,
--      \"/PATRICIA -- Practical Algorithm To Retrieve Information Coded In Alphanumeric/\",
--      Journal of the ACM, 15(4), October 1968, pages 514-534,
--      <https://doi.org/10.1145/321479.321481>.
--
-- Additionally, this implementation places bitmaps in the leaves of the tree.
-- Their size is the natural size of a machine word (32 or 64 bits) and greatly
-- reduces the memory footprint and execution times for dense sets, e.g. sets
-- where it is likely that many values lie close to each other. The asymptotics
-- are not affected by this optimization.
--
-----------------------------------------------------------------------------

module Data.IntSet (
            -- * Set type
#if !defined(TESTING)
              IntSet          -- instance Eq,Show
#else
              IntSet(..)      -- instance Eq,Show
#endif
            , Key

            -- * Construction
            , empty
            , singleton
            , fromList
            , fromRange
            , fromAscList
            , fromDistinctAscList

            -- * Insertion
            , insert

            -- * Deletion
            , delete

            -- * Generalized insertion/deletion
            , alterF

            -- * Query
            , member
            , notMember
            , lookupLT
            , lookupGT
            , lookupLE
            , lookupGE
            , IS.null
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
#if MIN_VERSION_base(4,18,0)
            , intersections
#endif
            , symmetricDifference
            , Intersection(..)

            -- * Filter
            , IS.filter
            , partition

            , takeWhileAntitone
            , dropWhileAntitone
            , spanAntitone

            , split
            , splitMember
            , splitRoot

            -- * Map
            , IS.map
            , mapMonotonic

            -- * Folds
            , IS.foldr
            , IS.foldl
            -- ** Strict folds
            , IS.foldr'
            , IS.foldl'
            -- ** Legacy folds
            , fold

            -- * Min\/Max
            , lookupMin
            , lookupMax
            , findMin
            , findMax
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax
            , maxView
            , minView

            -- * Conversion

            -- ** List
            , elems
            , toList
            , toAscList
            , toDescList

            -- * Debugging
            , showTree
            , showTreeWith
            ) where

import Data.IntSet.Internal.IntTreeCommons (Key)
import Data.IntSet.Internal as IS
