{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
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
-- == Implementation
--
-- The implementation is based on /big-endian patricia trees/.  This data
-- structure performs especially well on binary operations like 'union'
-- and 'intersection'. Additionally, benchmarks show that it is also
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
--
-- == Performance information
--
-- The time complexity is given for each operation in
-- [big-O notation](http://en.wikipedia.org/wiki/Big_O_notation), with \(n\)
-- referring to the number of entries in the map and \(W\) referring to the
-- number of bits in an 'Int' (32 or 64).
--
-- Operations like 'member', 'insert', and 'delete' have a worst-case
-- complexity of \(O(\min(n,W))\). This means that the operation can become
-- linear in the number of elements with a maximum of \(W\) -- the number of
-- bits in an 'Int' (32 or 64). These peculiar asymptotics are determined by the
-- depth of the Patricia trees:
--
-- * even for an extremely unbalanced tree, the depth cannot be larger than
--   the number of elements \(n\),
-- * each level of a Patricia tree determines at least one more bit
--   shared by all subelements, so there could not be more
--   than \(W\) levels.
--
-- If all \(n\) elements in the tree are between 0 and \(N\) (or, say, between
-- \(-N\) and \(N\)), the estimate can be refined to \(O(\min(n, \log N))\). If
-- the set is sufficiently "dense", this becomes \(O(\min(n, \log n))\) or
-- simply the familiar \(O(\log n)\), matching balanced binary trees.
--
-- The most performant scenario for 'IntSet' are elements from a contiguous
-- subset, in which case the complexity is proportional to \(\log n\), capped
-- by \(W\). The worst scenario are exponentially growing elements \(1,2,4,
-- \ldots,2^n\), for which complexity grows as fast as \(n\) but again is capped
-- by \(W\).
--
-- Binary set operations like 'union' and 'intersection' take
-- \(O(\min(n, m \log \frac{2^W}{m}))\) time, where \(m\) and \(n\)
-- are the sizes of the smaller and larger input sets respectively.
--
-----------------------------------------------------------------------------

module Data.IntSet (
            -- * Set type
              IntSet          -- instance Eq,Show
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
            , IS.null
            , size
            , compareSize
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
            , Intersection(..)

            -- * Filter
            , IS.filter
            , partition

            , takeWhileAntitone
            , dropWhileAntitone
            , spanAntitone

            , mapMaybe

            , split
            , splitMember
            , splitRoot

            -- * Map
            , IS.map
            , mapMonotonic

            -- * Folds
            , IS.foldr
            , IS.foldl
            , IS.foldMap
            -- ** Strict folds
            , IS.foldr'
            , IS.foldl'
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
            ) where

import Data.IntSet.Internal as IS
