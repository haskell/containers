{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE Safe #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMap
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- The @'IntMap' v@ type represents a finite map (sometimes called a dictionary)
-- from key of type @Int@ to values of type @v@.
--
-- This module re-exports the value lazy "Data.IntMap.Lazy" API.
--
-- This module is intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import Data.IntMap (IntMap)
-- >  import qualified Data.IntMap as IntMap
--
--
-- == Implementation
--
-- The implementation is based on /big-endian patricia trees/.  This data
-- structure performs especially well on binary operations like 'union'
-- and 'intersection'. Additionally, benchmarks show that it is also
-- (much) faster on insertions and deletions when compared to a generic
-- size-balanced map implementation (see "Data.Map").
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
--
-- == Performance information
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation <http://en.wikipedia.org/wiki/Big_O_notation>.
--
-- Many operations have a worst-case complexity of \(O(\min(n,W))\).
-- This means that the operation can become linear in the number of
-- elements with a maximum of \(W\) -- the number of bits in an 'Int'
-- (32 or 64). These peculiar asymptotics are determined by the depth
-- of the Patricia trees:
--
-- * even for an extremely unbalanced tree, the depth cannot be larger than
--   the number of elements \(n\),
-- * each level of a Patricia tree determines at least one more bit
--   shared by all subelements, so there could not be more
--   than \(W\) levels.
--
-- If all \(n\) keys in the tree are between 0 and \(N\) (or, say, between \(-N\) and \(N\)),
-- the estimate can be refined to \(O(\min(n, \log N))\). If the set of keys
-- is sufficiently "dense", this becomes \(O(\min(n, \log n))\) or simply
-- the familiar \(O(\log n)\), matching balanced binary trees.
--
-- The most performant scenario for 'IntMap' are keys from a contiguous subset,
-- in which case the complexity is proportional to \(\log n\), capped by \(W\).
-- The worst scenario are exponentially growing keys \(1,2,4,\ldots,2^n\),
-- for which complexity grows as fast as \(n\) but again is capped by \(W\).
--
-----------------------------------------------------------------------------

module Data.IntMap
    ( module Data.IntMap.Lazy
    ) where

import Data.IntMap.Lazy
