{-# LANGUAGE CPP #-}
#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Safe #-}
#endif
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
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
-- An efficient implementation of maps from integer keys to values
-- (dictionaries).
--
-- This module re-exports the value lazy "Data.IntMap.Lazy" API, plus
-- several deprecated value strict functions. Please note that these functions
-- have different strictness properties than those in "Data.IntMap.Strict":
-- they only evaluate the result of the combining function. For example, the
-- default value to 'insertWith'' is only evaluated if the combining function
-- is called and uses it.
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
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
#ifdef __GLASGOW_HASKELL__
-- For GHC, we disable these, pending removal. For anything else,
-- we just don't define them at all.
    , insertWith'
    , insertWithKey'
    , fold
    , foldWithKey
#endif
    ) where

import Data.IntMap.Lazy

#ifdef __GLASGOW_HASKELL__
import Utils.Containers.Internal.TypeError

-- | This function is being removed and is no longer usable.
-- Use 'Data.IntMap.Strict.insertWith'
insertWith' :: Whoops "Data.IntMap.insertWith' is gone. Use Data.IntMap.Strict.insertWith."
            => (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWith' _ _ _ _ = undefined

-- | This function is being removed and is no longer usable.
-- Use 'Data.IntMap.Strict.insertWithKey'.
insertWithKey' :: Whoops "Data.IntMap.insertWithKey' is gone. Use Data.IntMap.Strict.insertWithKey."
               => (Key -> a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWithKey' _ _ _ _ = undefined

-- | This function is being removed and is no longer usable.
-- Use 'Data.IntMap.Lazy.foldr'.
fold :: Whoops "Data.IntMap.fold' is gone. Use Data.IntMap.foldr or Prelude.foldr."
     => (a -> b -> b) -> b -> IntMap a -> b
fold _ _ _ = undefined

-- | This function is being removed and is no longer usable.
-- Use 'foldrWithKey'.
foldWithKey :: Whoops "Data.IntMap.foldWithKey is gone. Use foldrWithKey."
            => (Key -> a -> b -> b) -> b -> IntMap a -> b
foldWithKey _ _ _ = undefined
#endif
