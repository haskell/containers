{-# LANGUAGE CPP #-}
#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Safe #-}
#endif
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DataKinds, FlexibleContexts #-}
#endif
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE MonoLocalBinds #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMap
-- Copyright   :  Documentation & Interface (c) Daan Leijen 2002
--                Documentation (c) Andriy Palamarchuk 2008
--                Documentation & Implementation (c) Jonathan S. 2016
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
-- Operation comments contain the operation time complexity in
-- the Big-O notation <http://en.wikipedia.org/wiki/Big_O_notation>.
-- Many operations have a worst-case complexity of /O(min(n,W))/.
-- This means that the operation can become linear in the number of
-- elements with a maximum of /W/ -- the number of bits in an 'Int'
-- (32 or 64).
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

import Prelude hiding (foldr)
import qualified Data.IntMap.Strict as Strict
import Data.IntMap.Lazy

-- | /O(log n)/. Same as 'insertWith', but the result of the combining function
-- is evaluated to WHNF before inserted to the map.
{-# DEPRECATED insertWith' "As of version 0.5, replaced by 'Data.IntMap.Strict.insertWith'." #-}
{-# INLINE insertWith' #-}
insertWith' :: (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWith' = Strict.insertWith

-- | /O(log n)/. Same as 'insertWithKey', but the result of the combining
-- function is evaluated to WHNF before inserted to the map.
{-# DEPRECATED insertWithKey' "As of version 0.5, replaced by 'Data.IntMap.Strict.insertWithKey'." #-}
{-# INLINE insertWithKey' #-}
insertWithKey' :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWithKey' = Strict.insertWithKey

-- | /O(n)/. Fold the values in the map using the given
-- right-associative binary operator. This function is an equivalent
-- of 'foldr' and is present for compatibility only.
{-# DEPRECATED fold "As of version 0.5, replaced by 'foldr'." #-}
{-# INLINE fold #-}
fold :: (a -> b -> b) -> b -> IntMap a -> b
fold = foldr

-- | /O(n)/. Fold the keys and values in the map using the given
-- right-associative binary operator. This function is an equivalent
-- of 'foldrWithKey' and is present for compatibility only.
{-# DEPRECATED foldWithKey "As of version 0.5, replaced by 'foldrWithKey'." #-}
{-# INLINE foldWithKey #-}
foldWithKey :: (Key -> a -> b -> b) -> b -> IntMap a -> b
foldWithKey = foldrWithKey
