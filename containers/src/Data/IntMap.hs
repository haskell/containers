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
