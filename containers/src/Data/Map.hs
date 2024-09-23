{-# LANGUAGE CPP #-}
#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Safe #-}
#endif

#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DataKinds, FlexibleContexts, MonoLocalBinds #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- /Note:/ You should use "Data.Map.Strict" instead of this module if:
--
-- * You will eventually need all the values stored.
--
-- * The stored values don't represent large virtual data structures
-- to be lazily computed.
--
-- An efficient implementation of ordered maps from keys to values
-- (dictionaries).
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import qualified Data.Map as Map
--
-- The implementation of 'Map' is based on /size balanced/ binary trees (or
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
-- Note that the implementation is /left-biased/ -- the elements of a
-- first argument are always preferred to the second, for example in
-- 'union' or 'insert'.
--
-- /Warning/: The size of the map must not exceed @maxBound::Int@. Violation of
-- this condition is not detected and if the size limit is exceeded, its
-- behaviour is undefined.
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation (<http://en.wikipedia.org/wiki/Big_O_notation>).
-----------------------------------------------------------------------------

module Data.Map
    ( module Data.Map.Lazy
#ifdef __GLASGOW_HASKELL__
    , insertWith'
    , insertWithKey'
    , insertLookupWithKey'
    , fold
    , foldWithKey
#endif
    ) where

import Data.Map.Lazy

#ifdef __GLASGOW_HASKELL__
import Utils.Containers.Internal.TypeError

-- | This function is being removed and is no longer usable.
-- Use 'Data.Map.Strict.insertWith'.
insertWith' :: Whoops "Data.Map.insertWith' is gone. Use Data.Map.Strict.insertWith."
            => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith' _ _ _ _ = undefined

-- | This function is being removed and is no longer usable.
-- Use 'Data.Map.Strict.insertWithKey'.
insertWithKey' :: Whoops "Data.Map.insertWithKey' is gone. Use Data.Map.Strict.insertWithKey."
               => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey' _ _ _ _ = undefined

-- | This function is being removed and is no longer usable.
-- Use 'Data.Map.Strict.insertLookupWithKey'.
insertLookupWithKey' :: Whoops "Data.Map.insertLookupWithKey' is gone. Use Data.Map.Strict.insertLookupWithKey."
                     => (k -> a -> a -> a) -> k -> a -> Map k a
                     -> (Maybe a, Map k a)
insertLookupWithKey' _ _ _ _ = undefined

-- | This function is being removed and is no longer usable.
-- Use 'Data.Map.Strict.foldr'.
fold :: Whoops "Data.Map.fold is gone. Use foldr."
     => (a -> b -> b) -> b -> Map k a -> b
fold _ _ _ = undefined

-- | This function is being removed and is no longer usable.
-- Use 'foldrWithKey'.
foldWithKey :: Whoops "Data.Map.foldWithKey is gone. Use foldrWithKey."
            => (k -> a -> b -> b) -> b -> Map k a -> b
foldWithKey _ _ _ = undefined
#endif
