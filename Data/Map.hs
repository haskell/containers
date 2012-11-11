{-# LANGUAGE CPP #-}
#if !defined(TESTING) && __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map
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
-- This module re-exports the value lazy "Data.Map.Lazy" API, plus
-- several deprecated value strict functions. Please note that these functions
-- have different strictness properties than those in "Data.Map.Strict":
-- they only evaluate the values inserted into the map. For example, the
-- default value to 'insertWith'' is only evaluated if it's used, i.e. because
-- there's no value for the key already or because the higher-order argument
-- that combines the old and new value uses it.
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import qualified Data.Map as Map
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

module Data.Map
    ( module Data.Map.Lazy
    , insertWith'
    , insertWithKey'
    , insertLookupWithKey'
    , fold
    , foldWithKey
    ) where

import Prelude hiding (foldr)
import Data.Map.Base (Map(..), balanceL, balanceR)
import Data.Map.Lazy
import Data.StrictPair

-- | /Deprecated./ As of version 0.5, replaced by 'Data.Map.Strict.insertWith'.
--
-- /O(log n)/. Same as 'insertWith', but the value being inserted to the map is
-- evaluated to WHNF beforehand. In contrast to 'Data.Map.Strict.insertWith',
-- the value argument is not evaluted when not needed.
--
-- For example, to update a counter:
--
-- > insertWith' (+) k 1 m
--

insertWith' :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
-- We do not reuse Data.Map.Strict.insertWith, because it is stricter -- it
-- forces evaluation of the given value. Some people depend on the original
-- behaviour, which forces only the key and the result of combining function.
-- Particularly, people use insertWith' as a strict version of adjust, which
-- requires to use undefined in the place of the value.
insertWith' f = insertWithKey' (\_ x' y' -> f x' y')
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWith' #-}
#else
{-# INLINE insertWith' #-}
#endif

-- | /Deprecated./ As of version 0.5, replaced by
-- 'Data.Map.Strict.insertWithKey'.
--
-- /O(log n)/. Same as 'insertWithKey', but the value being inserted to the map is
-- evaluated to WHNF beforehand. In contrast to 'Data.Map.Strict.insertWithKey',
-- the value argument is not evaluted when not needed.

insertWithKey' :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
-- We do not reuse Data.Map.Strict.insertWithKey, because it is stricter -- it
-- forces evaluation of the given value.
insertWithKey' = go
  where
    go :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
    go _ kx _ _ | kx `seq` False = undefined
    go _ kx x Tip = x `seq` singleton kx x
    go f kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go f kx x l) r
            GT -> balanceR ky y l (go f kx x r)
            EQ -> let x' = f kx x y
                  in x' `seq` Bin sy kx x' l r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWithKey' #-}
#else
{-# INLINE insertWithKey' #-}
#endif

-- | /Deprecated./ As of version 0.5, replaced by
-- 'Data.Map.Strict.insertLookupWithKey'.
--
-- /O(log n)/. Same as 'insertLookupWithKey', but the value being inserted to
-- the map is evaluated to WHNF beforehand. In contrast to
-- 'Data.Map.Strict.insertLookupWithKey', the value argument is not evaluted
-- when not needed.

insertLookupWithKey' :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a
                     -> (Maybe a, Map k a)
-- We do not reuse Data.Map.Strict.insertLookupWithKey, because it is stricter -- it
-- forces evaluation of the given value.
insertLookupWithKey' f0 kx0 x0 t0 = toPair $ go f0 kx0 x0 t0
  where
    go :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> StrictPair (Maybe a) (Map k a)
    go _ kx _ _ | kx `seq` False = undefined
    go _ kx x Tip = x `seq` Nothing :*: singleton kx x
    go f kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> let (found :*: l') = go f kx x l
                  in found :*: balanceL ky y l' r
            GT -> let (found :*: r') = go f kx x r
                  in found :*: balanceR ky y l r'
            EQ -> let x' = f kx x y
                  in x' `seq` (Just y :*: Bin sy kx x' l r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertLookupWithKey' #-}
#else
{-# INLINE insertLookupWithKey' #-}
#endif

-- | /Deprecated./ As of version 0.5, replaced by 'foldr'.
--
-- /O(n)/. Fold the values in the map using the given right-associative
-- binary operator. This function is an equivalent of 'foldr' and is present
-- for compatibility only.
fold :: (a -> b -> b) -> b -> Map k a -> b
fold = foldr
{-# INLINE fold #-}

-- | /Deprecated./ As of version 0.4, replaced by 'foldrWithKey'.
--
-- /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator. This function is an equivalent of 'foldrWithKey' and is present
-- for compatibility only.
foldWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldWithKey = foldrWithKey
{-# INLINE foldWithKey #-}
