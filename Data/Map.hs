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
-- /Deprecated./ As of version 0.5, replaced by 'Data.Map.Lazy' and
-- 'Data.Map.Strict'. These two modules implement value lazy and value
-- strict maps, respectively.
-----------------------------------------------------------------------------

module Data.Map
    ( module Data.Map.Lazy
    , insertWith'
    , insertWithKey'
    , insertLookupWithKey'
    , fold
    , foldWithKey
    ) where

import Data.Map.Lazy
import qualified Data.Map.Lazy as L
import qualified Data.Map.Strict as S

-- | /Deprecated./ As of version 0.5, replaced by 'S.insertWith'.
--
-- /O(log n)/. Same as 'insertWith', but the combining function is
-- applied strictly.  This is often the most desirable behavior.
--
-- For example, to update a counter:
--
-- > insertWith' (+) k 1 m
--
insertWith' :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith' = S.insertWith
{-# INLINE insertWith' #-}

-- | /Deprecated./ As of version 0.5, replaced by 'S.insertWithKey'.
--
-- /O(log n)/. Same as 'insertWithKey', but the combining function is
-- applied strictly.
insertWithKey' :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey' = S.insertWithKey
{-# INLINE insertWithKey' #-}

-- | /Deprecated./ As of version 0.5, replaced by
-- 'S.insertLookupWithKey'.
--
-- /O(log n)/. A strict version of 'insertLookupWithKey'.
insertLookupWithKey' :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a
                     -> (Maybe a, Map k a)
insertLookupWithKey' = S.insertLookupWithKey
{-# INLINE insertLookupWithKey' #-}

-- | /Deprecated./ As of version 0.5, replaced by 'L.foldr'.
--
-- /O(n)/. Fold the values in the map using the given right-associative
-- binary operator. This function is an equivalent of 'foldr' and is present
-- for compatibility only.
fold :: (a -> b -> b) -> b -> Map k a -> b
fold = L.foldr
{-# INLINE fold #-}

-- | /Deprecated./ As of version 0.4, replaced by 'L.foldrWithKey'.
--
-- /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator. This function is an equivalent of 'foldrWithKey' and is present
-- for compatibility only.
foldWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldWithKey = foldrWithKey
{-# INLINE foldWithKey #-}
