{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Trustworthy #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMap.Merge.Strict
-- Copyright   :  (c) wren romano 2016
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- This module defines an API for writing functions that merge two
-- maps. The key functions are 'merge' and 'mergeA'.
-- Each of these can be used with several different \"merge tactics\".
--
-- The 'merge' and 'mergeA' functions are shared by
-- the lazy and strict modules. Only the choice of merge tactics
-- determines strictness. If you use 'Data.Map.Merge.Strict.mapMissing'
-- from this module then the results will be forced before they are
-- inserted. If you use 'Data.Map.Merge.Lazy.mapMissing' from
-- "Data.Map.Merge.Lazy" then they will not.
--
-- == Efficiency note
--
-- The 'Control.Category.Category', 'Applicative', and 'Monad' instances for
-- 'WhenMissing' tactics are included because they are valid. However, they are
-- inefficient in many cases and should usually be avoided. The instances
-- for 'WhenMatched' tactics should not pose any major efficiency problems.
--
-- @since 0.5.9

module Data.IntMap.Merge.Strict (
    -- ** Simple merge tactic types
      SimpleWhenMissing
    , SimpleWhenMatched

    -- ** General combining function
    , merge

    -- *** @WhenMatched@ tactics
    , zipWithMaybeMatched
    , zipWithMatched

    -- *** @WhenMissing@ tactics
    , mapMaybeMissing
    , dropMissing
    , preserveMissing
    , mapMissing
    , filterMissing

    -- ** Applicative merge tactic types
    , WhenMissing
    , WhenMatched

    -- ** Applicative general combining function
    , mergeA

    -- *** @WhenMatched@ tactics
    -- | The tactics described for 'merge' work for
    -- 'mergeA' as well. Furthermore, the following
    -- are available.
    , zipWithMaybeAMatched
    , zipWithAMatched

    -- *** @WhenMissing@ tactics
    -- | The tactics described for 'merge' work for
    -- 'mergeA' as well. Furthermore, the following
    -- are available.
    , traverseMaybeMissing
    , traverseMissing
    , filterAMissing

    -- ** Covariant maps for tactics
    , mapWhenMissing
    , mapWhenMatched

    -- ** Miscellaneous functions on tactics

    , runWhenMatched
    , runWhenMissing
    ) where

import Data.IntMap.Internal
  ( SimpleWhenMissing
  , SimpleWhenMatched
  , merge
  , dropMissing
  , preserveMissing
  , filterMissing
  , WhenMissing (..)
  , WhenMatched (..)
  , mergeA
  , filterAMissing
  , runWhenMatched
  , runWhenMissing
  )
import Data.IntMap.Strict.Internal
import Prelude hiding (filter, map, foldl, foldr)

-- | Map covariantly over a @'WhenMissing' f k x@.
mapWhenMissing :: Functor f => (a -> b) -> WhenMissing f x a -> WhenMissing f x b
mapWhenMissing f q = WhenMissing
  { missingSubtree = fmap (map f) . missingSubtree q
  , missingKey = \k x -> fmap (forceMaybe . fmap f) $ missingKey q k x}

-- | Map covariantly over a @'WhenMatched' f k x y@.
mapWhenMatched :: Functor f => (a -> b) -> WhenMatched f x y a -> WhenMatched f x y b
mapWhenMatched f q = WhenMatched
  { matchedKey = \k x y -> fmap (forceMaybe . fmap f) $ runWhenMatched q k x y }

-- | When a key is found in both maps, apply a function to the
-- key and values and maybe use the result in the merged map.
--
-- @
-- zipWithMaybeMatched :: (k -> x -> y -> Maybe z)
--                     -> SimpleWhenMatched k x y z
-- @
zipWithMaybeMatched :: Applicative f
                    => (Key -> x -> y -> Maybe z)
                    -> WhenMatched f x y z
zipWithMaybeMatched f = WhenMatched $
  \k x y -> pure $! forceMaybe $! f k x y
{-# INLINE zipWithMaybeMatched #-}

-- | When a key is found in both maps, apply a function to the
-- key and values, perform the resulting action, and maybe use
-- the result in the merged map.
--
-- This is the fundamental 'WhenMatched' tactic.
zipWithMaybeAMatched :: Applicative f
                     => (Key -> x -> y -> f (Maybe z))
                     -> WhenMatched f x y z
zipWithMaybeAMatched f = WhenMatched $
  \ k x y -> forceMaybe <$> f k x y
{-# INLINE zipWithMaybeAMatched #-}

-- | When a key is found in both maps, apply a function to the
-- key and values to produce an action and use its result in the merged map.
zipWithAMatched :: Applicative f
                => (Key -> x -> y -> f z)
                -> WhenMatched f x y z
zipWithAMatched f = WhenMatched $
  \ k x y -> (Just $!) <$> f k x y
{-# INLINE zipWithAMatched #-}

-- | When a key is found in both maps, apply a function to the
-- key and values and use the result in the merged map.
--
-- @
-- zipWithMatched :: (k -> x -> y -> z)
--                -> SimpleWhenMatched k x y z
-- @
zipWithMatched :: Applicative f
               => (Key -> x -> y -> z) -> WhenMatched f x y z
zipWithMatched f = WhenMatched $
  \k x y -> pure $! Just $! f k x y
{-# INLINE zipWithMatched #-}

-- | Map over the entries whose keys are missing from the other map,
-- optionally removing some. This is the most powerful 'SimpleWhenMissing'
-- tactic, but others are usually more efficient.
--
-- @
-- mapMaybeMissing :: (k -> x -> Maybe y) -> SimpleWhenMissing k x y
-- @
--
-- prop> mapMaybeMissing f = traverseMaybeMissing (\k x -> pure (f k x))
--
-- but @mapMaybeMissing@ uses fewer unnecessary 'Applicative' operations.
mapMaybeMissing :: Applicative f => (Key -> x -> Maybe y) -> WhenMissing f x y
mapMaybeMissing f = WhenMissing
  { missingSubtree = \m -> pure $! mapMaybeWithKey f m
  , missingKey = \k x -> pure $! forceMaybe $! f k x }
{-# INLINE mapMaybeMissing #-}

-- | Map over the entries whose keys are missing from the other map.
--
-- @
-- mapMissing :: (k -> x -> y) -> SimpleWhenMissing k x y
-- @
--
-- prop> mapMissing f = mapMaybeMissing (\k x -> Just $ f k x)
--
-- but @mapMissing@ is somewhat faster.
mapMissing :: Applicative f => (Key -> x -> y) -> WhenMissing f x y
mapMissing f = WhenMissing
  { missingSubtree = \m -> pure $! mapWithKey f m
  , missingKey = \k x -> pure $! Just $! f k x }
{-# INLINE mapMissing #-}

-- | Traverse over the entries whose keys are missing from the other map,
-- optionally producing values to put in the result.
-- This is the most powerful 'WhenMissing' tactic, but others are usually
-- more efficient.
traverseMaybeMissing :: Applicative f
                     => (Key -> x -> f (Maybe y)) -> WhenMissing f x y
traverseMaybeMissing f = WhenMissing
  { missingSubtree = traverseMaybeWithKey f
  , missingKey = \k x -> forceMaybe <$> f k x }
{-# INLINE traverseMaybeMissing #-}

-- | Traverse over the entries whose keys are missing from the other map.
traverseMissing :: Applicative f
                     => (Key -> x -> f y) -> WhenMissing f x y
traverseMissing f = WhenMissing
  { missingSubtree = traverseWithKey f
  , missingKey = \k x -> (Just $!) <$> f k x }
{-# INLINE traverseMissing #-}

forceMaybe :: Maybe a -> Maybe a
forceMaybe Nothing = Nothing
forceMaybe m@(Just !_) = m
{-# INLINE forceMaybe #-}
