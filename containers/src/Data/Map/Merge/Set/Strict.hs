{-# LANGUAGE BangPatterns #-}

-- |
-- This module defines an API for writing functions that merge a map and a
-- set into a map. The key functions are 'Data.Map.Merge.Set.Strict.merge' and
-- 'Data.Map.Merge.Set.Strict.mergeA'. Each of these can be used with several
-- different \"merge tactics\".
--
-- The @merge@ and @mergeA@ functions are shared by the lazy and strict
-- modules. Only the choice of merge tactics determines strictness.
-- If you use 'Data.Map.Merge.Set.Strict.mapMissing' from this module
-- then the results will be forced before they are inserted. If you use
-- 'Data.Map.Merge.Set.Lazy.mapMissing' from "Data.Map.Merge.Set.Lazy" then they
-- will not.
--
-- @since FIXME
--
module Data.Map.Merge.Set.Strict
  (
  -- ** Simple merge tactic types
    MS.SimpleWhenMissing
  , Internal.SimpleWhenMissingSet
  , Internal.SimpleWhenMatched

  -- ** General combining function
  , Internal.merge

  -- *** @WhenMatched@ tactics
  , Internal.dropMatched
  , Internal.filterMatched
  , mapMatched
  , mapMaybeMatched

  -- *** @WhenMissing@ tactics
  , MS.dropMissing
  , MS.preserveMissing
  , MS.mapMissing
  , MS.filterMissing
  , MS.mapMaybeMissing

  -- *** @WhenMissingSet@ tactics
  , Internal.dropMissingSet
  , generateMissingSet
  , generateMaybeMissingSet

  -- ** Applicative merge tactic types
  , MS.WhenMissing
  , Internal.WhenMissingSet
  , Internal.WhenMatched

  -- ** General combining function
  , Internal.mergeA

  -- *** @WhenMatched@ tactics
  , Internal.filterAMatched
  , traverseMatched
  , traverseMaybeMatched

  -- *** @WhenMissing@ tactics
  , MS.filterAMissing
  , MS.traverseMissing
  , MS.traverseMaybeMissing

  -- *** @WhenMissingSet@ tactics
  , generateAMissingSet
  , generateMaybeAMissingSet

  -- ** Miscellaneous
  , Internal.runWhenMatched
  , Internal.runWhenMissingSet
  ) where

import qualified Data.Map.Strict.Internal as MS
import qualified Data.Map.Merge.Set.Internal as Internal
import Data.Map.Merge.Set.Internal (WhenMatched(..), WhenMissingSet(..))

-- | When the key is found in both the map and the set, apply a function to the
-- key and the value in the map and use the result as the value for the result
-- map.
--
-- @since FIXME
mapMatched :: Applicative f => (k -> a -> b) -> WhenMatched f k a b
mapMatched f = WhenMatched (\k x -> pure (Just $! f k x))
{-# INLINE mapMatched #-}

-- | When a key is found in both the map and the set, apply a function to the
-- key and the value in the map and maybe use the result as the value for the
-- merged map.
--
-- @since FIXME
mapMaybeMatched :: Applicative f => (k -> a -> Maybe b) -> WhenMatched f k a b
mapMaybeMatched f = WhenMatched (\k x -> pure (forceMaybe (f k x)))
{-# INLINE mapMaybeMatched #-}

-- | When a key is found in both the map and the set, apply a function to the
-- key and the value in the map, and use the result of the action as the value
-- for the merged map.
--
-- @since FIXME
traverseMatched :: Functor f => (k -> a -> f b) -> WhenMatched f k a b
traverseMatched f = WhenMatched (\k x -> (Just $!) <$> f k x)
{-# INLINE traverseMatched #-}

-- | When a key is found in both the map and the set, apply a function to the
-- key and the value in the map, and maybe use the result of the action as the
-- value for the merged map.
--
-- @since FIXME
traverseMaybeMatched
  :: Functor f => (k -> a -> f (Maybe b)) -> WhenMatched f k a b
traverseMaybeMatched f = WhenMatched (\k x -> forceMaybe <$> f k x)
{-# INLINE traverseMaybeMatched #-}

-- | For keys that are present in the set but missing from the map, apply a
-- function and use the result as the value for the merge map.
--
-- @since FIXME
generateMissingSet :: Applicative f => (k -> a) -> WhenMissingSet f k a
generateMissingSet f = WhenMissingSet
  { missingSubtree = \s -> pure (MS.fromSet f s)
  , missingKey = \k -> pure (Just $! f k)
  }
{-# INLINE generateMissingSet #-}

-- | For keys that are present in the set but missing from the map, apply a
-- function, and use the result of the action as the value for the merged map.
--
-- @since FIXME
generateAMissingSet :: Applicative f => (k -> f a) -> WhenMissingSet f k a
generateAMissingSet f = WhenMissingSet
  { missingSubtree = MS.fromSetA f
  , missingKey = \k -> (Just $!) <$> f k
  }
{-# INLINE generateAMissingSet #-}

-- | For keys that are present in the set but missing from the map, apply a
-- function and maybe use the result as the value for the merged map.
--
-- @since FIXME
generateMaybeMissingSet
  :: Applicative f => (k -> Maybe a) -> WhenMissingSet f k a
generateMaybeMissingSet f = WhenMissingSet
  { missingSubtree = \s -> pure (MS.fromSetMaybe f s)
  , missingKey = \k -> pure (f k)
  }
{-# INLINE generateMaybeMissingSet #-}

-- | For keys that are present in the set but missing from the map, apply a
-- function, and maybe use the result of the action as the value for the merged
-- map.
--
-- @since FIXME
generateMaybeAMissingSet
  :: Applicative f => (k -> f (Maybe a)) -> WhenMissingSet f k a
generateMaybeAMissingSet f = WhenMissingSet
  { missingSubtree = MS.fromSetMaybeA f
  , missingKey = \k -> forceMaybe <$> f k
  }
{-# INLINE generateMaybeAMissingSet #-}

forceMaybe :: Maybe a -> Maybe a
forceMaybe Nothing = Nothing
forceMaybe m@(Just !_) = m
