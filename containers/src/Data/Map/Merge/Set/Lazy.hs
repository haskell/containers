-- |
-- This module defines an API for writing functions that merge a map and a set
-- into a map. The key functions are 'merge' and 'mergeA'.
-- Each of these can be used with several different \"merge tactics\".
--
-- The 'merge' and 'mergeA' functions are shared by the lazy and strict
-- modules. Only the choice of merge tactics determines strictness. If you
-- use 'Data.Map.Merge.Set.Strict.mapMissing' from "Data.Map.Merge.Set.Strict"
-- then the results will be forced before they are inserted. If you use
-- 'Data.Map.Merge.Set.Lazy.mapMissing' from this module then they will not.
--
-- @since FIXME
--
module Data.Map.Merge.Set.Lazy
  (
  -- ** Simple merge tactic types
    M.SimpleWhenMissing
  , Internal.SimpleWhenMatched
  , Internal.SimpleWhenMissingSet

  -- ** General combining function
  , Internal.merge

  -- *** @WhenMatched@ tactics
  , Internal.filterMatched
  , mapMatched
  , mapMaybeMatched

  -- *** @WhenMissing@ tactics
  , M.dropMissing
  , M.preserveMissing
  , M.mapMissing
  , M.filterMissing
  , M.mapMaybeMissing

  -- *** @WhenMissingSet@ tactics
  , Internal.dropMissingSet
  , generateMissingSet

  -- ** Applicative merge tactic types
  , M.WhenMissing
  , Internal.WhenMissingSet
  , Internal.WhenMatched

  -- ** General combining function
  , Internal.mergeA

  -- *** @WhenMatched@ tactics
  , Internal.filterAMatched
  , traverseMatched
  , traverseMaybeMatched

  -- *** @WhenMissing@ tactics
  , M.filterAMissing
  , M.traverseMissing
  , M.traverseMaybeMissing

  -- *** @WhenMissingSet@ tactics
  , generateAMissingSet

  -- ** Miscellaneous
  , Internal.runWhenMatched
  , Internal.runWhenMissingSet
  ) where

import qualified Data.Map.Internal as M
import qualified Data.Map.Merge.Set.Internal as Internal
import Data.Map.Merge.Set.Internal (WhenMatched(..), WhenMissingSet(..))

-- | When a key is found in both the map and the set, apply a function to the
-- key and the value in the map and use the result as the value for the merged
-- map.
--
-- @since FIXME
mapMatched :: Applicative f => (k -> a -> b) -> WhenMatched f k a b
mapMatched f = WhenMatched (\k x -> pure (Just (f k x)))
{-# INLINE mapMatched #-}

-- | When a key is found in both the map and the set, apply a function to the
-- key and the value in the map and maybe use the result as the value for the
-- merged map.
--
-- @since FIXME
mapMaybeMatched :: Applicative f => (k -> a -> Maybe b) -> WhenMatched f k a b
mapMaybeMatched f = WhenMatched (\k x -> pure (f k x))
{-# INLINE mapMaybeMatched #-}

-- | When a key is found in both the map and the set, apply a function to the
-- key and the value in the map, and use the result of the action as the value
-- for the merged map.
--
-- @since FIXME
traverseMatched :: Functor f => (k -> a -> f b) -> WhenMatched f k a b
traverseMatched f = WhenMatched (\k x -> Just <$> f k x)
{-# INLINE traverseMatched #-}

-- | When a key is found in both the map and the set, apply a function to the
-- key and the value in the map, and maybe use the result of the action as the
-- value for the merged map.
--
-- @since FIXME
traverseMaybeMatched :: (k -> a -> f (Maybe b)) -> WhenMatched f k a b
traverseMaybeMatched = WhenMatched

-- | For keys that are present in the set but missing from the map, apply a
-- function and use the result as the value for the merged map.
--
-- @since FIXME
generateMissingSet :: Applicative f => (k -> a) -> WhenMissingSet f k a
generateMissingSet f = WhenMissingSet
  { missingSubtree = pure . M.fromSet f
  , missingKey = pure . Just . f
  }
{-# INLINE generateMissingSet #-}

-- | For keys that are present in the set but missing from the map, apply a
-- function, and use the result of the action as the value for the merged map.
--
-- @since FIXME
generateAMissingSet :: Applicative f => (k -> f a) -> WhenMissingSet f k a
generateAMissingSet f = WhenMissingSet
  { missingSubtree = M.fromSetA f
  , missingKey = fmap Just . f
  }
{-# INLINE generateAMissingSet #-}
