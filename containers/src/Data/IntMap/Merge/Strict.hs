{-# LANGUAGE CPP, BangPatterns #-}
#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Safe #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMap.Merge.Strict
-- Copyright   :  Documentation & Interface (c) wren romano 2016
--                Documentation & Implementation (c) Jonathan "gereeter" S. 2020
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This module defines an API for writing functions that merge two
-- maps. The key functions are 'merge' and 'mergeA'.
-- Each of these can be used with several different "merge tactics".
--
-- The 'merge' and 'mergeA' functions are shared by
-- the lazy and strict modules. Only the choice of merge tactics
-- determines strictness. If you use 'Data.IntMap.Merge.Strict.mapMissing'
-- from this module then the results will be forced before they are
-- inserted. If you use 'Data.IntMap.Merge.Lazy.mapMissing' from
-- "Data.Map.Merge.Lazy" then they will not.
--
-- == Efficiency note
--
-- The 'Control.Category.Category', 'Applicative', and 'Monad' instances for
-- 'WhenMissing' tactics are included because they are valid. However, they are
-- inefficient in many cases and should usually be avoided. The instances
-- for 'WhenMatched' tactics should not pose any major efficiency problems.

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
    , dropMissing
    , preserveMissing
    , mapMissing
    , mapMaybeMissing
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

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative(..), (<$>))
#endif
import Control.Applicative (liftA2, liftA3)

import Prelude hiding (min, max)

import Data.IntMap.Internal
import Data.IntMap.Merge.Internal

(#!), (#) :: (a -> b) -> a -> b
(#!) = ($!)
(#) = ($)

{-# INLINE fmapMaybe' #-}
fmapMaybe' :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe' _ Nothing = Nothing
fmapMaybe' f (Just x) = Just $! f x

-- | Map over the entries whose keys are missing from the other map.
--
-- @
-- mapMissing :: (Key -> a -> b) -> SimpleWhenMissing a b
-- @
--
-- prop> mapMissing f = mapMaybeMissing (\k x -> Just $ f k x)
--
-- but @mapMissing@ is somewhat faster.
{-# INLINE mapMissing #-}
mapMissing :: Applicative f => (Key -> a -> b) -> WhenMissing f a b
mapMissing f = mapMissingUKey (\k v -> f (box k) v)

-- | Map over the entries whose keys are missing from the other map with a
-- function that takes an unboxed key. Identical in functionality to
-- 'mapMissing'.
mapMissingUKey :: Applicative f => (UKey -> a -> b) -> WhenMissing f a b
mapMissingUKey g = WhenMissing (\k v -> pure (Just $! g k v)) (pure . go g) (pure . go g) (pure . start g) where
    start _ Empty = Empty
    start f (NonEmpty min minV root) = NonEmpty min #! f (boundUKey min) minV # go f root

    go :: (UKey -> a -> b) -> Node t a -> Node t b
    go _ Tip = Tip
    go f (Bin k v l r) = Bin k #! f (boundUKey k) v # go f l # go f r

-- | Map over the entries whose keys are missing from the other map,
-- optionally removing some. This is the most powerful 'SimpleWhenMissing'
-- tactic, but others are usually more efficient.
--
-- @
-- mapMaybeMissing :: (Key -> a -> Maybe b) -> SimpleWhenMissing a b
-- @
--
-- prop> mapMaybeMissing f = traverseMaybeMissing (\k x -> pure (f k x))
--
-- but @mapMaybeMissing@ uses fewer unnecessary 'Applicative' operations.
{-# INLINE mapMaybeMissing #-}
mapMaybeMissing :: Applicative f => (Key -> a -> Maybe b) -> WhenMissing f a b
mapMaybeMissing f = mapMaybeMissingUKey (\k a -> f (box k) a)

-- | Map over the entries whose keys are missing from the other map using a
-- function taking an unboxed key, optionally removing some. Identical in
-- functionality to 'mapMaybeMissing'.
mapMaybeMissingUKey :: Applicative f => (UKey -> a -> Maybe b) -> WhenMissing f a b
mapMaybeMissingUKey f = WhenMissing (\k v -> case f k v of
    Nothing -> pure Nothing
    Just !b -> pure (Just b)) (pure . goLKeep) (pure . goRKeep) (pure . start)
  where
    start Empty = Empty
    start (NonEmpty min minV root) = case f (boundUKey min) minV of
        Just !minV' -> NonEmpty min minV' (goLKeep root)
        Nothing -> goL root

    goLKeep Tip = Tip
    goLKeep (Bin max maxV l r) = case f (boundUKey max) maxV of
        Just !maxV' -> Bin max maxV' (goLKeep l) (goRKeep r)
        Nothing -> binNodeMapL (goLKeep l) (goR r)

    goRKeep Tip = Tip
    goRKeep (Bin min minV l r) = case f (boundUKey min) minV of
        Just !minV' -> Bin min minV' (goLKeep l) (goRKeep r)
        Nothing -> binMapNodeR (goL l) (goRKeep r)

    goL Tip = Empty
    goL (Bin max maxV l r) = case f (boundUKey max) maxV of
        Just !maxV' -> binL (goL l) (NonEmpty max maxV' (goRKeep r))
        Nothing -> binL (goL l) (goR r)

    goR Tip = Empty
    goR (Bin min minV l r) = case f (boundUKey min) minV of
        Just !minV' -> binR (NonEmpty min minV' (goLKeep l)) (goR r)
        Nothing -> binR (goL l) (goR r)

-- | When a key is found in both maps, apply a function to the
-- key and values and maybe use the result in the merged map.
--
-- @
-- zipWithMaybeMatched :: (Key -> a -> b -> Maybe c)
--                     -> SimpleWhenMatched a b c
-- @
{-# INLINE zipWithMaybeMatched #-}
zipWithMaybeMatched :: Applicative f => (Key -> a -> b -> Maybe c) -> WhenMatched f a b c
zipWithMaybeMatched f = WhenMatched (\k a b -> case f (box k) a b of
    Nothing -> pure Nothing
    Just !c -> pure (Just c))

-- | When a key is found in both maps, apply a function to the
-- key and values and use the result in the merged map.
--
-- @
-- zipWithMatched :: (Key -> a -> b -> c)
--                -> SimpleWhenMatched a b c
-- @
{-# INLINE zipWithMatched #-}
zipWithMatched :: Applicative f => (Key -> a -> b -> c) -> WhenMatched f a b c
zipWithMatched f = zipWithMaybeMatched (\k a b -> Just $! f k a b)

-- | When a key is found in both maps, apply a function to the key
-- and values, perform the resulting action, and maybe use the
-- result in the merged map.
--
-- This is the fundamental 'WhenMatched' tactic.
--
-- @since 0.5.9
{-# INLINE zipWithMaybeAMatched #-}
zipWithMaybeAMatched
  :: (Key -> a -> b -> f (Maybe c))
  -> WhenMatched f a b c
zipWithMaybeAMatched f = WhenMatched (\k a b -> f (box k) a b)

-- | When a key is found in both maps, apply a function to the key
-- and values to produce an action and use its result in the merged
-- map.
--
-- @since 0.5.9
{-# INLINE zipWithAMatched #-}
zipWithAMatched
  :: Applicative f
  => (Key -> a -> b -> f c)
  -> WhenMatched f a b c
zipWithAMatched f = zipWithMaybeAMatched (\k a b -> Just <$> f k a b)

-- | Traverse over the entries whose keys are missing from the other
-- map, optionally producing values to put in the result. This is
-- the most powerful 'WhenMissing' tactic, but others are usually
-- more efficient.
--
-- @since 0.5.9
{-# INLINE traverseMaybeMissing #-}
traverseMaybeMissing
  :: Applicative f => (Key -> a -> f (Maybe b)) -> WhenMissing f a b
traverseMaybeMissing f = WhenMissing
    { missingAllL = start
    , missingLeft = goL
    , missingRight = goR
    , missingSingle = f' }
  where
    f' k a = f (box k) a

    start Empty = pure Empty
    start (NonEmpty min minV root) = liftA2 (maybe nodeToMapL (NonEmpty min $!)) (f' (boundUKey min) minV) (goL root)

    goL Tip = pure Tip
    goL (Bin max maxV l r) = liftA3 (\l' r' maxV' -> maybe extractBinL (Bin max $!) maxV' l' r') (goL l) (goR r) (f' (boundUKey max) maxV)

    goR Tip = pure Tip
    goR (Bin min minV l r) = liftA3 (\minV' l' r' -> maybe extractBinR (Bin min $!) minV' l' r') (f' (boundUKey min) minV) (goL l) (goR r)

-- | Traverse over the entries whose keys are missing from the other
-- map.
--
-- @since 0.5.9
{-# INLINE traverseMissing #-}
traverseMissing
  :: Applicative f => (Key -> a -> f b) -> WhenMissing f a b
traverseMissing f = WhenMissing
    { missingAllL = start
    , missingLeft = goL
    , missingRight = goR
    , missingSingle = \k v -> Just <$> f' k v }
  where
    f' k a = f (box k) a

    start Empty = pure Empty
    start (NonEmpty min minV root) = liftA2 (NonEmpty min $!) (f' (boundUKey min) minV) (goL root)

    goL Tip = pure Tip
    goL (Bin max maxV l r) = liftA3 (\l' r' !maxV' -> Bin max maxV' l' r') (goL l) (goR r) (f' (boundUKey max) maxV)

    goR Tip = pure Tip
    goR (Bin min minV l r) = liftA3 (\ !minV' l' r' -> Bin min minV' l' r') (f' (boundUKey min) minV) (goL l) (goR r)

-- | Map covariantly over a @'WhenMissing' f x@.
--
-- @since 0.5.9
{-# INLINE mapWhenMissing #-}
mapWhenMissing :: Functor f => (a -> b) -> WhenMissing f x a -> WhenMissing f x b
mapWhenMissing f miss = WhenMissing
    (\k x -> fmap (fmapMaybe' f) (missingSingle miss k x))
    (\l -> fmap (mapNodeStrict f) (missingLeft miss l))
    (\r -> fmap (mapNodeStrict f) (missingRight miss r))
    (\m -> fmap (mapStrict_ f) (missingAllL miss m))

-- | Map covariantly over a @'WhenMatched' f x y@.
--
-- @since 0.5.9
{-# INLINE mapWhenMatched #-}
mapWhenMatched :: Functor f => (a -> b) -> WhenMatched f x y a -> WhenMatched f x y b
mapWhenMatched f match = WhenMatched (\k x y -> fmap (fmapMaybe' f) (matchedSingle match k x y))
