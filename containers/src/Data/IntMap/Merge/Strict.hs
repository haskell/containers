{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMap.Merge.Strict
-- Copyright   :  (c) Jonathan S. 2016
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
-- The 'Category', 'Applicative', and 'Monad' instances for 'WhenMissing'
-- tactics are included because they are valid. However, they are
-- inefficient in many cases and should usually be avoided. The instances
-- for 'WhenMatched' tactics should not pose any major efficiency problems.

module Data.IntMap.Merge.Strict (
    -- ** Simple merge tactic types
      WhenMissing
    , WhenMatched

    -- ** General combining function
    , merge

    -- ** @WhenMatched@ tactics
    , zipWithMaybeMatched
    , zipWithMatched

    -- *** @WhenMissing@ tactics
    , dropMissing
    , preserveMissing
    , mapMissing
    , mapMaybeMissing
    , filterMissing
) where

import Prelude hiding (min, max)

import Data.IntMap.Internal
import Data.IntMap.Merge.Internal

(#!), (#) :: (a -> b) -> a -> b
(#!) = ($!)
(#) = ($)

-- | Map over the entries whose keys are missing from the other map.
--
-- @
-- mapMissing :: (Key -> a -> b) -> SimpleWhenMissing a b
-- @
--
-- prop> mapMissing f = mapMaybeMissing (\k x -> Just $ f k x)
--
-- but @mapMissing@ is somewhat faster.
mapMissing :: forall f a b. Applicative f => (Key -> a -> b) -> WhenMissing f a b
mapMissing f = WhenMissing (\k v -> Just $! f k v) go go (pure . start) where
    start (IntMap Empty) = IntMap Empty
    start (IntMap (NonEmpty min minV root)) = IntMap (NonEmpty min #! f min minV # go root)

    go :: Node t a -> Node t b
    go Tip = Tip
    go (Bin k v l r) = Bin k #! f k v # go l # go r

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
mapMaybeMissing :: Applicative f => (Key -> a -> Maybe b) -> WhenMissing f a b
mapMaybeMissing f = WhenMissing f goLKeep goRKeep (pure . start) where
    start (IntMap Empty) = IntMap Empty
    start (IntMap (NonEmpty min minV root)) = case f min minV of
        Just !minV' -> IntMap (NonEmpty min minV' (goLKeep root))
        Nothing -> IntMap (goL root)

    goLKeep Tip = Tip
    goLKeep (Bin max maxV l r) = case f max maxV of
        Just !maxV' -> Bin max maxV' (goLKeep l) (goRKeep r)
        Nothing -> case goR r of
            Empty -> goLKeep l
            NonEmpty max' maxV' r' -> Bin max' maxV' (goLKeep l) r'

    goRKeep Tip = Tip
    goRKeep (Bin min minV l r) = case f min minV of
        Just !minV' -> Bin min minV' (goLKeep l) (goRKeep r)
        Nothing -> case goL l of
            Empty -> goRKeep r
            NonEmpty min' minV' l' -> Bin min' minV' l' (goRKeep r)

    goL Tip = Empty
    goL (Bin max maxV l r) = case f max maxV of
        Just !maxV' -> case goL l of
            Empty -> case goRKeep r of
                Tip -> NonEmpty max maxV' Tip
                Bin minI minVI lI rI -> NonEmpty minI minVI (Bin max maxV' lI rI)
            NonEmpty min minV l' -> NonEmpty min minV (Bin max maxV' l' (goRKeep r))
        Nothing -> binL (goL l) (goR r)

    goR Tip = Empty
    goR (Bin min minV l r) = case f min minV of
        Just !minV' -> case goR r of
            Empty -> case goLKeep l of
                Tip -> NonEmpty min minV' Tip
                Bin maxI maxVI lI rI -> NonEmpty maxI maxVI (Bin min minV' lI rI)
            NonEmpty max maxV r' -> NonEmpty max maxV (Bin min minV' (goLKeep l) r')
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
zipWithMaybeMatched f = WhenMatched (\k a b -> case f k a b of
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
