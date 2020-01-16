{-# LANGUAGE CPP, BangPatterns #-}

#include "containers.h"

#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
#if MIN_VERSION_base(4,8,0) || __GLASGOW_HASKELL__ < 708
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
#endif

{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMap.Merge.Internal
-- Copyright   :  (c) Jonathan "gereeter" S. 2020
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- This contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.
--
-- = Description
--
-- This defines the data structures and core (hidden) manipulations
-- on representations.
-----------------------------------------------------------------------------

module Data.IntMap.Merge.Internal (
    -- * Types and inspection of tactics
      WhenMissing(..)
    , WhenMatched(..)
    , SimpleWhenMissing
    , SimpleWhenMatched
#if !MIN_VERSION_base(4,8,0)
    , Identity(..)
#endif
    , runWhenMissing
    , runWhenMatched
    , runWhenMissingAll

    -- * General combining functions
    , merge
    , mergeA

    -- * @WhenMissing@ tactics
    , dropMissing
    , preserveMissing
    , filterMissing
    , filterAMissing
) where

import Prelude hiding (min, max)

import Data.IntMap.Internal

import Control.Applicative (liftA2, liftA3)
#if MIN_VERSION_base (4,8,0)
import Data.Functor.Identity (Identity, runIdentity)
#else
import Control.Applicative (Applicative(..), (<$>))

#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce
#endif

-- | The identity type.
newtype Identity a = Identity { runIdentity :: a }

#if __GLASGOW_HASKELL__ >= 708
instance Functor Identity where
  fmap = coerce
instance Applicative Identity where
  (<*>) = coerce
  pure = Identity
#else
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
  Identity f <*> Identity x = Identity (f x)
  pure = Identity
#endif
#endif


-- | A tactic for dealing with keys present in one map but not the other in
-- 'merge' or 'mergeA'.
--
-- A tactic of type @ WhenMissing f a c @ is an abstract representation
-- of a function of type @ Key -> a -> f (Maybe c) @.
data WhenMissing f a b = WhenMissing {
    -- TODO: Instead of unconditionally putting the 'Maybe' inside the @f@,
    -- provide options for the deletion vs. not choice to be decided purely,
    -- before Applicative effects. Most merge tactics (all but 'filterAMissing'
    -- and 'Data.IntMap.Merge.Lazy.traverseMaybeMissing', it seems) can support
    -- this more precise representation, and it would allow 'mergeA' to make an
    -- optimized choice between 'Node' and 'IntMap_' intermediates. By changing
    -- its arguments into that form, 'merge' might even be implementable as
    -- efficiently as possible in terms of 'mergeA'.
    missingSingle :: UKey -> a -> f (Maybe b),
    missingLeft :: Node L a -> f (Node L b),
    missingRight :: Node R a -> f (Node R b),
    missingAllL :: IntMap_ L a -> f (IntMap_ L b)
}

-- | A tactic for dealing with keys present in one map but not the other in
-- 'merge'.
--
-- A tactic of type @ SimpleWhenMissing a c @ is an abstract representation
-- of a function of type @ Key -> a -> Maybe c @.
type SimpleWhenMissing = WhenMissing Identity

-- | Along with 'traverseMaybeMissing', witnesses the isomorphism
-- between @WhenMissing f x y@ and @Key -> x -> f (Maybe y)@.
--
-- @since 0.5.9
{-# INLINE runWhenMissing #-}
runWhenMissing :: WhenMissing f a b -> Key -> a -> f (Maybe b)
runWhenMissing miss k = missingSingle miss (unbox k)

-- | Apply a missing tactic to an entire map.
--
-- prop> runWhenMissingAll miss m = merge miss dropMissing dropMatched m empty
{-# INLINE runWhenMissingAll #-}
runWhenMissingAll :: Applicative f => WhenMissing f a b -> IntMap a -> f (IntMap b)
runWhenMissingAll miss (IntMap m) = IntMap <$> missingAllL miss m

-- | Drop all the entries whose keys are missing from the other
-- map.
--
-- @
-- dropMissing :: SimpleWhenMissing a b
-- @
--
-- prop> dropMissing = mapMaybeMissing (\_ _ -> Nothing)
--
-- but @dropMissing@ is much faster.
{-# INLINE dropMissing #-}
dropMissing :: Applicative f => WhenMissing f a b
dropMissing = WhenMissing (\_ _ -> pure Nothing) (const (pure Tip)) (const (pure Tip)) (const (pure Empty))

-- | Preserve, unchanged, the entries whose keys are missing from
-- the other map.
--
-- @
-- preserveMissing :: SimpleWhenMissing a a
-- @
--
-- prop> preserveMissing = Merge.Lazy.mapMaybeMissing (\_ x -> Just x)
--
-- but @preserveMissing@ is much faster.
{-# INLINE preserveMissing #-}
preserveMissing :: Applicative f => WhenMissing f a a
preserveMissing = WhenMissing (\_ v -> pure (Just v)) pure pure pure

-- | Filter the entries whose keys are missing from the other map.
--
-- @
-- filterMissing :: (Key -> x -> Bool) -> SimpleWhenMissing a a
-- @
--
-- prop> filterMissing f = Merge.Lazy.mapMaybeMissing $ \k x -> guard (f k x) *> Just x
--
-- but this should be a little faster.
{-# INLINE filterMissing #-}
filterMissing :: Applicative f => (Key -> a -> Bool) -> WhenMissing f a a
filterMissing p = filterMissingUKey (\k a -> p (box k) a)

-- | Filter the entries whose keys are missing from the other map with a
-- predicate taking unboxed keys. Identical in functionality to
-- 'filterMissing'.
filterMissingUKey :: Applicative f => (UKey -> a -> Bool) -> WhenMissing f a a
filterMissingUKey p = WhenMissing (\k v -> pure (if p k v then Just v else Nothing)) (pure . goLKeep) (pure . goRKeep) (pure . start) where
    start Empty = Empty
    start (NonEmpty min minV root)
        | p (boundUKey min) minV = NonEmpty min minV (goLKeep root)
        | otherwise = goL root

    goLKeep Tip = Tip
    goLKeep (Bin max maxV l r)
        | p (boundUKey max) maxV = Bin max maxV (goLKeep l) (goRKeep r)
        | otherwise = case goR r of
            Empty -> goLKeep l
            NonEmpty max' maxV' r' -> Bin max' maxV' (goLKeep l) r'

    goRKeep Tip = Tip
    goRKeep (Bin min minV l r)
        | p (boundUKey min) minV = Bin min minV (goLKeep l) (goRKeep r)
        | otherwise = case goL l of
            Empty -> goRKeep r
            NonEmpty min' minV' l' -> Bin min' minV' l' (goRKeep r)

    goL Tip = Empty
    goL (Bin max maxV l r)
        | p (boundUKey max) maxV = case goL l of
            Empty -> case goRKeep r of
                Tip -> NonEmpty (maxToMin max) maxV Tip
                Bin minI minVI lI rI -> NonEmpty minI minVI (Bin max maxV lI rI)
            NonEmpty min minV l' -> NonEmpty min minV (Bin max maxV l' (goRKeep r))
        | otherwise = binL (goL l) (goR r)

    goR Tip = Empty
    goR (Bin min minV l r)
        | p (boundUKey min) minV = case goR r of
            Empty -> case goLKeep l of
                Tip -> NonEmpty (minToMax min) minV Tip
                Bin maxI maxVI lI rI -> NonEmpty maxI maxVI (Bin min minV lI rI)
            NonEmpty max maxV r' -> NonEmpty max maxV (Bin min minV (goLKeep l) r')
        | otherwise = binR (goL l) (goR r)

-- | Filter the entries whose keys are missing from the other map
-- using some 'Applicative' action.
--
-- > filterAMissing f = Merge.Lazy.traverseMaybeMissing $
-- >   \k x -> (\b -> guard b *> Just x) <$> f k x
--
-- but this should be a little faster.
--
-- @since 0.5.9
{-# INLINE filterAMissing #-}
-- TODO: Use pointer equality to speed this up?
filterAMissing :: Applicative f => (Key -> a -> f Bool) -> WhenMissing f a a
filterAMissing p = filterAMissingUKey (\k a -> p (box k) a)

-- | Filter the entries whose keys are missing from the other map using some
-- 'Applicative' action that takes an unboxed key. Identical in functionality
-- to 'filterAMissing'.
filterAMissingUKey :: Applicative f => (UKey -> a -> f Bool) -> WhenMissing f a a
filterAMissingUKey f = WhenMissing
    { missingAllL = start
    , missingLeft = goL
    , missingRight = goR
    , missingSingle = \k v -> fmap (\keep -> if keep then Just v else Nothing) (f k v) }
  where
    start Empty = pure Empty
    start (NonEmpty min minV root) = liftA2 (\keepV root' -> if keepV then NonEmpty min minV root' else nodeToMapL root') (f (boundUKey min) minV) (goL root)

    goL Tip = pure Tip
    goL (Bin max maxV l r) = liftA3 (\l' r' keepMax -> if keepMax then Bin max maxV l' r' else extractBinL l' r') (goL l) (goR r) (f (boundUKey max) maxV)

    goR Tip = pure Tip
    goR (Bin min minV l r) = liftA3 (\keepMin l' r' -> if keepMin then Bin min minV l' r' else extractBinR l' r') (f (boundUKey min) minV) (goL l) (goR r)

-- | A tactic for dealing with keys present in both
-- maps in 'merge' or 'mergeA'.
--
-- A tactic of type @ WhenMatched f a b c @ is an abstract representation
-- of a function of type @ Key -> a -> b -> f (Maybe c) @.
newtype WhenMatched f a b c = WhenMatched {
    matchedSingle :: UKey -> a -> b -> f (Maybe c)
}

-- | A tactic for dealing with keys present in both maps in 'merge'.
--
-- A tactic of type @ SimpleWhenMatched a b c @ is an abstract representation
-- of a function of type @ Key -> a -> b -> Maybe c @.
type SimpleWhenMatched = WhenMatched Identity

-- | Along with 'zipWithMaybeAMatched', witnesses the isomorphism
-- between @WhenMatched f x y z@ and @Key -> x -> y -> f (Maybe z)@.
--
-- @since 0.5.9
{-# INLINE runWhenMatched #-}
runWhenMatched :: WhenMatched f a b c -> Key -> a -> b -> f (Maybe c)
runWhenMatched match k = matchedSingle match (unbox k)

-- | Merge two maps.
--
-- @merge@ takes two 'WhenMissing' tactics, a 'WhenMatched'
-- tactic and two maps. It uses the tactics to merge the maps.
-- Its behavior is best understood via its fundamental tactics,
-- 'mapMaybeMissing' and 'zipWithMaybeMatched'.
--
-- Consider
--
-- @
-- merge (mapMaybeMissing g1)
--              (mapMaybeMissing g2)
--              (zipWithMaybeMatched f)
--              m1 m2
-- @
--
-- Take, for example,
--
-- @
-- m1 = [(0, 'a'), (1, 'b'), (3,'c'), (4, 'd')]
-- m2 = [(1, "one"), (2, "two"), (4, "three")]
-- @
--
-- @merge@ will first ''align'' these maps by key:
--
-- @
-- m1 = [(0, 'a'), (1, 'b'),               (3,'c'), (4, 'd')]
-- m2 =           [(1, "one"), (2, "two"),          (4, "three")]
-- @
--
-- It will then pass the individual entries and pairs of entries
-- to @g1@, @g2@, or @f@ as appropriate:
--
-- @
-- maybes = [g1 0 'a', f 1 'b' "one", g2 2 "two", g1 3 'c', f 4 'd' "three"]
-- @
--
-- This produces a 'Maybe' for each key:
--
-- @
-- keys =     0        1          2           3        4
-- results = [Nothing, Just True, Just False, Nothing, Just True]
-- @
--
-- Finally, the @Just@ results are collected into a map:
--
-- @
-- return value = [(1, True), (2, False), (4, True)]
-- @
--
-- The other tactics below are optimizations or simplifications of
-- 'mapMaybeMissing' for special cases. Most importantly,
--
-- * 'dropMissing' drops all the keys.
-- * 'preserveMissing' leaves all the entries alone.
--
-- When 'merge' is given three arguments, it is inlined at the call
-- site. To prevent excessive inlining, you should typically use 'merge'
-- to define your custom combining functions.
--
--
-- Examples:
--
-- prop> unionWithKey f = merge preserveMissing preserveMissing (zipWithMatched f)
-- prop> intersectionWithKey f = merge dropMissing dropMissing (zipWithMatched f)
-- prop> differenceWith f = merge preserveMissing dropMissing f
-- prop> symmetricDifference = merge preserveMissing preserveMissing (zipWithMaybeMatched (\_ _ _ -> Nothing))
-- prop> mapEachPiece f g h = merge (mapMissing f) (mapMissing g) (zipWithMatched h)
{-# INLINE merge #-}
-- TODO: Implementing 'merge' in terms of 'mergeA' leaves a lot to be desired
-- on the performance front. Because the choice of whether to delete a key is
-- hidden behind Applicative effects, 'mergeA' must conservatively return
-- 'Node's from every intermediate function, even when an 'IntMap_' would be
-- more efficient and appropriate. (It could return 'IntMap_'s everywhere
-- instead, but that would have the same problem in the opposite direction.)
-- 'merge' has no such limitation, since recursion schemes and decisions about
-- what types of structures to return can be made based on the results of pure
-- tactics.
merge :: SimpleWhenMissing a c -> SimpleWhenMissing b c -> SimpleWhenMatched a b c -> IntMap a -> IntMap b -> IntMap c
merge miss1 miss2 match = \m1 m2 -> runIdentity (mergeA miss1 miss2 match m1 m2)

-- | An applicative version of 'merge'. Due to the necessity of performing actions
-- in order, this can be significantly slower than 'merge'.
--
-- 'mergeA' takes two 'WhenMissing' tactics, a 'WhenMatched'
-- tactic and two maps. It uses the tactics to merge the maps.
-- Its behavior is best understood via its fundamental tactics,
-- 'traverseMaybeMissing' and 'zipWithMaybeAMatched'.
--
-- Consider
--
-- @
-- mergeA (traverseMaybeMissing g1)
--               (traverseMaybeMissing g2)
--               (zipWithMaybeAMatched f)
--               m1 m2
-- @
--
-- Take, for example,
--
-- @
-- m1 = [(0, \'a\'), (1, \'b\'), (3,\'c\'), (4, \'d\')]
-- m2 = [(1, "one"), (2, "two"), (4, "three")]
-- @
--
-- 'mergeA' will first \"align\" these maps by key:
--
-- @
-- m1 = [(0, \'a\'), (1, \'b\'),               (3, \'c\'), (4, \'d\')]
-- m2 =           [(1, "one"), (2, "two"),           (4, "three")]
-- @
--
-- It will then pass the individual entries and pairs of entries
-- to @g1@, @g2@, or @f@ as appropriate:
--
-- @
-- actions = [g1 0 \'a\', f 1 \'b\' "one", g2 2 "two", g1 3 \'c\', f 4 \'d\' "three"]
-- @
--
-- Next, it will perform the actions in the @actions@ list in order from
-- left to right.
--
-- @
-- keys =     0        1          2           3        4
-- results = [Nothing, Just True, Just False, Nothing, Just True]
-- @
--
-- Finally, the @Just@ results are collected into a map:
--
-- @
-- return value = [(1, True), (2, False), (4, True)]
-- @
--
-- The other tactics below are optimizations or simplifications of
-- 'traverseMaybeMissing' for special cases. Most importantly,
--
-- * 'dropMissing' drops all the keys.
-- * 'preserveMissing' leaves all the entries alone.
-- * 'mapMaybeMissing' does not use the 'Applicative' context.
--
-- When 'mergeA' is given three arguments, it is inlined at the call
-- site. To prevent excessive inlining, you should generally only use
-- 'mergeA' to define custom combining functions.
--
-- @since 0.5.9
mergeA :: Applicative f => WhenMissing f a c -> WhenMissing f b c -> WhenMatched f a b c -> IntMap a -> IntMap b -> f (IntMap c)
mergeA miss1 miss2 match = start where
    start (IntMap Empty) (IntMap Empty) = pure (IntMap Empty)
    start (IntMap Empty) (IntMap !m2) = IntMap <$> missingAllL miss2 m2
    start (IntMap !m1) (IntMap Empty) = IntMap <$> missingAllL miss1 m1
    start (IntMap (NonEmpty min1 minV1 root1)) (IntMap (NonEmpty min2 minV2 root2))
        | min1 < min2 = makeIntMapNE min1 (missingSingle miss1 (boundUKey min1) minV1) (goL2 minV2 min1 root1 min2 root2)
        | min2 < min1 = makeIntMapNE min2 (missingSingle miss2 (boundUKey min2) minV2) (goL1 minV1 min1 root1 min2 root2)
        | otherwise = makeIntMapNE min1 (matchedSingle match (boundUKey min1) minV1 minV2) (goLFused min1 root1 root2)

    goL1 minV1 !min1 !n1 !min2 Tip = mapToNodeL min2 <$> missingAllL miss1 (NonEmpty min1 minV1 n1)
    goL1 minV1 !min1 !n1 !min2 n2@(Bin max2 _ _ _) | boundsDisjoint min1 max2 = liftA2 (maybeUnionDisjointL min2) (missingLeft miss2 n2) (missingAllL miss1 (NonEmpty min1 minV1 n1))
    goL1 minV1 !min1 Tip !min2 !n2 = goInsertL1 (boundKey min1) minV1 (xor (boundKey min1) min2) min2 n2
    goL1 minV1 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT | xor (boundKey min1) min2 < xor (boundKey min1) max2 -> liftA2 binNodeMapL (goL1 minV1 min1 n1 min2 l2) (missingAllR miss2 (NonEmpty max2 maxV2 r2))
           | max1 > max2 -> makeBinL max1 (missingSingle miss1 (boundUKey max1) maxV1) (missingLeft miss2 l2) (goR2 maxV2 max1 (Bin min1 minV1 l1 r1) max2 r2)
           | max1 < max2 -> makeBinL max2 (missingSingle miss2 (boundUKey max2) maxV2) (missingLeft miss2 l2) (goR1 maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2)
           | otherwise -> makeBinL max1 (matchedSingle match (boundUKey max1) maxV1 maxV2) (missingLeft miss2 l2) (goRFused max1 (Bin min1 minV1 l1 r1) r2)
        EQ | max1 > max2 -> makeBinL max1 (missingSingle miss1 (boundUKey max1) maxV1) (goL1 minV1 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
           | max1 < max2 -> makeBinL max2 (missingSingle miss2 (boundUKey max2) maxV2) (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> makeBinL max1 (matchedSingle match (boundUKey max1) maxV1 maxV2) (goL1 minV1 min1 l1 min2 l2) (goRFused max1 r1 r2)
        GT -> liftA2 binNodeMapL (goL1 minV1 min1 l1 min2 n2) (missingAllR miss1 (NonEmpty max1 maxV1 r1))

    goL2 minV2 !min1 Tip !min2 !n2 = mapToNodeL min1 <$> missingAllL miss2 (NonEmpty min2 minV2 n2)
    goL2 minV2 !min1 n1@(Bin max1 _ _ _) !min2 !n2 | boundsDisjoint min2 max1 = liftA2 (maybeUnionDisjointL min1) (missingLeft miss1 n1) (missingAllL miss2 (NonEmpty min2 minV2 n2))
    goL2 minV2 !min1 !n1 !min2 Tip = goInsertL2 (boundKey min2) minV2 (xor (boundKey min2) min1) min1 n1
    goL2 minV2 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        GT | xor (boundKey min2) min1 < xor (boundKey min2) max1 -> liftA2 binNodeMapL (goL2 minV2 min1 l1 min2 n2) (missingAllR miss1 (NonEmpty max1 maxV1 r1))
           | max1 > max2 -> makeBinL max1 (missingSingle miss1 (boundUKey max1) maxV1) (missingLeft miss1 l1) (goR2 maxV2 max1 r1 max2 (Bin min2 minV2 l2 r2))
           | max1 < max2 -> makeBinL max2 (missingSingle miss2 (boundUKey max2) maxV2) (missingLeft miss1 l1) (goR1 maxV1 max1 r1 max2 (Bin min2 minV2 l2 r2))
           | otherwise -> makeBinL max1 (matchedSingle match (boundUKey max1) maxV1 maxV2) (missingLeft miss1 l1) (goRFused max1 r1 (Bin min2 minV2 l2 r2))
        EQ | max1 > max2 -> makeBinL max1 (missingSingle miss1 (boundUKey max1) maxV1) (goL2 minV2 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
           | max1 < max2 -> makeBinL max2 (missingSingle miss2 (boundUKey max2) maxV2) (goL2 minV2 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> makeBinL max1 (matchedSingle match (boundUKey max1) maxV1 maxV2) (goL2 minV2 min1 l1 min2 l2) (goRFused max1 r1 r2)
        LT -> liftA2 binNodeMapL (goL2 minV2 min1 n1 min2 l2) (missingAllR miss2 (NonEmpty max2 maxV2 r2))

    goLFused !_ Tip !n2 = missingLeft miss2 n2
    goLFused !_ !n1 Tip = missingLeft miss1 n1
    goLFused !min n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min max1) (xorBounds min max2) of
        LT -> liftA2 binNodeMapL (goLFused min n1 l2) (missingAllR miss2 (NonEmpty max2 maxV2 r2))
        EQ | max1 > max2 -> makeBinL max1 (missingSingle miss1 (boundUKey max1) maxV1) (goLFused min l1 l2) (goR2 maxV2 max1 r1 max2 r2)
           | max1 < max2 -> makeBinL max2 (missingSingle miss2 (boundUKey max2) maxV2) (goLFused min l1 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> makeBinL max1 (matchedSingle match (boundUKey max1) maxV1 maxV2) (goLFused min l1 l2) (goRFused max1 r1 r2)
        GT -> liftA2 binNodeMapL (goLFused min l1 n2) (missingAllR miss1 (NonEmpty max1 maxV1 r1))

    goR1 maxV1 !max1 !n1 !max2 Tip = mapToNodeR max2 <$> missingAllR miss1 (NonEmpty max1 maxV1 n1)
    goR1 maxV1 !max1 !n1 !max2 n2@(Bin min2 _ _ _) | boundsDisjoint min2 max1 = liftA2 (maybeUnionDisjointR max2) (missingAllR miss1 (NonEmpty max1 maxV1 n1)) (missingRight miss2 n2)
    goR1 maxV1 !max1 Tip !max2 !n2 = goInsertR1 (boundKey max1) maxV1 (xor (boundKey max1) max2) max2 n2
    goR1 maxV1 !max1 n1@(Bin min1 minV1 l1 r1) !max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT | xor (boundKey max1) min2 > xor (boundKey max1) max2 -> liftA2 binMapNodeR (missingAllL miss2 (NonEmpty min2 minV2 l2)) (goR1 maxV1 max1 n1 max2 r2)
           | min1 < min2 -> makeBinR min1 (missingSingle miss1 (boundUKey min1) minV1) (goL2 minV2 min1 (Bin max1 maxV1 l1 r1) min2 l2) (missingRight miss2 r2)
           | min1 > min2 -> makeBinR min2 (missingSingle miss2 (boundUKey min2) minV2) (goL1 minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2) (missingRight miss2 r2)
           | otherwise -> makeBinR min1 (matchedSingle match (boundUKey min1) minV1 minV2) (goLFused min1 (Bin max1 maxV1 l1 r1) l2) (missingRight miss2 r2)
        EQ | min1 < min2 -> makeBinR min1 (missingSingle miss1 (boundUKey min1) minV1) (goL2 minV2 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | min1 > min2 -> makeBinR min2 (missingSingle miss2 (boundUKey min2) minV2) (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> makeBinR min1 (matchedSingle match (boundUKey min1) minV1 minV2) (goLFused min1 l1 l2) (goR1 maxV1 max1 r1 max2 r2)
        GT -> liftA2 binMapNodeR (missingAllL miss1 (NonEmpty min1 minV1 l1)) (goR1 maxV1 max1 r1 max2 n2)

    goR2 maxV2 !max1 Tip !max2 !n2 = mapToNodeR max1 <$> missingAllR miss2 (NonEmpty max2 maxV2 n2)
    goR2 maxV2 !max1 n1@(Bin min1 _ _ _) !max2 !n2 | boundsDisjoint min1 max2 = liftA2 (maybeUnionDisjointR max1) (missingAllR miss2 (NonEmpty max2 maxV2 n2)) (missingRight miss1 n1)
    goR2 maxV2 !max1 !n1 !max2 Tip = goInsertR2 (boundKey max2) maxV2 (xor (boundKey max1) max2) max1 n1
    goR2 maxV2 !max1 n1@(Bin min1 minV1 l1 r1) !max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        GT | xor (boundKey max2) min1 > xor (boundKey max2) max1 -> liftA2 binMapNodeR (missingAllL miss1 (NonEmpty min1 minV1 l1)) (goR2 maxV2 max1 r1 max2 n2)
           | min1 < min2 -> makeBinR min1 (missingSingle miss1 (boundUKey min1) minV1) (goL2 minV2 min1 l1 min2 (Bin max2 maxV2 l2 r2)) (missingRight miss1 r1)
           | min1 > min2 -> makeBinR min2 (missingSingle miss2 (boundUKey min2) minV2) (goL1 minV1 min1 l1 min2 (Bin max2 maxV2 l2 r2)) (missingRight miss1 r1)
           | otherwise -> makeBinR min1 (matchedSingle match (boundUKey min1) minV1 minV2) (goLFused min1 l1 (Bin max2 maxV2 l2 r2)) (missingRight miss1 r1)
        EQ | min1 < min2 -> makeBinR min1 (missingSingle miss1 (boundUKey min1) minV1) (goL2 minV2 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
           | min1 > min2 -> makeBinR min2 (missingSingle miss2 (boundUKey min2) minV2) (goL1 minV1 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
           | otherwise -> makeBinR min1 (matchedSingle match (boundUKey min1) minV1 minV2) (goLFused min1 l1 l2) (goR2 maxV2 max1 r1 max2 r2)
        LT -> liftA2 binMapNodeR (missingAllL miss2 (NonEmpty min2 minV2 l2)) (goR2 maxV2 max1 n1 max2 r2)

    goRFused !_ Tip !n2 = missingRight miss2 n2
    goRFused !_ !n1 Tip = missingRight miss1 n1
    goRFused !max n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max) (xorBounds min2 max) of
        LT -> liftA2 binMapNodeR (missingAllL miss2 (NonEmpty min2 minV2 l2)) (goRFused max n1 r2)
        EQ | min1 < min2 -> makeBinR min1 (missingSingle miss1 (boundUKey min1) minV1) (goL2 minV2 min1 l1 min2 l2) (goRFused max r1 r2)
           | min1 > min2 -> makeBinR min2 (missingSingle miss2 (boundUKey min2) minV2) (goL1 minV1 min1 l1 min2 l2) (goRFused max r1 r2)
           | otherwise -> makeBinR min1 (matchedSingle match (boundUKey min1) minV1 minV2) (goLFused min1 l1 l2) (goRFused max r1 r2)
        GT -> liftA2 binMapNodeR (missingAllL miss1 (NonEmpty min1 minV1 l1)) (goRFused max r1 n2)

    goInsertL1 !k v !_ _ Tip = makeSingleton k (missingSingle miss1 (unbox k) v)
    goInsertL1 !k v !xorCache min n@(Bin max maxV l r) = case compareMaxBound k max of
        InBound | xorCache < xorCacheMax -> liftA2 binNodeMapL (goInsertL1 k v xorCache min l) (missingAllR miss2 (NonEmpty max maxV r))
                | otherwise -> makeBinL max (missingSingle miss2 (boundUKey max) maxV) (missingLeft miss2 l) (goInsertR1 k v xorCacheMax max r)
        OutOfBound -> addMaxL min (Bound k) (missingSingle miss1 (unbox k) v) (missingLeft miss2 n)
        Matched -> makeBinL max (matchedSingle match (unbox k) v maxV) (missingLeft miss2 l) (missingRight miss2 r)
      where xorCacheMax = xor k max

    goInsertL2 !k v !_ _ Tip = makeSingleton k (missingSingle miss2 (unbox k) v)
    goInsertL2 !k v !xorCache min n@(Bin max maxV l r) = case compareMaxBound k max of
        InBound | xorCache < xorCacheMax -> liftA2 binNodeMapL (goInsertL2 k v xorCache min l) (missingAllR miss1 (NonEmpty max maxV r))
                | otherwise -> makeBinL max (missingSingle miss1 (boundUKey max) maxV) (missingLeft miss1 l) (goInsertR2 k v xorCacheMax max r)
        OutOfBound -> addMaxL min (Bound k) (missingSingle miss2 (unbox k) v) (missingLeft miss1 n)
        Matched -> makeBinL max (matchedSingle match (unbox k) maxV v) (missingLeft miss1 l) (missingRight miss1 r)
      where xorCacheMax = xor k max

    goInsertR1 !k v !_ _ Tip = makeSingleton k (missingSingle miss1 (unbox k) v)
    goInsertR1 !k v !xorCache max n@(Bin min minV l r) = case compareMinBound k min of
        InBound | xorCache < xorCacheMin -> liftA2 binMapNodeR (missingAllL miss2 (NonEmpty min minV l)) (goInsertR1 k v xorCache max r)
                | otherwise -> makeBinR min (missingSingle miss2 (boundUKey min) minV) (goInsertL1 k v xorCacheMin min l) (missingRight miss2 r)
        OutOfBound -> addMinR max (Bound k) (missingSingle miss1 (unbox k) v) (missingRight miss2 n)
        Matched -> makeBinR min (matchedSingle match (unbox k) v minV) (missingLeft miss2 l) (missingRight miss2 r)
      where xorCacheMin = xor k min

    goInsertR2 !k v !_ _ Tip = makeSingleton k (missingSingle miss2 (unbox k) v)
    goInsertR2 !k v !xorCache max n@(Bin min minV l r) = case compareMinBound k min of
        InBound | xorCache < xorCacheMin -> liftA2 binMapNodeR (missingAllL miss1 (NonEmpty min minV l)) (goInsertR2 k v xorCache max r)
                | otherwise -> makeBinR min (missingSingle miss1 (boundUKey min) minV) (goInsertL2 k v xorCacheMin min l) (missingRight miss1 r)
        OutOfBound -> addMinR max (Bound k) (missingSingle miss2 (unbox k) v) (missingRight miss1 n)
        Matched -> makeBinR min (matchedSingle match (unbox k) minV v) (missingLeft miss1 l) (missingRight miss1 r)
      where xorCacheMin = xor k min

    missingAllR whenMiss = fmap l2rMap . missingAllL whenMiss . r2lMap

{-# INLINE makeSingleton #-}
makeSingleton :: Functor f => Key -> f (Maybe v) -> f (Node d v)
makeSingleton !k v = make <$> v where
    make Nothing = Tip
    make (Just v') = Bin (Bound k) v' Tip Tip

{-# INLINE makeBinL #-}
makeBinL :: Applicative f => Bound R -> f (Maybe v) -> f (Node L v) -> f (Node R v) -> f (Node L v)
makeBinL !max maxV l r = liftA3 make l r maxV where
    make l' r' maxV' = maybe extractBinL (Bin max) maxV' l' r'

{-# INLINE makeBinR #-}
makeBinR :: Applicative f => Bound L -> f (Maybe v) -> f (Node L v) -> f (Node R v) -> f (Node R v)
makeBinR !min minV l r = liftA3 make minV l r where
    make minV' l' r' = maybe extractBinR (Bin min) minV' l' r'

{-# INLINE makeIntMapNE #-}
makeIntMapNE :: Applicative f => Bound L -> f (Maybe v) -> f (Node L v) -> f (IntMap v)
makeIntMapNE !min minV root = liftA2 make minV root where
    make Nothing root' = IntMap (nodeToMapL root')
    make (Just minV') root' = IntMap (NonEmpty min minV' root')

mapToNodeL :: Bound L -> IntMap_ L v -> Node L v
mapToNodeL !_ Empty = Tip
mapToNodeL !externalMin (NonEmpty min minV root) = insertMinL (xor (boundKey min) externalMin) min minV root

mapToNodeR :: Bound R -> IntMap_ R v -> Node R v
mapToNodeR !_ Empty = Tip
mapToNodeR !externalMax (NonEmpty max maxV root) = insertMaxR (xor (boundKey max) externalMax) max maxV root

binNodeMapL :: Node L v -> IntMap_ R v -> Node L v
binNodeMapL l Empty = l
binNodeMapL l (NonEmpty max maxV r) = Bin max maxV l r

binMapNodeR :: IntMap_ L v -> Node R v -> Node R v
binMapNodeR Empty r = r
binMapNodeR (NonEmpty min minV l) r = Bin min minV l r

{-# INLINE addMaxL #-}
addMaxL :: Applicative f => Bound L -> Bound R -> f (Maybe v) -> f (Node L v) -> f (Node L v)
addMaxL !min !k v n = liftA2 add n v where
    add n' Nothing = n'
    add Tip (Just v') = Bin k v' Tip Tip
    add n'@(Bin max maxV l r) (Just v')
        | xor (boundKey max) min < xorCacheMax = Bin k v' n' Tip
        | otherwise = Bin k v' l (insertMaxR xorCacheMax max maxV r)
      where
        xorCacheMax = xor (boundKey max) k

{-# INLINE addMinR #-}
addMinR :: Applicative f => Bound R -> Bound L -> f (Maybe v) -> f (Node R v) -> f (Node R v)
addMinR !max !k v n = liftA2 add v n where
    add Nothing n' = n'
    add (Just v') Tip = Bin k v' Tip Tip
    add (Just v') n'@(Bin min minV l r)
        | xor (boundKey min) max < xorCacheMin = Bin k v' Tip n'
        | otherwise = Bin k v' (insertMinL xorCacheMin min minV l) r
      where
        xorCacheMin = xor (boundKey min) k

maybeUnionDisjointL :: Bound L -> Node L v -> IntMap_ L v -> Node L v
maybeUnionDisjointL !min1 Tip !m2 = mapToNodeL min1 m2
maybeUnionDisjointL !_ !n1 Empty = n1
maybeUnionDisjointL !min1 !n1 (NonEmpty min2 minV2 root2) = unionDisjointL minV2 min1 n1 min2 root2

maybeUnionDisjointR :: Bound R -> IntMap_ R v -> Node R v -> Node R v
maybeUnionDisjointR !max2 !m1 Tip = mapToNodeR max2 m1
maybeUnionDisjointR !_ Empty !n2 = n2
maybeUnionDisjointR !max2 (NonEmpty max1 maxV1 root1) !n2 = unionDisjointR maxV1 max1 root1 max2 n2
