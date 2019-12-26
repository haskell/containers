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
-- Copyright   :  (c) Jonathan S. 2016
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

module Data.IntMap.Merge.Internal where

import Control.Applicative (Applicative(..))
import Prelude hiding (min, max)

import Data.IntMap.Internal

#if MIN_VERSION_base (4,8,0)
import Data.Functor.Identity (Identity, runIdentity)
#elif __GLASGOW_HASKELL__ >= 708
import Data.Coerce
#endif


#if !MIN_VERSION_base (4,8,0)
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
    missingSingle :: Key -> a -> f (Maybe b),
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
runWhenMissing = missingSingle

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
dropMissing = WhenMissing (const (const (pure Nothing))) (const (pure Tip)) (const (pure Tip)) (const (pure Empty))

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
filterMissing :: Applicative f => (Key -> a -> Bool) -> WhenMissing f a a
filterMissing p = WhenMissing (\k v -> pure (if p k v then Just v else Nothing)) (pure . goLKeep) (pure . goRKeep) (pure . start) where
    start Empty = Empty
    start (NonEmpty min minV root)
        | p (boundKey min) minV = NonEmpty min minV (goLKeep root)
        | otherwise = goL root

    goLKeep Tip = Tip
    goLKeep (Bin max maxV l r)
        | p (boundKey max) maxV = Bin max maxV (goLKeep l) (goRKeep r)
        | otherwise = case goR r of
            Empty -> goLKeep l
            NonEmpty max' maxV' r' -> Bin max' maxV' (goLKeep l) r'

    goRKeep Tip = Tip
    goRKeep (Bin min minV l r)
        | p (boundKey min) minV = Bin min minV (goLKeep l) (goRKeep r)
        | otherwise = case goL l of
            Empty -> goRKeep r
            NonEmpty min' minV' l' -> Bin min' minV' l' (goRKeep r)

    goL Tip = Empty
    goL (Bin max maxV l r)
        | p (boundKey max) maxV = case goL l of
            Empty -> case goRKeep r of
                Tip -> NonEmpty (maxToMin max) maxV Tip
                Bin minI minVI lI rI -> NonEmpty minI minVI (Bin max maxV lI rI)
            NonEmpty min minV l' -> NonEmpty min minV (Bin max maxV l' (goRKeep r))
        | otherwise = binL (goL l) (goR r)

    goR Tip = Empty
    goR (Bin min minV l r)
        | p (boundKey min) minV = case goR r of
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
filterAMissing  :: Applicative f => (Key -> a -> f Bool) -> WhenMissing f a a
filterAMissing f = WhenMissing
    { missingAllL = start
    , missingLeft = goL
    , missingRight = goR
    , missingSingle = \k v -> fmap (\keep -> if keep then Just v else Nothing) (f k v) }
  where
    start Empty = pure Empty
    start (NonEmpty min minV root) = (\keepV root' -> if keepV then NonEmpty min minV root' else nodeToMapL root') <$> f (boundKey min) minV <*> goL root

    goL Tip = pure Tip
    goL (Bin max maxV l r) = (\l' r' keepMax -> if keepMax then Bin max maxV l' r' else extractBinL l' r') <$> goL l <*> goR r <*> f (boundKey max) maxV

    goR Tip = pure Tip
    goR (Bin min minV l r) = (\keepMin l' r' -> if keepMin then Bin min minV l' r' else extractBinR l' r') <$> f (boundKey min) minV <*> goL l <*> goR r

-- | A tactic for dealing with keys present in both
-- maps in 'merge' or 'mergeA'.
--
-- A tactic of type @ WhenMatched f a b c @ is an abstract representation
-- of a function of type @ Key -> a -> b -> f (Maybe c) @.
newtype WhenMatched f a b c = WhenMatched {
    matchedSingle :: Key -> a -> b -> f (Maybe c)
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
runWhenMatched = matchedSingle

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
merge :: SimpleWhenMissing a c -> SimpleWhenMissing b c -> SimpleWhenMatched a b c -> IntMap a -> IntMap b -> IntMap c
merge miss1 miss2 match = \m1 m2 -> runIdentity (mergeA miss1 miss2 match m1 m2)
{- FIXME: The following is significantly faster, but incorrect.
merge miss1 miss2 match = start where
    start (IntMap Empty) (IntMap Empty) = IntMap Empty
    start (IntMap Empty) (IntMap !m2) = IntMap (missAllL miss2 m2)
    start (IntMap !m1) (IntMap Empty) = IntMap (missAllL miss1 m1)
    start (IntMap (NonEmpty min1 minV1 root1)) (IntMap (NonEmpty min2 minV2 root2))
        | min1 < min2 = case missSingle miss1 (boundKey min1) minV1 of
            Nothing -> IntMap (goL2 minV2 min1 root1 min2 root2)
            Just minV' -> IntMap (NonEmpty min1 minV' (goL2Keep minV2 min1 root1 min2 root2))
        | min1 > min2 = case missSingle miss2 (boundKey min2) minV2 of
            Nothing -> IntMap (goL1 minV1 min1 root1 min2 root2)
            Just minV' -> IntMap (NonEmpty min2 minV' (goL1Keep minV1 min1 root1 min2 root2))
        | otherwise = case matchSingle match (boundKey min1) minV1 minV2 of
            Nothing -> IntMap (goLFused min1 root1 root2)
            Just minV' -> IntMap (NonEmpty min1 minV' (goLFusedKeep min1 root1 root2))

    -- The merge code is structured as 12 very repetitive methods that merge nodes and a value associated with
    -- the bound of one of those nodes. These vary on 3 axes:
    --
    -- * The functions ending in L take and produce left nodes/maps, while those ending in R take and produce right nodes/maps
    -- * The functions ending with a 1 have the first argument "inside" the second, i.e., the L1 functions assume that min1 > min2
    --   and take minV1 and the R1 functions assume that max1 < max2 and take maxV1. The functions ending with a 2 are symmetrical,
    --   and the functions ending with Fused assume that the two maps are aligned: LFused assumes that min1 = min2 and RFused assumes that max1 = max2. 
    -- * The functions ending in Keep produce a Node, while the functions without Keep produce an IntMap_
    --
    -- See goL1Keep and goLFusedKeep for detailed description of the merging process.


    -- | Merge two left nodes and a minimum value for the first node into a new left node
    -- Precondition: min1 > min2
    -- goL1Keep :: a -> Key -> Node a -> Key -> Node b -> Node c

    -- We special case merging two empty nodes because the last time I checked it was faster than falling through to the next case
    goL1Keep minV1 !min1 Tip !_ Tip = case missSingle miss1 (boundKey min1) minV1 of
        Nothing -> Tip
        Just minV' -> Bin (minToMax min1) minV' Tip Tip

    -- If the second node is empty, then we basically need a copy of the first node. However, the presence of minV1 complicates things,
    -- so we need to insert it
    goL1Keep minV1 !min1 n1 !min2 Tip = case missSingle miss1 (boundKey min1) minV1 of
        Nothing -> missLeft miss1 n1
        Just minV' -> insertMinL (xor (boundKey min1) min2) min1 minV' (missLeft miss1 n1)

    -- We handle the case of nodes that cover disjoint ranges separately. The property of being disjoint, unlike a lot of things, remains
    -- constant as we recurse into subnodes, and this representation is particularly good at efficiently detecting it. By assumption,
    -- min1 > min2, so we don't need to handle the case of min2 > max1.
    goL1Keep minV1 !min1 !n1 !min2 n2@(Bin max2 _ _ _) | boundsDisjoint min1 max2 = case missAllL miss1 (NonEmpty min1 minV1 n1) of
        Empty -> missLeft miss2 n2
        NonEmpty min1' minV1' n1' -> case missLeft miss2 n2 of
            Tip -> insertMinL (xor (boundKey min1') min2) min1' minV1' n1'
            n2'@(Bin _ _ _ _) -> unionDisjointL minV1' min2 n2' min1' n1'

    -- If the first node is empty, we still need to insert minV1
    goL1Keep minV1 !min1 Tip !min2 n2 = goInsertL1 (boundKey min1) minV1 (xor (boundKey min1) min2) min2 n2

    -- This is the meat of the method. Since we already know that the two nodes cover overlapping ranges, there are three possibilities:
    -- * Node 2 splits first, so we need to merge n1 with either l2 or r2
    -- * Both nodes split at the same time, so we need to merge l1 with l2 and r1 with r2
    -- * Node 1 splits first, so we need to merge n2 with either l1 or r1
    goL1Keep minV1 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        -- Node 2 splits first. Knowing that min1 < min2 doesn't really help here, so our first job is to determine if we need
        -- to merge n1 with l2 or with r2. We do this with the same navigational test used in, e.g., lookup, using an arbirary key
        -- from node 1 (in this case we chose min1). If that key would be on the left side of node 2, then (since node 1 covers a smaller
        -- binary range) the whole node 1 must fit in on the left side of node 2.
        --
        -- In the specific case of merging n1 with l2, we don't have to do any more comparisons: we already know that min1 > min2,
        -- so we should be calling an L1 function
        LT | xor (boundKey min1) min2 < xor (boundKey min1) max2 -> binL2 max2 maxV2 (goL1Keep minV1 min1 n1 min2 l2) (missRight miss2 r2)
           -- At this point, we know that we need to merge n1 with r2. There are two things needed to do this:
           -- * n1 needs to be converted to a right node to match r2.
           -- * We need to compare max1 and max2 to figure out which will be the maximum of the combined node and to
           --   decide which (R1, R2, or RFused) function to recurse to.
           | max1 > max2 -> case missSingle miss1 (boundKey max1) maxV1 of
               -- If we had an optimized goR2 (no keep), then calling using it is more efficient than
               -- calling goR2Keep and having to extract a new maximum from the result. Therefore, we
               -- first check if we can keep our existing maximum, and if not, call goR2.
               Nothing -> maybeBinL l' (goR2 maxV2 max1 (Bin min1 minV1 l1 r1) max2 r2)
               Just maxV' -> Bin max1 maxV' l' (goR2Keep maxV2 max1 (Bin min1 minV1 l1 r1) max2 r2)
           | max1 < max2 -> case missSingle miss2 (boundKey max2) maxV2 of
               Nothing -> maybeBinL l' (goR1 maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2)
               Just maxV' -> Bin max2 maxV' l' (goR1Keep maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2)
           | otherwise -> case matchSingle match (boundKey max1) maxV1 maxV2 of
               Nothing -> maybeBinL l' (goRFused max1 (Bin min1 minV1 l1 r1) r2)
               Just maxV' -> Bin max1 maxV' l' (goRFusedKeep max1 (Bin min1 minV1 l1 r1) r2)
         where
           {-# INLINE l' #-}
           l' = missLeft miss2 l2

        -- The two nodes split at the same time. In this case we need to merge l1 and l2 and r1 and r2. We already know that
        -- min1 > min2, so merging the left nodes is easy, but we need to branch to figure out which right merging function to call
        -- and which maximum to keep.
        EQ | max1 > max2 -> case missSingle miss1 (boundKey max1) maxV1 of
               Nothing -> maybeBinL l' (goR2 maxV2 max1 r1 max2 r2)
               Just maxV' -> Bin max1 maxV' l' (goR2Keep maxV2 max1 r1 max2 r2)
           | max1 < max2 -> case missSingle miss2 (boundKey max2) maxV2 of
               Nothing -> maybeBinL l' (goR1 maxV1 max1 r1 max2 r2)
               Just maxV' -> Bin max2 maxV' l' (goR1Keep maxV1 max1 r1 max2 r2)
           | otherwise -> case matchSingle match (boundKey max1) maxV1 maxV2 of
               Nothing -> maybeBinL l' (goRFused max1 r1 r2)
               Just maxV' -> Bin max1 maxV' l' (goRFusedKeep max1 r1 r2)
         where
           {-# INLINE l' #-}
           l' = goL1Keep minV1 min1 l1 min2 l2

        -- The simplest case is when node 1 splits first, meaning that we need to merge n2 and l1 or r1. However, since we already know
        -- that min1 > min2, n2 must be merged with l1 instead of r1, and we already know the correct method to call.
        GT -> binL1 max1 maxV1 (goL1Keep minV1 min1 l1 min2 n2) (missRight miss1 r1)


    -- Merge two left nodes and a minimum value for the second node into a new left node
    -- Precondition: min2 > min1
    -- goL2Keep :: b -> Key -> Node a -> Key -> Node b -> Node c
    goL2Keep minV2 !_ Tip !min2 Tip = case missSingle miss2 (boundKey min2) minV2 of
        Nothing -> Tip
        Just minV' -> Bin (minToMax min2) minV' Tip Tip
    goL2Keep minV2 !min1 Tip !min2 n2 = case missSingle miss2 (boundKey min2) minV2 of
        Nothing -> missLeft miss2 n2
        Just minV' -> insertMinL (xor (boundKey min2) min1) min2 minV' (missLeft miss2 n2)
    goL2Keep minV2 !min1 n1@(Bin max1 _ _ _) !min2 !n2 | boundsDisjoint min2 max1 = case missAllL miss2 (NonEmpty min2 minV2 n2) of
        Empty -> missLeft miss1 n1
        NonEmpty min2' minV2' n2' -> case missLeft miss1 n1 of
            Tip -> insertMinL (xor (boundKey min2') min1) min2' minV2' n2'
            n1'@(Bin _ _ _ _) -> unionDisjointL minV2' min1 n1' min2' n2'
    goL2Keep minV2 !min1 !n1 !min2 Tip = goInsertL2 (boundKey min2) minV2 (xor (boundKey min2) min1) min1 n1
    goL2Keep minV2 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        GT | xor (boundKey min2) min1 < xor (boundKey min2) max1 -> binL1 max1 maxV1 (goL2Keep minV2 min1 l1 min2 n2) (missRight miss1 r1)
           | max1 > max2 -> case missSingle miss1 (boundKey max1) maxV1 of
               Nothing -> maybeBinL l' (goR2 maxV2 max1 r1 max2 (Bin min2 minV2 l2 r2))
               Just maxV' -> Bin max1 maxV' l' (goR2Keep maxV2 max1 r1 max2 (Bin min2 minV2 l2 r2))
           | max1 < max2 -> case missSingle miss2 (boundKey max2) maxV2 of
               Nothing -> maybeBinL l' (goR1 maxV1 max1 r1 max2 (Bin min2 minV2 l2 r2))
               Just maxV' -> Bin max2 maxV' l' (goR1Keep maxV1 max1 r1 max2 (Bin min2 minV2 l2 r2))
           | otherwise -> case matchSingle match (boundKey max1) maxV1 maxV2 of
               Nothing -> maybeBinL l' (goRFused max1 r1 (Bin min2 minV2 l2 r2))
               Just maxV' -> Bin max1 maxV' l' (goRFusedKeep max1 r1 (Bin min2 minV2 l2 r2))
         where
           {-# INLINE l' #-}
           l' = missLeft miss1 l1
        EQ | max1 > max2 -> case missSingle miss1 (boundKey max1) maxV1 of
               Nothing -> maybeBinL l' (goR2 maxV2 max1 r1 max2 r2)
               Just maxV' -> Bin max1 maxV' l' (goR2Keep maxV2 max1 r1 max2 r2)
           | max1 < max2 -> case missSingle miss2 (boundKey max2) maxV2 of
               Nothing -> maybeBinL l' (goR1 maxV1 max1 r1 max2 r2)
               Just maxV' -> Bin max2 maxV' l' (goR1Keep maxV1 max1 r1 max2 r2)
           | otherwise -> case matchSingle match (boundKey max1) maxV1 maxV2 of
               Nothing -> maybeBinL l' (goRFused max1 r1 r2)
               Just maxV' -> Bin max1 maxV' l' (goRFusedKeep max1 r1 r2)
         where
           {-# INLINE l' #-}
           l' = goL2Keep minV2 min1 l1 min2 l2
        LT -> binL2 max2 maxV2 (goL2Keep minV2 min1 n1 min2 l2) (missRight miss2 r2)


    -- | Merge two left nodes that share a minimum bound.

    -- We can special case the merging of two empty nodes. This is currently commented out in an attempt to
    -- match union as closely as possible
--    goLFusedKeep !_ Tip Tip = Tip

    -- If one of the nodes is empty, we can just use the other one. Unlike the case of misaligned nodes, we don't have an
    -- extra value to insert
    goLFusedKeep !_ Tip n2 = missLeft miss2 n2
    goLFusedKeep !_ n1 Tip = missLeft miss1 n1

    -- Since the two nodes are joined at the left, the choices are considerable limited in comparison to the misaligned case.
    -- If node 1 splits first, n2 must be merged with l1 and if node 2 splits first, n1 must be merged with l2. The equal case
    -- is still the same as in the misaligned case, since we need to determine which maximum to use and which goR to call.
    goLFusedKeep !min n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min max1) (xorBounds min max2) of
        LT -> binL2 max2 maxV2 (goLFusedKeep min n1 l2) (missRight miss2 r2)
        EQ | max1 > max2 -> case missSingle miss1 (boundKey max1) maxV1 of
               Nothing -> maybeBinL l' (goR2 maxV2 max1 r1 max2 r2)
               Just maxV' -> Bin max1 maxV' l' (goR2Keep maxV2 max1 r1 max2 r2)
           | max1 < max2 -> case missSingle miss2 (boundKey max2) maxV2 of
               Nothing -> maybeBinL l' (goR1 maxV1 max1 r1 max2 r2)
               Just maxV' -> Bin max2 maxV' l' (goR1Keep maxV1 max1 r1 max2 r2)
           | otherwise -> case matchSingle match (boundKey max1) maxV1 maxV2 of
               Nothing -> maybeBinL l' (goRFused max1 r1 r2)
               Just maxV' -> Bin max1 maxV' l' (goRFusedKeep max1 r1 r2)
         where
           {-# INLINE l' #-}
           l' = goLFusedKeep min l1 l2
        GT -> binL1 max1 maxV1 (goLFusedKeep min l1 n2) (missRight miss1 r1)

    -- Merge two right nodes and a maximum value for the first node into a new right node
    -- Precondition: max1 < max2
    -- goR1Keep :: a -> Key -> Node a -> Key -> Node b -> Node c
    goR1Keep maxV1 !max1 Tip !_ Tip = case missSingle miss1 (boundKey max1) maxV1 of
        Nothing -> Tip
        Just maxV' -> Bin (maxToMin max1) maxV' Tip Tip
    goR1Keep maxV1 !max1 !n1 !max2 Tip = case missSingle miss1 (boundKey max1) maxV1 of
        Nothing -> missRight miss1 n1
        Just maxV' -> insertMaxR (xor (boundKey max1) max2) max1 maxV' (missRight miss1 n1)
    goR1Keep maxV1 !max1 !n1 !max2 n2@(Bin min2 _ _ _) | boundsDisjoint min2 max1 = case missAllR miss1 (NonEmpty max1 maxV1 n1) of
        Empty -> missRight miss2 n2
        NonEmpty max1' maxV1' n1' -> case missRight miss2 n2 of
            Tip -> insertMaxR (xor (boundKey max1') max2) max1' maxV1' n1'
            n2'@(Bin _ _ _ _) -> unionDisjointR maxV1' max1' n1' max2 n2'
    goR1Keep maxV1 !max1 Tip !max2 n2 = goInsertR1 (boundKey max1) maxV1 (xor (boundKey max1) max2) max2 n2
    goR1Keep maxV1 !max1 n1@(Bin min1 minV1 l1 r1) !max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT | xor (boundKey max1) min2 > xor (boundKey max1) max2 -> binR2 min2 minV2 (missLeft miss2 l2) (goR1Keep maxV1 max1 n1 max2 r2)
           | min1 < min2 -> case missSingle miss1 (boundKey min1) minV1 of
               Nothing -> maybeBinR (goL2 minV2 min1 (Bin max1 maxV1 l1 r1) min2 l2) r'
               Just minV' -> Bin min1 minV' (goL2Keep minV2 min1 (Bin max1 maxV1 l1 r1) min2 l2) r'
           | min1 > min2 -> case missSingle miss2 (boundKey min2) minV2 of
               Nothing -> maybeBinR (goL1 minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2) r'
               Just minV' -> Bin min2 minV' (goL1Keep minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2) r'
           | otherwise -> case matchSingle match (boundKey min1) minV1 minV2 of
               Nothing -> maybeBinR (goLFused min1 (Bin max1 maxV1 l1 r1) l2) r'
               Just minV' -> Bin min1 minV' (goLFusedKeep min1 (Bin max1 maxV1 l1 r1) l2) r'
         where
           {-# INLINE r' #-}
           r' = missRight miss2 r2
        EQ | min1 < min2 -> case missSingle miss1 (boundKey min1) minV1 of
               Nothing -> maybeBinR (goL2 minV2 min1 l1 min2 l2) r'
               Just minV' -> Bin min1 minV' (goL2Keep minV2 min1 l1 min2 l2) r'
           | min1 > min2 -> case missSingle miss2 (boundKey min2) minV2 of
               Nothing -> maybeBinR (goL1 minV1 min1 l1 min2 l2) r'
               Just minV' -> Bin min2 minV' (goL1Keep minV1 min1 l1 min2 l2) r'
           | otherwise -> case matchSingle match (boundKey min1) minV1 minV2 of
               Nothing -> maybeBinR (goLFused min1 l1 l2) r'
               Just minV' -> Bin min1 minV' (goLFusedKeep min1 l1 l2) r'
         where
           {-# INLINE r' #-}
           r' = goR1Keep maxV1 max1 r1 max2 r2
        GT -> binR1 min1 minV1 (missLeft miss1 l1) (goR1Keep maxV1 max1 r1 max2 n2)

    -- Merge two left nodes and a minimum value for the second node into a new left node
    -- Precondition: max2 < max1
    -- goR2Keep :: b -> Key -> Node a -> Key -> Node b -> Node c
    goR2Keep maxV2 !_ Tip !max2 Tip = case missSingle miss2 (boundKey max2) maxV2 of
        Nothing -> Tip
        Just maxV' -> Bin (maxToMin max2) maxV' Tip Tip
    goR2Keep maxV2 !max1 Tip !max2 n2 = case missSingle miss2 (boundKey max2) maxV2 of
        Nothing -> missRight miss2 n2
        Just maxV' -> insertMaxR (xor (boundKey max2) max1) max2 maxV' (missRight miss2 n2)
    goR2Keep maxV2 !max1 n1@(Bin min1 _ _ _) !max2 !n2 | boundsDisjoint min1 max2 = case missAllR miss2 (NonEmpty max2 maxV2 n2) of
        Empty -> missRight miss1 n1
        NonEmpty max2' maxV2' n2' -> case missRight miss1 n1 of
            Tip -> insertMaxR (xor (boundKey max2') max1) max2' maxV2' n2'
            n1'@(Bin _ _ _ _) -> unionDisjointR maxV2' max2' n2' max1 n1'
    goR2Keep maxV2 !max1 !n1 !max2 Tip = goInsertR2 (boundKey max2) maxV2 (xor (boundKey max2) max1) max1 n1
    goR2Keep maxV2 !max1 n1@(Bin min1 minV1 l1 r1) !max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        GT | xor (boundKey max2) min1 > xor (boundKey max2) max1 -> binR1 min1 minV1 (missLeft miss1 l1) (goR2Keep maxV2 max1 r1 max2 n2)
           | min1 < min2 -> case missSingle miss1 (boundKey min1) minV1 of
               Nothing -> maybeBinR (goL2 minV2 min1 l1 min2 (Bin max2 maxV2 l2 r2)) r'
               Just minV' -> Bin min1 minV' (goL2Keep minV2 min1 l1 min2 (Bin max2 maxV2 l2 r2)) r'
           | min1 > min2 -> case missSingle miss2 (boundKey min2) minV2 of
               Nothing -> maybeBinR (goL1 minV1 min1 l1 min2 (Bin max2 maxV2 l2 r2)) r'
               Just minV' -> Bin min2 minV' (goL1Keep minV1 min1 l1 min2 (Bin max2 maxV2 l2 r2)) r'
           | otherwise -> case matchSingle match (boundKey min1) minV1 minV2 of
               Nothing -> maybeBinR (goLFused min1 l1 (Bin max2 maxV2 l2 r2)) r'
               Just minV' -> Bin min1 minV' (goLFusedKeep min1 l1 (Bin max2 maxV2 l2 r2)) r'
         where
           {-# INLINE r' #-}
           r' = missRight miss1 r1
        EQ | min1 < min2 -> case missSingle miss1 (boundKey min1) minV1 of
               Nothing -> maybeBinR (goL2 minV2 min1 l1 min2 l2) r'
               Just minV' -> Bin min1 minV' (goL2Keep minV2 min1 l1 min2 l2) r'
           | min1 > min2 -> case missSingle miss2 (boundKey min2) minV2 of
               Nothing -> maybeBinR (goL1 minV1 min1 l1 min2 l2) r'
               Just minV' -> Bin min2 minV' (goL1Keep minV1 min1 l1 min2 l2) r'
           | otherwise -> case matchSingle match (boundKey min1) minV1 minV2 of
               Nothing -> maybeBinR (goLFused min1 l1 l2) r'
               Just minV' -> Bin min1 minV' (goLFusedKeep min1 l1 l2) r'
         where
           {-# INLINE r' #-}
           r' = goR2Keep maxV2 max1 r1 max2 r2
        LT -> binR2 min2 minV2 (missLeft miss2 l2) (goR2Keep maxV2 max1 n1 max2 r2)

--    goRFusedKeep !_ Tip Tip = Tip
    goRFusedKeep !_ Tip n2 = missRight miss2 n2
    goRFusedKeep !_ n1 Tip = missRight miss1 n1
    goRFusedKeep !max n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max) (xorBounds min2 max) of
        LT -> binR2 min2 minV2 (missLeft miss2 l2) (goRFusedKeep max n1 r2)
        EQ | min1 < min2 -> case missSingle miss1 (boundKey min1) minV1 of
               Nothing -> maybeBinR (goL2 minV2 min1 l1 min2 l2) r'
               Just minV' -> Bin min1 minV' (goL2Keep minV2 min1 l1 min2 l2) r'
           | min1 > min2 -> case missSingle miss2 (boundKey min2) minV2 of
               Nothing -> maybeBinR (goL1 minV1 min1 l1 min2 l2) r'
               Just minV' -> Bin min2 minV' (goL1Keep minV1 min1 l1 min2 l2) r'
           | otherwise -> case matchSingle match (boundKey min1) minV1 minV2 of
               Nothing -> maybeBinR (goLFused min1 l1 l2) r'
               Just minV' -> Bin min1 minV' (goLFusedKeep min1 l1 l2) r'
         where
           {-# INLINE r' #-}
           r' = goRFusedKeep max r1 r2
        GT -> binR1 min1 minV1 (missLeft miss1 l1) (goRFusedKeep max r1 n2)

    -- TODO: These are inefficient, obviously correct implementations. See intersection
    -- and difference for examples of specialized implementations
    goL1 minV1 !min1 !n1 !min2 !n2 = nodeToMapL (goL1Keep minV1 min1 n1 min2 n2)
    goL2 minV2 !min1 !n1 !min2 !n2 = nodeToMapL (goL2Keep minV2 min1 n1 min2 n2)
    goLFused !min !n1 !n2 = nodeToMapL (goLFusedKeep min n1 n2)
    goR1 maxV1 !max1 !n1 !max2 !n2 = nodeToMapR (goR1Keep maxV1 max1 n1 max2 n2)
    goR2 maxV2 !max1 !n1 !max2 !n2 = nodeToMapR (goR2Keep maxV2 max1 n1 max2 n2)
    goRFused !max !n1 !n2 = nodeToMapR (goRFusedKeep max n1 n2)

    goInsertL1 !k v !_ _ Tip = case missSingle miss1 k v of
        Nothing -> Tip
        Just v' -> Bin (Bound k) v' Tip Tip
    goInsertL1 !k v !xorCache min (Bin max maxV l r)
        | inMaxBound k max = if xorCache < xorCacheMax
            then binL2 max maxV (goInsertL1 k v xorCache min l) (missRight miss2 r)
            else binL2 max maxV (missLeft miss2 l) (goInsertR1 k v xorCacheMax max r)
        | outOfMaxBound k max = case missSingle miss1 k v of
            Nothing -> missLeft miss2 (Bin max maxV l r)
            Just v' -> if xor (boundKey max) min < xorCacheMax
                       then Bin (Bound k) v' (missLeft miss2 (Bin max maxV l r)) Tip
                       else Bin (Bound k) v' (missLeft miss2 l) (missRight miss2 (insertMaxR xorCacheMax max maxV r))
        | otherwise = case matchSingle match k v maxV of
            Nothing -> extractBinL (missLeft miss2 l) (missRight miss2 r) -- TODO: do extractBin first?
            Just maxV' -> Bin max maxV' (missLeft miss2 l) (missRight miss2 r)
      where xorCacheMax = xor k max

    goInsertL2 !k v !_ _ Tip = case missSingle miss2 k v of
        Nothing -> Tip
        Just v' -> Bin (Bound k) v' Tip Tip
    goInsertL2 !k v !xorCache min (Bin max maxV l r)
        | inMaxBound k max = if xorCache < xorCacheMax
            then binL1 max maxV (goInsertL2 k v xorCache min l) (missRight miss1 r)
            else binL1 max maxV (missLeft miss1 l) (goInsertR2 k v xorCacheMax max r)
        | outOfMaxBound k max = case missSingle miss2 k v of
            Nothing -> missLeft miss1 (Bin max maxV l r)
            Just v' -> if xor (boundKey max) min < xorCacheMax
                       then Bin (Bound k) v' (missLeft miss1 (Bin max maxV l r)) Tip
                       else Bin (Bound k) v' (missLeft miss1 l) (missRight miss1 (insertMaxR xorCacheMax max maxV r))
        | otherwise = case matchSingle match k maxV v of
            Nothing -> extractBinL (missLeft miss1 l) (missRight miss1 r) -- TODO: do extractBin first?
            Just maxV' -> Bin max maxV' (missLeft miss1 l) (missRight miss1 r)
      where xorCacheMax = xor k max

    goInsertR1 k v !_ _ Tip = case missSingle miss1 k v of
        Nothing -> Tip
        Just v' -> Bin (Bound k) v' Tip Tip
    goInsertR1 k v !xorCache max (Bin min minV l r)
        | inMinBound k min = if xorCache < xorCacheMin
            then binR2 min minV (missLeft miss2 l) (goInsertR1 k v xorCache max r)
            else binR2 min minV (goInsertL1 k v xorCacheMin min l) (missRight miss2 r)
        | outOfMinBound k min = case missSingle miss1 k v of
            Nothing -> missRight miss2 (Bin min minV l r)
            Just v' -> if xor (boundKey min) max < xorCacheMin
                       then Bin (Bound k) v' Tip (missRight miss2 (Bin min minV l r))
                       else Bin (Bound k) v' (missLeft miss2 (insertMinL xorCacheMin min minV l)) (missRight miss2 r)
        | otherwise = case matchSingle match k v minV of
            Nothing -> extractBinR (missLeft miss2 l) (missRight miss2 r) -- TODO: do extractBin first?
            Just minV' -> Bin min minV' (missLeft miss2 l) (missRight miss2 r)
      where xorCacheMin = xor k min

    goInsertR2 !k v !_ _ Tip = case missSingle miss2 k v of
        Nothing -> Tip
        Just v' -> Bin (Bound k) v' Tip Tip
    goInsertR2 !k v !xorCache max (Bin min minV l r)
        | inMinBound k min = if xorCache < xorCacheMin
            then binR1 min minV (missLeft miss1 l) (goInsertR2 k v xorCache max r)
            else binR1 min minV (goInsertL2 k v xorCacheMin min l) (missRight miss1 r)
        | outOfMinBound k min = case missSingle miss2 k v of
            Nothing -> missRight miss1 (Bin min minV l r)
            Just v' -> if xor (boundKey min) max < xorCacheMin
                       then Bin (Bound k) v' Tip (missRight miss1 (Bin min minV l r))
                       else Bin (Bound k) v' (missLeft miss1 (insertMinL xorCacheMin min minV l)) (missRight miss1 r)
        | otherwise = case matchSingle match k minV v of
            Nothing -> extractBinR (missLeft miss1 l) (missRight miss1 r) -- TODO: do extractBin first?
            Just minV' -> Bin min minV' (missLeft miss1 l) (missRight miss1 r)
      where xorCacheMin = xor k min

    {-# INLINE binL1 #-}
    binL1 k1 v1 l r = case missSingle miss1 (boundKey k1) v1 of
        Nothing -> extractBinL l r
        Just v' -> Bin k1 v' l r

    {-# INLINE binL2 #-}
    binL2 k2 v2 l r = case missSingle miss2 (boundKey k2) v2 of
        Nothing -> extractBinL l r
        Just v' -> Bin k2 v' l r

    {-# INLINE binR1 #-}
    binR1 k1 v1 l r = case missSingle miss1 (boundKey k1) v1 of
        Nothing -> extractBinR l r
        Just v' -> Bin k1 v' l r

    {-# INLINE binR2 #-}
    binR2 k2 v2 l r = case missSingle miss2 (boundKey k2) v2 of
        Nothing -> extractBinR l r
        Just v' -> Bin k2 v' l r

    -- To avoid the messy pain of putting runIdentity everywhere, we use pure versions of the input functions.
    {-# INLINE missSingle #-}
    missSingle whenMiss k v = runIdentity (missingSingle whenMiss k v)

    {-# INLINE missLeft #-}
    missLeft whenMiss l = runIdentity (missingLeft whenMiss l)

    {-# INLINE missRight #-}
    missRight whenMiss r = runIdentity (missingRight whenMiss r)

    {-# INLINE missAllL #-}
    missAllL whenMiss m = runIdentity (missingAllL whenMiss m)

    {-# INLINE missAllR #-}
    missAllR whenMiss m = l2rMap (missAllL whenMiss (r2lMap m))

    {-# INLINE matchSingle #-}
    matchSingle whenMatch k v1 v2 = runIdentity (matchedSingle whenMatch k v1 v2)
-}

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
        | min1 < min2 = (\v m -> IntMap (maybeInsertMin min1 v m)) <$> missingSingle miss1 (boundKey min1) minV1 <*> goL2 minV2 min1 root1 min2 root2
        | min2 < min1 = (\v m -> IntMap (maybeInsertMin min2 v m)) <$> missingSingle miss2 (boundKey min2) minV2 <*> goL1 minV1 min1 root1 min2 root2
        | otherwise = (\v m -> IntMap (maybeInsertMin min1 v m)) <$> matchedSingle match (boundKey min1) minV1 minV2 <*> goLFused min1 root1 root2

    goL1 minV1 !min1 !n1 !_ Tip = missingAllL miss1 (NonEmpty min1 minV1 n1)
    goL1 minV1 !min1 !n1 !min2 n2@(Bin max2 _ _ _) | boundsDisjoint min1 max2 = maybeUnionDisjointL min2 <$> missingLeft miss2 n2 <*> missingAllL miss1 (NonEmpty min1 minV1 n1)
    goL1 minV1 !min1 Tip !min2 !n2 = goInsertL1 (boundKey min1) minV1 (xor (boundKey min1) min2) min2 n2
    goL1 minV1 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT | xor (boundKey min1) min2 < xor (boundKey min1) max2 -> binL <$> goL1 minV1 min1 n1 min2 l2 <*> missingAllR miss2 (NonEmpty max2 maxV2 r2)
           | max1 > max2 -> (\l' rm v -> nodeToMapL (maybeBinL l' (maybeInsertMax max1 v rm))) <$> missingLeft miss2 l2 <*> goR2 maxV2 max1 (Bin min1 minV1 l1 r1) max2 r2 <*> missingSingle miss1 (boundKey max1) maxV1
           | max1 < max2 -> (\l' rm v -> nodeToMapL (maybeBinL l' (maybeInsertMax max2 v rm))) <$> missingLeft miss2 l2 <*> goR1 maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2 <*> missingSingle miss2 (boundKey max2) maxV2
           | otherwise -> (\l' rm v -> nodeToMapL (maybeBinL l' (maybeInsertMax max1 v rm))) <$> missingLeft miss2 l2 <*> goRFused max1 (Bin min1 minV1 l1 r1) r2 <*> matchedSingle match (boundKey max1) maxV1 maxV2
        EQ | max1 > max2 -> (\l' rm v -> binL l' (maybeInsertMax max1 v rm)) <$> goL1 minV1 min1 l1 min2 l2 <*> goR2 maxV2 max1 r1 max2 r2 <*> missingSingle miss1 (boundKey max1) maxV1
           | max1 < max2 -> (\l' rm v -> binL l' (maybeInsertMax max2 v rm)) <$> goL1 minV1 min1 l1 min2 l2 <*> goR1 maxV1 max1 r1 max2 r2 <*> missingSingle miss2 (boundKey max2) maxV2
           | otherwise -> (\l' rm v -> binL l' (maybeInsertMax max1 v rm)) <$> goL1 minV1 min1 l1 min2 l2 <*> goRFused max1 r1 r2 <*> matchedSingle match (boundKey max1) maxV1 maxV2
        GT -> binL <$> goL1 minV1 min1 l1 min2 n2 <*> missingAllR miss1 (NonEmpty max1 maxV1 r1)

    goL2 minV2 !_ Tip !min2 !n2 = missingAllL miss2 (NonEmpty min2 minV2 n2)
    goL2 minV2 !min1 n1@(Bin max1 _ _ _) !min2 !n2 | boundsDisjoint min2 max1 = maybeUnionDisjointL min1 <$> missingLeft miss1 n1 <*> missingAllL miss2 (NonEmpty min2 minV2 n2)
    goL2 minV2 !min1 !n1 !min2 Tip = goInsertL2 (boundKey min2) minV2 (xor (boundKey min2) min1) min1 n1
    goL2 minV2 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        GT | xor (boundKey min2) min1 < xor (boundKey min2) max1 -> binL <$> goL2 minV2 min1 l1 min2 n2 <*> missingAllR miss1 (NonEmpty max1 maxV1 r1)
           | max1 > max2 -> (\l' rm v -> nodeToMapL (maybeBinL l' (maybeInsertMax max1 v rm))) <$> missingLeft miss1 l1 <*> goR2 maxV2 max1 r1 max2 (Bin min2 minV2 l2 r2) <*> missingSingle miss1 (boundKey max1) maxV1
           | max1 < max2 -> (\l' rm v -> nodeToMapL (maybeBinL l' (maybeInsertMax max2 v rm))) <$> missingLeft miss1 l1 <*> goR1 maxV1 max1 r1 max2 (Bin min2 minV2 l2 r2) <*> missingSingle miss2 (boundKey max2) maxV2
           | otherwise -> (\l' rm v -> nodeToMapL (maybeBinL l' (maybeInsertMax max1 v rm))) <$> missingLeft miss1 l1 <*> goRFused max1 r1 (Bin min2 minV2 l2 r2) <*> matchedSingle match (boundKey max1) maxV1 maxV2
        EQ | max1 > max2 -> (\l' rm v -> binL l' (maybeInsertMax max1 v rm)) <$> goL2 minV2 min1 l1 min2 l2 <*> goR2 maxV2 max1 r1 max2 r2 <*> missingSingle miss1 (boundKey max1) maxV1
           | max1 < max2 -> (\l' rm v -> binL l' (maybeInsertMax max2 v rm)) <$> goL2 minV2 min1 l1 min2 l2 <*> goR1 maxV1 max1 r1 max2 r2 <*> missingSingle miss2 (boundKey max2) maxV2
           | otherwise -> (\l' rm v -> binL l' (maybeInsertMax max1 v rm)) <$> goL2 minV2 min1 l1 min2 l2 <*> goRFused max1 r1 r2 <*> matchedSingle match (boundKey max1) maxV1 maxV2
        LT -> binL <$> goL2 minV2 min1 n1 min2 l2 <*> missingAllR miss2 (NonEmpty max2 maxV2 r2)

    goLFused !_ Tip !n2 = nodeToMapL <$> missingLeft miss2 n2
    goLFused !_ !n1 Tip = nodeToMapL <$> missingLeft miss1 n1
    goLFused !min n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min max1) (xorBounds min max2) of
        LT -> binL <$> goLFused min n1 l2 <*> missingAllR miss2 (NonEmpty max2 maxV2 r2)
        EQ | max1 > max2 -> (\l' rm v -> binL l' (maybeInsertMax max1 v rm)) <$> goLFused min l1 l2 <*> goR2 maxV2 max1 r1 max2 r2 <*> missingSingle miss1 (boundKey max1) maxV1
           | max1 < max2 -> (\l' rm v -> binL l' (maybeInsertMax max2 v rm)) <$> goLFused min l1 l2 <*> goR1 maxV1 max1 r1 max2 r2 <*> missingSingle miss2 (boundKey max2) maxV2
           | otherwise -> (\l' rm v -> binL l' (maybeInsertMax max1 v rm)) <$> goLFused min l1 l2 <*> goRFused max1 r1 r2 <*> matchedSingle match (boundKey max1) maxV1 maxV2
        GT -> binL <$> goLFused min l1 n2 <*> missingAllR miss1 (NonEmpty max1 maxV1 r1)

    goR1 maxV1 !max1 !n1 !_ Tip = missingAllR miss1 (NonEmpty max1 maxV1 n1)
    goR1 maxV1 !max1 !n1 !max2 n2@(Bin min2 _ _ _) | boundsDisjoint min2 max1 = maybeUnionDisjointR max2 <$> missingAllR miss1 (NonEmpty max1 maxV1 n1) <*> missingRight miss2 n2
    goR1 maxV1 !max1 Tip !max2 !n2 = goInsertR1 (boundKey max1) maxV1 (xor (boundKey max1) max2) max2 n2
    goR1 maxV1 !max1 n1@(Bin min1 minV1 l1 r1) !max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT | xor (boundKey max1) min2 > xor (boundKey max1) max2 -> binR <$> missingAllL miss2 (NonEmpty min2 minV2 l2) <*> goR1 maxV1 max1 n1 max2 r2
           | min1 < min2 -> (\v lm r' -> nodeToMapR (maybeBinR (maybeInsertMin min1 v lm) r')) <$> missingSingle miss1 (boundKey min1) minV1 <*> goL2 minV2 min1 (Bin max1 maxV1 l1 r1) min2 l2 <*> missingRight miss2 r2
           | min1 > min2 -> (\v lm r' -> nodeToMapR (maybeBinR (maybeInsertMin min2 v lm) r')) <$> missingSingle miss2 (boundKey min2) minV2 <*> goL1 minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2 <*> missingRight miss2 r2
           | otherwise -> (\v lm r' -> nodeToMapR (maybeBinR (maybeInsertMin min1 v lm) r')) <$> matchedSingle match (boundKey min1) minV1 minV2 <*> goLFused min1 (Bin max1 maxV1 l1 r1) l2 <*> missingRight miss2 r2
        EQ | min1 < min2 -> (\v lm r' -> binR (maybeInsertMin min1 v lm) r') <$> missingSingle miss1 (boundKey min1) minV1 <*> goL2 minV2 min1 l1 min2 l2 <*> goR1 maxV1 max1 r1 max2 r2
           | min1 > min2 -> (\v lm r' -> binR (maybeInsertMin min2 v lm) r') <$> missingSingle miss2 (boundKey min2) minV2 <*> goL1 minV1 min1 l1 min2 l2 <*> goR1 maxV1 max1 r1 max2 r2
           | otherwise -> (\v lm r' -> binR (maybeInsertMin min1 v lm) r') <$> matchedSingle match (boundKey min1) minV1 minV2 <*> goLFused min1 l1 l2 <*> goR1 maxV1 max1 r1 max2 r2
        GT -> binR <$> missingAllL miss1 (NonEmpty min1 minV1 l1) <*> goR1 maxV1 max1 r1 max2 n2

    goR2 maxV2 !_ Tip !max2 !n2 = missingAllR miss2 (NonEmpty max2 maxV2 n2)
    goR2 maxV2 !max1 n1@(Bin min1 _ _ _) !max2 !n2 | boundsDisjoint min1 max2 = maybeUnionDisjointR max1 <$> missingAllR miss2 (NonEmpty max2 maxV2 n2) <*> missingRight miss1 n1
    goR2 maxV2 !max1 !n1 !max2 Tip = goInsertR2 (boundKey max2) maxV2 (xor (boundKey max1) max2) max1 n1
    goR2 maxV2 !max1 n1@(Bin min1 minV1 l1 r1) !max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        GT | xor (boundKey max2) min1 > xor (boundKey max2) max1 -> binR <$> missingAllL miss1 (NonEmpty min1 minV1 l1) <*> goR2 maxV2 max1 r1 max2 n2
           | min1 < min2 -> (\v lm r' -> nodeToMapR (maybeBinR (maybeInsertMin min1 v lm) r')) <$> missingSingle miss1 (boundKey min1) minV1 <*> goL2 minV2 min1 l1 min2 (Bin max2 maxV2 l2 r2) <*> missingRight miss1 r1
           | min1 > min2 -> (\v lm r' -> nodeToMapR (maybeBinR (maybeInsertMin min2 v lm) r')) <$> missingSingle miss2 (boundKey min2) minV2 <*> goL1 minV1 min1 l1 min2 (Bin max2 maxV2 l2 r2) <*> missingRight miss1 r1
           | otherwise -> (\v lm r' -> nodeToMapR (maybeBinR (maybeInsertMin min1 v lm) r')) <$> matchedSingle match (boundKey min1) minV1 minV2 <*> goLFused min1 l1 (Bin max2 maxV2 l2 r2) <*> missingRight miss1 r1
        EQ | min1 < min2 -> (\v lm r' -> binR (maybeInsertMin min1 v lm) r') <$> missingSingle miss1 (boundKey min1) minV1 <*> goL2 minV2 min1 l1 min2 l2 <*> goR2 maxV2 max1 r1 max2 r2
           | min1 > min2 -> (\v lm r' -> binR (maybeInsertMin min2 v lm) r') <$> missingSingle miss2 (boundKey min2) minV2 <*> goL1 minV1 min1 l1 min2 l2 <*> goR2 maxV2 max1 r1 max2 r2
           | otherwise -> (\v lm r' -> binR (maybeInsertMin min1 v lm) r') <$> matchedSingle match (boundKey min1) minV1 minV2 <*> goLFused min1 l1 l2 <*> goR2 maxV2 max1 r1 max2 r2
        LT -> binR <$> missingAllL miss2 (NonEmpty min2 minV2 l2) <*> goR2 maxV2 max1 n1 max2 r2

    goRFused !_ Tip !n2 = nodeToMapR <$> missingRight miss2 n2
    goRFused !_ !n1 Tip = nodeToMapR <$> missingRight miss1 n1
    goRFused !max n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max) (xorBounds min2 max) of
        LT -> binR <$> missingAllL miss2 (NonEmpty min2 minV2 l2) <*> goRFused max n1 r2
        EQ | min1 < min2 -> (\v lm r' -> binR (maybeInsertMin min1 v lm) r') <$> missingSingle miss1 (boundKey min1) minV1 <*> goL2 minV2 min1 l1 min2 l2 <*> goRFused max r1 r2
           | min1 > min2 -> (\v lm r' -> binR (maybeInsertMin min2 v lm) r') <$> missingSingle miss2 (boundKey min2) minV2 <*> goL1 minV1 min1 l1 min2 l2 <*> goRFused max r1 r2
           | otherwise -> (\v lm r' -> binR (maybeInsertMin min1 v lm) r') <$> matchedSingle match (boundKey min1) minV1 minV2 <*> goLFused min1 l1 l2 <*> goRFused max r1 r2
        GT -> binR <$> missingAllL miss1 (NonEmpty min1 minV1 l1) <*> goRFused max r1 n2

    goInsertL1 !k v !_ _ Tip = maybeSingleton k <$> missingSingle miss1 k v
    goInsertL1 !k v !xorCache min n@(Bin max maxV l r)
        | inMaxBound k max = if xorCache < xorCacheMax
            then binL <$> goInsertL1 k v xorCache min l <*> missingAllR miss2 (NonEmpty max maxV r)
            else (\l' rm maxV' -> nodeToMapL (maybeBinL l' (maybeInsertMax max maxV' rm))) <$> missingLeft miss2 l <*> goInsertR1 k v xorCacheMax max r <*> missingSingle miss2 (boundKey max) maxV
        | outOfMaxBound k max = (\n' v' -> r2lMap (maybeInsertMax (Bound k) v' (l2rMap (nodeToMapL n')))) <$> missingLeft miss2 n <*> missingSingle miss1 k v
        | otherwise = (\l' r' v' -> nodeToMapL (maybe extractBinL (Bin max) v' l' r')) <$> missingLeft miss2 l <*> missingRight miss2 r <*> matchedSingle match k v maxV
      where xorCacheMax = xor k max

    goInsertL2 !k v !_ _ Tip = maybeSingleton k <$> missingSingle miss2 k v
    goInsertL2 !k v !xorCache min n@(Bin max maxV l r)
        | inMaxBound k max = if xorCache < xorCacheMax
            then binL <$> goInsertL2 k v xorCache min l <*> missingAllR miss1 (NonEmpty max maxV r)
            else (\l' rm maxV' -> nodeToMapL (maybeBinL l' (maybeInsertMax max maxV' rm))) <$> missingLeft miss1 l <*> goInsertR2 k v xorCacheMax max r <*> missingSingle miss1 (boundKey max) maxV
        | outOfMaxBound k max = (\n' v' -> r2lMap (maybeInsertMax (Bound k) v' (l2rMap (nodeToMapL n')))) <$> missingLeft miss1 n <*> missingSingle miss2 k v
        | otherwise = (\l' r' v' -> nodeToMapL (maybe extractBinL (Bin max) v' l' r')) <$> missingLeft miss1 l <*> missingRight miss1 r <*> matchedSingle match k maxV v
      where xorCacheMax = xor k max

    goInsertR1 !k v !_ _ Tip = maybeSingleton k <$> missingSingle miss1 k v
    goInsertR1 !k v !xorCache max n@(Bin min minV l r)
        | inMinBound k min = if xorCache < xorCacheMin
            then binR <$> missingAllL miss2 (NonEmpty min minV l) <*> goInsertR1 k v xorCache max r
            else (\minV' lm r' -> nodeToMapR (maybeBinR (maybeInsertMin min minV' lm) r')) <$> missingSingle miss2 (boundKey min) minV <*> goInsertL1 k v xorCacheMin min l <*> missingRight miss2 r
        | outOfMinBound k min = (\v' n' -> l2rMap (maybeInsertMin (Bound k) v' (r2lMap (nodeToMapR n')))) <$> missingSingle miss1 k v <*> missingRight miss2 n
        | otherwise = (\v' l' r' -> nodeToMapR (maybe extractBinR (Bin min) v' l' r')) <$> matchedSingle match k v minV <*> missingLeft miss2 l <*> missingRight miss2 r
      where xorCacheMin = xor k min

    goInsertR2 !k v !_ _ Tip = maybeSingleton k <$> missingSingle miss2 k v
    goInsertR2 !k v !xorCache max n@(Bin min minV l r)
        | inMinBound k min = if xorCache < xorCacheMin
            then binR <$> missingAllL miss1 (NonEmpty min minV l) <*> goInsertR2 k v xorCache max r
            else (\minV' lm r' -> nodeToMapR (maybeBinR (maybeInsertMin min minV' lm) r')) <$> missingSingle miss1 (boundKey min) minV <*> goInsertL2 k v xorCacheMin min l <*> missingRight miss1 r
        | outOfMinBound k min = (\v' n' -> l2rMap (maybeInsertMin (Bound k) v' (r2lMap (nodeToMapR n')))) <$> missingSingle miss2 k v <*> missingRight miss1 n
        | otherwise = (\v' l' r' -> nodeToMapR (maybe extractBinR (Bin min) v' l' r')) <$> matchedSingle match k minV v <*> missingLeft miss1 l <*> missingRight miss1 r
      where xorCacheMin = xor k min

    missingAllR whenMiss = fmap l2rMap . missingAllL whenMiss . r2lMap

maybeSingleton :: Key -> Maybe v -> IntMap_ d v
maybeSingleton !_ Nothing = Empty
maybeSingleton !k (Just v) = NonEmpty (Bound k) v Tip

maybeBinL :: Node L v -> IntMap_ R v -> Node L v
maybeBinL l Empty = l
maybeBinL l (NonEmpty max maxV r) = Bin max maxV l r

maybeBinR :: IntMap_ L v -> Node R v -> Node R v
maybeBinR Empty r = r
maybeBinR (NonEmpty min minV l) r = Bin min minV l r

maybeInsertMin :: Bound L -> Maybe v -> IntMap_ L v -> IntMap_ L v
maybeInsertMin !_ Nothing !m = m
maybeInsertMin !k (Just v) Empty = NonEmpty k v Tip
maybeInsertMin !k (Just v) (NonEmpty min minV root) = NonEmpty k v (insertMinL (xor (boundKey min) k) min minV root)

maybeInsertMax :: Bound R -> Maybe v -> IntMap_ R v -> IntMap_ R v
maybeInsertMax !_ Nothing !m = m
maybeInsertMax !k (Just v) Empty = NonEmpty k v Tip
maybeInsertMax !k (Just v) (NonEmpty max maxV root) = NonEmpty k v (insertMaxR (xor (boundKey max) k) max maxV root)

maybeUnionDisjointL :: Bound L -> Node L v -> IntMap_ L v -> IntMap_ L v
maybeUnionDisjointL !_ Tip !m2 = m2
maybeUnionDisjointL !_ !n1 Empty = nodeToMapL n1
maybeUnionDisjointL !min1 !n1 (NonEmpty min2 minV2 root2) = nodeToMapL (unionDisjointL minV2 min1 n1 min2 root2)

maybeUnionDisjointR :: Bound R -> IntMap_ R v -> Node R v -> IntMap_ R v
maybeUnionDisjointR !_ !m1 Tip = m1
maybeUnionDisjointR !_ Empty !n2 = nodeToMapR n2
maybeUnionDisjointR !max2 (NonEmpty max1 maxV1 root1) !n2 = nodeToMapR (unionDisjointR maxV1 max1 root1 max2 n2)
