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
--
-- = The Merging Process
--
-- Since keys have consistent positions in trie-like structures, merging is
-- largely a problem of alignment. Considering the sets of keys matching the
-- bit prefix of a node, we have the following six cases:
--
-- 1. Disjoint ranges
--
--    > Node 1:   ##
--    > Node 2:         ####
--
--    Depending on the type of merge, we combine the nodes. If
--    'Data.IntMap.Lazy.union', we create a new 'Bin' combining the nodes.
--    If 'Data.IntMap.Lazy.intersection', we return 'Empty'. In general, both
--    nodes get passed through the 'WhenMissing' tactics then combined.
--
-- 2. Equal ranges
--
--    > Node 1: ####
--    > Node 2: ####
--
--    We recurively merge node 1's left branch with node 2's left branch and
--    node 1's right branch with node 2's right branch.
--
-- 3. 1-in-2 (left)
--
--    > Node 1: ##
--    > Node 2: ####
--
--    We recursively merge node 1 with node 2's left branch.
--
-- 4. 1-in-2 (right)
--
--    > Node 1:   ##
--    > Node 2: ####
--
--    We recursively merge node 1 with node 2's right branch.
--
-- 5. 2-in-1 (left)
--
--    > Node 1: ####
--    > Node 2: ##
--
--    We recursively merge node 1's left branch with node 2.
--
-- 6. 2-in-1 (right)
--
--    > Node 1: ####
--    > Node 2:   ##
--
--    We recursively merge node 1's right branch with node 2.
--
-- Distinguishing the latter 5 cases is much harder with the disjoint ranges
-- case in the mix, so eliminating that is the first step. With the min/max
-- implicit representation, we can test for that case by seeing if the minimum
-- of one node is greater than the maximum of the other node 'boundsDisjoint'.
-- Technically, this condition can also trigger in the other cases, since the
-- cases defined above are about shared bits, not being in between the minima
-- and maxima. For example, a minimum of -1 shares no bits with a maximum of 0,
-- so for the purposes of the cases above, a node with those bounds would have
-- a "shared bits range" of all integers. However, determining that two nodes
-- will never overlap is a useful condition in its own right. If taking an
-- intersection, for example, we can immediately return 'Empty', and even the
-- union case can be simplified (see 'unionDisjointL' and 'unionDisjointR') if
-- not to a single 'Bin' node.
--
-- Once the ranges of the nodes are known to be overlapping, we can compare
-- range sizes to distinguish between the equal, 1-in-2, and 2-in-1 cases. When
-- the minumum and maximum bounds for a node are XORed with each other (using
-- 'xorBounds'), all shared prefix bits will cancel each other out and produce
-- zeros, and the first bit where the bounds disagree will become the result's
-- most significant set bit (MSB). This justifies using 'compareMSB' to compare
-- how many shared prefix bits the two nodes have. The left and right variants
-- of the 1-in-2 and 2-in-1 cases can then be distinguished using the same
-- techniques as in single-key queries, taking a key from the smaller node and
-- determining which branch it belongs in.
--
-- == Bound Complications
--
-- Unfortunately, since our tree structure is a bit more complicated than a
-- PATRICIA tree, there is more complexity involved. Instead of just aligning
-- internal nodes and merging values at the keys, we need to interleave the
-- combination of values with the alignment of nodes. At every point in the
-- recursion, whenever we produce a composite node, we need to produce the key
-- and value that go along with that node.
--
-- Determining which bound to keep and what values to combine is
-- straightforward; the lesser of the two minima is the new minimum, and the
-- greater of the two maxima is new maximum. However, the unused key/value
-- pair (if there is one) needs to be pushed downward in the recursion to be
-- included in the merged map. This necessitates three variants of each helper
-- function, corresponding to the three choices of pushing down the value from
-- node 1, pushing down the value from node 2, and pushing down neither. For
-- example, @goL1@ is called when @min1 > min2@, @goL2@ when @min2 > min1@, and
-- @goLFused@ when @min1 = min2@.
--
-- As we are forced to do these comparisons anyway, we can use them to reduce
-- the number of cases to consider. When @min1 > min2@, for example, @min2@
-- cannot be greater than @max1@, so a single comparison suffices to determine
-- whether the ranges are disjoint, and the 2-in-1 (right) case is impossible.
--
-- == Base Case: Merging leaves into trees
--
-- In the base case, when the recursion hits the leaves, there are two cases.
-- If the 'Tip' corresponds to the bound that was pushed down from higher up
-- in the tree, then the merge operation begins to look like an insertion (in
-- the case of 'Data.IntMap.Lazy.union'), deletion (in
-- 'Data.IntMap.Lazy.difference' for the second map's leaves), or lookup (in
-- 'Data.IntMap.Lazy.intersection'). These helpers match the general structure
-- of normal single-key operations. However, there still need to be two
-- variants for which map's leaves are being inserted into which other map.
--
-- If the 'Tip' corresponds to the bound that was already handled earlier in
-- the merge process, however, the recursion can immediately end.
--
-- == Deletion and Choice of Intermediate
--
-- Each step in the merge process as described so far processes a single key
-- (the merged bound for the node), recursion on the left branch (or a left
-- branch taken from one map), and recursion on the right brnach (or a right
-- branch taken from one map). This naturally corresponds to the arguments of
-- 'Bin' (a key/value pair and two 'Node's). However, that only matches up
-- when all of the keys are preserved, as in 'Data.IntMap.Lazy.union'. If the
-- merged bound is instead deleted, then it needs to be replaced with a bound
-- pulled from one of the two recursive cases.
--
-- If the helper functions return 'Node's, extracting bounds from the recursive
-- cases using 'deleteMinL' and 'deleteMaxR' (or a wrapper function like
-- 'extractBinL' or 'extractBinR') is an expensive operation. After traversing
-- and reconstructing the subtrees in the merging provcess, they need to be
-- re-traversed and reconstructed again to pull out one of the entries. A more
-- efficient option would be to fuse the traversals, returning an 'IntMap_'
-- the recursive case. Since an 'IntMap_' already contains its own bound pulled
-- out, we can just put the pieces together in constant time.
--
-- However, 'IntMap_' isn't a universally better choice for intermediate type.
-- When the outside key is kept, the 'IntMap_' needs to be converted back into
-- a 'Node', causing the same problem, but with insertion instead of deletion.
-- Both intermediate types are appropriate, often in the same merge operation,
-- depending only on whether the external bound has been kept or deleted.
-- 'Data.IntMap.Lazy.union' always keeps its keys so uses 'Node's everywhere.
-- 'Data.IntMap.Lazy.intersection' keeps its keys when they match, so
-- @go{L,R}Fused@ return 'Node's, but mismatched keys are dropped, so
-- @go{L,R}{1,2}@ return 'IntMap_'s. A function like
-- 'Data.IntMap.Lazy.differenceWith' needs two variants of @go{L,R}Fused@
-- since different matched keys are kept and deleted in the same merge
-- operation.
--
-- 'mergeA' is a particularly tricky function with regards to this decision.
-- Unlike the specialized merge operations, we don't statically know which keys
-- will be deleted, and unlike 'Data.IntMap.Lazy.differenceWith', we don't even
-- have that information at runtime: merge tactics return @f ('Maybe' a)@
-- where @f@ is an arbitrary 'Applicative', and we can't choose what actions to
-- combine with that value based on the value itself. (As an aside, a 'Monad'
-- bound would not help either despite providing exactly that kind of choice,
-- as the actions associated with maximum bounds need to be sequenced after the
-- actions of the recursive cases.) Therefore, we need to choose a
-- representation with a good average case. If keys are being deleted, then the
-- maps returned by the recursive cases will be smaller, making the overhead of
-- pulling new bounds out also smaller. Therefore, universally using 'Node's
-- has a smaller overhead than universally using 'IntMap_'s and so is a better
-- choice.
--
-- TODO: Theoretically, the recursive case could return both a 'Node' and an
-- 'IntMap_', stored in a lazy pair. This would allow selecting which to use
-- and only evaluating the one that is needed. It may also just result in
-- excessive work.
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
    , traverseMaybeMissingUKeyLazy
) where

import Prelude hiding (min, max)

import Data.IntMap.Internal

import Control.Applicative (liftA2, liftA3)
import Control.Monad ((<=<))
import qualified Control.Category as Category (Category(..))
#if MIN_VERSION_base (4,8,0)
import Data.Functor.Identity (Identity, runIdentity)
#else
import Data.Functor ((<$))
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
--
-- @since 0.5.9
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

-- | @since 0.5.9
instance (Applicative f, Monad f) => Category.Category (WhenMissing f) where
    {-# INLINE id #-}
    id = preserveMissing

    {-# INLINE (.) #-}
    missF . missG = WhenMissing
        (\k a -> do
            mb <- missingSingle missG k a
            case mb of
                Nothing -> return Nothing
                Just b -> missingSingle missF k b)
        (missingLeft missF <=< missingLeft missG)
        (missingRight missF <=< missingRight missG)
        (missingAllL missF <=< missingAllL missG)

-- | @since 0.5.9
instance (Applicative f, Monad f) => Functor (WhenMissing f a) where
    {-# INLINE fmap #-}
    fmap f miss = WhenMissing
        (\k a -> fmap f <$> missingSingle miss k a)
        (\l -> fmap f <$> missingLeft miss l)
        (\r -> fmap f <$> missingRight miss r)
        (\m -> fmap f <$> missingAllL miss m)

-- | Equivalent to @ReaderT Key (ReaderT x (ReaderT y (MaybeT f)))@
--
-- @since 0.5.9
instance (Applicative f, Monad f) => Applicative (WhenMissing f a) where
    {-# INLINE pure #-}
    pure x = WhenMissing
        (\_ _ -> pure (Just x))
        (\l -> pure (x <$ l))
        (\r -> pure (x <$ r))
        (\m -> pure (x <$ m))

    {-# INLINE (<*>) #-}
    missF <*> missX = traverseMaybeMissingUKeyLazy $ \k a ->
        liftA2 (<*>) (missingSingle missF k a) (missingSingle missX k a)

-- | Equivalent to @ReaderT Key (ReaderT x (ReaderT y (MaybeT f)))@
--
-- @since 0.5.9
instance (Applicative f, Monad f) => Monad (WhenMissing f a) where
#if !MIN_VERSION_base(4,8,0)
    {-# INLINE return #-}
    return = pure
#endif

    {-# INLINE (>>=) #-}
    missM >>= f = traverseMaybeMissingUKeyLazy $ \k a -> do
        mb <- missingSingle missM k a
        case mb of
            Nothing -> return Nothing
            Just b -> missingSingle (f b) k a

-- | The inverse of 'missingSingle'. Is in the @Internal@ module for type class
-- instances.
{-# INLINE traverseMaybeMissingUKeyLazy #-}
traverseMaybeMissingUKeyLazy :: Applicative f => (UKey -> a -> f (Maybe b)) -> WhenMissing f a b
traverseMaybeMissingUKeyLazy f = WhenMissing
    { missingAllL = start
    , missingLeft = goL
    , missingRight = goR
    , missingSingle = f }
  where
    start Empty = pure Empty
    start (NonEmpty min minV root) = liftA2 (maybe nodeToMapL (NonEmpty min)) (f (boundUKey min) minV) (goL root)

    goL Tip = pure Tip
    goL (Bin max maxV l r) = liftA3 (\l' r' maxV' -> maybe extractBinL (Bin max) maxV' l' r') (goL l) (goR r) (f (boundUKey max) maxV)

    goR Tip = pure Tip
    goR (Bin min minV l r) = liftA3 (\minV' l' r' -> maybe extractBinR (Bin min) minV' l' r') (f (boundUKey min) minV) (goL l) (goR r)

-- | A tactic for dealing with keys present in one map but not the other in
-- 'merge'.
--
-- A tactic of type @ SimpleWhenMissing a c @ is an abstract representation
-- of a function of type @ Key -> a -> Maybe c @.
--
-- @since 0.5.9
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
--
-- @since 0.5.9
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
--
-- @since 0.5.9
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
--
-- @since 0.5.9
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
        | otherwise = binNodeMapL (goLKeep l) (goR r)

    goRKeep Tip = Tip
    goRKeep (Bin min minV l r)
        | p (boundUKey min) minV = Bin min minV (goLKeep l) (goRKeep r)
        | otherwise = binMapNodeR (goL l) (goRKeep r)

    goL Tip = Empty
    goL (Bin max maxV l r)
        | p (boundUKey max) maxV = binL (goL l) (NonEmpty max maxV (goRKeep r))
        | otherwise = binL (goL l) (goR r)

    goR Tip = Empty
    goR (Bin min minV l r)
        | p (boundUKey min) minV = binR (NonEmpty min minV (goLKeep l)) (goR r)
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
--
-- @since 0.5.9
newtype WhenMatched f a b c = WhenMatched {
    matchedSingle :: UKey -> a -> b -> f (Maybe c)
}

-- | @since 0.5.9
instance (Applicative f, Monad f) => Category.Category (WhenMatched f a) where
    -- TODO: Expose this and it symmetric pair as @firstMatched@ and
    -- @secondMatched@?
    {-# INLINE id #-}
    id = WhenMatched (\_ _ b -> pure (Just b))

    {-# INLINE (.) #-}
    matchF . matchG = WhenMatched $ \k a b -> do
        mc <- matchedSingle matchG k a b
        case mc of
            Nothing -> return Nothing
            Just c' -> matchedSingle matchF k a c'

-- | @since 0.5.9
instance Functor f => Functor (WhenMatched f a b) where
    {-# INLINE fmap #-}
    fmap f match = WhenMatched (\k a b -> fmap f <$> matchedSingle match k a b)

-- | Equivalent to @ReaderT Key (ReaderT x (ReaderT y (MaybeT f)))@
--
-- @since 0.5.9
instance (Applicative f, Monad f) => Applicative (WhenMatched f a b) where
    {-# INLINE pure #-}
    pure x = WhenMatched (\_ _ _ -> pure (Just x))

    {-# INLINE (<*>) #-}
    matchF <*> matchX = WhenMatched $ \k a b ->
        liftA2 (<*>) (matchedSingle matchF k a b) (matchedSingle matchX k a b)

-- | Equivalent to @ReaderT Key (ReaderT x (ReaderT y (MaybeT f)))@
--
-- @since 0.5.9
instance (Applicative f, Monad f) => Monad (WhenMatched f a b) where
#if !MIN_VERSION_base(4,8,0)
    {-# INLINE return #-}
    return = pure
#endif

    {-# INLINE (>>=) #-}
    matchM >>= f = WhenMatched $ \k a b -> do
        mc <- matchedSingle matchM k a b
        case mc of
            Nothing -> return Nothing
            Just c -> matchedSingle (f c) k a b

-- | A tactic for dealing with keys present in both maps in 'merge'.
--
-- A tactic of type @ SimpleWhenMatched a b c @ is an abstract representation
-- of a function of type @ Key -> a -> b -> Maybe c @.
--
-- @since 0.5.9
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
--
-- @since 0.5.9
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

-- | An applicative version of 'merge'.
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
{-# INLINE mergeA #-}
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

    goInsertL1 !k v !_ !_ Tip = makeSingleton k (missingSingle miss1 (unbox k) v)
    goInsertL1 !k v !xorCache !min n@(Bin max maxV l r) = case compareMaxBound k max of
        InBound | xorCache < xorCacheMax -> liftA2 binNodeMapL (goInsertL1 k v xorCache min l) (missingAllR miss2 (NonEmpty max maxV r))
                | otherwise -> makeBinL max (missingSingle miss2 (boundUKey max) maxV) (missingLeft miss2 l) (goInsertR1 k v xorCacheMax max r)
        OutOfBound -> addMaxL min (Bound k) (missingSingle miss1 (unbox k) v) (missingLeft miss2 n)
        Matched -> makeBinL max (matchedSingle match (unbox k) v maxV) (missingLeft miss2 l) (missingRight miss2 r)
      where xorCacheMax = xor k max

    goInsertL2 !k v !_ !_ Tip = makeSingleton k (missingSingle miss2 (unbox k) v)
    goInsertL2 !k v !xorCache !min n@(Bin max maxV l r) = case compareMaxBound k max of
        InBound | xorCache < xorCacheMax -> liftA2 binNodeMapL (goInsertL2 k v xorCache min l) (missingAllR miss1 (NonEmpty max maxV r))
                | otherwise -> makeBinL max (missingSingle miss1 (boundUKey max) maxV) (missingLeft miss1 l) (goInsertR2 k v xorCacheMax max r)
        OutOfBound -> addMaxL min (Bound k) (missingSingle miss2 (unbox k) v) (missingLeft miss1 n)
        Matched -> makeBinL max (matchedSingle match (unbox k) maxV v) (missingLeft miss1 l) (missingRight miss1 r)
      where xorCacheMax = xor k max

    goInsertR1 !k v !_ !_ Tip = makeSingleton k (missingSingle miss1 (unbox k) v)
    goInsertR1 !k v !xorCache !max n@(Bin min minV l r) = case compareMinBound k min of
        InBound | xorCache < xorCacheMin -> liftA2 binMapNodeR (missingAllL miss2 (NonEmpty min minV l)) (goInsertR1 k v xorCache max r)
                | otherwise -> makeBinR min (missingSingle miss2 (boundUKey min) minV) (goInsertL1 k v xorCacheMin min l) (missingRight miss2 r)
        OutOfBound -> addMinR max (Bound k) (missingSingle miss1 (unbox k) v) (missingRight miss2 n)
        Matched -> makeBinR min (matchedSingle match (unbox k) v minV) (missingLeft miss2 l) (missingRight miss2 r)
      where xorCacheMin = xor k min

    goInsertR2 !k v !_ !_ Tip = makeSingleton k (missingSingle miss2 (unbox k) v)
    goInsertR2 !k v !xorCache !max n@(Bin min minV l r) = case compareMinBound k min of
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
