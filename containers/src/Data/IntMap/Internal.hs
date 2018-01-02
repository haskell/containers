{-# LANGUAGE CPP, BangPatterns #-}
#if !defined(TESTING) && __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMap.Internal
-- Copyright   :  Documentation & Interface (c) Daan Leijen 2002
--                Documentation (c) Andriy Palamarchuk 2008
--                Documentation & Implementation (c) Jonathan S. 2016
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- The contents of this module may change __in any way whatsoever__
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
-- @since 0.5.9
-----------------------------------------------------------------------------

-- [Note: Local 'go' functions and capturing]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Care must be taken when using 'go' function which captures an argument.
-- Sometimes (for example when the argument is passed to a data constructor,
-- as in insert), GHC heap-allocates more than necessary. Therefore C-- code
-- must be checked for increased allocation when creating and modifying such
-- functions.

module Data.IntMap.Internal where

import Control.DeepSeq (NFData(..))
import Control.Applicative (Applicative(..))

import Data.Monoid (Monoid(..))
import qualified Data.List (foldl')
import qualified Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup(..), stimesIdempotentMonoid)
#endif

import Data.Functor ((<$>))

import Data.Word (Word)
import qualified Data.Bits (xor)

import qualified Data.IntSet (IntSet, fromDistinctAscList, member, notMember)
import Utils.Containers.Internal.StrictPair (StrictPair(..))

import Prelude hiding (foldr, foldl, lookup, null, map, min, max)

type Key = Int

i2w :: Int -> Word
i2w = fromIntegral

-- We need to compare xors using unsigned comparisons
xor :: Key -> Key -> Word
xor a b = Data.Bits.xor (i2w a) (i2w b)

-- Phantom types used to separate the types of left and right nodes.
-- They are uninhabited simply to ensure that they are only used as type parameters.
newtype L = L L
newtype R = R R

-- | A map of integers to values @a@.
newtype IntMap a = IntMap (IntMap_ L a) deriving (Eq)
data IntMap_ t a = NonEmpty {-# UNPACK #-} !Key a !(Node t a) | Empty deriving (Eq)
data Node t a = Bin {-# UNPACK #-} !Key a !(Node L a) !(Node R a) | Tip deriving (Eq, Show)

instance Show a => Show (IntMap a) where
    show m = "fromList " ++ show (toList m)

instance Functor IntMap where
    fmap f (IntMap m) = IntMap (fmap f m)

instance Functor (IntMap_ t) where
    fmap _ Empty = Empty
    fmap f (NonEmpty min minV node) = NonEmpty min (f minV) (fmap f node)

instance Functor (Node t) where
    fmap _ Tip = Tip
    fmap f (Bin k v l r) = Bin k (f v) (fmap f l) (fmap f r)

instance Data.Foldable.Foldable IntMap where
    foldMap f = start
      where
        start (IntMap Empty) = mempty
        start (IntMap (NonEmpty _ minV root)) = f minV `mappend` goL root

        goL Tip = mempty
        goL (Bin _ maxV l r) = goL l `mappend` goR r `mappend` f maxV

        goR Tip = mempty
        goR (Bin _ minV l r) = f minV `mappend` goL l `mappend` goR r

    foldr = foldr
    foldl = foldl
#if MIN_VERSION_base(4,6,0)
    foldr' = foldr'
    foldl' = foldl'
#endif

instance Traversable IntMap where
    traverse f = start
      where
        start (IntMap Empty) = pure (IntMap Empty)
        start (IntMap (NonEmpty min minV node)) = (\minV' root' -> IntMap (NonEmpty min minV' root')) <$> f minV <*> goL node

        goL Tip = pure Tip
        goL (Bin max maxV l r) = (\l' r' v' -> Bin max v' l' r') <$> goL l <*> goR r <*> f maxV

        goR Tip = pure Tip
        goR (Bin min minV l r) = Bin min <$> f minV <*> goL l <*> goR r

instance Monoid (IntMap a) where
    mempty = empty
    mappend = union

#if MIN_VERSION_base(4,9,0)
instance Semigroup (IntMap a) where
    stimes = stimesIdempotentMonoid
#endif

instance NFData a => NFData (IntMap a) where
    rnf (IntMap Empty) = ()
    rnf (IntMap (NonEmpty _ v n)) = rnf v `seq` rnf n

instance NFData a => NFData (Node t a) where
    rnf Tip = ()
    rnf (Bin _ v l r) = rnf v `seq` rnf l `seq` rnf r

-- | /O(min(n,W))/. Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'
(!) :: IntMap a -> Key -> a
(!) m k = findWithDefault (error $ "IntMap.!: key " ++ show k ++ " is not an element of the map") k m

-- | Same as 'difference'.
(\\) :: IntMap a -> IntMap b -> IntMap a
(\\) = difference

-- | /O(1)/. Is the map empty?
--
-- > Data.IntMap.null empty             == True
-- > Data.IntMap.null (singleton 1 'a') == False
null :: IntMap a -> Bool
null (IntMap Empty) = True
null _ = False

-- | /O(n)/. Number of elements in the map.
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3
size :: IntMap a -> Int
size (IntMap Empty) = 0
size (IntMap (NonEmpty _ _ node)) = sizeNode node where
    sizeNode :: Node t a -> Int
    sizeNode Tip = 1
    sizeNode (Bin _ _ l r) = sizeNode l + sizeNode r

-- | /O(min(n,W))/. Is the key a member of the map?
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False
member :: Key -> IntMap a -> Bool
member k = k `seq` start
  where
    start (IntMap Empty) = False
    start (IntMap (NonEmpty min _ node))
        | k < min = False
        | k == min = True
        | otherwise = goL (xor min k) node

    goL !_ Tip = False
    goL !xorCache (Bin max _ l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | k > max = False
        | otherwise = True
      where xorCacheMax = xor k max

    goR !_ Tip = False
    goR !xorCache (Bin min _ l r)
        | k > min = if xorCache < xorCacheMin
                    then goR xorCache r
                    else goL xorCacheMin l
        | k < min = False
        | otherwise = True
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. Is the key not a member of the map?
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True
notMember :: Key -> IntMap a -> Bool
notMember k = k `seq` start
  where
    start (IntMap Empty) = True
    start (IntMap (NonEmpty min _ node))
        | k < min = True
        | k == min = False
        | otherwise = goL (xor min k) node

    goL !_ Tip = True
    goL !xorCache (Bin max _ l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | k > max = True
        | otherwise = False
      where xorCacheMax = xor k max

    goR !_ Tip = True
    goR !xorCache (Bin min _ l r)
        | k > min = if xorCache < xorCacheMin
                    then goR xorCache r
                    else goL xorCacheMin l
        | k < min = True
        | otherwise = False
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. Lookup the value at a key in the map. See also 'Data.Map.lookup'.
lookup :: Key -> IntMap a -> Maybe a
lookup k = k `seq` start
  where
    start (IntMap Empty) = Nothing
    start (IntMap (NonEmpty min minV node))
        | k < min = Nothing
        | k == min = Just minV
        | otherwise = goL (xor min k) node

    goL !_ Tip = Nothing
    goL !xorCache (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | k > max = Nothing
        | otherwise = Just maxV
      where xorCacheMax = xor k max

    goR !_ Tip = Nothing
    goR !xorCache (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then goR xorCache r
                    else goL xorCacheMin l
        | k < min = Nothing
        | otherwise = Just minV
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. The expression @'findWithDefault' def k map@
-- returns the value at key @k@ or returns @def@ when the key is not an
-- element of the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'
findWithDefault :: a -> Key -> IntMap a -> a
findWithDefault def k = k `seq` start
  where
    start (IntMap Empty) = def
    start (IntMap (NonEmpty min minV node))
        | k < min = def
        | k == min = minV
        | otherwise = goL (xor min k) node

    goL !_ Tip = def
    goL !xorCache (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | k > max = def
        | otherwise = maxV
      where xorCacheMax = xor k max

    goR !_ Tip = def
    goR !xorCache (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then goR xorCache r
                    else goL xorCacheMin l
        | k < min = def
        | otherwise = minV
      where  xorCacheMin = xor min k

-- | /O(log n)/. Find largest key smaller than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupLT 3 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLT 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
lookupLT :: Key -> IntMap a -> Maybe (Key, a)
lookupLT k = k `seq` start
  where
    start (IntMap Empty) = Nothing
    start (IntMap (NonEmpty min minV node))
        | min >= k = Nothing
        | otherwise = Just (goL (xor min k) min minV node)

    goL !_ min minV Tip = (min, minV)
    goL !xorCache min minV (Bin max maxV l r)
        | max < k = (max, maxV)
        | xorCache < xorCacheMax = goL xorCache min minV l
        | otherwise = goR xorCacheMax r min minV l
      where
        xorCacheMax = xor k max

    goR !_ Tip fMin fMinV fallback = getMax fMin fMinV fallback
    goR !xorCache (Bin min minV l r) fMin fMinV fallback
        | min >= k = getMax fMin fMinV fallback
        | xorCache < xorCacheMin = goR xorCache r min minV l
        | otherwise = goL xorCacheMin min minV l
      where
        xorCacheMin = xor min k

    getMax min minV Tip = (min, minV)
    getMax _   _   (Bin max maxV _ _) = (max, maxV)

-- | /O(log n)/. Find largest key smaller or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupLE 2 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLE 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupLE 5 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
lookupLE :: Key -> IntMap a -> Maybe (Key, a)
lookupLE k = k `seq` start
  where
    start (IntMap Empty) = Nothing
    start (IntMap (NonEmpty min minV node))
        | min > k = Nothing
        | otherwise = Just (goL (xor min k) min minV node)

    goL !_ min minV Tip = (min, minV)
    goL !xorCache min minV (Bin max maxV l r)
        | max <= k = (max, maxV)
        | xorCache < xorCacheMax = goL xorCache min minV l
        | otherwise = goR xorCacheMax r min minV l
      where
        xorCacheMax = xor k max

    goR !_ Tip fMin fMinV fallback = getMax fMin fMinV fallback
    goR !xorCache (Bin min minV l r) fMin fMinV fallback
        | min > k = getMax fMin fMinV fallback
        | xorCache < xorCacheMin = goR xorCache r min minV l
        | otherwise = goL xorCacheMin min minV l
      where
        xorCacheMin = xor min k

    getMax min minV Tip = (min, minV)
    getMax _   _   (Bin max maxV _ _) = (max, maxV)

-- | /O(log n)/. Find smallest key greater than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupGT 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGT 5 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGT :: Key -> IntMap a -> Maybe (Key, a)
lookupGT k = k `seq` start
  where
    start (IntMap Empty) = Nothing
    start (IntMap (NonEmpty min minV Tip))
        | min <= k = Nothing
        | otherwise = Just (min, minV)
    start (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | max <= k = Nothing
        | otherwise = Just (goR (xor k max) max maxV (Bin min minV l r))

    goL !_ Tip fMax fMaxV fallback = getMin fMax fMaxV fallback
    goL !xorCache (Bin max maxV l r) fMax fMaxV fallback
        | max <= k = getMin fMax fMaxV fallback
        | xorCache < xorCacheMax = goL xorCache l max maxV r
        | otherwise = goR xorCacheMax max maxV r
      where
        xorCacheMax = xor k max

    goR !_ max maxV Tip = (max, maxV)
    goR !xorCache max maxV (Bin min minV l r)
        | min > k = (min, minV)
        | xorCache < xorCacheMin = goR xorCache max maxV r
        | otherwise = goL xorCacheMin l max maxV r
      where
        xorCacheMin = xor min k

    getMin max maxV Tip = (max, maxV)
    getMin _   _   (Bin min minV _ _) = (min, minV)

-- | /O(log n)/. Find smallest key greater or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupGE 3 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupGE 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGE 6 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGE :: Key -> IntMap a -> Maybe (Key, a)
lookupGE k = k `seq` start
  where
    start (IntMap Empty) = Nothing
    start (IntMap (NonEmpty min minV Tip))
        | min < k = Nothing
        | otherwise = Just (min, minV)
    start (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | max < k = Nothing
        | otherwise = Just (goR (xor k max) max maxV (Bin min minV l r))

    goL !_ Tip fMax fMaxV fallback = getMin fMax fMaxV fallback
    goL !xorCache (Bin max maxV l r) fMax fMaxV fallback
        | max < k = getMin fMax fMaxV fallback
        | xorCache < xorCacheMax = goL xorCache l max maxV r
        | otherwise = goR xorCacheMax max maxV r
      where
        xorCacheMax = xor k max

    goR !_ max maxV Tip = (max, maxV)
    goR !xorCache max maxV (Bin min minV l r)
        | min >= k = (min, minV)
        | xorCache < xorCacheMin = goR xorCache max maxV r
        | otherwise = goL xorCacheMin l max maxV r
      where
        xorCacheMin = xor min k

    getMin max maxV Tip = (max, maxV)
    getMin _   _   (Bin min minV _ _) = (min, minV)

-- | /O(1)/. The empty map.
--
-- > empty      == fromList []
-- > size empty == 0
empty :: IntMap a
empty = IntMap Empty

-- | /O(min(n,W))/. Delete a key and its value from the map.
-- When the key is not a member of the map, the original map is returned.
delete :: Key -> IntMap a -> IntMap a
delete k = k `seq` start
  where
    start (IntMap Empty) = IntMap Empty
    start m@(IntMap (NonEmpty min _ Tip))
        | k == min = IntMap Empty
        | otherwise = m
    start m@(IntMap (NonEmpty min minV root@(Bin max maxV l r)))
        | k < min = m
        | k == min = let DR min' minV' root' = deleteMinL max maxV l r in IntMap (NonEmpty min' minV' root')
        | otherwise = IntMap (NonEmpty min minV (deleteL k (xor min k) root))

-- TODO: Does a strict pair work? My guess is not, as GHC was already
-- unboxing the tuple, but it would be simpler to use one of those.
-- | Without this specialized type (I was just using a tuple), GHC's
-- CPR correctly unboxed the tuple, but it couldn't unbox the returned
-- Key, leading to lots of inefficiency (3x slower than stock Data.IntMap)
data DeleteResult t a = DR {-# UNPACK #-} !Key a !(Node t a)

-- | /O(n+m)/. The (left-biased) union of two maps.
-- It prefers the first map when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
--
-- > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]
union :: IntMap a -> IntMap a -> IntMap a
union = start
  where
    start (IntMap Empty) m2 = m2
    start m1 (IntMap Empty) = m1
    start (IntMap (NonEmpty min1 minV1 root1)) (IntMap (NonEmpty min2 minV2 root2))
        | min1 < min2 = IntMap (NonEmpty min1 minV1 (goL2 minV2 min1 root1 min2 root2))
        | min1 > min2 = IntMap (NonEmpty min2 minV2 (goL1 minV1 min1 root1 min2 root2))
        | otherwise = IntMap (NonEmpty min1 minV1 (goLFused min1 root1 root2)) -- we choose min1 arbitrarily, as min1 == min2

    goL1 minV1 !min1 Tip !_    Tip = Bin min1 minV1 Tip Tip
    goL1 minV1 !min1 !n1 !min2 Tip = insertMinL (xor min1 min2) min1 minV1 n1
    goL1 minV1 !min1 !n1 !min2 n2@(Bin max2 _ _ _) | min1 > max2 = unionDisjointL minV1 min2 n2 min1 n1
    goL1 minV1 !min1 Tip !min2 !n2 = goInsertL1 min1 minV1 (xor min1 min2) min2 n2
    goL1 minV1 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
         LT | xor min1 min2 < xor min1 max2 -> Bin max2 maxV2 (goL1 minV1 min1 n1 min2 l2) r2 -- we choose min1 arbitrarily - we just need something from tree 1
            | max1 > max2 -> Bin max1 maxV1 l2 (goR2 maxV2 max1 (Bin min1 minV1 l1 r1) max2 r2)
            | max1 < max2 -> Bin max2 maxV2 l2 (goR1 maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2)
            | otherwise -> Bin max1 maxV1 l2 (goRFused max1 (Bin min1 minV1 l1 r1) r2) -- we choose max1 arbitrarily, as max1 == max2
         EQ | max1 > max2 -> Bin max1 maxV1 (goL1 minV1 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
            | max1 < max2 -> Bin max2 maxV2 (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
            | otherwise -> Bin max1 maxV1 (goL1 minV1 min1 l1 min2 l2) (goRFused max1 r1 r2) -- we choose max1 arbitrarily, as max1 == max2
         GT -> Bin max1 maxV1 (goL1 minV1 min1 l1 min2 n2) r1

    goL2 minV2 !_    Tip !min2 Tip = Bin min2 minV2 Tip Tip
    goL2 minV2 !min1 Tip !min2 !n2 = insertMinL (xor min1 min2) min2 minV2 n2
    goL2 minV2 !min1 n1@(Bin max1 _ _ _) !min2 !n2 | min2 > max1 = unionDisjointL minV2 min1 n1 min2 n2
    goL2 minV2 !min1 !n1 !min2 Tip = goInsertL2 min2 minV2 (xor min1 min2) min1 n1
    goL2 minV2 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
         GT | xor min1 min2 < xor min2 max1 -> Bin max1 maxV1 (goL2 minV2 min1 l1 min2 n2) r1 -- we choose min2 arbitrarily - we just need something from tree 2
            | max1 > max2 -> Bin max1 maxV1 l1 (goR2 maxV2 max1 r1 max2 (Bin min2 minV2 l2 r2))
            | max1 < max2 -> Bin max2 maxV2 l1 (goR1 maxV1 max1 r1 max2 (Bin min2 minV2 l2 r2))
            | otherwise -> Bin max1 maxV1 l1 (goRFused max1 r1 (Bin min2 minV2 l2 r2)) -- we choose max1 arbitrarily, as max1 == max2
         EQ | max1 > max2 -> Bin max1 maxV1 (goL2 minV2 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
            | max1 < max2 -> Bin max2 maxV2 (goL2 minV2 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
            | otherwise -> Bin max1 maxV1 (goL2 minV2 min1 l1 min2 l2) (goRFused max1 r1 r2) -- we choose max1 arbitrarily, as max1 == max2
         LT -> Bin max2 maxV2 (goL2 minV2 min1 n1 min2 l2) r2

    -- 'goLFused' is called instead of 'goL' if the minimums of the two trees are the same
    -- Note that because of this property, the trees cannot be disjoint, so we can skip most of the checks in 'goL'
    goLFused !_ Tip !n2 = n2
    goLFused !_ !n1 Tip = n1
    goLFused !min n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xor min max1) (xor min max2) of
        LT -> Bin max2 maxV2 (goLFused min n1 l2) r2
        EQ | max1 > max2 -> Bin max1 maxV1 (goLFused min l1 l2) (goR2 maxV2 max1 r1 max2 r2)
           | max1 < max2 -> Bin max2 maxV2 (goLFused min l1 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> Bin max1 maxV1 (goLFused min l1 l2) (goRFused max1 r1 r2) -- we choose max1 arbitrarily, as max1 == max2
        GT -> Bin max1 maxV1 (goLFused min l1 n2) r1

    goR1 maxV1 !max1 Tip !_    Tip = Bin max1 maxV1 Tip Tip
    goR1 maxV1 !max1 !n1 !max2 Tip = insertMaxR (xor max1 max2) max1 maxV1 n1
    goR1 maxV1 !max1 !n1 !max2 n2@(Bin min2 _ _ _) | min2 > max1 = unionDisjointR maxV1 max1 n1 max2 n2
    goR1 maxV1 !max1 Tip !max2 !n2 = goInsertR1 max1 maxV1 (xor max1 max2) max2 n2
    goR1 maxV1 !max1 n1@(Bin min1 minV1 l1 r1) !max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
         LT | xor min2 max1 > xor max1 max2 -> Bin min2 minV2 l2 (goR1 maxV1 max1 n1 max2 r2) -- we choose max1 arbitrarily - we just need something from tree 1
            | min1 < min2 -> Bin min1 minV1 (goL2 minV2 min1 (Bin max1 maxV1 l1 r1) min2 l2) r2
            | min1 > min2 -> Bin min2 minV2 (goL1 minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2) r2
            | otherwise -> Bin min1 minV1 (goLFused min1 (Bin max1 maxV1 l1 r1) l2) r2 -- we choose min1 arbitrarily, as min1 == min2
         EQ | min1 < min2 -> Bin min1 minV1 (goL2 minV2 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
            | min1 > min2 -> Bin min2 minV2 (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
            | otherwise -> Bin min1 minV1 (goLFused min1 l1 l2) (goR1 maxV1 max1 r1 max2 r2) -- we choose min1 arbitrarily, as min1 == min2
         GT -> Bin min1 minV1 l1 (goR1 maxV1 max1 r1 max2 n2)

    goR2 maxV2 !_    Tip !max2 Tip = Bin max2 maxV2 Tip Tip
    goR2 maxV2 !max1 Tip !max2 !n2 = insertMaxR (xor max1 max2) max2 maxV2 n2
    goR2 maxV2 !max1 n1@(Bin min1 _ _ _) !max2 !n2 | min1 > max2 = unionDisjointR maxV2 max2 n2 max1 n1
    goR2 maxV2 !max1 !n1 !max2 Tip = goInsertR2 max2 maxV2 (xor max1 max2) max1 n1
    goR2 maxV2 !max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
         GT | xor min1 max2 > xor max2 max1 -> Bin min1 minV1 l1 (goR2 maxV2 max1 r1 max2 n2) -- we choose max2 arbitrarily - we just need something from tree 2
            | min1 < min2 -> Bin min1 minV1 (goL2 minV2 min1 l1 min2 (Bin max2 maxV2 l2 r2)) r1
            | min1 > min2 -> Bin min2 minV2 (goL1 minV1 min1 l1 min2 (Bin max2 maxV2 l2 r2)) r1
            | otherwise -> Bin min1 minV1 (goLFused min1 l1 (Bin max2 maxV2 l2 r2)) r1 -- we choose min1 arbitrarily, as min1 == min2
         EQ | min1 < min2 -> Bin min1 minV1 (goL2 minV2 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
            | min1 > min2 -> Bin min2 minV2 (goL1 minV1 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
            | otherwise -> Bin min1 minV1 (goLFused min1 l1 l2) (goR2 maxV2 max1 r1 max2 r2) -- we choose min1 arbitrarily, as min1 == min2
         LT -> Bin min2 minV2 l2 (goR2 maxV2 max1 n1 max2 r2)

    -- 'goRFused' is called instead of 'goR' if the minimums of the two trees are the same
    -- Note that because of this property, the trees cannot be disjoint, so we can skip most of the checks in 'goR'
    goRFused !_ Tip n2 = n2
    goRFused !_ n1 Tip = n1
    goRFused !max n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 minV2 l2 r2) = case compareMSB (xor min1 max) (xor min2 max) of
        LT -> Bin min2 minV2 l2 (goRFused max n1 r2)
        EQ | min1 < min2 -> Bin min1 minV1 (goL2 minV2 min1 l1 min2 l2) (goRFused max r1 r2)
           | min1 > min2 -> Bin min2 minV2 (goL1 minV1 min1 l1 min2 l2) (goRFused max r1 r2)
           | otherwise -> Bin min1 minV1 (goLFused min1 l1 l2) (goRFused max r1 r2) -- we choose min1 arbitrarily, as min1 == min2
        GT -> Bin min1 minV1 l1 (goRFused max r1 n2)

    goInsertL1 k v !_        _    Tip = Bin k v Tip Tip
    goInsertL1 k v !xorCache min (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max maxV (goInsertL1 k v xorCache min l) r
                    else Bin max maxV l (goInsertR1 k v xorCacheMax max r)
        | k > max = if xor min max < xorCacheMax
                    then Bin k v (Bin max maxV l r) Tip
                    else Bin k v l (insertMaxR xorCacheMax max maxV r)
        | otherwise = Bin max v l r
      where xorCacheMax = xor k max

    goInsertR1 k v !_        _    Tip = Bin k v Tip Tip
    goInsertR1 k v !xorCache max (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min minV l (goInsertR1 k v xorCache max r)
                    else Bin min minV (goInsertL1 k v xorCacheMin min l) r
        | k < min = if xor min max < xorCacheMin
                    then Bin k v Tip (Bin min minV l r)
                    else Bin k v (insertMinL xorCacheMin min minV l) r
        | otherwise = Bin min v l r
      where xorCacheMin = xor min k

    goInsertL2 k v !_        _    Tip = Bin k v Tip Tip
    goInsertL2 k v !xorCache min (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max maxV (goInsertL2 k v xorCache min l) r
                    else Bin max maxV l (goInsertR2 k v xorCacheMax max r)
        | k > max = if xor min max < xorCacheMax
                    then Bin k v (Bin max maxV l r) Tip
                    else Bin k v l (insertMaxR xorCacheMax max maxV r)
        | otherwise = Bin max maxV l r
      where xorCacheMax = xor k max

    goInsertR2 k v !_        _    Tip = Bin k v Tip Tip
    goInsertR2 k v !xorCache max (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min minV l (goInsertR2 k v xorCache max r)
                    else Bin min minV (goInsertL2 k v xorCacheMin min l) r
        | k < min = if xor min max < xorCacheMin
                    then Bin k v Tip (Bin min minV l r)
                    else Bin k v (insertMinL xorCacheMin min minV l) r
        | otherwise = Bin min minV l r
      where xorCacheMin = xor min k

unionDisjointL :: a -> Key -> Node L a -> Key -> Node L a -> Node L a
unionDisjointL _ !_ Tip !_ !_ = error "Data.IntMap.unionDisjoint: impossible"
unionDisjointL minV2 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 Tip
    | xor min1 max1 < xor min2 max1 = Bin min2 minV2 n1 Tip
    | otherwise = Bin min2 minV2 l1 (insertMaxR (xor min2 max1) max1 maxV1 r1)
unionDisjointL minV2 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 (Bin max2 maxV2 l2 r2)
    | not (xor min1 max1 `ltMSB` xor min1 max2) = Bin max2 maxV2 l1 (unionDisjointR maxV1 max1 r1 max2 (Bin min2 minV2 l2 r2))
    | not (xor min2 max2 `ltMSB` xor min1 max2) = Bin max2 maxV2 (unionDisjointL minV2 min1 n1 min2 l2) r2
    | otherwise = Bin max2 maxV2 n1 (Bin min2 minV2 l2 r2)

unionDisjointR :: a -> Key -> Node R a -> Key -> Node R a -> Node R a
unionDisjointR _ !_ !_ !_ Tip = error "Data.IntMap.unionDisjoint: impossible"
unionDisjointR maxV1 !max1 Tip !max2 n2@(Bin min2 minV2 l2 r2)
    | xor min2 max2 < xor min2 max1 = Bin max1 maxV1 Tip n2
    | otherwise = Bin max1 maxV1 (insertMinL (xor min2 max1) min2 minV2 l2) r2
unionDisjointR maxV1 !max1 (Bin min1 minV1 l1 r1) !max2 n2@(Bin min2 minV2 l2 r2)
    | not (xor min2 max2 `ltMSB` xor min1 max2) = Bin min1 minV1 (unionDisjointL minV2 min1 (Bin max1 maxV1 l1 r1) min2 l2) r2
    | not (xor min1 max1 `ltMSB` xor min1 max2) = Bin min1 minV1 l1 (unionDisjointR maxV1 max1 r1 max2 n2)
    | otherwise = Bin min1 minV1 (Bin max1 maxV1 l1 r1) n2

-- | The union of a list of maps.
--
-- > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "b"), (5, "a"), (7, "C")]
-- > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
-- >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]
unions :: [IntMap a] -> IntMap a
unions = Data.List.foldl' union empty

-- | /O(n+m)/. Difference between two maps (based on keys).
--
-- > difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 3 "b"
difference :: IntMap a -> IntMap b -> IntMap a
difference = start
  where
    start (IntMap Empty) !_ = IntMap Empty
    start !m (IntMap Empty) = m
    start (IntMap (NonEmpty min1 minV1 root1)) (IntMap (NonEmpty min2 _ root2))
        | min1 < min2 = IntMap (NonEmpty min1 minV1 (goL2 min1 root1 min2 root2))
        | min1 > min2 = IntMap (goL1 minV1 min1 root1 min2 root2)
        | otherwise = IntMap (goLFused min1 root1 root2)

    goL1 minV1 min1 Tip min2 n2 = goLookupL min1 minV1 (xor min1 min2) n2
    goL1 minV1 min1 n1 _ Tip = NonEmpty min1 minV1 n1
    goL1 minV1 min1 n1@(Bin _ _ _ _) _ (Bin max2 _ _ _) | min1 > max2 = NonEmpty min1 minV1 n1
    goL1 minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 _ l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT | xor min2 min1 < xor min1 max2 -> goL1 minV1 min1 n1 min2 l2 -- min1 is arbitrary here - we just need something from tree 1
           | max1 > max2 -> r2lMap $ NonEmpty max1 maxV1 (goR2 max1 (Bin min1 minV1 l1 r1) max2 r2)
           | max1 < max2 -> r2lMap $ goR1 maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2
           | otherwise -> r2lMap $ goRFused max1 (Bin min1 minV1 l1 r1) r2
        EQ | max1 > max2 -> binL (goL1 minV1 min1 l1 min2 l2) (NonEmpty max1 maxV1 (goR2 max1 r1 max2 r2))
           | max1 < max2 -> binL (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> binL (goL1 minV1 min1 l1 min2 l2) (goRFused max1 r1 r2)
        GT -> binL (goL1 minV1 min1 l1 min2 n2) (NonEmpty max1 maxV1 r1)

    goL2 !_   Tip !_   !_  = Tip
    goL2 min1 n1  min2 Tip = deleteL min2 (xor min1 min2) n1
    goL2 _ n1@(Bin max1 _ _ _) min2 (Bin _ _ _ _) | min2 > max1 = n1
    goL2 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 _ l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT -> goL2 min1 n1 min2 l2
        EQ | max1 > max2 -> Bin max1 maxV1 (goL2 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | max1 < max2 -> case goR1 maxV1 max1 r1 max2 r2 of
                Empty -> goL2 min1 l1 min2 l2
                NonEmpty max' maxV' r' -> Bin max' maxV' (goL2 min1 l1 min2 l2) r'
           | otherwise -> case goRFused max1 r1 r2 of
                Empty -> goL2 min1 l1 min2 l2
                NonEmpty max' maxV' r' -> Bin max' maxV' (goL2 min1 l1 min2 l2) r'
        GT | xor min1 min2 < xor min2 max1 -> Bin max1 maxV1 (goL2 min1 l1 min2 n2) r1 -- min2 is arbitrary here - we just need something from tree 2
           | max1 > max2 -> Bin max1 maxV1 l1 (goR2 max1 r1 max2 (Bin min2 dummyV l2 r2))
           | max1 < max2 -> case goR1 maxV1 max1 r1 max2 (Bin min2 dummyV l2 r2) of
                Empty -> l1
                NonEmpty max' maxV' r' -> Bin max' maxV' l1 r'
           | otherwise -> case goRFused max1 r1 (Bin min2 dummyV l2 r2) of
                Empty -> l1
                NonEmpty max' maxV' r' -> Bin max' maxV' l1 r'

    goLFused !_ Tip !_ = Empty
    goLFused !_ (Bin max1 maxV1 l1 r1) Tip = case deleteMinL max1 maxV1 l1 r1 of
        DR min' minV' n' -> NonEmpty min' minV' n'
    goLFused !min n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 _ l2 r2) = case compareMSB (xor min max1) (xor min max2) of
        LT -> goLFused min n1 l2
        EQ | max1 > max2 -> binL (goLFused min l1 l2) (NonEmpty max1 maxV1 (goR2 max1 r1 max2 r2))
           | max1 < max2 -> binL (goLFused min l1 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> binL (goLFused min l1 l2) (goRFused max1 r1 r2) -- we choose max1 arbitrarily, as max1 == max2
        GT -> binL (goLFused min l1 n2) (NonEmpty max1 maxV1 r1)

    goR1 maxV1 max1 Tip max2 n2 = goLookupR max1 maxV1 (xor max1 max2) n2
    goR1 maxV1 max1 n1 _ Tip = NonEmpty max1 maxV1 n1
    goR1 maxV1 max1 n1@(Bin _ _ _ _) _ (Bin min2 _ _ _) | min2 > max1 = NonEmpty max1 maxV1 n1
    goR1 maxV1 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 _ l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT | xor min2 max1 > xor max1 max2 -> goR1 maxV1 max1 n1 max2 r2 -- max1 is arbitrary here - we just need something from tree 1
           | min1 < min2 -> l2rMap $ NonEmpty min1 minV1 (goL2 min1 (Bin max1 maxV1 l1 r1) min2 l2)
           | min1 > min2 -> l2rMap $ goL1 minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2
           | otherwise -> l2rMap $ goLFused min1 (Bin max1 maxV1 l1 r1) l2
        EQ | min1 < min2 -> binR (NonEmpty min1 minV1 (goL2 min1 l1 min2 l2)) (goR1 maxV1 max1 r1 max2 r2)
           | min1 > min2 -> binR (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> binR (goLFused min1 l1 l2) (goR1 maxV1 max1 r1 max2 r2)
        GT -> binR (NonEmpty min1 minV1 l1) (goR1 maxV1 max1 r1 max2 n2)

    goR2 !_   Tip !_   !_  = Tip
    goR2 max1 n1  max2 Tip = deleteR max2 (xor max1 max2) n1
    goR2 _ n1@(Bin min1 _ _ _) max2 (Bin _ _ _ _) | min1 > max2 = n1
    goR2 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 _ l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT -> goR2 max1 n1 max2 r2
        EQ | min1 < min2 -> Bin min1 minV1 (goL2 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | min1 > min2 -> case goL1 minV1 min1 l1 min2 l2 of
                Empty -> goR2 max1 r1 max2 r2
                NonEmpty min' minV' l' -> Bin min' minV' l' (goR2 max1 r1 max2 r2)
           | otherwise -> case goLFused min1 l1 l2 of
                Empty -> goR2 max1 r1 max2 r2
                NonEmpty min' minV' l' -> Bin min' minV' l' (goR2 max1 r1 max2 r2)
        GT | xor min1 max2 > xor max2 max1 -> Bin min1 minV1 l1 (goR2 max1 r1 max2 n2) -- max2 is arbitrary here - we just need something from tree 2
           | min1 < min2 -> Bin min1 minV1 (goL2 min1 l1 min2 (Bin max2 dummyV l2 r2)) r1
           | min1 > min2 -> case goL1 minV1 min1 l1 min2 (Bin max2 dummyV l2 r2) of
                Empty -> r1
                NonEmpty min' minV' l' -> Bin min' minV' l' r1
           | otherwise -> case goLFused min1 l1 (Bin max2 dummyV l2 r2) of
                Empty -> r1
                NonEmpty min' minV' l' -> Bin min' minV' l' r1

    goRFused !_ Tip !_ = Empty
    goRFused !_ (Bin min1 minV1 l1 r1) Tip = case deleteMaxR min1 minV1 l1 r1 of
        DR max' maxV' n' -> NonEmpty max' maxV' n'
    goRFused !max n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 _ l2 r2) = case compareMSB (xor min1 max) (xor min2 max) of
        LT -> goRFused max n1 r2
        EQ | min1 < min2 -> binR (NonEmpty min1 minV1 (goL2 min1 l1 min2 l2)) (goRFused max r1 r2)
           | min1 > min2 -> binR (goL1 minV1 min1 l1 min2 l2) (goRFused max r1 r2)
           | otherwise -> binR (goLFused min1 l1 l2) (goRFused max r1 r2) -- we choose min1 arbitrarily, as min1 == min2
        GT -> binR (NonEmpty min1 minV1 l1) (goRFused max r1 n2)

    goLookupL k v !_ Tip = NonEmpty k v Tip
    goLookupL k v !xorCache (Bin max _ l r)
        | k < max = if xorCache < xorCacheMax
                    then goLookupL k v xorCache l
                    else goLookupR k v xorCacheMax r
        | k > max = NonEmpty k v Tip
        | otherwise = Empty
      where xorCacheMax = xor k max

    goLookupR k v !_ Tip = NonEmpty k v Tip
    goLookupR k v !xorCache (Bin min _ l r)
        | k > min = if xorCache < xorCacheMin
                    then goLookupR k v xorCache r
                    else goLookupL k v xorCacheMin l
        | k < min = NonEmpty k v Tip
        | otherwise = Empty
      where xorCacheMin = xor min k

    dummyV = error "impossible"

-- | /O(n+m)/. The (left-biased) intersection of two maps (based on keys).
--
-- > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"
intersection :: IntMap a -> IntMap b -> IntMap a
intersection = start
  where
    start (IntMap Empty) !_ = IntMap Empty
    start !_ (IntMap Empty) = IntMap Empty
    start (IntMap (NonEmpty min1 minV1 root1)) (IntMap (NonEmpty min2 _ root2))
        | min1 < min2 = IntMap (fromSketchy (goL2 min1 root1 min2 root2))
        | min1 > min2 = IntMap (fromSketchy (goL1 minV1 min1 root1 min2 root2))
        | otherwise = IntMap (NonEmpty min1 minV1 (goLFused min1 root1 root2)) -- we choose min1 arbitrarily, as min1 == min2

    -- TODO: This scheme might produce lots of unnecessary l2r and r2l calls. This should be rectified.

    goL1 _     !_   !_  !_   Tip = toSketchy Empty
    goL1 minV1 min1 Tip min2 n2  = goLookupL1 min1 minV1 (xor min1 min2) n2
    goL1 _ min1 (Bin _ _ _ _) _ (Bin max2 _ _ _) | min1 > max2 = toSketchy Empty
    goL1 minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 _ l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT | xor min2 min1 < xor min1 max2 -> goL1 minV1 min1 n1 min2 l2 -- min1 is arbitrary here - we just need something from tree 1
           | max1 > max2 -> r2lSketchyMap $ goR2 max1 (Bin min1 minV1 l1 r1) max2 r2
           | max1 < max2 -> r2lSketchyMap $ goR1 maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2
           | otherwise -> toSketchy $ r2lMap $ NonEmpty max1 maxV1 (goRFused max1 (Bin min1 minV1 l1 r1) r2)
        EQ | max1 > max2 -> sketchyBinL (goL1 minV1 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | max1 < max2 -> sketchyBinL (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> case fromSketchy (goL1 minV1 min1 l1 min2 l2) of
                Empty -> toSketchy $ r2lMap (NonEmpty max1 maxV1 (goRFused max1 r1 r2))
                NonEmpty min' minV' l' -> toSketchy (NonEmpty min' minV' (Bin max1 maxV1 l' (goRFused max1 r1 r2)))
        GT -> goL1 minV1 min1 l1 min2 n2

    goL2 !_   Tip !_   !_  = toSketchy Empty
    goL2 min1 n1  min2 Tip = goLookupL2 min2 (xor min1 min2) n1
    goL2 _ (Bin max1 _ _ _) min2 (Bin _ _ _ _) | min2 > max1 = toSketchy Empty
    goL2 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 _ l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT -> goL2 min1 n1 min2 l2
        EQ | max1 > max2 -> sketchyBinL (goL2 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | max1 < max2 -> sketchyBinL (goL2 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> case fromSketchy (goL2 min1 l1 min2 l2) of
                Empty -> toSketchy (r2lMap (NonEmpty max1 maxV1 (goRFused max1 r1 r2)))
                NonEmpty min' minV' l' -> toSketchy (NonEmpty min' minV' (Bin max1 maxV1 l' (goRFused max1 r1 r2)))
        GT | xor min1 min2 < xor min2 max1 -> goL2 min1 l1 min2 n2 -- min2 is arbitrary here - we just need something from tree 2
           | max1 > max2 -> r2lSketchyMap $ goR2 max1 r1 max2 (Bin min2 dummyV l2 r2)
           | max1 < max2 -> r2lSketchyMap $ goR1 maxV1 max1 r1 max2 (Bin min2 dummyV l2 r2)
           | otherwise -> toSketchy $ r2lMap $ NonEmpty max1 maxV1 (goRFused max1 r1 (Bin min2 dummyV l2 r2))

    goLFused !_ Tip !_ = Tip
    goLFused !_ !_ Tip = Tip
    goLFused !min n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 _ l2 r2) = case compareMSB (xor min max1) (xor min max2) of
            LT -> goLFused min n1 l2
            EQ | max1 > max2 -> case fromSketchy (goR2 max1 r1 max2 r2) of
                    Empty -> l'
                    NonEmpty max' maxV' r' -> Bin max' maxV' l' r'
               | max1 < max2 -> case fromSketchy (goR1 maxV1 max1 r1 max2 r2) of
                    Empty -> l'
                    NonEmpty max' maxV' r' -> Bin max' maxV' l' r'
               | otherwise -> Bin max1 maxV1 l' (goRFused max1 r1 r2) -- we choose max1 arbitrarily, as max1 == max2
             where
               l' = goLFused min l1 l2
            GT -> goLFused min l1 n2

    goR1 _     !_   !_  !_   Tip = toSketchy Empty
    goR1 maxV1 max1 Tip max2 n2  = goLookupR1 max1 maxV1 (xor max1 max2) n2
    goR1 _ max1 (Bin _ _ _ _) _ (Bin min2 _ _ _) | min2 > max1 = toSketchy Empty
    goR1 maxV1 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 _ l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT | xor min2 max1 > xor max1 max2 -> goR1 maxV1 max1 n1 max2 r2 -- max1 is arbitrary here - we just need something from tree 1
           | min1 < min2 -> l2rSketchyMap $ goL2 min1 (Bin max1 maxV1 l1 r1) min2 l2
           | min1 > min2 -> l2rSketchyMap $ goL1 minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2
           | otherwise -> toSketchy $ l2rMap $ NonEmpty min1 minV1 (goLFused min1 (Bin max1 maxV1 l1 r1) l2)
        EQ | min1 < min2 -> sketchyBinR (goL2 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | min1 > min2 -> sketchyBinR (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> case fromSketchy (goR1 maxV1 max1 r1 max2 r2) of
                Empty -> toSketchy $ l2rMap (NonEmpty min1 minV1 (goLFused min1 l1 l2))
                NonEmpty max' maxV' r' -> toSketchy $ NonEmpty max' maxV' (Bin min1 minV1 (goLFused min1 l1 l2) r')
        GT -> goR1 maxV1 max1 r1 max2 n2

    goR2 !_   Tip !_   !_  = toSketchy Empty
    goR2 max1 n1  max2 Tip = goLookupR2 max2 (xor max1 max2) n1
    goR2 _ (Bin min1 _ _ _) max2 (Bin _ _ _ _) | min1 > max2 = toSketchy Empty
    goR2 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 _ l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT -> goR2 max1 n1 max2 r2
        EQ | min1 < min2 -> sketchyBinR (goL2 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | min1 > min2 -> sketchyBinR (goL1 minV1 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | otherwise -> case fromSketchy (goR2 max1 r1 max2 r2) of
                Empty -> toSketchy $ l2rMap (NonEmpty min1 minV1 (goLFused min1 l1 l2))
                NonEmpty max' maxV' r' -> toSketchy $ NonEmpty max' maxV' (Bin min1 minV1 (goLFused min1 l1 l2) r')
        GT | xor min1 max2 > xor max2 max1 -> goR2 max1 r1 max2 n2 -- max2 is arbitrary here - we just need something from tree 2
           | min1 < min2 -> l2rSketchyMap $ goL2 min1 l1 min2 (Bin max2 dummyV l2 r2)
           | min1 > min2 -> l2rSketchyMap $ goL1 minV1 min1 l1 min2 (Bin max2 dummyV l2 r2)
           | otherwise -> toSketchy $ l2rMap $ NonEmpty min1 minV1 (goLFused min1 l1 (Bin max2 dummyV l2 r2))

    goRFused !_ Tip !_ = Tip
    goRFused !_ !_ Tip = Tip
    goRFused !max n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 _ l2 r2) = case compareMSB (xor min1 max) (xor min2 max) of
            LT -> goRFused max n1 r2
            EQ | min1 < min2 -> case fromSketchy (goL2 min1 l1 min2 l2) of
                    Empty -> r'
                    NonEmpty min' minV' l' -> Bin min' minV' l' r'
               | min1 > min2 -> case fromSketchy (goL1 minV1 min1 l1 min2 l2) of
                    Empty -> r'
                    NonEmpty min' minV' l' -> Bin min' minV' l' r'
               | otherwise -> Bin min1 minV1 (goLFused min1 l1 l2) r' -- we choose max1 arbitrarily, as max1 == max2
             where
               r' = goRFused max r1 r2
            GT -> goRFused max r1 n2

    goLookupL1 !_ _ !_ Tip = toSketchy Empty
    goLookupL1 k v !xorCache (Bin max _ l r)
        | k < max = if xorCache < xorCacheMax
                    then goLookupL1 k v xorCache l
                    else goLookupR1 k v xorCacheMax r
        | k > max = toSketchy Empty
        | otherwise = toSketchy $ NonEmpty k v Tip
      where xorCacheMax = xor k max

    goLookupR1 !_ _ !_ Tip = toSketchy Empty
    goLookupR1 k v !xorCache (Bin min _ l r)
        | k > min = if xorCache < xorCacheMin
                    then goLookupR1 k v xorCache r
                    else goLookupL1 k v xorCacheMin l
        | k < min = toSketchy Empty
        | otherwise = toSketchy $ NonEmpty k v Tip
      where xorCacheMin = xor min k

    goLookupL2 !_ !_ Tip = toSketchy Empty
    goLookupL2 k !xorCache (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then goLookupL2 k xorCache l
                    else goLookupR2 k xorCacheMax r
        | k > max = toSketchy Empty
        | otherwise = toSketchy (NonEmpty k maxV Tip)
      where xorCacheMax = xor k max

    goLookupR2 !_ !_ Tip = toSketchy Empty
    goLookupR2 k !xorCache (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then goLookupR2 k xorCache r
                    else goLookupL2 k xorCacheMin l
        | k < min = toSketchy Empty
        | otherwise = toSketchy (NonEmpty k minV Tip)
      where xorCacheMin = xor min k

    dummyV = error "impossible"

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
--
-- For example,
--
-- > elems map = foldr (:) [] map
--
-- > let f a len = len + (length a)
-- > foldr f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldr :: (a -> b -> b) -> b -> IntMap a -> b
foldr f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty _ minV root)) = f minV (goL root z)

    goL Tip acc = acc
    goL (Bin _ maxV l r) acc = goL l (goR r (f maxV acc))

    goR Tip acc = acc
    goR (Bin _ minV l r) acc = f minV (goL l (goR r acc))

-- | /O(n)/. Fold the values in the map using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
--
-- For example,
--
-- > elems = reverse . foldl (flip (:)) []
--
-- > let f len a = len + (length a)
-- > foldl f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldl :: (a -> b -> a) -> a -> IntMap b -> a
foldl f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty _ minV root)) = goL (f z minV) root

    goL acc Tip = acc
    goL acc (Bin _ maxV l r) = f (goR (goL acc l) r) maxV

    goR acc Tip = acc
    goR acc (Bin _ minV l r) = goR (goL (f acc minV) l) r

-- | /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
--
-- For example,
--
-- > keys map = foldrWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldrWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"
foldrWithKey :: (Key -> a -> b -> b) -> b -> IntMap a -> b
foldrWithKey f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty min minV root)) = f min minV (goL root z)

    goL Tip acc = acc
    goL (Bin max maxV l r) acc = goL l (goR r (f max maxV acc))

    goR Tip acc = acc
    goR (Bin min minV l r) acc = f min minV (goL l (goR r acc))

-- | /O(n)/. Fold the keys and values in the map using the given left-associative
-- binary operator, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
--
-- For example,
--
-- > keys = reverse . foldlWithKey (\ks k x -> k:ks) []
--
-- > let f result k a = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldlWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (3:b)(5:a)"
foldlWithKey :: (a -> Key -> b -> a) -> a -> IntMap b -> a
foldlWithKey f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty min minV root)) = goL (f z min minV) root

    goL acc Tip = acc
    goL acc (Bin max maxV l r) = f (goR (goL acc l) r) max maxV

    goR acc Tip = acc
    goR acc (Bin min minV l r) = goR (goL (f acc min minV) l) r

-- | /O(n)/. Fold the keys and values in the map using the given monoid, such that
--
-- @'foldMapWithKey' f = 'Prelude.fold' . 'mapWithKey' f@
--
-- This can be an asymptotically faster than 'foldrWithKey' or 'foldlWithKey' for some monoids.
foldMapWithKey :: Monoid m => (Key -> a -> m) -> IntMap a -> m
foldMapWithKey f = start
  where
    start (IntMap Empty) = mempty
    start (IntMap (NonEmpty min minV root)) = f min minV `mappend` goL root

    goL Tip = mempty
    goL (Bin max maxV l r) = goL l `mappend` goR r `mappend` f max maxV

    goR Tip = mempty
    goR (Bin min minV l r) = f min minV `mappend` goL l `mappend` goR r

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> IntMap a -> b
foldr' f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty _ minV root)) = f minV $! goL root $! z

    goL Tip acc = acc
    goL (Bin _ maxV l r) acc = goL l $! goR r $! f maxV $! acc

    goR Tip acc = acc
    goR (Bin _ minV l r) acc = f minV $! goL l $! goR r $! acc

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> b -> a) -> a -> IntMap b -> a
foldl' f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty _ minV root)) = s goL (s f z minV) root

    goL acc Tip = acc
    goL acc (Bin _ maxV l r) = s f (s goR (s goL acc l) r) maxV

    goR acc Tip = acc
    goR acc (Bin _ minV l r) = s goR (s goL (s f acc minV) l) r

    s = ($!)

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldrWithKey' :: (Key -> a -> b -> b) -> b -> IntMap a -> b
foldrWithKey' f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty min minV root)) = f min minV $! goL root $! z

    goL Tip acc = acc
    goL (Bin max maxV l r) acc = goL l $! goR r $! f max maxV $! acc

    goR Tip acc = acc
    goR (Bin min minV l r) acc = f min minV $! goL l $! goR r $! acc

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlWithKey' :: (a -> Key -> b -> a) -> a -> IntMap b -> a
foldlWithKey' f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty min minV root)) = s goL (s f z min minV) root

    goL acc Tip = acc
    goL acc (Bin max maxV l r) = s f (s goR (s goL acc l) r) max maxV

    goR acc Tip = acc
    goR acc (Bin min minV l r) = s goR (s goL (s f acc min minV) l) r

    s = ($!)

-- TODO: make the conversion functions good producers

-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
-- Subject to list fusion.
--
-- > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
-- > elems empty == []
elems :: IntMap a -> [a]
elems = foldr (:) []

-- | /O(n)/. Return all keys of the map in ascending order. Subject to list
-- fusion.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []
keys :: IntMap a -> [Key]
keys = foldrWithKey (\k _ l -> k : l) []

-- | /O(n)/. An alias for 'toAscList'. Returns all key\/value pairs in the
-- map in ascending key order. Subject to list fusion.
--
-- > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > assocs empty == []
assocs :: IntMap a -> [(Key, a)]
assocs = toAscList

-- | /O(n*min(n,W))/. The set of all keys of the map.
--
-- > keysSet (fromList [(5,"a"), (3,"b")]) == Data.IntSet.fromList [3,5]
-- > keysSet empty == Data.IntSet.empty
keysSet :: IntMap a -> Data.IntSet.IntSet
keysSet = Data.IntSet.fromDistinctAscList . keys

-- | /O(n)/. Convert the map to a list of key\/value pairs.
toList :: IntMap a -> [(Key, a)]
toList = toAscList

-- | /O(n)/. Convert the map to a list of key\/value pairs where the
-- keys are in ascending order. Subject to list fusion.
--
-- > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
toAscList :: IntMap a -> [(Key, a)]
toAscList = foldrWithKey (\k v l -> (k, v) : l) []

-- | /O(n)/. Convert the map to a list of key\/value pairs where the keys
-- are in descending order. Subject to list fusion.
--
-- > toDescList (fromList [(5,"a"), (3,"b")]) == [(5,"a"), (3,"b")]
toDescList :: IntMap a -> [(Key, a)]
toDescList = foldlWithKey (\l k v -> (k, v) : l) []

-- | A stack used in the in-order building of IntMaps.
data BuildStack a = Push {-# UNPACK #-} !Key a !(Node L a) !(BuildStack a) | StackBase

pushBuildStack :: Word -> Key -> a -> Node R a -> BuildStack a -> BuildStack a
pushBuildStack !xorCache !k v !r (Push min minV l stk)
    | xor min k < xorCache = pushBuildStack xorCache k v (Bin min minV l r) stk
pushBuildStack !_ !k v Tip !stk = Push k v Tip stk
pushBuildStack !_ !k v (Bin min minV l r) !stk = Push min minV (Bin k v l r) stk

completeBuildStack :: Key -> a -> Node R a -> BuildStack a -> IntMap_ L a
completeBuildStack !max maxV !r StackBase = r2lMap (NonEmpty max maxV r)
completeBuildStack !max maxV !r (Push min minV l stk) = completeBuildStack max maxV (Bin min minV l r) stk

-- | /O(n)/. Filter all values that satisfy some predicate.
--
-- > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > filter (> "x") (fromList [(5,"a"), (3,"b")]) == empty
-- > filter (< "a") (fromList [(5,"a"), (3,"b")]) == empty
filter :: (a -> Bool) -> IntMap a -> IntMap a
filter p = filterWithKey (const p)

-- | /O(n)/. Filter all keys\/values that satisfy some predicate.
--
-- > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
filterWithKey :: (Key -> a -> Bool) -> IntMap a -> IntMap a
filterWithKey p = start
  where
    start (IntMap Empty) = IntMap Empty
    start (IntMap (NonEmpty min minV root))
        | p min minV = IntMap (NonEmpty min minV (goL root))
        | otherwise = IntMap (goDeleteL root)

    goL Tip = Tip
    goL (Bin max maxV l r)
        | p max maxV = Bin max maxV (goL l) (goR r)
        | otherwise = case goDeleteR r of
            Empty -> goL l
            NonEmpty max' maxV' r' -> Bin max' maxV' (goL l) r'

    goR Tip = Tip
    goR (Bin min minV l r)
        | p min minV = Bin min minV (goL l) (goR r)
        | otherwise = case goDeleteL l of
            Empty -> goR r
            NonEmpty min' minV' l' -> Bin min' minV' l' (goR r)

    goDeleteL Tip = Empty
    goDeleteL (Bin max maxV l r)
        | p max maxV = case goDeleteL l of
            Empty -> case goR r of
                Tip -> NonEmpty max maxV Tip
                Bin minI minVI lI rI -> NonEmpty minI minVI (Bin max maxV lI rI)
            NonEmpty min minV l' -> NonEmpty min minV (Bin max maxV l' (goR r))
        | otherwise = binL (goDeleteL l) (goDeleteR r)

    goDeleteR Tip = Empty
    goDeleteR (Bin min minV l r)
        | p min minV = case goDeleteR r of
            Empty -> case goL l of
                Tip -> NonEmpty min minV Tip
                Bin maxI maxVI lI rI -> NonEmpty maxI maxVI (Bin min minV lI rI)
            NonEmpty max maxV r' -> NonEmpty max maxV (Bin min minV (goL l) r')
        | otherwise = binR (goDeleteL l) (goDeleteR r)

-- | /O(n+m)/. The restriction of a map to the keys in a set.
--
-- @
-- m `restrictKeys` s = 'filterWithKey' (\k _ -> k `'IntSet.member'` s) m
-- @
--
-- @since 0.5.8
restrictKeys :: IntMap a -> Data.IntSet.IntSet -> IntMap a
restrictKeys m s = filterWithKey (\k _ -> Data.IntSet.member k s) m

-- | Remove all the keys in a given set from a map.
--
-- @
-- m `withoutKeys` s = 'filterWithKey' (\k _ -> k `'IntSet.notMember'` s) m
-- @
--
-- @since 0.5.8
withoutKeys :: IntMap a -> Data.IntSet.IntSet -> IntMap a
withoutKeys m s = filterWithKey (\k _ -> Data.IntSet.notMember k s) m

-- | /O(n)/. Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
partition :: (a -> Bool) -> IntMap a -> (IntMap a, IntMap a)
partition p = partitionWithKey (const p)

-- | /O(n)/. Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
-- > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
partitionWithKey :: (Key -> a -> Bool) -> IntMap a -> (IntMap a, IntMap a)
partitionWithKey p = start
  where
    start (IntMap Empty) = (IntMap Empty, IntMap Empty)
    start (IntMap (NonEmpty min minV root))
        | p min minV = let t :*: f = goTrueL root
                       in (IntMap (NonEmpty min minV t), IntMap f)
        | otherwise  = let t :*: f = goFalseL root
                       in (IntMap t, IntMap (NonEmpty min minV f))

    goTrueL Tip = Tip :*: Empty
    goTrueL (Bin max maxV l r)
        | p max maxV = let tl :*: fl = goTrueL l
                           tr :*: fr = goTrueR r
                       in Bin max maxV tl tr :*: binL fl fr
        | otherwise = let tl :*: fl = goTrueL l
                          tr :*: fr = goFalseR r
                          t = case tr of
                            Empty -> tl
                            NonEmpty max' maxV' r' -> Bin max' maxV' tl r'
                          f = case fl of
                            Empty -> r2lMap $ NonEmpty max maxV fr
                            NonEmpty min' minV' l' -> NonEmpty min' minV' (Bin max maxV l' fr)
                      in t :*: f

    goTrueR Tip = Tip :*: Empty
    goTrueR (Bin min minV l r)
        | p min minV = let tl :*: fl = goTrueL l
                           tr :*: fr = goTrueR r
                       in Bin min minV tl tr :*: binR fl fr
        | otherwise = let tl :*: fl = goFalseL l
                          tr :*: fr = goTrueR r
                          t = case tl of
                            Empty -> tr
                            NonEmpty min' minV' l' -> Bin min' minV' l' tr
                          f = case fr of
                            Empty -> l2rMap $ NonEmpty min minV fl
                            NonEmpty max' maxV' r' -> NonEmpty max' maxV' (Bin min minV fl r')
                      in t :*: f

    goFalseL Tip = Empty :*: Tip
    goFalseL (Bin max maxV l r)
        | p max maxV = let tl :*: fl = goFalseL l
                           tr :*: fr = goTrueR r
                           t = case tl of
                             Empty -> r2lMap $ NonEmpty max maxV tr
                             NonEmpty min' minV' l' -> NonEmpty min' minV' (Bin max maxV l' tr)
                           f = case fr of
                             Empty -> fl
                             NonEmpty max' maxV' r' -> Bin max' maxV' fl r'
                       in t :*: f
        | otherwise = let tl :*: fl = goFalseL l
                          tr :*: fr = goFalseR r
                      in binL tl tr :*: Bin max maxV fl fr

    goFalseR Tip = Empty :*: Tip
    goFalseR (Bin min minV l r)
        | p min minV = let tl :*: fl = goTrueL l
                           tr :*: fr = goFalseR r
                           t = case tr of
                             Empty -> l2rMap $ NonEmpty min minV tl
                             NonEmpty max' maxV' r' -> NonEmpty max' maxV' (Bin min minV tl r')
                           f = case fl of
                             Empty -> fr
                             NonEmpty min' minV' l' -> Bin min' minV' l' fr
                       in t :*: f
        | otherwise = let tl :*: fl = goFalseL l
                          tr :*: fr = goFalseR r
                      in binR tl tr :*: Bin min minV fl fr

-- | /O(min(n,W))/. The expression (@'split' k map@) is a pair @(map1,map2)@
-- where all keys in @map1@ are lower than @k@ and all keys in
-- @map2@ larger than @k@. Any key equal to @k@ is found in neither @map1@ nor @map2@.
--
-- > split 2 (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3,"b"), (5,"a")])
-- > split 3 (fromList [(5,"a"), (3,"b")]) == (empty, singleton 5 "a")
-- > split 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > split 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", empty)
-- > split 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], empty)
split :: Key -> IntMap a -> (IntMap a, IntMap a)
split k m = case splitLookup k m of
    (lt, _, gt) -> (lt, gt)


data SplitLookup a = SplitLookup !(IntMap a) !(Maybe a) !(IntMap a)

mapLT :: (IntMap a -> IntMap a) -> SplitLookup a -> SplitLookup a
mapLT f (SplitLookup lt fnd gt) = SplitLookup (f lt) fnd gt
{-# INLINE mapLT #-}

mapGT :: (IntMap a -> IntMap a) -> SplitLookup a -> SplitLookup a
mapGT f (SplitLookup lt fnd gt) = SplitLookup lt fnd (f gt)
{-# INLINE mapGT #-}

-- | /O(min(n,W))/. Performs a 'split' but also returns whether the pivot
-- key was found in the original map.
--
-- > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (empty, Nothing, fromList [(3,"b"), (5,"a")])
-- > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (empty, Just "b", singleton 5 "a")
-- > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Nothing, singleton 5 "a")
-- > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", empty)
-- > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], Nothing, empty)
splitLookup :: Key -> IntMap a -> (IntMap a, Maybe a, IntMap a)
splitLookup k = k `seq` start
  where
    start (IntMap Empty) = (IntMap Empty, Nothing, IntMap Empty)
    start m@(IntMap (NonEmpty min minV root))
        | k > min = case root of
            Tip -> (m, Nothing, IntMap Empty)
            Bin max maxV l r | k < max -> let (DR glb glbV lt, eq, DR lub lubV gt) = go (xor min k) min minV (xor k max) max maxV l r
                                          in (IntMap (r2lMap (NonEmpty glb glbV lt)), eq, IntMap (NonEmpty lub lubV gt))
                             | k > max -> (m, Nothing, IntMap Empty)
                             | otherwise -> let DR max' maxV' root' = deleteMaxR min minV l r
                                            in (IntMap (r2lMap (NonEmpty max' maxV' root')), Just maxV, IntMap Empty)

        | k < min = (IntMap Empty, Nothing, m)
        | otherwise = case root of
            Tip -> (IntMap Empty, Just minV, IntMap Empty)
            Bin max maxV l r -> let DR min' minV' root' = deleteMinL max maxV l r
                                in (IntMap Empty, Just minV, IntMap (NonEmpty min' minV' root'))

    go xorCacheMin min minV xorCacheMax max maxV l r
        | xorCacheMin < xorCacheMax = case l of
            Tip -> (DR min minV Tip, Nothing, r2lDR (DR max maxV r))
            Bin maxI maxVI lI rI
                | k < maxI -> let (lt, eq, DR minI minVI gt) = go xorCacheMin min minV (xor k maxI) maxI maxVI lI rI
                              in (lt, eq, DR minI minVI (Bin max maxV gt r))
                | k > maxI -> (l2rDR (DR min minV l), Nothing, r2lDR (DR max maxV r))
                | otherwise -> (deleteMaxR min minV lI rI, Just maxVI, r2lDR (DR max maxV r))
        | otherwise = case r of
            Tip -> (l2rDR (DR min minV l), Nothing, DR max maxV Tip)
            Bin minI minVI lI rI
                | k > minI -> let (DR maxI maxVI lt, eq, gt) = go (xor minI k) minI minVI xorCacheMax max maxV lI rI
                              in (DR maxI maxVI (Bin min minV l lt), eq, gt)
                | k < minI -> (l2rDR (DR min minV l), Nothing, r2lDR (DR max maxV r))
                | otherwise -> (l2rDR (DR min minV l), Just minVI, deleteMinL max maxV lI rI)

-- | /O(1)/.  Decompose a map into pieces based on the structure of the underlying
-- tree.  This function is useful for consuming a map in parallel.
--
-- No guarantee is made as to the sizes of the pieces; an internal, but
-- deterministic process determines this.  However, it is guaranteed that the
-- pieces returned will be in ascending order (all elements in the first submap
-- less than all elements in the second, and so on).
--
-- Examples:
--
-- > splitRoot (fromList (zip [1..6::Int] ['a'..])) ==
-- >   [fromList [(1,'a'),(2,'b'),(3,'c')],fromList [(4,'d'),(5,'e'),(6,'f')]]
--
-- > splitRoot empty == []
--
--  Note that the current implementation does not return more than two submaps,
--  but you should not depend on this behaviour because it can change in the
--  future without notice.
{-# INLINE splitRoot #-}
splitRoot :: IntMap a -> [IntMap a]
splitRoot (IntMap Empty) = []
splitRoot m@(IntMap (NonEmpty _ _ Tip)) = [m]
splitRoot (IntMap (NonEmpty min minV (Bin max maxV l r))) = [IntMap (NonEmpty min minV l), IntMap (r2lMap (NonEmpty max maxV r))]

-- | /O(n+m)/. Is this a submap?
-- Defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
isSubmapOf :: Eq a => IntMap a -> IntMap a -> Bool
isSubmapOf = isSubmapOfBy (==)

{- | /O(n+m)/.
 The expression (@'isSubmapOfBy' f m1 m2@) returns 'True' if
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':

  > isSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])

 But the following are all 'False':

  > isSubmapOfBy (==) (fromList [(1,2)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (<) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
-}
isSubmapOfBy :: (a -> b -> Bool) -> IntMap a -> IntMap b -> Bool
isSubmapOfBy p = start
  where
    start (IntMap Empty) !_ = True
    start !_ (IntMap Empty) = False
    start (IntMap (NonEmpty min1 minV1 root1)) (IntMap (NonEmpty min2 minV2 root2))
        | min1 < min2 = False
        | min1 > min2 = goL minV1 min1 root1 min2 root2
        | otherwise = p minV1 minV2 && goLFused min1 root1 root2

    goL minV1 min1 Tip min2 n2 = goLookupL min1 minV1 (xor min1 min2) n2
    goL _     _    _   _    Tip = False
    goL minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 (Bin max2 maxV2 l2 r2)
        | max1 > max2 = False
        | max1 < max2 = case xor min1 max1 `ltMSB` xor min2 max2 of
            True | xor min2 min1 < xor min1 max2 -> goL minV1 min1 n1 min2 l2 -- LT
                 | otherwise -> goR maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2
            False -> goL minV1 min1 l1 min2 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p maxV1 maxV2 && case xor min1 max1 `ltMSB` xor min2 max1 of
            True -> goRFused max1 (Bin min1 minV1 l1 r1) r2 -- LT
            False -> goL minV1 min1 l1 min2 l2 && goRFused max1 r1 r2 -- EQ

    goLFused _ Tip _ = True
    goLFused _ _ Tip = False
    goLFused min n1@(Bin max1 maxV1 l1 r1) (Bin max2 maxV2 l2 r2)
        | max1 > max2 = False
        | max1 < max2 = case xor min max1 `ltMSB` xor min max2 of
            True -> goLFused min n1 l2
            False -> goLFused min l1 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p maxV1 maxV2 && goLFused min l1 l2 && goRFused max1 r1 r2

    goR maxV1 max1 Tip max2 n2 = goLookupR max1 maxV1 (xor max1 max2) n2
    goR _     _    _   _    Tip = False
    goR maxV1 max1 n1@(Bin min1 minV1 l1 r1) max2 (Bin min2 minV2 l2 r2)
        | min1 < min2 = False
        | min1 > min2 = case xor min1 max1 `ltMSB` xor min2 max2 of
            True | xor min2 max1 > xor max1 max2 -> goR maxV1 max1 n1 max2 r2 -- LT
                 | otherwise -> goL minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2
            False -> goL minV1 min1 l1 min2 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p minV1 minV2 && case xor min1 max1 `ltMSB` xor min2 max1 of
            True -> goLFused min1 (Bin max1 maxV1 l1 r1) l2 -- LT
            False -> goLFused min1 l1 l2 && goR maxV1 max1 r1 max2 r2 -- EQ

    goRFused _ Tip _ = True
    goRFused _ _ Tip = False
    goRFused max n1@(Bin min1 minV1 l1 r1) (Bin min2 minV2 l2 r2)
        | min1 < min2 = False
        | min1 > min2 = case xor min1 max `ltMSB` xor min2 max of
            True -> goRFused max n1 r2
            False -> goL minV1 min1 l1 min2 l2 && goRFused max r1 r2 -- EQ
        | otherwise = p minV1 minV2 && goLFused min1 l1 l2 && goRFused max r1 r2

    goLookupL _ _ !_ Tip = False
    goLookupL k v !xorCache (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then goLookupL k v xorCache l
                    else goLookupR k v xorCacheMax r
        | k > max = False
        | otherwise = p v maxV
      where xorCacheMax = xor k max

    goLookupR _ _ !_ Tip = False
    goLookupR k v !xorCache (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then goLookupR k v xorCache r
                    else goLookupL k v xorCacheMin l
        | k < min = False
        | otherwise = p v minV
      where  xorCacheMin = xor min k

-- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: Eq a => IntMap a -> IntMap a -> Bool
isProperSubmapOf = isProperSubmapOfBy (==)

{- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
@m1@ and @m2@ are not equal,
all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
applied to their respective values. For example, the following
expressions are all 'True':

> isProperSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
> isProperSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])

But the following are all 'False':

> isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])
> isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
> isProperSubmapOfBy (<) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
-}
isProperSubmapOfBy :: (a -> b -> Bool) -> IntMap a -> IntMap b -> Bool
isProperSubmapOfBy p m1 m2 = submapCmp p m1 m2 == LT

submapCmp :: (a -> b -> Bool) -> IntMap a -> IntMap b -> Ordering
submapCmp p = start
  where
    start (IntMap Empty) (IntMap Empty) = EQ
    start (IntMap Empty) !_ = LT
    start !_ (IntMap Empty) = GT
    start (IntMap (NonEmpty min1 minV1 root1)) (IntMap (NonEmpty min2 minV2 root2))
        | min1 < min2 = GT
        | min1 > min2 = fromBool $ goL minV1 min1 root1 min2 root2
        | p minV1 minV2 = goLFused min1 root1 root2
        | otherwise = GT

    goL minV1 min1 Tip min2 n2 = goLookupL min1 minV1 (xor min1 min2) n2
    goL _     _    _   _    Tip = False
    goL minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 (Bin max2 maxV2 l2 r2)
        | max1 > max2 = False
        | max1 < max2 = case xor min1 max1 `ltMSB` xor min2 max2 of
            True | xor min2 min1 < xor min1 max2 -> goL minV1 min1 n1 min2 l2 -- LT
                 | otherwise -> goR maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2
            False -> goL minV1 min1 l1 min2 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p maxV1 maxV2 && case xor min1 max1 `ltMSB` xor min2 max1 of
            True -> goRFusedBool max1 (Bin min1 minV1 l1 r1) r2 -- LT
            False -> goL minV1 min1 l1 min2 l2 && goRFusedBool max1 r1 r2 -- EQ

    goLFused _ Tip Tip = EQ
    goLFused _ Tip _ = LT
    goLFused _ _ Tip = GT
    goLFused min n1@(Bin max1 maxV1 l1 r1) (Bin max2 maxV2 l2 r2)
        | max1 > max2 = GT
        | max1 < max2 = fromBool $ case xor min max1 `ltMSB` xor min max2 of
            True -> goLFusedBool min n1 l2
            False -> goLFusedBool min l1 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | p maxV1 maxV2 = goLFused min l1 l2 `combine` goRFused max1 r1 r2
        | otherwise = GT

    goLFusedBool _ Tip _ = True
    goLFusedBool _ _ Tip = False
    goLFusedBool min n1@(Bin max1 maxV1 l1 r1) (Bin max2 maxV2 l2 r2)
        | max1 > max2 = False
        | max1 < max2 = case xor min max1 `ltMSB` xor min max2 of
            True -> goLFusedBool min n1 l2
            False -> goLFusedBool min l1 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p maxV1 maxV2 && goLFusedBool min l1 l2 && goRFusedBool max1 r1 r2

    goR maxV1 max1 Tip max2 n2 = goLookupR max1 maxV1 (xor max1 max2) n2
    goR _     _    _   _    Tip = False
    goR maxV1 max1 n1@(Bin min1 minV1 l1 r1) max2 (Bin min2 minV2 l2 r2)
        | min1 < min2 = False
        | min1 > min2 = case xor min1 max1 `ltMSB` xor min2 max2 of
            True | xor min2 max1 > xor max1 max2 -> goR maxV1 max1 n1 max2 r2 -- LT
                 | otherwise -> goL minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2
            False -> goL minV1 min1 l1 min2 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p minV1 minV2 && case xor min1 max1 `ltMSB` xor min2 max1 of
            True -> goLFusedBool min1 (Bin max1 maxV1 l1 r1) l2 -- LT
            False -> goLFusedBool min1 l1 l2 && goR maxV1 max1 r1 max2 r2 -- EQ

    goRFused _ Tip Tip = EQ
    goRFused _ Tip _ = LT
    goRFused _ _ Tip = GT
    goRFused max n1@(Bin min1 minV1 l1 r1) (Bin min2 minV2 l2 r2)
        | min1 < min2 = GT
        | min1 > min2 = fromBool $ case xor min1 max `ltMSB` xor min2 max of
            True -> goRFusedBool max n1 r2
            False -> goL minV1 min1 l1 min2 l2 && goRFusedBool max r1 r2 -- EQ
        | p minV1 minV2 = goLFused min1 l1 l2 `combine` goRFused max r1 r2
        | otherwise = GT

    goRFusedBool _ Tip _ = True
    goRFusedBool _ _ Tip = False
    goRFusedBool max n1@(Bin min1 minV1 l1 r1) (Bin min2 minV2 l2 r2)
        | min1 < min2 = False
        | min1 > min2 = case xor min1 max `ltMSB` xor min2 max of
            True -> goRFusedBool max n1 r2
            False -> goL minV1 min1 l1 min2 l2 && goRFusedBool max r1 r2 -- EQ
        | otherwise = p minV1 minV2 && goLFusedBool min1 l1 l2 && goRFusedBool max r1 r2

    goLookupL _ _ !_ Tip = False
    goLookupL k v !xorCache (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then goLookupL k v xorCache l
                    else goLookupR k v xorCacheMax r
        | k > max = False
        | otherwise = p v maxV
      where xorCacheMax = xor k max

    goLookupR _ _ !_ Tip = False
    goLookupR k v !xorCache (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then goLookupR k v xorCache r
                    else goLookupL k v xorCacheMin l
        | k < min = False
        | otherwise = p v minV
      where  xorCacheMin = xor min k

    fromBool True = LT
    fromBool False = GT

    combine GT _ = GT
    combine _ GT = GT
    combine EQ EQ = EQ
    combine _ _ = LT

-- | /O(1)/. The minimal key of the map.
findMin :: IntMap a -> (Key, a)
findMin (IntMap Empty) = error "findMin: empty map has no minimal element"
findMin (IntMap (NonEmpty min minV _)) = (min, minV)

-- | /O(1)/. The maximal key of the map.
findMax :: IntMap a -> (Key, a)
findMax (IntMap Empty) = error "findMin: empty map has no minimal element"
findMax (IntMap (NonEmpty min minV root)) = case root of
    Tip -> (min, minV)
    Bin max maxV _ _ -> (max, maxV)

-- | /O(min(n,W))/. Delete the minimal key. Returns an empty map if the map is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Map.Map' &#8211;
-- versions prior to 0.5 threw an error if the 'IntMap' was already empty.
deleteMin :: IntMap a -> IntMap a
deleteMin (IntMap Empty) = IntMap Empty
deleteMin m = delete (fst (findMin m)) m

-- | /O(min(n,W))/. Delete the maximal key. Returns an empty map if the map is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Map.Map' &#8211;
-- versions prior to 0.5 threw an error if the 'IntMap' was already empty.
deleteMax :: IntMap a -> IntMap a
deleteMax (IntMap Empty) = IntMap Empty
deleteMax m = delete (fst (findMax m)) m

-- | /O(min(n,W))/. Delete and find the minimal element.
deleteFindMin :: IntMap a -> ((Key, a), IntMap a)
deleteFindMin m = let (k, a) = findMin m
                  in ((k, a), delete k m)

-- | /O(min(n,W))/. Delete and find the maximal element.
deleteFindMax :: IntMap a -> ((Key, a), IntMap a)
deleteFindMax m = let (k, a) = findMax m
                  in ((k, a), delete k m)

-- | /O(min(n,W))/. Retrieves the minimal key of the map, and the map
-- stripped of that element, or 'Nothing' if passed an empty map.
minView :: IntMap a -> Maybe (a, IntMap a)
minView (IntMap Empty) = Nothing
minView m = let (k, a) = findMin m
            in Just (a, delete k m)

-- | /O(min(n,W))/. Retrieves the maximal key of the map, and the map
-- stripped of that element, or 'Nothing' if passed an empty map.
maxView :: IntMap a -> Maybe (a, IntMap a)
maxView (IntMap Empty) = Nothing
maxView m = let (k, a) = findMax m
            in Just (a, delete k m)

-- | /O(min(n,W))/. Retrieves the minimal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
-- > minViewWithKey empty == Nothing
minViewWithKey :: IntMap a -> Maybe ((Key, a), IntMap a)
minViewWithKey (IntMap Empty) = Nothing
minViewWithKey m = let (k, a) = findMin m
                   in Just ((k, a), delete k m)

-- | /O(min(n,W))/. Retrieves the maximal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
-- > maxViewWithKey empty == Nothing
maxViewWithKey :: IntMap a -> Maybe ((Key, a), IntMap a)
maxViewWithKey (IntMap Empty) = Nothing
maxViewWithKey m = let (k, a) = findMax m
                   in Just ((k, a), delete k m)

----------------------------

-- | Show the tree that implements the map.
showTree :: Show a => IntMap a -> String
showTree = unlines . aux where
    aux (IntMap Empty) = []
    aux (IntMap (NonEmpty min minV node)) = (show min ++ " " ++ show minV) : auxNode False node
    auxNode :: Show a => Bool -> Node t a -> [String]
    auxNode _ Tip = ["+-."]
    auxNode lined (Bin bound val l r) = ["+--" ++ show bound ++ " " ++ show val, prefix : "  |"] ++ fmap indent (auxNode True l) ++ [prefix : "  |"] ++ fmap indent (auxNode False r)
      where
        prefix = if lined then '|' else ' '
        indent line = prefix : "  " ++ line

showTreeWith :: Show a => Bool -> Bool -> IntMap a -> String
showTreeWith _ _ = showTree

valid :: IntMap a -> Bool
valid = start
  where
    start (IntMap Empty) = True
    start (IntMap (NonEmpty min _ root)) = allKeys (> min) root && goL min root

    goL _    Tip = True
    goL min (Bin max _ l r) =
           allKeys (< max) l
        && allKeys (< max) r
        && allKeys (\k -> xor min k < xor k max) l
        && allKeys (\k -> xor min k > xor k max) r
        && goL min l
        && goR max r

    goR _    Tip = True
    goR max (Bin min _ l r) =
           allKeys (> min) l
        && allKeys (> min) r
        && allKeys (\k -> xor min k < xor k max) l
        && allKeys (\k -> xor min k > xor k max) r
        && goL min l
        && goR max r

    allKeys :: (Key -> Bool) -> Node t a -> Bool
    allKeys _ Tip = True
    allKeys p (Bin b _ l r) = p b && allKeys p l && allKeys p r

-- | /O(1)/. Returns whether the most significant bit of its first
-- argument is less significant than the most significant bit of its
-- second argument.
{-# INLINE ltMSB #-}
ltMSB :: Word -> Word -> Bool
ltMSB x y = x < y && x < Data.Bits.xor x y

{-# INLINE compareMSB #-}
compareMSB :: Word -> Word -> Ordering
compareMSB x y = case compare x y of
    LT | x < Data.Bits.xor x y -> LT
    GT | y < Data.Bits.xor x y -> GT
    _ -> EQ

{-# INLINE binL #-}
binL :: IntMap_ L a -> IntMap_ R a -> IntMap_ L a
binL Empty r = r2lMap r
binL l Empty = l
binL (NonEmpty min minV l) (NonEmpty max maxV r) = NonEmpty min minV (Bin max maxV l r)

{-# INLINE binR #-}
binR :: IntMap_ L a -> IntMap_ R a -> IntMap_ R a
binR Empty r = r
binR l Empty = l2rMap l
binR (NonEmpty min minV l) (NonEmpty max maxV r) = NonEmpty max maxV (Bin min minV l r)

{-# INLINE l2rMap #-}
l2rMap :: IntMap_ L a -> IntMap_ R a
l2rMap Empty = Empty
l2rMap (NonEmpty min minV Tip) = NonEmpty min minV Tip
l2rMap (NonEmpty min minV (Bin max maxV l r)) = NonEmpty max maxV (Bin min minV l r)

{-# INLINE r2lMap #-}
r2lMap :: IntMap_ R a -> IntMap_ L a
r2lMap Empty = Empty
r2lMap (NonEmpty max maxV Tip) = NonEmpty max maxV Tip
r2lMap (NonEmpty max maxV (Bin min minV l r)) = NonEmpty min minV (Bin max maxV l r)

{-# INLINE l2rDR #-}
l2rDR :: DeleteResult L a -> DeleteResult R a
l2rDR (DR min minV Tip) = DR min minV Tip
l2rDR (DR min minV (Bin max maxV l r)) = DR max maxV (Bin min minV l r)

{-# INLINE r2lDR #-}
r2lDR :: DeleteResult t a -> DeleteResult t' a
r2lDR (DR max maxV Tip) = DR max maxV Tip
r2lDR (DR max maxV (Bin min minV l r)) = DR min minV (Bin max maxV l r)

-- | Insert a key/value pair to a left node where the key is smaller than
-- any present in that node. Requires the xor of the inserted key and the
-- key immediately prior to it (the minimum bound of the node).
insertMinL :: Word -> Key -> a -> Node L a -> Node L a
insertMinL !_ !min minV Tip = Bin min minV Tip Tip
insertMinL !xorCache !min minV (Bin max maxV l r)
    -- Although the new minimum is not directly passed into 'insertMinL',
    -- it is captured in the 'xorCache'. We use standard navigation to
    -- determine whether 'min' should belong in the left or right branch.
    -- Since 'min' is, by assumption, smaller than any key in the tree,
    -- if 'min' is assigned to the right branch than the entire subtree
    -- must fit in the right branch. Otherwise, we need to continue recursing.
    | xor min max < xorCache = Bin max maxV Tip (Bin min minV l r)
    | otherwise = Bin max maxV (insertMinL xorCache min minV l) r

-- | Insert a key/value pair to a right node where the key is greater than
-- any present in that node. Requires the xor of the inserted key and the
-- key immediately following it (the maximum bound of the node).
insertMaxR :: Word -> Key -> a -> Node R a -> Node R a
insertMaxR !_ !max maxV Tip = Bin max maxV Tip Tip
insertMaxR !xorCache !max maxV (Bin min minV l r)
    | xor min max < xorCache = Bin min minV (Bin max maxV l r) Tip
    | otherwise = Bin min minV l (insertMaxR xorCache max maxV r)

-- | Delete the minimum key/value pair from an unpacked left node, returning
-- a new left node in a DeleteResult.
deleteMinL :: Key -> a -> Node L a -> Node R a -> DeleteResult L a
deleteMinL !max maxV Tip Tip = DR max maxV Tip
deleteMinL !max maxV Tip (Bin min minV l r) = DR min minV (Bin max maxV l r)
deleteMinL !max maxV (Bin innerMax innerMaxV innerL innerR) r =
    let DR min minV inner = deleteMinL innerMax innerMaxV innerL innerR
    in  DR min minV (Bin max maxV inner r)

-- | Delete the maximum key/value pair from an unpacked right node, returning
-- a new right node in a DeleteResult.
deleteMaxR :: Key -> a -> Node L a -> Node R a -> DeleteResult R a
deleteMaxR !min minV Tip Tip = DR min minV Tip
deleteMaxR !min minV (Bin max maxV l r) Tip = DR max maxV (Bin min minV l r)
deleteMaxR !min minV l (Bin innerMin innerMinV innerL innerR) =
    let DR max maxV inner = deleteMaxR innerMin innerMinV innerL innerR
    in  DR max maxV (Bin min minV l inner)

-- | Combine two disjoint nodes into a new left node. This is not cheap.
extractBinL :: Node L a -> Node R a -> Node L a
extractBinL l Tip = l
extractBinL l (Bin min minV innerL innerR) =
    let DR max maxV r = deleteMaxR min minV innerL innerR
    in Bin max maxV l r

-- | Combine two disjoint nodes into a new right node. This is not cheap.
extractBinR :: Node L a -> Node R a -> Node R a
extractBinR Tip r = r
extractBinR (Bin max maxV innerL innerR) r =
    let DR min minV l = deleteMinL max maxV innerL innerR
    in Bin min minV l r

nodeToMapL :: Node L a -> IntMap_ L a
nodeToMapL Tip = Empty
nodeToMapL (Bin max maxV innerL innerR) =
    let DR min minV l = deleteMinL max maxV innerL innerR
    in NonEmpty min minV l

nodeToMapR :: Node R a -> IntMap_ R a
nodeToMapR Tip = Empty
nodeToMapR (Bin min minV innerL innerR) =
    let DR max maxV r = deleteMaxR min minV innerL innerR
    in NonEmpty max maxV r

-- | Delete a key from a left node. Takes the xor of the deleted key and
-- the minimum bound of that node.
deleteL :: Key -> Word -> Node L a -> Node L a
deleteL !_ !_ Tip = Tip
deleteL !k !xorCache n@(Bin max maxV l r)
    | k < max = if xorCache < xorCacheMax
                then Bin max maxV (deleteL k xorCache l) r
                else Bin max maxV l (deleteR k xorCacheMax r)
    | k > max = n
    | otherwise = extractBinL l r
  where xorCacheMax = xor k max

-- | Delete a key from a right node. Takes the xor of the deleted key and
-- the maximum bound of that node.
deleteR :: Key -> Word -> Node R a -> Node R a
deleteR !_ !_ Tip = Tip
deleteR !k !xorCache n@(Bin min minV l r)
    | k > min = if xorCache < xorCacheMin
                then Bin min minV l (deleteR k xorCache r)
                else Bin min minV (deleteL k xorCacheMin l) r
    | k < min = n
    | otherwise = extractBinR l r
  where xorCacheMin = xor min k

data SketchyIntMap_ t a = SIM !Bool {-# UNPACK #-} !Key a !(Node t a)

{-# INLINE fromSketchy #-}
fromSketchy :: SketchyIntMap_ t a -> IntMap_ t a
fromSketchy (SIM False _ _ _) = Empty
fromSketchy (SIM True k v root) = NonEmpty k v root

{-# INLINE toSketchy #-}
toSketchy :: IntMap_ t a -> SketchyIntMap_ t a
toSketchy Empty = SIM False 0 (error "sketchyEmpty") Tip
toSketchy (NonEmpty k v root) = SIM True k v root

{-# INLINE sketchyBinL #-}
sketchyBinL :: SketchyIntMap_ L a -> SketchyIntMap_ R a -> SketchyIntMap_ L a
sketchyBinL l r = toSketchy (binL (fromSketchy l) (fromSketchy r))

{-# INLINE sketchyBinR #-}
sketchyBinR :: SketchyIntMap_ L a -> SketchyIntMap_ R a -> SketchyIntMap_ R a
sketchyBinR l r = toSketchy (binR (fromSketchy l) (fromSketchy r))

{-# INLINE l2rSketchyMap #-}
l2rSketchyMap :: SketchyIntMap_ L a -> SketchyIntMap_ R a
l2rSketchyMap = toSketchy . l2rMap . fromSketchy

{-# INLINE r2lSketchyMap #-}
r2lSketchyMap :: SketchyIntMap_ R a -> SketchyIntMap_ L a
r2lSketchyMap = toSketchy . r2lMap . fromSketchy
