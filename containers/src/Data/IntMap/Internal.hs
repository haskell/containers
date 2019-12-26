{-# LANGUAGE CPP, BangPatterns #-}
#if defined(__GLASGOW_HASKELL__)
{-# LANGUAGE TypeFamilies #-}
#if !defined(TESTING)
{-# LANGUAGE Safe #-}
#endif
#endif

{-# OPTIONS_HADDOCK not-home #-}

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
-- = Tree Structure
--
-- This implementation uses a novel modification of /big-endian patricia trees/, structured
-- as a vantage-point tree under the xor metric.
--
-- = Derivation
--
-- At its core, 'IntMap'\'s representation can be derived by a series of optimizations from
-- simpler structures:
--
-- * A bitwise trie is compressed into a PATRICIA tree (a bitwise radix tree) by
--  merging series of nodes that have no branches.
-- * The prefix labels of PATRICIA tree nodes are represented implicitly by storing the minimum
--  and maximum keys in a subtree.
-- * Minima and maxima are only stored once, at the topmost location that they appear.
-- * Values are stored next to their associated keys, rather than in the leaves.
--
-- Each of these steps is explained in detail below.
--
-- == The basic integer map: the bitwise trie
--
-- We are trying to create an efficient, simple mapping from integers to values. The most common
-- approaches are hash tables, which are not persistent (though we can come close with HAMTs),
-- and binary search trees, which work well, but don't use any special properties of the integer.
-- Thinking of integers not as numbers but as strings of bits, we use a /trie/, where a string is
-- interpreted as a series of instructions for which branch to take when navigating the tree. As
-- bits are particularly simple, so is the resulting structure:
--
-- > data IntMap a = Bin (IntMap a) (IntMap a) | Tip a | Nil
--
-- The `Bin` constructor represents a bitwise branch, and the `Tip` constructor comes after 64
-- 64 `Bin` construtors in the tree (on a 64-bit machine). The associated basic operations navigate
-- the tree by reading a key bit by bit, at each node taking the branch associated with the current
-- bit:
--
-- > lookup :: Int -> IntMap a -> Maybe a
-- > lookup k = go (finiteBitSize k - 1)
-- >   where
-- >     go b (Bin l r) = if testBit k b
-- >                      then go (b - 1) l
-- >                      else go (b - 1) r
-- >     go _ (Tip x) = Just x
-- >     go _ Nil = Nothing
-- >
-- > insert :: Int -> a -> IntMap a -> IntMap a
-- > insert k a = go (finiteBitSize k - 1)
-- >   where
-- >     go (-1) _ = Tip a
-- >     go b (Bin l r) = if testBit k b
-- >                      then Bin (go (b - 1) l) r
-- >                      else Bin l (go (b - 1) r)
-- >     go b _ = if testBit b k
-- >              then Bin (go (b + 1) Nil) Nil
-- >              else Bin Nil (go (b + 1) Nil)
--
-- 'delete' follows similarly, and the uniform structure means that even 'union' isn't too hard,
-- a welcome fact given the complexity of merging binary search trees. Unfortunately, this
-- approach is extremely slow and space-inefficient. To see why, look at the tree structure
-- for @'singleton' 5 "hello"@:
--
-- > +-0-.
-- >     +-0-.
-- >         +-0-.
-- >             +-0-.
-- >                 +-0-.
-- >                     +-0-.
-- >                         +-0-.
-- >                             +-0-.
-- >                                 +-0-.
-- >                                     +-0-.
-- >                                         +-0-.
-- >                                             +-0-.
-- >                                                 +-0-.
-- >                                                     +-1-.
-- >                                                         +-0-.
-- >                                                             +-1- "hello"
--
-- Note that, for brevity, the word size in this diagram is 16 bits. It would be 4 times longer
-- for a 64-bit system. In this atrocious tree structure, there is one pointer for every bit, a
-- 64-fold explosion in space. Arguably worse is the fact that every single 'lookup',
-- 'Data.IntMap.Lazy.insert', or 'delete' must traverse 64 pointers, resulting in 64 cache misses
-- and a corresponding slowdown.
--
-- == Path compression: PATRICIA trees and the previous version of 'Data.IntMap'
--
-- To reduce space usage, we compress nodes that only have one child. Since they form a
-- linear chain, we can concatenate the bits within that chain, recording which branches would be
-- taken. For example, again temporarily shortening the word size to 16 bits:
--
-- >>> singleton 5 "hello"
-- +-0000000000000101- "hello"
--
-- >>> fromList [(1, "1"), (4, "4"), (5, "5")]
-- +-0000000000000-.
--                 +-001- "1"
--                 +-10-.
--                      +-0- "4"
--                      +-1- "5"
--
-- This is much more space-efficient, and the basic operations, while more complicated, are still
-- straightforward. In Haskell, the structure is
--
-- > data IntMap a = Bin Prefix Mask (IntMap a) (IntMap a) | Tip Int a | Nil
--
-- The @Mask@ tells how long the @Prefix@ is, and the @Int@ in the @Tip@ nodes encodes the
-- remaining bits. This representation, known as the big-endian PATRICIA tree, is what the
-- previous iteration of 'IntMap' used.
--
-- == Implicit prefixes: a simpler representation
--
-- In the PATRICIA tree representation above, we explicitly stored the common prefix of all the
-- keys in a subtree. However, this prefix is not needed if we know the minimum and maximum keys.
-- The common prefix of a set of keys is the same as the common prefix of the minimum and maximum.
-- Replacing the @Prefix@, @Mask@ pair with a minimum and maximum, we get
--
-- > data IntMap a = Bin MinBound MaxBound (IntMap a) (IntMap a) | Tip Int a | Nil
--
-- The tree structure looks identical, just with different labels on the edges:
--
-- >>> singleton 5 "hello"
-- +-5- "hello"
--
-- >>> fromList [(1, "1"), (4, "4"), (5, "5")]
-- +-(1,5)-.
--         +-1- "1"
--         +-(4,5)-.
--                 +-4- "4"
--                 +-5- "5"
--
-- Traversing this tree efficiently is a bit more difficult, but still possible. See 'xor' for
-- details. Moreover, since the tree contains exact minima and maxima, 'lookup' can already be
-- more efficient than in a PATRICIA tree. Even if a key matches the prefix of common bits, if the
-- key is out of the bounds of a subtree, a search can terminate early with 'Nothing'. However,
-- there are bigger gains to be had.
--
-- == Removing redundancy
--
-- The above representation stores many keys repeatedly. In the @{1,4,5}@ example, 1 was stored
-- twice, 4 was stored twice, and 5 was stored three times. The minimum and maximum keys of a
-- tree are necessarily keys stored in that tree and moreover are minima and maxima of subtrees.
-- In the @{1,4,5}@ example, we know from the root node that the minimum is 1 and the maximum is
-- 5. At the first branch, we split the set into two parts, @{1}@ and @{4,5}@. However, the
-- minimum of the set of lesser keys is equal to the minimum of the original set. Similarly, the
-- maximum of the set of greater keys is equal to the maximum of the original set.
--
-- We can restructure the tree to store only one new value at each branch, removing the redundancy.
-- In nodes storing a set of lesser keys, we already know the minimum when traversing the tree
-- downward, so we only need to store the new maximum. In nodes storing a set of greater keys, we
-- know the maximum and store the new minimum. The root still needs both the minimum and the
-- maximum, so we need an extra layer to store that information:
--
-- > data IntMap a = Empty | NonEmpty Bound (Node a)
-- > data Node a = Bin Bound (Node a) (Node a) | Tip a
--
-- The trees are no longer quite as easy to read at a glance, since keys are no longer visible in
-- order at the leaves. It can be difficult to tell the difference at a glance between a node
-- storing a minimum and a node storing a maximum. (The actual implementation uses phantom types
-- to ensure no code gets this wrong.)
--
-- >>> singleton 5 "hello"
-- 5
-- +- "hello"
--
-- >>> fromList [(1, "1"), (4, "4"), (5, "5")]
-- 1
-- +-5-.
--     +- "1"
--     +-4-.
--         +- "4"
--         +- "5"
--
-- Although the nonuniform tree structure results in more complex code, we save a word in each
-- node.
--
-- == Moving the values upward
--
-- The above change removed the redundancy in keys perfectly, so each key is stored only once.
-- However, the values are still stored in leaves, now far away from their associated keys.
-- There is no reason this has to be true now that each keys has a unique location in the tree. We
-- simplify by moving the values next to their keys:
--
-- > data IntMap a = Empty | NonEmpty Bound a (Node a)
-- > data Node a = Bin Bound a (Node a) (Node a) | Tip
--
-- Although nodes still switch between minima and maxima, they can be visualized and manipulated
-- more cleanly since it is clear which keys are tied to which values.
--
-- >>> singleton 5 "hello"
-- 5 "hello"
-- +-
--
-- >>> fromList [(1, "1"), (4, "4"), (5, "5")]
-- 1 "1"
-- +-5-. "5"
--     +-
--     +-4-. "4"
--         +-
--         +-
--
-- This simpler representation translates to even more savings in both space and time. Since the
-- leaves no longer store any information, GHC will create a single static @Tip@ object and reuse
-- it for all leaves, the equivalent of representing leaves with a null pointer. This saves on
-- allocations and the metadata necessary for garbage collection and lazy evaluation.
-- Additionally, successful lookups can terminate as soon as they see the correct key instead of
-- dereferencing a chain of pointers all the way to the leaves. This means fewer cache misses and
-- a shorter loop.
--
-- = References and Further Reading
--
-- Morrison introduced PATRICIA trees in:
--
-- * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve Information Coded In Alphanumeric/\"
--   Journal of the ACM, 15(4), October 1968, pages 514-534.
--
-- Okasaki and Gill proposed using them in a functional context and provided implementations,
-- benchmarks, and discussion in:
--
-- * Chris Okasaki and Andy Gill, \"/Fast Mergeable Integer Maps/\",
--   Workshop on ML, September 1998, pages 77-86,
--   <https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.5452>.
--
-- Kmett proposed replacing explicit prefixes with min/max pairs in:
--
-- * Edward Kmett, \"/Revisiting Matrix Multiplication, Part IV: IntMap!?/\",
--   School of Haskell, 25 August 2013,
--   <https://www.schoolofhaskell.com/user/edwardk/revisiting-matrix-multiplication/part-4>.
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

import qualified Data.List (foldl')
import qualified Data.Foldable (Foldable(..))
#if MIN_VERSION_base(4,9,0)
#if MIN_VERSION_base(4,11,0)
import Data.Semigroup (stimes)
#else
import Data.Semigroup (Semigroup(..))
#endif
import Data.Semigroup (stimesIdempotentMonoid)
#endif

#if !MIN_VERSION_base(4,8,0)
import Data.Word (Word)
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable(..))
import Control.Applicative (Applicative(..), (<$>))
#endif

import qualified Data.Bits (xor)

import qualified Data.IntSet (IntSet, fromDistinctAscList, member, notMember)
import Utils.Containers.Internal.StrictPair (StrictPair(..))

import Prelude hiding (foldr, foldl, lookup, null, map, min, max)

type Key = Int

i2w :: Int -> Word
i2w = fromIntegral

-- | Xor a key with a bound for the purposes of navigation within the tree.
--
-- The navigation process is as follows. Suppose we are looking up a key `k` in a tree. We know
-- the minimum key in the tree, `min`, and the maximum key, `max`. Represented in binary:
--
-- >           shared prefix   bit to split on
-- >            /----------\  /
-- > min:       010010010101 0 ????????
-- > max:       010010010101 1 ????????
-- > k:         010010010101 ? ????????
--
-- To figure out in which subtree might contain `k`, we need to know whether the bit to split on
-- is zero or one. Now, if it is zero, then
--
-- > xor k min: 000000000000 0 ????????
-- > xor k max: 000000000000 1 ????????
--
-- If it is one:
--
-- > xor k min: 000000000000 1 ????????
-- > xor k max: 000000000000 0 ????????
--
-- Therefore, the splitting bit is set iff @'xor' k min > 'xor' k max@. Put another way, the key
-- shares more bits with the bound that it is closer to under the xor metric, since exclusive or
-- maps shared bits to zero. The metric perspective also makes it clear why this works unmodified
-- in the presence of negative numbers, despite storing negative numbers (with a set sign bit) in
-- the left branch normally identified with an unset bit. As long as the comparison is done
-- unsigned (metrics are always nonnegative), negative integers will be closer to other negative
-- integers than they are to nonnegative integers.
{-# INLINE xor #-}
xor :: Key -> Bound t -> Word
xor a (Bound b) = Data.Bits.xor (i2w a) (i2w b)

-- | Xor the minimum and maximum keys of a tree. The most significant bit of the result indicates
-- which bit to split on for navigation and is useful in merging maps to tell whether nodes from
-- different maps branch at the same time.
{-# INLINE xorBounds #-}
xorBounds :: Bound L -> Bound R -> Word
xorBounds (Bound min) (Bound max) = Data.Bits.xor (i2w min) (i2w max)

{-# INLINE boundsDisjoint #-}
boundsDisjoint :: Bound L -> Bound R -> Bool
boundsDisjoint (Bound min) (Bound max) = min > max

-- Phantom types used to separate the types of left and right nodes.
-- They are uninhabited simply to ensure that they are only used as type parameters.
newtype L = L L
newtype R = R R

#if defined(__GLASGOW_HASKELL__)
-- TODO: If we are relying on GHC features anyway, L and R could be a new kind.
newtype Bound t = Bound { boundKey :: Key } deriving (Eq, Ord, Show)

type family Flipped t
type instance Flipped L = R
type instance Flipped R = L
#else
-- Without type families, we can't track min vs. max correctly, so we just don't by making that parameter ignored
type Bound t = Bound_
newtype Bound_ = Bound { boundKey :: Key } deriving (Eq, Ord, Show)
-- This, like L and R, is uninhabited to ensure that it is only used as a type parameter
newtype Flipped t = Flipped (Flipped t)
#endif

inMinBound :: Key -> Bound L -> Bool
inMinBound k (Bound min) = k > min

inMaxBound :: Key -> Bound R -> Bool
inMaxBound k (Bound max) = k < max

outOfMinBound :: Key -> Bound L -> Bool
outOfMinBound k (Bound min) = k < min

outOfMaxBound :: Key -> Bound R -> Bool
outOfMaxBound k (Bound max) = k > max

-- | A map of integers to values @a@.
newtype IntMap a = IntMap (IntMap_ L a) deriving (Eq)
data IntMap_ t a = NonEmpty {-# UNPACK #-} !(Bound t) a !(Node t a) | Empty deriving (Eq)
data Node t a = Bin {-# UNPACK #-} !(Bound (Flipped t)) a !(Node L a) !(Node R a) | Tip deriving (Eq, Show)

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
    (<>) = union
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


-- | /O(min(n,W))/. Find the value at a key.
-- Returns 'Nothing' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] !? 1 == Nothing
-- > fromList [(5,'a'), (3,'b')] !? 5 == Just 'a'
--
-- @since 0.5.11
(!?) :: IntMap a -> Key -> Maybe a
(!?) m k = lookup k m

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
        | outOfMinBound k min = False
        | k == boundKey min = True
        | otherwise = goL (xor k min) node

    goL !_ Tip = False
    goL !xorCache (Bin max _ l r)
        | inMaxBound k max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | outOfMaxBound k max = False
        | otherwise = True
      where xorCacheMax = xor k max

    goR !_ Tip = False
    goR !xorCache (Bin min _ l r)
        | inMinBound k min = if xorCache < xorCacheMin
                    then goR xorCache r
                    else goL xorCacheMin l
        | outOfMinBound k min = False
        | otherwise = True
      where xorCacheMin = xor k min

-- | /O(min(n,W))/. Is the key not a member of the map?
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True
notMember :: Key -> IntMap a -> Bool
notMember k = k `seq` start
  where
    start (IntMap Empty) = True
    start (IntMap (NonEmpty min _ node))
        | outOfMinBound k min = True
        | k == boundKey min = False
        | otherwise = goL (xor k min) node

    goL !_ Tip = True
    goL !xorCache (Bin max _ l r)
        | inMaxBound k max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | outOfMaxBound k max = True
        | otherwise = False
      where xorCacheMax = xor k max

    goR !_ Tip = True
    goR !xorCache (Bin min _ l r)
        | inMinBound k min = if xorCache < xorCacheMin
                    then goR xorCache r
                    else goL xorCacheMin l
        | outOfMinBound k min = True
        | otherwise = False
      where xorCacheMin = xor k min

-- | /O(min(n,W))/. Lookup the value at a key in the map. See also 'Data.Map.lookup'.
lookup :: Key -> IntMap a -> Maybe a
lookup k = k `seq` start
  where
    start (IntMap Empty) = Nothing
    start (IntMap (NonEmpty min minV node))
        | outOfMinBound k min = Nothing
        | k == boundKey min = Just minV
        | otherwise = goL (xor k min) node

    goL !_ Tip = Nothing
    goL !xorCache (Bin max maxV l r)
        | inMaxBound k max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | outOfMaxBound k max = Nothing
        | otherwise = Just maxV
      where xorCacheMax = xor k max

    goR !_ Tip = Nothing
    goR !xorCache (Bin min minV l r)
        | inMinBound k min = if xorCache < xorCacheMin
                    then goR xorCache r
                    else goL xorCacheMin l
        | outOfMinBound k min = Nothing
        | otherwise = Just minV
      where xorCacheMin = xor k min

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
        | outOfMinBound k min = def
        | k == boundKey min = minV
        | otherwise = goL (xor k min) node

    goL !_ Tip = def
    goL !xorCache (Bin max maxV l r)
        | inMaxBound k max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | outOfMaxBound k max = def
        | otherwise = maxV
      where xorCacheMax = xor k max

    goR !_ Tip = def
    goR !xorCache (Bin min minV l r)
        | inMinBound k min = if xorCache < xorCacheMin
                    then goR xorCache r
                    else goL xorCacheMin l
        | outOfMinBound k min = def
        | otherwise = minV
      where  xorCacheMin = xor k min

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
        | boundKey min >= k = Nothing
        | otherwise = Just (goL (xor k min) min minV node)

    goL !_ min minV Tip = (boundKey min, minV)
    goL !xorCache min minV (Bin max maxV l r)
        | boundKey max < k = (boundKey max, maxV)
        | xorCache < xorCacheMax = goL xorCache min minV l
        | otherwise = goR xorCacheMax r min minV l
      where
        xorCacheMax = xor k max

    goR !_ Tip fMin fMinV fallback = getMax fMin fMinV fallback
    goR !xorCache (Bin min minV l r) fMin fMinV fallback
        | boundKey min >= k = getMax fMin fMinV fallback
        | xorCache < xorCacheMin = goR xorCache r min minV l
        | otherwise = goL xorCacheMin min minV l
      where
        xorCacheMin = xor k min

    getMax min minV Tip = (boundKey min, minV)
    getMax _   _   (Bin max maxV _ _) = (boundKey max, maxV)

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
        | boundKey min > k = Nothing
        | otherwise = Just (goL (xor k min) min minV node)

    goL !_ min minV Tip = (boundKey min, minV)
    goL !xorCache min minV (Bin max maxV l r)
        | boundKey max <= k = (boundKey max, maxV)
        | xorCache < xorCacheMax = goL xorCache min minV l
        | otherwise = goR xorCacheMax r min minV l
      where
        xorCacheMax = xor k max

    goR !_ Tip fMin fMinV fallback = getMax fMin fMinV fallback
    goR !xorCache (Bin min minV l r) fMin fMinV fallback
        | boundKey min > k = getMax fMin fMinV fallback
        | xorCache < xorCacheMin = goR xorCache r min minV l
        | otherwise = goL xorCacheMin min minV l
      where
        xorCacheMin = xor k min

    getMax min minV Tip = (boundKey min, minV)
    getMax _   _   (Bin max maxV _ _) = (boundKey max, maxV)

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
        | boundKey min <= k = Nothing
        | otherwise = Just (boundKey min, minV)
    start (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | boundKey max <= k = Nothing
        | otherwise = Just (goR (xor k max) max maxV (Bin min minV l r))

    goL !_ Tip fMax fMaxV fallback = getMin fMax fMaxV fallback
    goL !xorCache (Bin max maxV l r) fMax fMaxV fallback
        | boundKey max <= k = getMin fMax fMaxV fallback
        | xorCache < xorCacheMax = goL xorCache l max maxV r
        | otherwise = goR xorCacheMax max maxV r
      where
        xorCacheMax = xor k max

    goR !_ max maxV Tip = (boundKey max, maxV)
    goR !xorCache max maxV (Bin min minV l r)
        | boundKey min > k = (boundKey min, minV)
        | xorCache < xorCacheMin = goR xorCache max maxV r
        | otherwise = goL xorCacheMin l max maxV r
      where
        xorCacheMin = xor k min

    getMin max maxV Tip = (boundKey max, maxV)
    getMin _   _   (Bin min minV _ _) = (boundKey min, minV)

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
        | boundKey min < k = Nothing
        | otherwise = Just (boundKey min, minV)
    start (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | boundKey max < k = Nothing
        | otherwise = Just (goR (xor k max) max maxV (Bin min minV l r))

    goL !_ Tip fMax fMaxV fallback = getMin fMax fMaxV fallback
    goL !xorCache (Bin max maxV l r) fMax fMaxV fallback
        | boundKey max < k = getMin fMax fMaxV fallback
        | xorCache < xorCacheMax = goL xorCache l max maxV r
        | otherwise = goR xorCacheMax max maxV r
      where
        xorCacheMax = xor k max

    goR !_ max maxV Tip = (boundKey max, maxV)
    goR !xorCache max maxV (Bin min minV l r)
        | boundKey min >= k = (boundKey min, minV)
        | xorCache < xorCacheMin = goR xorCache max maxV r
        | otherwise = goL xorCacheMin l max maxV r
      where
        xorCacheMin = xor k min

    getMin max maxV Tip = (boundKey max, maxV)
    getMin _   _   (Bin min minV _ _) = (boundKey min, minV)

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
        | k == boundKey min = IntMap Empty
        | otherwise = m
    start m@(IntMap (NonEmpty min minV root@(Bin max maxV l r)))
        | outOfMinBound k min = m
        | k == boundKey min = let DR min' minV' root' = deleteMinL max maxV l r in IntMap (NonEmpty min' minV' root')
        | otherwise = IntMap (NonEmpty min minV (deleteL k (xor k min) root))

-- TODO: Does a strict pair work? My guess is not, as GHC was already
-- unboxing the tuple, but it would be simpler to use one of those.
-- | Without this specialized type (I was just using a tuple), GHC's
-- CPR correctly unboxed the tuple, but it couldn't unbox the returned
-- Key, leading to lots of inefficiency (3x slower than stock Data.IntMap)
data DeleteResult t a = DR {-# UNPACK #-} !(Bound t) a !(Node t a)

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

    goL1 minV1 !min1 Tip !_    Tip = Bin (minToMax min1) minV1 Tip Tip
    goL1 minV1 !min1 !n1 !min2 Tip = insertMinL (xor (boundKey min1) min2) min1 minV1 n1
    goL1 minV1 !min1 !n1 !min2 n2@(Bin max2 _ _ _) | boundsDisjoint min1 max2 = unionDisjointL minV1 min2 n2 min1 n1
    goL1 minV1 !min1 Tip !min2 !n2 = goInsertL1 (boundKey min1) minV1 (xor (boundKey min1) min2) min2 n2
    goL1 minV1 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
         LT | xor (boundKey min1) min2 < xor (boundKey min1) max2 -> Bin max2 maxV2 (goL1 minV1 min1 n1 min2 l2) r2 -- we choose min1 arbitrarily - we just need something from tree 1
            | max1 > max2 -> Bin max1 maxV1 l2 (goR2 maxV2 max1 (Bin min1 minV1 l1 r1) max2 r2)
            | max1 < max2 -> Bin max2 maxV2 l2 (goR1 maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2)
            | otherwise -> Bin max1 maxV1 l2 (goRFused max1 (Bin min1 minV1 l1 r1) r2) -- we choose max1 arbitrarily, as max1 == max2
         EQ | max1 > max2 -> Bin max1 maxV1 (goL1 minV1 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
            | max1 < max2 -> Bin max2 maxV2 (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
            | otherwise -> Bin max1 maxV1 (goL1 minV1 min1 l1 min2 l2) (goRFused max1 r1 r2) -- we choose max1 arbitrarily, as max1 == max2
         GT -> Bin max1 maxV1 (goL1 minV1 min1 l1 min2 n2) r1

    goL2 minV2 !_    Tip !min2 Tip = Bin (minToMax min2) minV2 Tip Tip
    goL2 minV2 !min1 Tip !min2 !n2 = insertMinL (xor (boundKey min2) min1) min2 minV2 n2
    goL2 minV2 !min1 n1@(Bin max1 _ _ _) !min2 !n2 | boundsDisjoint min2 max1 = unionDisjointL minV2 min1 n1 min2 n2
    goL2 minV2 !min1 !n1 !min2 Tip = goInsertL2 (boundKey min2) minV2 (xor (boundKey min2) min1) min1 n1
    goL2 minV2 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
         GT | xor (boundKey min2) min1 < xor (boundKey min2) max1 -> Bin max1 maxV1 (goL2 minV2 min1 l1 min2 n2) r1 -- we choose min2 arbitrarily - we just need something from tree 2
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
    goLFused !min n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min max1) (xorBounds min max2) of
        LT -> Bin max2 maxV2 (goLFused min n1 l2) r2
        EQ | max1 > max2 -> Bin max1 maxV1 (goLFused min l1 l2) (goR2 maxV2 max1 r1 max2 r2)
           | max1 < max2 -> Bin max2 maxV2 (goLFused min l1 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> Bin max1 maxV1 (goLFused min l1 l2) (goRFused max1 r1 r2) -- we choose max1 arbitrarily, as max1 == max2
        GT -> Bin max1 maxV1 (goLFused min l1 n2) r1

    goR1 maxV1 !max1 Tip !_    Tip = Bin (maxToMin max1) maxV1 Tip Tip
    goR1 maxV1 !max1 !n1 !max2 Tip = insertMaxR (xor (boundKey max1) max2) max1 maxV1 n1
    goR1 maxV1 !max1 !n1 !max2 n2@(Bin min2 _ _ _) | boundsDisjoint min2 max1 = unionDisjointR maxV1 max1 n1 max2 n2
    goR1 maxV1 !max1 Tip !max2 !n2 = goInsertR1 (boundKey max1) maxV1 (xor (boundKey max1) max2) max2 n2
    goR1 maxV1 !max1 n1@(Bin min1 minV1 l1 r1) !max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
         LT | xor (boundKey max1) min2 > xor (boundKey max1) max2 -> Bin min2 minV2 l2 (goR1 maxV1 max1 n1 max2 r2) -- we choose max1 arbitrarily - we just need something from tree 1
            | min1 < min2 -> Bin min1 minV1 (goL2 minV2 min1 (Bin max1 maxV1 l1 r1) min2 l2) r2
            | min1 > min2 -> Bin min2 minV2 (goL1 minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2) r2
            | otherwise -> Bin min1 minV1 (goLFused min1 (Bin max1 maxV1 l1 r1) l2) r2 -- we choose min1 arbitrarily, as min1 == min2
         EQ | min1 < min2 -> Bin min1 minV1 (goL2 minV2 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
            | min1 > min2 -> Bin min2 minV2 (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
            | otherwise -> Bin min1 minV1 (goLFused min1 l1 l2) (goR1 maxV1 max1 r1 max2 r2) -- we choose min1 arbitrarily, as min1 == min2
         GT -> Bin min1 minV1 l1 (goR1 maxV1 max1 r1 max2 n2)

    goR2 maxV2 !_    Tip !max2 Tip = Bin (maxToMin max2) maxV2 Tip Tip
    goR2 maxV2 !max1 Tip !max2 !n2 = insertMaxR (xor (boundKey max2) max1) max2 maxV2 n2
    goR2 maxV2 !max1 n1@(Bin min1 _ _ _) !max2 !n2 | boundsDisjoint min1 max2 = unionDisjointR maxV2 max2 n2 max1 n1
    goR2 maxV2 !max1 !n1 !max2 Tip = goInsertR2 (boundKey max2) maxV2 (xor (boundKey max2) max1) max1 n1
    goR2 maxV2 !max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
         GT | xor (boundKey max2) min1 > xor (boundKey max2) max1 -> Bin min1 minV1 l1 (goR2 maxV2 max1 r1 max2 n2) -- we choose max2 arbitrarily - we just need something from tree 2
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
    goRFused !max n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max) (xorBounds min2 max) of
        LT -> Bin min2 minV2 l2 (goRFused max n1 r2)
        EQ | min1 < min2 -> Bin min1 minV1 (goL2 minV2 min1 l1 min2 l2) (goRFused max r1 r2)
           | min1 > min2 -> Bin min2 minV2 (goL1 minV1 min1 l1 min2 l2) (goRFused max r1 r2)
           | otherwise -> Bin min1 minV1 (goLFused min1 l1 l2) (goRFused max r1 r2) -- we choose min1 arbitrarily, as min1 == min2
        GT -> Bin min1 minV1 l1 (goRFused max r1 n2)

    goInsertL1 k v !_        _    Tip = Bin (Bound k) v Tip Tip
    goInsertL1 k v !xorCache min (Bin max maxV l r)
        | inMaxBound k max = if xorCache < xorCacheMax
                    then Bin max maxV (goInsertL1 k v xorCache min l) r
                    else Bin max maxV l (goInsertR1 k v xorCacheMax max r)
        | outOfMaxBound k max = if xor (boundKey max) min < xorCacheMax
                    then Bin (Bound k) v (Bin max maxV l r) Tip
                    else Bin (Bound k) v l (insertMaxR xorCacheMax max maxV r)
        | otherwise = Bin max v l r
      where xorCacheMax = xor k max

    goInsertR1 k v !_        _    Tip = Bin (Bound k) v Tip Tip
    goInsertR1 k v !xorCache max (Bin min minV l r)
        | inMinBound k min = if xorCache < xorCacheMin
                    then Bin min minV l (goInsertR1 k v xorCache max r)
                    else Bin min minV (goInsertL1 k v xorCacheMin min l) r
        | outOfMinBound k min = if xor (boundKey min) max < xorCacheMin
                    then Bin (Bound k) v Tip (Bin min minV l r)
                    else Bin (Bound k) v (insertMinL xorCacheMin min minV l) r
        | otherwise = Bin min v l r
      where xorCacheMin = xor k min

    goInsertL2 k v !_        _    Tip = Bin (Bound k) v Tip Tip
    goInsertL2 k v !xorCache min (Bin max maxV l r)
        | inMaxBound k max = if xorCache < xorCacheMax
                    then Bin max maxV (goInsertL2 k v xorCache min l) r
                    else Bin max maxV l (goInsertR2 k v xorCacheMax max r)
        | outOfMaxBound k max = if xor (boundKey max) min < xorCacheMax
                    then Bin (Bound k) v (Bin max maxV l r) Tip
                    else Bin (Bound k) v l (insertMaxR xorCacheMax max maxV r)
        | otherwise = Bin max maxV l r
      where xorCacheMax = xor k max

    goInsertR2 k v !_        _    Tip = Bin (Bound k) v Tip Tip
    goInsertR2 k v !xorCache max (Bin min minV l r)
        | inMinBound k min = if xorCache < xorCacheMin
                    then Bin min minV l (goInsertR2 k v xorCache max r)
                    else Bin min minV (goInsertL2 k v xorCacheMin min l) r
        | outOfMinBound k min = if xor (boundKey min) max < xorCacheMin
                    then Bin (Bound k) v Tip (Bin min minV l r)
                    else Bin (Bound k) v (insertMinL xorCacheMin min minV l) r
        | otherwise = Bin min minV l r
      where xorCacheMin = xor k min

unionDisjointL :: a -> Bound L -> Node L a -> Bound L -> Node L a -> Node L a
unionDisjointL _ !_ Tip !_ !_ = error "Data.IntMap.unionDisjoint: impossible"
unionDisjointL minV2 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 Tip
    | xorBounds min1 max1 < xorBounds min2 max1 = Bin (minToMax min2) minV2 n1 Tip
    | otherwise = Bin (minToMax min2) minV2 l1 (insertMaxR (xor (boundKey max1) min2) max1 maxV1 r1)
unionDisjointL minV2 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 (Bin max2 maxV2 l2 r2)
    | not (xorBounds min1 max1 `ltMSB` xorBounds min1 max2) = Bin max2 maxV2 l1 (unionDisjointR maxV1 max1 r1 max2 (Bin min2 minV2 l2 r2))
    | not (xorBounds min2 max2 `ltMSB` xorBounds min1 max2) = Bin max2 maxV2 (unionDisjointL minV2 min1 n1 min2 l2) r2
    | otherwise = Bin max2 maxV2 n1 (Bin min2 minV2 l2 r2)

unionDisjointR :: a -> Bound R -> Node R a -> Bound R -> Node R a -> Node R a
unionDisjointR _ !_ !_ !_ Tip = error "Data.IntMap.unionDisjoint: impossible"
unionDisjointR maxV1 !max1 Tip !max2 n2@(Bin min2 minV2 l2 r2)
    | xorBounds min2 max2 < xorBounds min2 max1 = Bin (maxToMin max1) maxV1 Tip n2
    | otherwise = Bin (maxToMin max1) maxV1 (insertMinL (xor (boundKey min2) max1) min2 minV2 l2) r2
unionDisjointR maxV1 !max1 (Bin min1 minV1 l1 r1) !max2 n2@(Bin min2 minV2 l2 r2)
    | not (xorBounds min2 max2 `ltMSB` xorBounds min1 max2) = Bin min1 minV1 (unionDisjointL minV2 min1 (Bin max1 maxV1 l1 r1) min2 l2) r2
    | not (xorBounds min1 max1 `ltMSB` xorBounds min1 max2) = Bin min1 minV1 l1 (unionDisjointR maxV1 max1 r1 max2 n2)
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

    goL1 minV1 min1 Tip min2 n2 = goLookupL (boundKey min1) minV1 (xor (boundKey min1) min2) n2
    goL1 minV1 min1 n1 _ Tip = NonEmpty min1 minV1 n1
    goL1 minV1 min1 n1@(Bin _ _ _ _) _ (Bin max2 _ _ _) | boundsDisjoint min1 max2 = NonEmpty min1 minV1 n1
    goL1 minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 _ l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT | xor (boundKey min1) min2 < xor (boundKey min1) max2 -> goL1 minV1 min1 n1 min2 l2 -- min1 is arbitrary here - we just need something from tree 1
           | max1 > max2 -> r2lMap $ NonEmpty max1 maxV1 (goR2 max1 (Bin min1 minV1 l1 r1) max2 r2)
           | max1 < max2 -> r2lMap $ goR1 maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2
           | otherwise -> r2lMap $ goRFused max1 (Bin min1 minV1 l1 r1) r2
        EQ | max1 > max2 -> binL (goL1 minV1 min1 l1 min2 l2) (NonEmpty max1 maxV1 (goR2 max1 r1 max2 r2))
           | max1 < max2 -> binL (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> binL (goL1 minV1 min1 l1 min2 l2) (goRFused max1 r1 r2)
        GT -> binL (goL1 minV1 min1 l1 min2 n2) (NonEmpty max1 maxV1 r1)

    goL2 !_   Tip !_   !_  = Tip
    goL2 min1 n1  min2 Tip = deleteL (boundKey min2) (xor (boundKey min2) min1) n1
    goL2 _ n1@(Bin max1 _ _ _) min2 (Bin _ _ _ _) | boundsDisjoint min2 max1 = n1
    goL2 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 _ l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT -> goL2 min1 n1 min2 l2
        EQ | max1 > max2 -> Bin max1 maxV1 (goL2 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | max1 < max2 -> case goR1 maxV1 max1 r1 max2 r2 of
                Empty -> goL2 min1 l1 min2 l2
                NonEmpty max' maxV' r' -> Bin max' maxV' (goL2 min1 l1 min2 l2) r'
           | otherwise -> case goRFused max1 r1 r2 of
                Empty -> goL2 min1 l1 min2 l2
                NonEmpty max' maxV' r' -> Bin max' maxV' (goL2 min1 l1 min2 l2) r'
        GT | xor (boundKey min2) min1 < xor (boundKey min2) max1 -> Bin max1 maxV1 (goL2 min1 l1 min2 n2) r1 -- min2 is arbitrary here - we just need something from tree 2
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
    goLFused !min n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 _ l2 r2) = case compareMSB (xorBounds min max1) (xorBounds min max2) of
        LT -> goLFused min n1 l2
        EQ | max1 > max2 -> binL (goLFused min l1 l2) (NonEmpty max1 maxV1 (goR2 max1 r1 max2 r2))
           | max1 < max2 -> binL (goLFused min l1 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> binL (goLFused min l1 l2) (goRFused max1 r1 r2) -- we choose max1 arbitrarily, as max1 == max2
        GT -> binL (goLFused min l1 n2) (NonEmpty max1 maxV1 r1)

    goR1 maxV1 max1 Tip max2 n2 = goLookupR (boundKey max1) maxV1 (xor (boundKey max1) max2) n2
    goR1 maxV1 max1 n1 _ Tip = NonEmpty max1 maxV1 n1
    goR1 maxV1 max1 n1@(Bin _ _ _ _) _ (Bin min2 _ _ _) | boundsDisjoint min2 max1 = NonEmpty max1 maxV1 n1
    goR1 maxV1 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 _ l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT | xor (boundKey max1) min2 > xor (boundKey max1) max2 -> goR1 maxV1 max1 n1 max2 r2 -- max1 is arbitrary here - we just need something from tree 1
           | min1 < min2 -> l2rMap $ NonEmpty min1 minV1 (goL2 min1 (Bin max1 maxV1 l1 r1) min2 l2)
           | min1 > min2 -> l2rMap $ goL1 minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2
           | otherwise -> l2rMap $ goLFused min1 (Bin max1 maxV1 l1 r1) l2
        EQ | min1 < min2 -> binR (NonEmpty min1 minV1 (goL2 min1 l1 min2 l2)) (goR1 maxV1 max1 r1 max2 r2)
           | min1 > min2 -> binR (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> binR (goLFused min1 l1 l2) (goR1 maxV1 max1 r1 max2 r2)
        GT -> binR (NonEmpty min1 minV1 l1) (goR1 maxV1 max1 r1 max2 n2)

    goR2 !_   Tip !_   !_  = Tip
    goR2 max1 n1  max2 Tip = deleteR (boundKey max2) (xor (boundKey max2) max1) n1
    goR2 _ n1@(Bin min1 _ _ _) max2 (Bin _ _ _ _) | boundsDisjoint min1 max2 = n1
    goR2 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 _ l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT -> goR2 max1 n1 max2 r2
        EQ | min1 < min2 -> Bin min1 minV1 (goL2 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | min1 > min2 -> case goL1 minV1 min1 l1 min2 l2 of
                Empty -> goR2 max1 r1 max2 r2
                NonEmpty min' minV' l' -> Bin min' minV' l' (goR2 max1 r1 max2 r2)
           | otherwise -> case goLFused min1 l1 l2 of
                Empty -> goR2 max1 r1 max2 r2
                NonEmpty min' minV' l' -> Bin min' minV' l' (goR2 max1 r1 max2 r2)
        GT | xor (boundKey max2) min1 > xor (boundKey max2) max1 -> Bin min1 minV1 l1 (goR2 max1 r1 max2 n2) -- max2 is arbitrary here - we just need something from tree 2
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
    goRFused !max n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 _ l2 r2) = case compareMSB (xorBounds min1 max) (xorBounds min2 max) of
        LT -> goRFused max n1 r2
        EQ | min1 < min2 -> binR (NonEmpty min1 minV1 (goL2 min1 l1 min2 l2)) (goRFused max r1 r2)
           | min1 > min2 -> binR (goL1 minV1 min1 l1 min2 l2) (goRFused max r1 r2)
           | otherwise -> binR (goLFused min1 l1 l2) (goRFused max r1 r2) -- we choose min1 arbitrarily, as min1 == min2
        GT -> binR (NonEmpty min1 minV1 l1) (goRFused max r1 n2)

    goLookupL k v !_ Tip = NonEmpty (Bound k) v Tip
    goLookupL k v !xorCache (Bin max _ l r)
        | inMaxBound k max = if xorCache < xorCacheMax
                    then goLookupL k v xorCache l
                    else goLookupR k v xorCacheMax r
        | outOfMaxBound k max = NonEmpty (Bound k) v Tip
        | otherwise = Empty
      where xorCacheMax = xor k max

    goLookupR k v !_ Tip = NonEmpty (Bound k) v Tip
    goLookupR k v !xorCache (Bin min _ l r)
        | inMinBound k min = if xorCache < xorCacheMin
                    then goLookupR k v xorCache r
                    else goLookupL k v xorCacheMin l
        | outOfMinBound k min = NonEmpty (Bound k) v Tip
        | otherwise = Empty
      where xorCacheMin = xor k min

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
        | min1 < min2 = IntMap (goL2 min1 root1 min2 root2)
        | min1 > min2 = IntMap (goL1 minV1 min1 root1 min2 root2)
        | otherwise = IntMap (NonEmpty min1 minV1 (goLFused min1 root1 root2)) -- we choose min1 arbitrarily, as min1 == min2

    -- TODO: This scheme might produce lots of unnecessary l2r and r2l calls. This should be rectified.

    goL1 _     !_   !_  !_   Tip = Empty
    goL1 minV1 min1 Tip min2 n2  = goLookupL1 (boundKey min1) minV1 (xor (boundKey min1) min2) n2
    goL1 _ min1 (Bin _ _ _ _) _ (Bin max2 _ _ _) | boundsDisjoint min1 max2 = Empty
    goL1 minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 _ l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT | xor (boundKey min1) min2 < xor (boundKey min1) max2 -> goL1 minV1 min1 n1 min2 l2 -- min1 is arbitrary here - we just need something from tree 1
           | max1 > max2 -> r2lMap $ goR2 max1 (Bin min1 minV1 l1 r1) max2 r2
           | max1 < max2 -> r2lMap $ goR1 maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2
           | otherwise -> r2lMap $ NonEmpty max1 maxV1 (goRFused max1 (Bin min1 minV1 l1 r1) r2)
        EQ | max1 > max2 -> binL (goL1 minV1 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | max1 < max2 -> binL (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> case goL1 minV1 min1 l1 min2 l2 of
                Empty -> r2lMap (NonEmpty max1 maxV1 (goRFused max1 r1 r2))
                NonEmpty min' minV' l' -> NonEmpty min' minV' (Bin max1 maxV1 l' (goRFused max1 r1 r2))
        GT -> goL1 minV1 min1 l1 min2 n2

    goL2 !_   Tip !_   !_  = Empty
    goL2 min1 n1  min2 Tip = goLookupL2 (boundKey min2) (xor (boundKey min2) min1) n1
    goL2 _ (Bin max1 _ _ _) min2 (Bin _ _ _ _) | boundsDisjoint min2 max1 = Empty
    goL2 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 _ l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT -> goL2 min1 n1 min2 l2
        EQ | max1 > max2 -> binL (goL2 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | max1 < max2 -> binL (goL2 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> case goL2 min1 l1 min2 l2 of
                Empty -> r2lMap (NonEmpty max1 maxV1 (goRFused max1 r1 r2))
                NonEmpty min' minV' l' -> NonEmpty min' minV' (Bin max1 maxV1 l' (goRFused max1 r1 r2))
        GT | xor (boundKey min2) min1 < xor (boundKey min2) max1 -> goL2 min1 l1 min2 n2 -- min2 is arbitrary here - we just need something from tree 2
           | max1 > max2 -> r2lMap $ goR2 max1 r1 max2 (Bin min2 dummyV l2 r2)
           | max1 < max2 -> r2lMap $ goR1 maxV1 max1 r1 max2 (Bin min2 dummyV l2 r2)
           | otherwise -> r2lMap $ NonEmpty max1 maxV1 (goRFused max1 r1 (Bin min2 dummyV l2 r2))

    goLFused !_ Tip !_ = Tip
    goLFused !_ !_ Tip = Tip
    goLFused !min n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 _ l2 r2) = case compareMSB (xorBounds min max1) (xorBounds min max2) of
            LT -> goLFused min n1 l2
            EQ | max1 > max2 -> case goR2 max1 r1 max2 r2 of
                    Empty -> l'
                    NonEmpty max' maxV' r' -> Bin max' maxV' l' r'
               | max1 < max2 -> case goR1 maxV1 max1 r1 max2 r2 of
                    Empty -> l'
                    NonEmpty max' maxV' r' -> Bin max' maxV' l' r'
               | otherwise -> Bin max1 maxV1 l' (goRFused max1 r1 r2) -- we choose max1 arbitrarily, as max1 == max2
             where
               l' = goLFused min l1 l2
            GT -> goLFused min l1 n2

    goR1 _     !_   !_  !_   Tip = Empty
    goR1 maxV1 max1 Tip max2 n2  = goLookupR1 (boundKey max1) maxV1 (xor (boundKey max1) max2) n2
    goR1 _ max1 (Bin _ _ _ _) _ (Bin min2 _ _ _) | boundsDisjoint min2 max1 = Empty
    goR1 maxV1 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 _ l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT | xor (boundKey max1) min2 > xor (boundKey max1) max2 -> goR1 maxV1 max1 n1 max2 r2 -- max1 is arbitrary here - we just need something from tree 1
           | min1 < min2 -> l2rMap $ goL2 min1 (Bin max1 maxV1 l1 r1) min2 l2
           | min1 > min2 -> l2rMap $ goL1 minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2
           | otherwise -> l2rMap $ NonEmpty min1 minV1 (goLFused min1 (Bin max1 maxV1 l1 r1) l2)
        EQ | min1 < min2 -> binR (goL2 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | min1 > min2 -> binR (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> case goR1 maxV1 max1 r1 max2 r2 of
                Empty -> l2rMap (NonEmpty min1 minV1 (goLFused min1 l1 l2))
                NonEmpty max' maxV' r' -> NonEmpty max' maxV' (Bin min1 minV1 (goLFused min1 l1 l2) r')
        GT -> goR1 maxV1 max1 r1 max2 n2

    goR2 !_   Tip !_   !_  = Empty
    goR2 max1 n1  max2 Tip = goLookupR2 (boundKey max2) (xor (boundKey max2) max1) n1
    goR2 _ (Bin min1 _ _ _) max2 (Bin _ _ _ _) | boundsDisjoint min1 max2 = Empty
    goR2 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 _ l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT -> goR2 max1 n1 max2 r2
        EQ | min1 < min2 -> binR (goL2 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | min1 > min2 -> binR (goL1 minV1 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | otherwise -> case goR2 max1 r1 max2 r2 of
                Empty -> l2rMap (NonEmpty min1 minV1 (goLFused min1 l1 l2))
                NonEmpty max' maxV' r' -> NonEmpty max' maxV' (Bin min1 minV1 (goLFused min1 l1 l2) r')
        GT | xor (boundKey max2) min1 > xor (boundKey max2) max1 -> goR2 max1 r1 max2 n2 -- max2 is arbitrary here - we just need something from tree 2
           | min1 < min2 -> l2rMap $ goL2 min1 l1 min2 (Bin max2 dummyV l2 r2)
           | min1 > min2 -> l2rMap $ goL1 minV1 min1 l1 min2 (Bin max2 dummyV l2 r2)
           | otherwise -> l2rMap $ NonEmpty min1 minV1 (goLFused min1 l1 (Bin max2 dummyV l2 r2))

    goRFused !_ Tip !_ = Tip
    goRFused !_ !_ Tip = Tip
    goRFused !max n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 _ l2 r2) = case compareMSB (xorBounds min1 max) (xorBounds min2 max) of
            LT -> goRFused max n1 r2
            EQ | min1 < min2 -> case goL2 min1 l1 min2 l2 of
                    Empty -> r'
                    NonEmpty min' minV' l' -> Bin min' minV' l' r'
               | min1 > min2 -> case goL1 minV1 min1 l1 min2 l2 of
                    Empty -> r'
                    NonEmpty min' minV' l' -> Bin min' minV' l' r'
               | otherwise -> Bin min1 minV1 (goLFused min1 l1 l2) r' -- we choose max1 arbitrarily, as max1 == max2
             where
               r' = goRFused max r1 r2
            GT -> goRFused max r1 n2

    goLookupL1 !_ _ !_ Tip = Empty
    goLookupL1 k v !xorCache (Bin max _ l r)
        | inMaxBound k max = if xorCache < xorCacheMax
                    then goLookupL1 k v xorCache l
                    else goLookupR1 k v xorCacheMax r
        | outOfMaxBound k max = Empty
        | otherwise = NonEmpty (Bound k) v Tip
      where xorCacheMax = xor k max

    goLookupR1 !_ _ !_ Tip = Empty
    goLookupR1 k v !xorCache (Bin min _ l r)
        | inMinBound k min = if xorCache < xorCacheMin
                    then goLookupR1 k v xorCache r
                    else goLookupL1 k v xorCacheMin l
        | outOfMinBound k min = Empty
        | otherwise = NonEmpty (Bound k) v Tip
      where xorCacheMin = xor k min

    goLookupL2 !_ !_ Tip = Empty
    goLookupL2 k !xorCache (Bin max maxV l r)
        | inMaxBound k max = if xorCache < xorCacheMax
                    then goLookupL2 k xorCache l
                    else goLookupR2 k xorCacheMax r
        | outOfMaxBound k max = Empty
        | otherwise = NonEmpty (Bound k) maxV Tip
      where xorCacheMax = xor k max

    goLookupR2 !_ !_ Tip = Empty
    goLookupR2 k !xorCache (Bin min minV l r)
        | inMinBound k min = if xorCache < xorCacheMin
                    then goLookupR2 k xorCache r
                    else goLookupL2 k xorCacheMin l
        | outOfMinBound k min = Empty
        | otherwise = NonEmpty (Bound k) minV Tip
      where xorCacheMin = xor k min

    dummyV = error "impossible"

-- | /O(n+m)/. Check whether the key sets of two maps are disjoint
-- (i.e. their 'intersection' is empty).
--
-- > disjoint (fromList [(2,'a')]) (fromList [(1,()), (3,())])   == True
-- > disjoint (fromList [(2,'a')]) (fromList [(1,'a'), (2,'b')]) == False
-- > disjoint (fromList [])        (fromList [])                 == True
--
-- > disjoint a b == null (intersection a b)
--
-- @since 0.6.2.1
disjoint :: IntMap a -> IntMap b -> Bool
disjoint = start
  where
    start (IntMap Empty) !_ = True
    start !_ (IntMap Empty) = True
    start (IntMap (NonEmpty min1 _ root1)) (IntMap (NonEmpty min2 _ root2))
        | min1 < min2 = goL min2 root2 min1 root1
        | min1 > min2 = goL min1 root1 min2 root2
        | otherwise = False

    goL :: Bound L -> Node L x -> Bound L -> Node L y -> Bool
    goL !_   !_  !_   Tip = True
    goL min1 Tip min2 n2  = goLookupL (boundKey min1) (xor (boundKey min1) min2) n2
    goL min1 (Bin _ _ _ _) _ (Bin max2 _ _ _) | boundsDisjoint min1 max2 = True
    goL min1 n1@(Bin max1 _ l1 r1) min2 n2@(Bin max2 _ l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT | xor (boundKey min1) min2 < xor (boundKey min1) max2 -> goL min1 n1 min2 l2 -- min1 is arbitrary here - we just need something from tree 1
           | max1 > max2 -> goR max2 r2 max1 (Bin min1 dummyV l1 r1)
           | max1 < max2 -> goR max1 (Bin min1 dummyV l1 r1) max2 r2
           | otherwise -> False
        EQ | max1 > max2 -> goL min1 l1 min2 l2 && goR max2 r2 max1 r1
           | max1 < max2 -> goL min1 l1 min2 l2 && goR max1 r1 max2 r2
           | otherwise -> False
        GT -> goL min1 l1 min2 n2

    goR :: Bound R -> Node R x -> Bound R -> Node R y -> Bool
    goR !_   !_  !_   Tip = True
    goR max1 Tip max2 n2  = goLookupR (boundKey max1) (xor (boundKey max1) max2) n2
    goR max1 (Bin _ _ _ _) _ (Bin min2 _ _ _) | boundsDisjoint min2 max1 = True
    goR max1 n1@(Bin min1 _ l1 r1) max2 n2@(Bin min2 _ l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT | xor (boundKey max1) min2 > xor (boundKey max1) max2 -> goR max1 n1 max2 r2 -- max1 is arbitrary here - we just need something from tree 1
           | min1 < min2 -> goL min2 l2 min1 (Bin max1 dummyV l1 r1)
           | min1 > min2 -> goL min1 (Bin max1 dummyV l1 r1) min2 l2
           | otherwise -> False
        EQ | min1 < min2 -> goL min2 l2 min1 l1 && goR max1 r1 max2 r2
           | min1 > min2 -> goL min1 l1 min2 l2 && goR max1 r1 max2 r2
           | otherwise -> False
        GT -> goR max1 r1 max2 n2

    goLookupL !_ !_ Tip = True
    goLookupL k !xorCache (Bin max _ l r)
        | inMaxBound k max = if xorCache < xorCacheMax
                    then goLookupL k xorCache l
                    else goLookupR k xorCacheMax r
        | outOfMaxBound k max = True
        | otherwise = False
      where xorCacheMax = xor k max

    goLookupR !_ !_ Tip = True
    goLookupR k !xorCache (Bin min _ l r)
        | inMinBound k min = if xorCache < xorCacheMin
                    then goLookupR k xorCache r
                    else goLookupL k xorCacheMin l
        | outOfMinBound k min = True
        | otherwise = False
      where xorCacheMin = xor k min

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
    start (IntMap (NonEmpty min minV root)) = f (boundKey min) minV (goL root z)

    goL Tip acc = acc
    goL (Bin max maxV l r) acc = goL l (goR r (f (boundKey max) maxV acc))

    goR Tip acc = acc
    goR (Bin min minV l r) acc = f (boundKey min) minV (goL l (goR r acc))

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
    start (IntMap (NonEmpty min minV root)) = goL (f z (boundKey min) minV) root

    goL acc Tip = acc
    goL acc (Bin max maxV l r) = f (goR (goL acc l) r) (boundKey max) maxV

    goR acc Tip = acc
    goR acc (Bin min minV l r) = goR (goL (f acc (boundKey min) minV) l) r

-- | /O(n)/. Fold the keys and values in the map using the given monoid, such that
--
-- @'foldMapWithKey' f = 'Prelude.fold' . 'mapWithKey' f@
--
-- This can be an asymptotically faster than 'foldrWithKey' or 'foldlWithKey' for some monoids.
foldMapWithKey :: Monoid m => (Key -> a -> m) -> IntMap a -> m
foldMapWithKey f = start
  where
    start (IntMap Empty) = mempty
    start (IntMap (NonEmpty min minV root)) = f (boundKey min) minV `mappend` goL root

    goL Tip = mempty
    goL (Bin max maxV l r) = goL l `mappend` goR r `mappend` f (boundKey max) maxV

    goR Tip = mempty
    goR (Bin min minV l r) = f (boundKey min) minV `mappend` goL l `mappend` goR r

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
    start (IntMap (NonEmpty min minV root)) = f (boundKey min) minV $! goL root $! z

    goL Tip acc = acc
    goL (Bin max maxV l r) acc = goL l $! goR r $! f (boundKey max) maxV $! acc

    goR Tip acc = acc
    goR (Bin min minV l r) acc = f (boundKey min) minV $! goL l $! goR r $! acc

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlWithKey' :: (a -> Key -> b -> a) -> a -> IntMap b -> a
foldlWithKey' f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty min minV root)) = s goL (s f z (boundKey min) minV) root

    goL acc Tip = acc
    goL acc (Bin max maxV l r) = s f (s goR (s goL acc l) r) (boundKey max) maxV

    goR acc Tip = acc
    goR acc (Bin min minV l r) = s goR (s goL (s f acc (boundKey min) minV) l) r

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
data BuildStack a = Push {-# UNPACK #-} !(Bound L) a !(Node L a) !(BuildStack a) | StackBase

pushBuildStack :: Word -> Key -> a -> Node R a -> BuildStack a -> BuildStack a
pushBuildStack !xorCache !k v !r (Push min minV l stk)
    | xor k min < xorCache = pushBuildStack xorCache k v (Bin min minV l r) stk
pushBuildStack !_ !k v Tip !stk = Push (Bound k) v Tip stk
pushBuildStack !_ !k v (Bin min minV l r) !stk = Push min minV (Bin (Bound k) v l r) stk

completeBuildStack :: Bound R -> a -> Node R a -> BuildStack a -> IntMap_ L a
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
        | p (boundKey min) minV = IntMap (NonEmpty min minV (goL root))
        | otherwise = IntMap (goDeleteL root)

    goL Tip = Tip
    goL (Bin max maxV l r)
        | p (boundKey max) maxV = Bin max maxV (goL l) (goR r)
        | otherwise = case goDeleteR r of
            Empty -> goL l
            NonEmpty max' maxV' r' -> Bin max' maxV' (goL l) r'

    goR Tip = Tip
    goR (Bin min minV l r)
        | p (boundKey min) minV = Bin min minV (goL l) (goR r)
        | otherwise = case goDeleteL l of
            Empty -> goR r
            NonEmpty min' minV' l' -> Bin min' minV' l' (goR r)

    goDeleteL Tip = Empty
    goDeleteL (Bin max maxV l r)
        | p (boundKey max) maxV = case goDeleteL l of
            Empty -> case goR r of
                Tip -> NonEmpty (maxToMin max) maxV Tip
                Bin minI minVI lI rI -> NonEmpty minI minVI (Bin max maxV lI rI)
            NonEmpty min minV l' -> NonEmpty min minV (Bin max maxV l' (goR r))
        | otherwise = binL (goDeleteL l) (goDeleteR r)

    goDeleteR Tip = Empty
    goDeleteR (Bin min minV l r)
        | p (boundKey min) minV = case goDeleteR r of
            Empty -> case goL l of
                Tip -> NonEmpty (minToMax min) minV Tip
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
        | p (boundKey min) minV = let t :*: f = goTrueL root
                                   in (IntMap (NonEmpty min minV t), IntMap f)
        | otherwise  = let t :*: f = goFalseL root
                       in (IntMap t, IntMap (NonEmpty min minV f))

    goTrueL Tip = Tip :*: Empty
    goTrueL (Bin max maxV l r)
        | p (boundKey max) maxV = let tl :*: fl = goTrueL l
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
        | p (boundKey min) minV = let tl :*: fl = goTrueL l
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
        | p (boundKey max) maxV = let tl :*: fl = goFalseL l
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
        | p (boundKey min) minV = let tl :*: fl = goTrueL l
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
        | inMinBound k min = case root of
            Tip -> (m, Nothing, IntMap Empty)
            Bin max maxV l r | inMaxBound k max -> let (DR glb glbV lt, eq, DR lub lubV gt) = go (xor k min) min minV (xor k max) max maxV l r
                                          in (IntMap (r2lMap (NonEmpty glb glbV lt)), eq, IntMap (NonEmpty lub lubV gt))
                             | outOfMaxBound k max -> (m, Nothing, IntMap Empty)
                             | otherwise -> let DR max' maxV' root' = deleteMaxR min minV l r
                                            in (IntMap (r2lMap (NonEmpty max' maxV' root')), Just maxV, IntMap Empty)

        | outOfMinBound k min = (IntMap Empty, Nothing, m)
        | otherwise = case root of
            Tip -> (IntMap Empty, Just minV, IntMap Empty)
            Bin max maxV l r -> let DR min' minV' root' = deleteMinL max maxV l r
                                in (IntMap Empty, Just minV, IntMap (NonEmpty min' minV' root'))

    go xorCacheMin min minV xorCacheMax max maxV l r
        | xorCacheMin < xorCacheMax = case l of
            Tip -> (DR (minToMax min) minV Tip, Nothing, r2lDR (DR max maxV r))
            Bin maxI maxVI lI rI
                | inMaxBound k maxI -> let (lt, eq, DR minI minVI gt) = go xorCacheMin min minV (xor k maxI) maxI maxVI lI rI
                              in (lt, eq, DR minI minVI (Bin max maxV gt r))
                | outOfMaxBound k maxI -> (l2rDR (DR min minV l), Nothing, r2lDR (DR max maxV r))
                | otherwise -> (deleteMaxR min minV lI rI, Just maxVI, r2lDR (DR max maxV r))
        | otherwise = case r of
            Tip -> (l2rDR (DR min minV l), Nothing, DR (maxToMin max) maxV Tip)
            Bin minI minVI lI rI
                | inMinBound k minI -> let (DR maxI maxVI lt, eq, gt) = go (xor k minI) minI minVI xorCacheMax max maxV lI rI
                              in (DR maxI maxVI (Bin min minV l lt), eq, gt)
                | outOfMinBound k minI -> (l2rDR (DR min minV l), Nothing, r2lDR (DR max maxV r))
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

    goL minV1 min1 Tip min2 n2 = goLookupL (boundKey min1) minV1 (xor (boundKey min1) min2) n2
    goL _     _    _   _    Tip = False
    goL minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 (Bin max2 maxV2 l2 r2)
        | max1 > max2 = False
        | max1 < max2 = case xorBounds min1 max1 `ltMSB` xorBounds min2 max2 of
            True | xor (boundKey min1) min2 < xor (boundKey min1) max2 -> goL minV1 min1 n1 min2 l2 -- LT
                 | otherwise -> goR maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2
            False -> goL minV1 min1 l1 min2 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p maxV1 maxV2 && case xorBounds min1 max1 `ltMSB` xorBounds min2 max1 of
            True -> goRFused max1 (Bin min1 minV1 l1 r1) r2 -- LT
            False -> goL minV1 min1 l1 min2 l2 && goRFused max1 r1 r2 -- EQ

    goLFused _ Tip _ = True
    goLFused _ _ Tip = False
    goLFused min n1@(Bin max1 maxV1 l1 r1) (Bin max2 maxV2 l2 r2)
        | max1 > max2 = False
        | max1 < max2 = case xorBounds min max1 `ltMSB` xorBounds min max2 of
            True -> goLFused min n1 l2
            False -> goLFused min l1 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p maxV1 maxV2 && goLFused min l1 l2 && goRFused max1 r1 r2

    goR maxV1 max1 Tip max2 n2 = goLookupR (boundKey max1) maxV1 (xor (boundKey max1) max2) n2
    goR _     _    _   _    Tip = False
    goR maxV1 max1 n1@(Bin min1 minV1 l1 r1) max2 (Bin min2 minV2 l2 r2)
        | min1 < min2 = False
        | min1 > min2 = case xorBounds min1 max1 `ltMSB` xorBounds min2 max2 of
            True | xor (boundKey max1) min2 > xor (boundKey max1) max2 -> goR maxV1 max1 n1 max2 r2 -- LT
                 | otherwise -> goL minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2
            False -> goL minV1 min1 l1 min2 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p minV1 minV2 && case xorBounds min1 max1 `ltMSB` xorBounds min2 max1 of
            True -> goLFused min1 (Bin max1 maxV1 l1 r1) l2 -- LT
            False -> goLFused min1 l1 l2 && goR maxV1 max1 r1 max2 r2 -- EQ

    goRFused _ Tip _ = True
    goRFused _ _ Tip = False
    goRFused max n1@(Bin min1 minV1 l1 r1) (Bin min2 minV2 l2 r2)
        | min1 < min2 = False
        | min1 > min2 = case xorBounds min1 max `ltMSB` xorBounds min2 max of
            True -> goRFused max n1 r2
            False -> goL minV1 min1 l1 min2 l2 && goRFused max r1 r2 -- EQ
        | otherwise = p minV1 minV2 && goLFused min1 l1 l2 && goRFused max r1 r2

    goLookupL _ _ !_ Tip = False
    goLookupL k v !xorCache (Bin max maxV l r)
        | inMaxBound k max = if xorCache < xorCacheMax
                    then goLookupL k v xorCache l
                    else goLookupR k v xorCacheMax r
        | outOfMaxBound k max = False
        | otherwise = p v maxV
      where xorCacheMax = xor k max

    goLookupR _ _ !_ Tip = False
    goLookupR k v !xorCache (Bin min minV l r)
        | inMinBound k min = if xorCache < xorCacheMin
                    then goLookupR k v xorCache r
                    else goLookupL k v xorCacheMin l
        | outOfMinBound k min = False
        | otherwise = p v minV
      where xorCacheMin = xor k min

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

    goL minV1 min1 Tip min2 n2 = goLookupL (boundKey min1) minV1 (xor (boundKey min1) min2) n2
    goL _     _    _   _    Tip = False
    goL minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 (Bin max2 maxV2 l2 r2)
        | max1 > max2 = False
        | max1 < max2 = case xorBounds min1 max1 `ltMSB` xorBounds min2 max2 of
            True | xor (boundKey min1) min2 < xor (boundKey min1) max2 -> goL minV1 min1 n1 min2 l2 -- LT
                 | otherwise -> goR maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2
            False -> goL minV1 min1 l1 min2 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p maxV1 maxV2 && case xorBounds min1 max1 `ltMSB` xorBounds min2 max1 of
            True -> goRFusedBool max1 (Bin min1 minV1 l1 r1) r2 -- LT
            False -> goL minV1 min1 l1 min2 l2 && goRFusedBool max1 r1 r2 -- EQ

    goLFused _ Tip Tip = EQ
    goLFused _ Tip _ = LT
    goLFused _ _ Tip = GT
    goLFused min n1@(Bin max1 maxV1 l1 r1) (Bin max2 maxV2 l2 r2)
        | max1 > max2 = GT
        | max1 < max2 = fromBool $ case xorBounds min max1 `ltMSB` xorBounds min max2 of
            True -> goLFusedBool min n1 l2
            False -> goLFusedBool min l1 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | p maxV1 maxV2 = goLFused min l1 l2 `combine` goRFused max1 r1 r2
        | otherwise = GT

    goLFusedBool _ Tip _ = True
    goLFusedBool _ _ Tip = False
    goLFusedBool min n1@(Bin max1 maxV1 l1 r1) (Bin max2 maxV2 l2 r2)
        | max1 > max2 = False
        | max1 < max2 = case xorBounds min max1 `ltMSB` xorBounds min max2 of
            True -> goLFusedBool min n1 l2
            False -> goLFusedBool min l1 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p maxV1 maxV2 && goLFusedBool min l1 l2 && goRFusedBool max1 r1 r2

    goR maxV1 max1 Tip max2 n2 = goLookupR (boundKey max1) maxV1 (xor (boundKey max1) max2) n2
    goR _     _    _   _    Tip = False
    goR maxV1 max1 n1@(Bin min1 minV1 l1 r1) max2 (Bin min2 minV2 l2 r2)
        | min1 < min2 = False
        | min1 > min2 = case xorBounds min1 max1 `ltMSB` xorBounds min2 max2 of
            True | xor (boundKey max1) min2 > xor (boundKey max1) max2 -> goR maxV1 max1 n1 max2 r2 -- LT
                 | otherwise -> goL minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2
            False -> goL minV1 min1 l1 min2 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p minV1 minV2 && case xorBounds min1 max1 `ltMSB` xorBounds min2 max1 of
            True -> goLFusedBool min1 (Bin max1 maxV1 l1 r1) l2 -- LT
            False -> goLFusedBool min1 l1 l2 && goR maxV1 max1 r1 max2 r2 -- EQ

    goRFused _ Tip Tip = EQ
    goRFused _ Tip _ = LT
    goRFused _ _ Tip = GT
    goRFused max n1@(Bin min1 minV1 l1 r1) (Bin min2 minV2 l2 r2)
        | min1 < min2 = GT
        | min1 > min2 = fromBool $ case xorBounds min1 max `ltMSB` xorBounds min2 max of
            True -> goRFusedBool max n1 r2
            False -> goL minV1 min1 l1 min2 l2 && goRFusedBool max r1 r2 -- EQ
        | p minV1 minV2 = goLFused min1 l1 l2 `combine` goRFused max r1 r2
        | otherwise = GT

    goRFusedBool _ Tip _ = True
    goRFusedBool _ _ Tip = False
    goRFusedBool max n1@(Bin min1 minV1 l1 r1) (Bin min2 minV2 l2 r2)
        | min1 < min2 = False
        | min1 > min2 = case xorBounds min1 max `ltMSB` xorBounds min2 max of
            True -> goRFusedBool max n1 r2
            False -> goL minV1 min1 l1 min2 l2 && goRFusedBool max r1 r2 -- EQ
        | otherwise = p minV1 minV2 && goLFusedBool min1 l1 l2 && goRFusedBool max r1 r2

    goLookupL _ _ !_ Tip = False
    goLookupL k v !xorCache (Bin max maxV l r)
        | inMaxBound k max = if xorCache < xorCacheMax
                    then goLookupL k v xorCache l
                    else goLookupR k v xorCacheMax r
        | outOfMaxBound k max = False
        | otherwise = p v maxV
      where xorCacheMax = xor k max

    goLookupR _ _ !_ Tip = False
    goLookupR k v !xorCache (Bin min minV l r)
        | inMinBound k min = if xorCache < xorCacheMin
                    then goLookupR k v xorCache r
                    else goLookupL k v xorCacheMin l
        | outOfMinBound k min = False
        | otherwise = p v minV
      where  xorCacheMin = xor k min

    fromBool True = LT
    fromBool False = GT

    combine GT _ = GT
    combine _ GT = GT
    combine EQ EQ = EQ
    combine _ _ = LT


-- | /O(1)/. The minimal key of the map. Returns 'Nothing' if the map is empty.
lookupMin :: IntMap a -> Maybe (Key, a)
lookupMin (IntMap Empty) = Nothing
lookupMin (IntMap (NonEmpty min minV _)) = Just (boundKey min, minV)

-- | /O(1)/. The maximal key of the map. Returns 'Nothing' if the map is empty.
lookupMax :: IntMap a -> Maybe (Key, a)
lookupMax (IntMap Empty) = Nothing
lookupMax (IntMap (NonEmpty min minV root)) = case root of
    Tip -> Just (boundKey min, minV)
    Bin max maxV _ _ -> Just (boundKey max, maxV)

-- | /O(1)/. The minimal key of the map.
findMin :: IntMap a -> (Key, a)
findMin (IntMap Empty) = error "findMin: empty map has no minimal element"
findMin (IntMap (NonEmpty min minV _)) = (boundKey min, minV)

-- | /O(1)/. The maximal key of the map.
findMax :: IntMap a -> (Key, a)
findMax (IntMap Empty) = error "findMin: empty map has no minimal element"
findMax (IntMap (NonEmpty min minV root)) = case root of
    Tip -> (boundKey min, minV)
    Bin max maxV _ _ -> (boundKey max, maxV)

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

-- | /O(1)/. Returns whether the most significant bit of its first
-- argument is less significant than the most significant bit of its
-- second argument.

-- This works by measuring whether x is in between 0 and y but closer to 0 (in the xor metric).
{-# INLINE ltMSB #-}
ltMSB :: Word -> Word -> Bool
ltMSB x y = x < y && x < Data.Bits.xor x y

-- See 'ltMSB' for why this works
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

{-# INLINE minToMax #-}
minToMax :: Bound L -> Bound R
minToMax = Bound . boundKey

{-# INLINE maxToMin #-}
maxToMin :: Bound R -> Bound L
maxToMin = Bound . boundKey

{-# INLINE l2rMap #-}
l2rMap :: IntMap_ L a -> IntMap_ R a
l2rMap Empty = Empty
l2rMap (NonEmpty min minV Tip) = NonEmpty (minToMax min) minV Tip
l2rMap (NonEmpty min minV (Bin max maxV l r)) = NonEmpty max maxV (Bin min minV l r)

{-# INLINE r2lMap #-}
r2lMap :: IntMap_ R a -> IntMap_ L a
r2lMap Empty = Empty
r2lMap (NonEmpty max maxV Tip) = NonEmpty (maxToMin max) maxV Tip
r2lMap (NonEmpty max maxV (Bin min minV l r)) = NonEmpty min minV (Bin max maxV l r)

{-# INLINE l2rDR #-}
l2rDR :: DeleteResult L a -> DeleteResult R a
l2rDR (DR min minV Tip) = DR (minToMax min) minV Tip
l2rDR (DR min minV (Bin max maxV l r)) = DR max maxV (Bin min minV l r)

{-# INLINE r2lDR #-}
r2lDR :: DeleteResult R a -> DeleteResult L a
r2lDR (DR max maxV Tip) = DR (maxToMin max) maxV Tip
r2lDR (DR max maxV (Bin min minV l r)) = DR min minV (Bin max maxV l r)

-- | Insert a key/value pair to a left node where the key is smaller than
-- any present in that node. Requires the xor of the inserted key and the
-- key immediately prior to it (the minimum bound of the node).
insertMinL :: Word -> Bound L -> a -> Node L a -> Node L a
insertMinL !_ !min minV Tip = Bin (minToMax min) minV Tip Tip
insertMinL !xorCache !min minV (Bin max maxV l r)
    -- Although the new minimum is not directly passed into 'insertMinL',
    -- it is captured in the 'xorCache'. We use standard navigation to
    -- determine whether 'min' should belong in the left or right branch.
    -- Since 'min' is, by assumption, smaller than any key in the tree,
    -- if 'min' is assigned to the right branch than the entire subtree
    -- must fit in the right branch. Otherwise, we need to continue recursing.
    | xor (boundKey min) max < xorCache = Bin max maxV Tip (Bin min minV l r)
    | otherwise = Bin max maxV (insertMinL xorCache min minV l) r

-- | Insert a key/value pair to a right node where the key is greater than
-- any present in that node. Requires the xor of the inserted key and the
-- key immediately following it (the maximum bound of the node).
insertMaxR :: Word -> Bound R -> a -> Node R a -> Node R a
insertMaxR !_ !max maxV Tip = Bin (maxToMin max) maxV Tip Tip
insertMaxR !xorCache !max maxV (Bin min minV l r)
    | xor (boundKey max) min < xorCache = Bin min minV (Bin max maxV l r) Tip
    | otherwise = Bin min minV l (insertMaxR xorCache max maxV r)

-- | Delete the minimum key/value pair from an unpacked left node, returning
-- a new left node in a DeleteResult.
deleteMinL :: Bound R -> a -> Node L a -> Node R a -> DeleteResult L a
deleteMinL !max maxV Tip Tip = DR (maxToMin max) maxV Tip
deleteMinL !max maxV Tip (Bin min minV l r) = DR min minV (Bin max maxV l r)
deleteMinL !max maxV (Bin innerMax innerMaxV innerL innerR) r =
    let DR min minV inner = deleteMinL innerMax innerMaxV innerL innerR
    in  DR min minV (Bin max maxV inner r)

-- | Delete the maximum key/value pair from an unpacked right node, returning
-- a new right node in a DeleteResult.
deleteMaxR :: Bound L -> a -> Node L a -> Node R a -> DeleteResult R a
deleteMaxR !min minV Tip Tip = DR (minToMax min) minV Tip
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
    | inMaxBound k max = if xorCache < xorCacheMax
                then Bin max maxV (deleteL k xorCache l) r
                else Bin max maxV l (deleteR k xorCacheMax r)
    | outOfMaxBound k max = n
    | otherwise = extractBinL l r
  where xorCacheMax = xor k max

-- | Delete a key from a right node. Takes the xor of the deleted key and
-- the maximum bound of that node.
deleteR :: Key -> Word -> Node R a -> Node R a
deleteR !_ !_ Tip = Tip
deleteR !k !xorCache n@(Bin min minV l r)
    | inMinBound k min = if xorCache < xorCacheMin
                then Bin min minV l (deleteR k xorCache r)
                else Bin min minV (deleteL k xorCacheMin l) r
    | outOfMinBound k min = n
    | otherwise = extractBinR l r
  where xorCacheMin = xor k min
