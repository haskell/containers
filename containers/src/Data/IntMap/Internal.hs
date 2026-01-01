{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}
#endif

{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMap.Internal
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
--                (c) wren romano 2016
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
--
-- = Finite Int Maps (lazy interface internals)
--
-- The @'IntMap' v@ type represents a finite map (sometimes called a dictionary)
-- from keys of type @Int@ to values of type @v@.
--
--
-- == Implementation
--
-- The implementation is based on /big-endian patricia trees/.  This data
-- structure performs especially well on binary operations like 'union'
-- and 'intersection'. Additionally, benchmarks show that it is also
-- (much) faster on insertions and deletions when compared to a generic
-- size-balanced map implementation (see "Data.Map").
--
--    * Chris Okasaki and Andy Gill,
--      \"/Fast Mergeable Integer Maps/\",
--      Workshop on ML, September 1998, pages 77-86,
--      <https://web.archive.org/web/20150417234429/https://ittc.ku.edu/~andygill/papers/IntMap98.pdf>.
--
--    * D.R. Morrison,
--      \"/PATRICIA -- Practical Algorithm To Retrieve Information Coded In Alphanumeric/\",
--      Journal of the ACM, 15(4), October 1968, pages 514-534,
--      <https://doi.org/10.1145/321479.321481>.
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


-- [Note: Order of constructors]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The order of constructors of IntMap matters when considering performance.
-- Currently in GHC 7.0, when type has 3 constructors, they are matched from
-- the first to the last -- the best performance is achieved when the
-- constructors are ordered by frequency.
-- On GHC 7.0, reordering constructors from Nil | Tip | Bin to Bin | Tip | Nil
-- improves the benchmark by circa 10%.
--

module Data.IntMap.Internal (
    -- * Map type
      IntMap(..)          -- instance Eq,Show
    , Key

    -- * Operators
    , (!), (!?), (\\)

    -- * Query
    , null
    , size
    , compareSize
    , member
    , notMember
    , lookup
    , findWithDefault
    , lookupLT
    , lookupGT
    , lookupLE
    , lookupGE
    , disjoint

    -- * Construction
    , empty
    , singleton
    , fromSet
    , fromSetA

    -- ** Insertion
    , insert
    , insertWith
    , insertWithKey
    , insertLookupWithKey

    -- ** Delete\/Update
    , delete
    , pop
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , upsert
    , updateLookupWithKey
    , alter
    , alterF

    -- * Combine

    -- ** Union
    , union
    , unionWith
    , unionWithKey
    , unions
    , unionsWith

    -- ** Difference
    , difference
    , differenceWith
    , differenceWithKey

    -- ** Intersection
    , intersection
    , intersectionWith
    , intersectionWithKey

    -- ** Symmetric difference
    , symmetricDifference

    -- ** Compose
    , compose

    -- ** General combining function
    , SimpleWhenMissing
    , SimpleWhenMatched
    , runWhenMatched
    , runWhenMissing
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

    -- ** Applicative general combining function
    , WhenMissing (..)
    , WhenMatched (..)
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

    -- ** Deprecated general combining function
    , mergeWithKey
    , mergeWithKey'

    -- * Traversal
    -- ** Map
    , map
    , mapWithKey
    , traverseWithKey
    , traverseMaybeWithKey
    , mapAccum
    , mapAccumWithKey
    , mapAccumRWithKey
    , mapKeys
    , mapKeysWith
    , mapKeysMonotonic

    -- * Folds
    , foldr
    , foldl
    , foldrWithKey
    , foldlWithKey
    , foldMapWithKey

    -- ** Strict folds
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'

    -- * Conversion
    , elems
    , keys
    , assocs
    , keysSet

    -- ** Lists
    , toList
    , fromList
    , fromListWith
    , fromListWithKey

    -- ** Ordered lists
    , toAscList
    , toDescList
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromDistinctAscList

    -- * Filter
    , filter
    , filterKeys
    , filterWithKey
    , restrictKeys
    , withoutKeys
    , partition
    , partitionWithKey

    , takeWhileAntitone
    , dropWhileAntitone
    , spanAntitone

    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey

    , split
    , splitLookup
    , splitRoot

    -- * Submap
    , isSubmapOf, isSubmapOfBy
    , isProperSubmapOf, isProperSubmapOfBy

    -- * Min\/Max
    , lookupMin
    , lookupMax
    , findMin
    , findMax
    , deleteMin
    , deleteMax
    , deleteFindMin
    , deleteFindMax
    , updateMin
    , updateMax
    , updateMinWithKey
    , updateMaxWithKey
    , minView
    , maxView
    , minViewWithKey
    , maxViewWithKey

    -- * Debugging
    , showTree
    , showTreeWith

    -- * Utility
    , link
    , linkKey
    , bin
    , binCheckL
    , binCheckR
    , MonoState(..)
    , Stack(..)
    , ascLinkTop
    , ascLinkAll
    , IntMapBuilder(..)
    , BStack(..)
    , emptyB
    , insertB
    , finishB
    , moveToB
    , MoveResult(..)

    -- * Used by "IntMap.Merge.Lazy" and "IntMap.Merge.Strict"
    , mapWhenMissing
    , mapWhenMatched
    , lmapWhenMissing
    , contramapFirstWhenMatched
    , contramapSecondWhenMatched
    , mapGentlyWhenMissing
    , mapGentlyWhenMatched
    ) where

import Data.Functor.Identity (Identity (..))
import Data.Semigroup (Semigroup(stimes))
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.Semigroup (stimesIdempotentMonoid)
import Data.Functor.Classes

import Control.DeepSeq (NFData(rnf),NFData1(liftRnf))
import Data.Bits
import qualified Data.Foldable as Foldable
import Data.Maybe (fromMaybe)
import Utils.Containers.Internal.Prelude hiding
  (lookup, map, filter, foldr, foldl, foldl', foldMap, null)
import Prelude ()

import Data.IntSet.Internal (IntSet)
import qualified Data.IntSet.Internal as IntSet
import Data.IntSet.Internal.IntTreeCommons
  ( Key
  , Prefix(..)
  , nomatch
  , left
  , signBranch
  , mask
  , branchMask
  , branchPrefix
  , TreeTreeBranch(..)
  , treeTreeBranch
  , i2w
  , Order(..)
  )
import Utils.Containers.Internal.BitUtil (shiftLL, shiftRL, iShiftRL, wordSize)
import Utils.Containers.Internal.Strict
  (StrictPair(..), StrictTriple(..), toPair)

#ifdef __GLASGOW_HASKELL__
import Data.Coerce
import Data.Data (Data(..), Constr, mkConstr, constrIndex,
                  DataType, mkDataType, gcast1)
import qualified Data.Data as Data
import GHC.Exts (build)
import qualified GHC.Exts as GHCExts
import Language.Haskell.TH.Syntax (Lift)
-- See Note [ Template Haskell Dependencies ]
import Language.Haskell.TH ()
#endif
#if defined(__GLASGOW_HASKELL__) || defined(__MHS__)
import Text.Read
#endif
import qualified Control.Category as Category


{--------------------------------------------------------------------
  Types
--------------------------------------------------------------------}


-- | A map of integers to values @a@.

-- See Note: Order of constructors
data IntMap a = Bin {-# UNPACK #-} !Prefix
                    !(IntMap a)
                    !(IntMap a)
              | Tip {-# UNPACK #-} !Key a
              | Nil

--
-- Note [IntMap structure and invariants]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- * Nil is never found as a child of Bin.
--
-- * The Prefix of a Bin indicates the common high-order bits that all keys in
--   the Bin share.
--
-- * The least significant set bit of the Int value of a Prefix is called the
--   mask bit.
--
-- * All the bits to the left of the mask bit are called the shared prefix. All
--   keys stored in the Bin begin with the shared prefix.
--
-- * All keys in the left child of the Bin have the mask bit unset, and all keys
--   in the right child have the mask bit set. It follows that
--
--   1. The Int value of the Prefix of a Bin is the smallest key that can be
--      present in the right child of the Bin.
--
--   2. All keys in the right child of a Bin are greater than keys in the
--      left child, with one exceptional situation. If the Bin separates
--      negative and non-negative keys, the mask bit is the sign bit and the
--      left child stores the non-negative keys while the right child stores the
--      negative keys.
--
-- * All bits to the right of the mask bit are set to 0 in a Prefix.
--

-- See Note [Okasaki-Gill] for how the implementation here relates to the one in
-- Okasaki and Gill's paper.

#ifdef __GLASGOW_HASKELL__
-- | @since 0.6.6
deriving instance Lift a => Lift (IntMap a)
#endif

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}

-- | \(O(\min(n,W))\). Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- __Note__: This function is partial. Prefer '!?'.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'

(!) :: IntMap a -> Key -> a
(!) m k = find k m

-- | \(O(\min(n,W))\). Find the value at a key.
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
m1 \\ m2 = difference m1 m2

infixl 9 !?,\\{-This comment teaches CPP correct behaviour -}

{--------------------------------------------------------------------
  Types
--------------------------------------------------------------------}

-- | @mempty@ = 'empty'
instance Monoid (IntMap a) where
    mempty  = empty
    mconcat = unions
#if !MIN_VERSION_base(4,11,0)
    mappend = (<>)
#endif

-- | @(<>)@ = 'union'
--
-- @since 0.5.7
instance Semigroup (IntMap a) where
    (<>)    = union
    stimes  = stimesIdempotentMonoid

-- | Folds in order of increasing key.
instance Foldable.Foldable IntMap where
  fold = foldMap id
  {-# INLINABLE fold #-}
  foldr = foldr
  {-# INLINE foldr #-}
  foldl = foldl
  {-# INLINE foldl #-}
  foldMap = foldMap
  {-# INLINE foldMap #-}
  foldl' = foldl'
  {-# INLINE foldl' #-}
  foldr' = foldr'
  {-# INLINE foldr' #-}
  length = size
  {-# INLINE length #-}
  null   = null
  {-# INLINE null #-}
  toList = elems -- NB: Foldable.toList /= IntMap.toList
  {-# INLINE toList #-}
  elem = go
    where go !_ Nil = False
          go x (Tip _ y) = x == y
          go x (Bin _ l r) = go x l || go x r
  {-# INLINABLE elem #-}
  maximum = start
    where start Nil = error "Data.Foldable.maximum (for Data.IntMap): empty map"
          start (Tip _ y) = y
          start (Bin p l r)
            | signBranch p = go (start r) l
            | otherwise = go (start l) r

          go !m Nil = m
          go m (Tip _ y) = max m y
          go m (Bin _ l r) = go (go m l) r
  {-# INLINABLE maximum #-}
  minimum = start
    where start Nil = error "Data.Foldable.minimum (for Data.IntMap): empty map"
          start (Tip _ y) = y
          start (Bin p l r)
            | signBranch p = go (start r) l
            | otherwise = go (start l) r

          go !m Nil = m
          go m (Tip _ y) = min m y
          go m (Bin _ l r) = go (go m l) r
  {-# INLINABLE minimum #-}
  sum = foldl' (+) 0
  {-# INLINABLE sum #-}
  product = foldl' (*) 1
  {-# INLINABLE product #-}

-- | Traverses in order of increasing key.
instance Traversable IntMap where
    traverse f = traverseWithKey (\_ -> f)
    {-# INLINE traverse #-}

instance NFData a => NFData (IntMap a) where
    rnf Nil = ()
    rnf (Tip _ v) = rnf v
    rnf (Bin _ l r) = rnf l `seq` rnf r

-- | @since 0.8
instance NFData1 IntMap where
    liftRnf rnfx = go
      where
      go Nil         = ()
      go (Tip _ v)   = rnfx v
      go (Bin _ l r) = go l `seq` go r

#if __GLASGOW_HASKELL__

{--------------------------------------------------------------------
  A Data instance
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We provide limited reflection services for the sake of data abstraction.

instance Data a => Data (IntMap a) where
  gfoldl f z im = z fromList `f` (toList im)
  toConstr _     = fromListConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  dataTypeOf _   = intMapDataType
  dataCast1 f    = gcast1 f

fromListConstr :: Constr
fromListConstr = mkConstr intMapDataType "fromList" [] Data.Prefix

intMapDataType :: DataType
intMapDataType = mkDataType "Data.IntMap.Internal.IntMap" [fromListConstr]

#endif

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | \(O(1)\). Is the map empty?
--
-- > Data.IntMap.null (empty)           == True
-- > Data.IntMap.null (singleton 1 'a') == False

null :: IntMap a -> Bool
null Nil = True
null _   = False
{-# INLINE null #-}

-- | \(O(n)\). Number of entries in the map.
--
-- __Note__: Unlike @Data.Map.'Data.Map.Lazy.size'@, this is /not/ \(O(1)\).
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3
--
-- See also: 'compareSize'
size :: IntMap a -> Int
size = go 0
  where
    go !acc (Bin _ l r) = go (go acc l) r
    go acc (Tip _ _) = 1 + acc
    go acc Nil = acc

-- | \(O(\min(n,c))\). Compare the number of entries in the map to an @Int@.
--
-- @compareSize m c@ returns the same result as @compare ('size' m) c@ but is
-- more efficient when @c@ is smaller than the size of the map.
--
-- @since FIXME
compareSize :: IntMap a -> Int -> Ordering
compareSize Nil c0 = compare 0 c0
compareSize _ c0 | c0 <= 0 = GT
compareSize t c0 = compare 0 (go t (c0 - 1))
  where
    go (Bin _ _ _) 0 = -1
    go (Bin _ l r) c
      | c' < 0 = c'
      | otherwise = go r c'
      where
        c' = go l (c - 1)
    go _ c = c

-- | \(O(\min(n,W))\). Is the key a member of the map?
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False

-- See Note: Local 'go' functions and capturing]
member :: Key -> IntMap a -> Bool
member !k = go
  where
    go (Bin p l r)
      | nomatch k p = False
      | left k p    = go l
      | otherwise   = go r
    go (Tip kx _) = k == kx
    go Nil = False

-- | \(O(\min(n,W))\). Is the key not a member of the map?
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True

notMember :: Key -> IntMap a -> Bool
notMember k m = not $ member k m

-- | \(O(\min(n,W))\). Look up the value at a key in the map. See also 'Data.Map.lookup'.

-- See Note: Local 'go' functions and capturing
lookup :: Key -> IntMap a -> Maybe a
lookup !k = go
  where
    go (Bin p l r) | left k p  = go l
                   | otherwise = go r
    go (Tip kx x) | k == kx   = Just x
                  | otherwise = Nothing
    go Nil = Nothing

-- See Note: Local 'go' functions and capturing]
find :: Key -> IntMap a -> a
find !k = go
  where
    go (Bin p l r) | left k p  = go l
                   | otherwise = go r
    go (Tip kx x) | k == kx   = x
                  | otherwise = not_found
    go Nil = not_found

    not_found = error ("IntMap.!: key " ++ show k ++ " is not an element of the map")

-- | \(O(\min(n,W))\). The expression @('findWithDefault' def k map)@
-- returns the value at key @k@ or returns @def@ when the key is not an
-- element of the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'

-- See Note: Local 'go' functions and capturing]
findWithDefault :: a -> Key -> IntMap a -> a
findWithDefault def !k = go
  where
    go (Bin p l r) | nomatch k p = def
                   | left k p    = go l
                   | otherwise   = go r
    go (Tip kx x) | k == kx   = x
                  | otherwise = def
    go Nil = def

-- | \(O(\min(n,W))\). Find largest key smaller than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupLT 3 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLT 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')

-- See Note: Local 'go' functions and capturing.
lookupLT :: Key -> IntMap a -> Maybe (Key, a)
lookupLT !k t = case t of
    Bin p l r | signBranch p -> if k >= 0 then go r l else go Nil r
    _ -> go Nil t
  where
    go def (Bin p l r)
      | nomatch k p = if k < unPrefix p then unsafeFindMax def else unsafeFindMax r
      | left k p  = go def l
      | otherwise = go l r
    go def (Tip ky y)
      | k <= ky   = unsafeFindMax def
      | otherwise = Just (ky, y)
    go def Nil = unsafeFindMax def

-- | \(O(\min(n,W))\). Find smallest key greater than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupGT 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGT 5 (fromList [(3,'a'), (5,'b')]) == Nothing

-- See Note: Local 'go' functions and capturing.
lookupGT :: Key -> IntMap a -> Maybe (Key, a)
lookupGT !k t = case t of
    Bin p l r | signBranch p -> if k >= 0 then go Nil l else go l r
    _ -> go Nil t
  where
    go def (Bin p l r)
      | nomatch k p = if k < unPrefix p then unsafeFindMin l else unsafeFindMin def
      | left k p  = go r l
      | otherwise = go def r
    go def (Tip ky y)
      | k >= ky   = unsafeFindMin def
      | otherwise = Just (ky, y)
    go def Nil = unsafeFindMin def

-- | \(O(\min(n,W))\). Find largest key smaller or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupLE 2 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLE 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupLE 5 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')

-- See Note: Local 'go' functions and capturing.
lookupLE :: Key -> IntMap a -> Maybe (Key, a)
lookupLE !k t = case t of
    Bin p l r | signBranch p -> if k >= 0 then go r l else go Nil r
    _ -> go Nil t
  where
    go def (Bin p l r)
      | nomatch k p = if k < unPrefix p then unsafeFindMax def else unsafeFindMax r
      | left k p  = go def l
      | otherwise = go l r
    go def (Tip ky y)
      | k < ky    = unsafeFindMax def
      | otherwise = Just (ky, y)
    go def Nil = unsafeFindMax def

-- | \(O(\min(n,W))\). Find smallest key greater or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupGE 3 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupGE 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGE 6 (fromList [(3,'a'), (5,'b')]) == Nothing

-- See Note: Local 'go' functions and capturing.
lookupGE :: Key -> IntMap a -> Maybe (Key, a)
lookupGE !k t = case t of
    Bin p l r | signBranch p -> if k >= 0 then go Nil l else go l r
    _ -> go Nil t
  where
    go def (Bin p l r)
      | nomatch k p = if k < unPrefix p then unsafeFindMin l else unsafeFindMin def
      | left k p  = go r l
      | otherwise = go def r
    go def (Tip ky y)
      | k > ky    = unsafeFindMin def
      | otherwise = Just (ky, y)
    go def Nil = unsafeFindMin def


-- Helper function for lookupGE and lookupGT. It assumes that if a Bin node is
-- given, it has m > 0.
unsafeFindMin :: IntMap a -> Maybe (Key, a)
unsafeFindMin Nil = Nothing
unsafeFindMin (Tip ky y) = Just (ky, y)
unsafeFindMin (Bin _ l _) = unsafeFindMin l

-- Helper function for lookupLE and lookupLT. It assumes that if a Bin node is
-- given, it has m > 0.
unsafeFindMax :: IntMap a -> Maybe (Key, a)
unsafeFindMax Nil = Nothing
unsafeFindMax (Tip ky y) = Just (ky, y)
unsafeFindMax (Bin _ _ r) = unsafeFindMax r

{--------------------------------------------------------------------
  Disjoint
--------------------------------------------------------------------}
-- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
-- Check whether the key sets of two maps are disjoint
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
disjoint Nil _ = True
disjoint _ Nil = True
disjoint (Tip kx _) ys = notMember kx ys
disjoint xs (Tip ky _) = notMember ky xs
disjoint t1@(Bin p1 l1 r1) t2@(Bin p2 l2 r2) = case treeTreeBranch p1 p2 of
  ABL -> disjoint l1 t2
  ABR -> disjoint r1 t2
  BAL -> disjoint t1 l2
  BAR -> disjoint t1 r2
  EQL -> disjoint l1 l2 && disjoint r1 r2
  NOM -> True

{--------------------------------------------------------------------
  Compose
--------------------------------------------------------------------}
-- | Given maps @bc@ and @ab@, relate the keys of @ab@ to the values of @bc@,
-- by using the values of @ab@ as keys for lookups in @bc@.
--
-- Complexity: \( O(n * \min(m,W)) \), where \(m\) is the size of the first argument
--
-- > compose (fromList [('a', "A"), ('b', "B")]) (fromList [(1,'a'),(2,'b'),(3,'z')]) = fromList [(1,"A"),(2,"B")]
--
-- @
-- ('compose' bc ab '!?') = (bc '!?') <=< (ab '!?')
-- @
--
-- __Note:__ Prior to v0.6.4, "Data.IntMap.Strict" exposed a version of
-- 'compose' that forced the values of the output 'IntMap'. This version does
-- not force these values.
--
-- @since 0.6.3.1
compose :: IntMap c -> IntMap Int -> IntMap c
compose bc !ab
  | null bc = empty
  | otherwise = mapMaybe (bc !?) ab

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | \(O(1)\). The empty map.
--
-- > empty      == fromList []
-- > size empty == 0

empty :: IntMap a
empty
  = Nil
{-# INLINE empty #-}

-- | \(O(1)\). A map of one element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1

singleton :: Key -> a -> IntMap a
singleton k x
  = Tip k x
{-# INLINE singleton #-}

{--------------------------------------------------------------------
  Insert
--------------------------------------------------------------------}
-- | \(O(\min(n,W))\). Insert a new key\/value pair in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value, i.e. 'insert' is equivalent to
-- @'insertWith' 'const'@.
--
-- > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
-- > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
-- > insert 5 'x' empty                         == singleton 5 'x'

insert :: Key -> a -> IntMap a -> IntMap a
insert !k x t@(Bin p l r)
  | nomatch k p = linkKey k (Tip k x) p t
  | left k p    = Bin p (insert k x l) r
  | otherwise   = Bin p l (insert k x r)
insert k x t@(Tip ky _)
  | k==ky         = Tip k x
  | otherwise     = link k (Tip k x) ky t
insert k x Nil = Tip k x

-- right-biased insertion, used by 'union'
-- | \(O(\min(n,W))\). Insert with a combining function.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert @f new_value old_value@.
--
-- > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
-- > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"
--
-- Also see the performance note on 'fromListWith'.

insertWith :: (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWith f k x t
  = insertWithKey (\_ x' y' -> f x' y') k x t

-- | \(O(\min(n,W))\). Insert with a combining function.
-- @'insertWithKey' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert @f key new_value old_value@.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
-- > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"
--
-- Also see the performance note on 'fromListWith'.

insertWithKey :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWithKey f !k x t@(Bin p l r)
  | nomatch k p = linkKey k (Tip k x) p t
  | left k p    = Bin p (insertWithKey f k x l) r
  | otherwise   = Bin p l (insertWithKey f k x r)
insertWithKey f k x t@(Tip ky y)
  | k == ky       = Tip k (f k x y)
  | otherwise     = link k (Tip k x) ky t
insertWithKey _ k x Nil = Tip k x

-- | \(O(\min(n,W))\). The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
-- > insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
-- > insertLookupWithKey f 5 "xxx" empty                         == (Nothing,  singleton 5 "xxx")
--
-- This is how to define @insertLookup@ using @insertLookupWithKey@:
--
-- > let insertLookup kx x t = insertLookupWithKey (\_ a _ -> a) kx x t
-- > insertLookup 5 "x" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "x")])
-- > insertLookup 7 "x" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "x")])
--
-- Also see the performance note on 'fromListWith'.

insertLookupWithKey :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> (Maybe a, IntMap a)
insertLookupWithKey f !k x t@(Bin p l r)
  | nomatch k p = (Nothing,linkKey k (Tip k x) p t)
  | left k p    = let (found,l') = insertLookupWithKey f k x l
                  in (found,Bin p l' r)
  | otherwise   = let (found,r') = insertLookupWithKey f k x r
                  in (found,Bin p l r')
insertLookupWithKey f k x t@(Tip ky y)
  | k == ky       = (Just y,Tip k (f k x y))
  | otherwise     = (Nothing,link k (Tip k x) ky t)
insertLookupWithKey _ k x Nil = (Nothing,Tip k x)


{--------------------------------------------------------------------
  Deletion
--------------------------------------------------------------------}
-- | \(O(\min(n,W))\). Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
--
-- > delete 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > delete 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > delete 5 empty                         == empty

delete :: Key -> IntMap a -> IntMap a
delete !k t@(Bin p l r)
  | nomatch k p = t
  | left k p    = binCheckL p (delete k l) r
  | otherwise   = binCheckR p l (delete k r)
delete k t@(Tip ky _)
  | k == ky       = Nil
  | otherwise     = t
delete _k Nil = Nil

-- | \(O(\min(n,W))\). Pop an entry from the map.
--
-- Returns @Nothing@ if the key is not in the map. Otherwise returns @Just@ the
-- value at the key and a map with the entry removed.
--
-- @
-- pop 1 (fromList [(0,"a"),(2,"b"),(4,"c")]) == Nothing
-- pop 2 (fromList [(0,"a"),(2,"b"),(4,"c")]) == Just ("b",fromList [(0,"a"),(4,"c")])
-- @
--
-- @since FIXME
pop :: Key -> IntMap a -> Maybe (a, IntMap a)
pop k0 t0 = case go k0 t0 of
  Popped (Just y) t -> Just (y, t)
  _ -> Nothing
  where
    go !k (Bin p l r)
      | nomatch k p = Popped Nothing Nil
      | left k p = case go k l of
          Popped y@(Just _) l' -> Popped y (binCheckL p l' r)
          q -> q
      | otherwise = case go k r of
          Popped y@(Just _) r' -> Popped y (binCheckR p l r')
          q -> q
    go !k (Tip ky y)
      | k == ky = Popped (Just y) Nil
      | otherwise = Popped Nothing Nil
    go !_ Nil = Popped Nothing Nil

-- See Note [Popped impl] in Data.Map.Internal
data Popped k a = Popped
#if __GLASGOW_HASKELL__ >= 906
  {-# UNPACK #-}
#endif
  !(Maybe a)
  !(IntMap a)

-- | \(O(\min(n,W))\). Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty

adjust ::  (a -> a) -> Key -> IntMap a -> IntMap a
adjust f k m
  = adjustWithKey (\_ x -> f x) k m

-- | \(O(\min(n,W))\). Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty

adjustWithKey ::  (Key -> a -> a) -> Key -> IntMap a -> IntMap a
adjustWithKey f !k (Bin p l r)
  | left k p      = Bin p (adjustWithKey f k l) r
  | otherwise     = Bin p l (adjustWithKey f k r)
adjustWithKey f k t@(Tip ky y)
  | k == ky       = Tip ky (f k y)
  | otherwise     = t
adjustWithKey _ _ Nil = Nil


-- | \(O(\min(n,W))\). The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

update ::  (a -> Maybe a) -> Key -> IntMap a -> IntMap a
update f
  = updateWithKey (\_ x -> f x)

-- | \(O(\min(n,W))\). The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f k x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateWithKey ::  (Key -> a -> Maybe a) -> Key -> IntMap a -> IntMap a
updateWithKey f !k (Bin p l r)
  | left k p      = binCheckL p (updateWithKey f k l) r
  | otherwise     = binCheckR p l (updateWithKey f k r)
updateWithKey f k t@(Tip ky y)
  | k == ky       = case (f k y) of
                      Just y' -> Tip ky y'
                      Nothing -> Nil
  | otherwise     = t
updateWithKey _ _ Nil = Nil

-- | \(O(\min(n,W))\). Update the value at a key or insert a value if the key is
-- not in the map.
--
-- @
-- let inc = maybe 1 (+1)
-- upsert inc 100 (fromList [(100,1),(300,2)]) == fromList [(100,2),(300,2)]
-- upsert inc 200 (fromList [(100,1),(300,2)]) == fromList [(100,1),(200,1),(300,2)]
-- @
--
-- @since FIXME
upsert :: (Maybe a -> a) -> Key -> IntMap a -> IntMap a
upsert f !k t@(Bin p l r)
  | nomatch k p = linkKey k (Tip k (f Nothing)) p t
  | left k p = Bin p (upsert f k l) r
  | otherwise = Bin p l (upsert f k r)
upsert f !k t@(Tip ky y)
  | k == ky = Tip ky (f (Just y))
  | otherwise = link k (Tip k (f Nothing)) ky t
upsert f !k Nil = Tip k (f Nothing)

-- | \(O(\min(n,W))\). Look up and update.
-- This function returns the original value, if it is updated.
-- This is different behavior than 'Data.Map.updateLookupWithKey'.
-- Returns the original key value if the map entry is deleted.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:new a")])
-- > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")
--
-- See also: 'pop'

updateLookupWithKey ::  (Key -> a -> Maybe a) -> Key -> IntMap a -> (Maybe a,IntMap a)
updateLookupWithKey f !k (Bin p l r)
  | left k p      = let !(found,l') = updateLookupWithKey f k l
                    in (found,binCheckL p l' r)
  | otherwise     = let !(found,r') = updateLookupWithKey f k r
                    in (found,binCheckR p l r')
updateLookupWithKey f k t@(Tip ky y)
  | k==ky         = case (f k y) of
                      Just y' -> (Just y,Tip ky y')
                      Nothing -> (Just y,Nil)
  | otherwise     = (Nothing,t)
updateLookupWithKey _ _ Nil = (Nothing,Nil)



-- | \(O(\min(n,W))\). The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in an 'IntMap'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: (Maybe a -> Maybe a) -> Key -> IntMap a -> IntMap a
alter f !k t@(Bin p l r)
  | nomatch k p = case f Nothing of
                    Nothing -> t
                    Just x -> linkKey k (Tip k x) p t
  | left k p    = binCheckL p (alter f k l) r
  | otherwise   = binCheckR p l (alter f k r)
alter f k t@(Tip ky y)
  | k==ky         = case f (Just y) of
                      Just x -> Tip ky x
                      Nothing -> Nil
  | otherwise     = case f Nothing of
                      Just x -> link k (Tip k x) ky t
                      Nothing -> Tip ky y
alter f k Nil     = case f Nothing of
                      Just x -> Tip k x
                      Nothing -> Nil

-- | \(O(\min(n,W))\). The expression (@'alterF' f k map@) alters the value @x@ at
-- @k@, or absence thereof.  'alterF' can be used to inspect, insert, delete,
-- or update a value in an 'IntMap'.  In short : @'lookup' k \<$\> 'alterF' f k m = f
-- ('lookup' k m)@.
--
-- Example:
--
-- @
-- interactiveAlter :: Int -> IntMap String -> IO (IntMap String)
-- interactiveAlter k m = alterF f k m where
--   f Nothing = do
--      putStrLn $ show k ++
--          " was not found in the map. Would you like to add it?"
--      getUserResponse1 :: IO (Maybe String)
--   f (Just old) = do
--      putStrLn $ "The key is currently bound to " ++ show old ++
--          ". Would you like to change or delete it?"
--      getUserResponse2 :: IO (Maybe String)
-- @
--
-- 'alterF' is the most general operation for working with an individual
-- key that may or may not be in a given map.
--
-- Note: 'alterF' is a flipped version of the @at@ combinator from
-- @Control.Lens.At@.
--
-- @since 0.5.8

alterF :: Functor f
       => (Maybe a -> f (Maybe a)) -> Key -> IntMap a -> f (IntMap a)
-- This implementation was stolen from 'Control.Lens.At'.
alterF f k m = (<$> f mv) $ \fres ->
  case fres of
    Nothing -> maybe m (const (delete k m)) mv
    Just v' -> insert k v' m
  where mv = lookup k m

{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
-- | The union of a list of maps.
--
-- > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "b"), (5, "a"), (7, "C")]
-- > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
-- >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]

unions :: Foldable f => f (IntMap a) -> IntMap a
unions xs
  = Foldable.foldl' union empty xs

-- | The union of a list of maps, with a combining operation.
--
-- > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]

unionsWith :: Foldable f => (a->a->a) -> f (IntMap a) -> IntMap a
unionsWith f ts
  = Foldable.foldl' (unionWith f) empty ts

-- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
-- The (left-biased) union of two maps.
-- It prefers the first map when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
--
-- > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]

union :: IntMap a -> IntMap a -> IntMap a
union m1 m2
  = mergeWithKey' Bin const id id m1 m2

-- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
-- The union with a combining function.
--
-- > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]
--
-- Also see the performance note on 'fromListWith'.

unionWith :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWith f m1 m2
  = unionWithKey (\_ x y -> f x y) m1 m2

-- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
-- The union with a combining function.
--
-- > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]
--
-- Also see the performance note on 'fromListWith'.

unionWithKey :: (Key -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWithKey f m1 m2
  = mergeWithKey' Bin (\(Tip k1 x1) (Tip _k2 x2) -> Tip k1 (f k1 x1 x2)) id id m1 m2

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
-- Difference between two maps (based on keys).
--
-- > difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 3 "b"

difference :: IntMap a -> IntMap b -> IntMap a
difference m1 m2
  = mergeWithKey (\_ _ _ -> Nothing) id (const Nil) m1 m2

-- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
-- Difference with a combining function.
--
-- > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- > differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
-- >     == singleton 3 "b:B"

differenceWith :: (a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
differenceWith f m1 m2
  = differenceWithKey (\_ x y -> f x y) m1 m2

-- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
-- Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference).
-- If it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- > differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
-- >     == singleton 3 "3:b|B"

differenceWithKey :: (Key -> a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
differenceWithKey f m1 m2
  = mergeWithKey f id (const Nil) m1 m2


-- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
-- Remove all the keys in a given set from a map.
--
-- @
-- m \`withoutKeys\` s = 'filterWithKey' (\\k _ -> k ``IntSet.notMember`` s) m
-- @
--
-- @since 0.5.8
withoutKeys :: IntMap a -> IntSet -> IntMap a
withoutKeys t1@(Bin p1 l1 r1) t2@(IntSet.Bin p2 l2 r2) = case treeTreeBranch p1 p2 of
  ABL -> binCheckL p1 (withoutKeys l1 t2) r1
  ABR -> binCheckR p1 l1 (withoutKeys r1 t2)
  BAL -> withoutKeys t1 l2
  BAR -> withoutKeys t1 r2
  EQL -> bin p1 (withoutKeys l1 l2) (withoutKeys r1 r2)
  NOM -> t1
  where
withoutKeys t1@(Bin _ _ _) (IntSet.Tip p2 bm2) = withoutKeysTip t1 p2 bm2
withoutKeys t1@(Bin _ _ _) IntSet.Nil = t1
withoutKeys t1@(Tip k1 _) t2
    | k1 `IntSet.member` t2 = Nil
    | otherwise = t1
withoutKeys Nil _ = Nil

withoutKeysTip :: IntMap a -> Int -> IntSet.BitMap -> IntMap a
withoutKeysTip t@(Bin p l r) !p2 !bm2
  | IntSet.suffixOf (unPrefix p) /= 0 =
      if IntSet.prefixOf (unPrefix p) == p2
      then restrictBM t (complement bm2)
      else t
  | nomatch p2 p = t
  | left p2 p    = binCheckL p (withoutKeysTip l p2 bm2) r
  | otherwise    = binCheckR p l (withoutKeysTip r p2 bm2)
withoutKeysTip t@(Tip kx _) !p2 !bm2
  | IntSet.prefixOf kx == p2 && IntSet.bitmapOf kx .&. bm2 /= 0 = Nil
  | otherwise = t
withoutKeysTip Nil !_ !_ = Nil

{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
-- The (left-biased) intersection of two maps (based on keys).
--
-- > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"

intersection :: IntMap a -> IntMap b -> IntMap a
intersection m1 m2
  = mergeWithKey' bin const (const Nil) (const Nil) m1 m2


-- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
-- The restriction of a map to the keys in a set.
--
-- @
-- m \`restrictKeys\` s = 'filterWithKey' (\\k _ -> k ``IntSet.member`` s) m
-- @
--
-- @since 0.5.8
restrictKeys :: IntMap a -> IntSet -> IntMap a
restrictKeys t1@(Bin p1 l1 r1) t2@(IntSet.Bin p2 l2 r2) = case treeTreeBranch p1 p2 of
  ABL -> restrictKeys l1 t2
  ABR -> restrictKeys r1 t2
  BAL -> restrictKeys t1 l2
  BAR -> restrictKeys t1 r2
  EQL -> bin p1 (restrictKeys l1 l2) (restrictKeys r1 r2)
  NOM -> Nil
restrictKeys t1@(Bin _ _ _) (IntSet.Tip p2 bm2) = restrictKeysTip t1 p2 bm2
restrictKeys (Bin _ _ _) IntSet.Nil = Nil
restrictKeys t1@(Tip k1 _) t2
    | k1 `IntSet.member` t2 = t1
    | otherwise = Nil
restrictKeys Nil _ = Nil

restrictKeysTip :: IntMap a -> Int -> IntSet.BitMap -> IntMap a
restrictKeysTip t@(Bin p l r) !p2 !bm2
  | IntSet.suffixOf (unPrefix p) /= 0 =
      if IntSet.prefixOf (unPrefix p) == p2
      then restrictBM t bm2
      else Nil
  | nomatch p2 p = Nil
  | left p2 p    = restrictKeysTip l p2 bm2
  | otherwise    = restrictKeysTip r p2 bm2
restrictKeysTip t@(Tip kx _) !p2 !bm2
  | IntSet.prefixOf kx == p2 && IntSet.bitmapOf kx .&. bm2 /= 0 = t
  | otherwise = Nil
restrictKeysTip Nil !_ !_ = Nil

-- Must be called on an IntMap whose keys fit in the given IntSet BitMap's Tip.
-- Keeps keys that match the BitMap.
-- Returns early as an optimization, i.e. if the tree can be entirely kept or
-- discarded there is no need to recursively visit the children.
restrictBM :: IntMap a -> IntSet.BitMap -> IntMap a
restrictBM t@(Bin p l r) !bm
  | bm' == 0 = Nil
  | bm' == -1 = t
  | otherwise = bin p (restrictBM l bm) (restrictBM r bm)
  where
    -- Here we care about the "submask" of bm corresponding the current Bin's
    -- range. So we create bm', where this submask is at the lowest position and
    -- and all other bits are set to the highest bit of the submask (using an
    -- arithmetic shiftR). Now bm' is 0 when the submask is empty and -1 when
    -- the submask is full.
    px = IntSet.suffixOf (unPrefix p)
    px1 = px - 1
    min_ = px .&. px1
    max_ = px .|. px1
    sh = (wordSize - 1) - max_
    bm' = (w2i bm `unsafeShiftL` sh) `unsafeShiftR` (sh + min_)
restrictBM t@(Tip k _) !bm
  | IntSet.bitmapOf k .&. bm /= 0 = t
  | otherwise = Nil
restrictBM Nil !_ = Nil

w2i :: Word -> Int
w2i = fromIntegral

-- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
-- The intersection with a combining function.
--
-- > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"

intersectionWith :: (a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWith f m1 m2
  = intersectionWithKey (\_ x y -> f x y) m1 m2

-- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
-- The intersection with a combining function.
--
-- > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- > intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "5:a|A"

intersectionWithKey :: (Key -> a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWithKey f m1 m2
  = mergeWithKey' bin (\(Tip k1 x1) (Tip _k2 x2) -> Tip k1 (f k1 x1 x2)) (const Nil) (const Nil) m1 m2

{--------------------------------------------------------------------
  Symmetric difference
--------------------------------------------------------------------}

-- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
-- The symmetric difference of two maps.
--
-- The result contains entries whose keys appear in exactly one of the two maps.
--
-- @
-- symmetricDifference
--   (fromList [(0,\'q\'),(2,\'b\'),(4,\'w\'),(6,\'o\')])
--   (fromList [(0,\'e\'),(3,\'r\'),(6,\'t\'),(9,\'s\')])
-- ==
-- fromList [(2,\'b\'),(3,\'r\'),(4,\'w\'),(9,\'s\')]
-- @
--
-- @since 0.8
symmetricDifference :: IntMap a -> IntMap a -> IntMap a
symmetricDifference t1@(Bin p1 l1 r1) t2@(Bin p2 l2 r2) =
  case treeTreeBranch p1 p2 of
    ABL -> binCheckL p1 (symmetricDifference l1 t2) r1
    ABR -> binCheckR p1 l1 (symmetricDifference r1 t2)
    BAL -> binCheckL p2 (symmetricDifference t1 l2) r2
    BAR -> binCheckR p2 l2 (symmetricDifference t1 r2)
    EQL -> bin p1 (symmetricDifference l1 l2) (symmetricDifference r1 r2)
    NOM -> link (unPrefix p1) t1 (unPrefix p2) t2
symmetricDifference t1@(Bin _ _ _) t2@(Tip k2 _) = symDiffTip t2 k2 t1
symmetricDifference t1@(Bin _ _ _) Nil = t1
symmetricDifference t1@(Tip k1 _) t2 = symDiffTip t1 k1 t2
symmetricDifference Nil t2 = t2

symDiffTip :: IntMap a -> Int -> IntMap a -> IntMap a
symDiffTip !t1 !k1 = go
  where
    go t2@(Bin p2 l2 r2)
      | nomatch k1 p2 = linkKey k1 t1 p2 t2
      | left k1 p2 = binCheckL p2 (go l2) r2
      | otherwise = binCheckR p2 l2 (go r2)
    go t2@(Tip k2 _)
      | k1 == k2 = Nil
      | otherwise = link k1 t1 k2 t2
    go Nil = t1

{--------------------------------------------------------------------
  MergeWithKey
--------------------------------------------------------------------}

-- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
-- A high-performance universal combining function. Using
-- 'mergeWithKey', all combining functions can be defined without any loss of
-- efficiency (with exception of 'union', 'difference' and 'intersection',
-- where sharing of some nodes is lost with 'mergeWithKey').
--
-- __Warning__: Please make sure you know what is going on when using 'mergeWithKey',
-- otherwise you can be surprised by unexpected code growth or even
-- corruption of the data structure.
--
-- When 'mergeWithKey' is given three arguments, it is inlined to the call
-- site. You should therefore use 'mergeWithKey' only to define your custom
-- combining functions. For example, you could define 'unionWithKey',
-- 'differenceWithKey' and 'intersectionWithKey' as
--
-- > myUnionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) id id m1 m2
-- > myDifferenceWithKey f m1 m2 = mergeWithKey f id (const empty) m1 m2
-- > myIntersectionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) (const empty) (const empty) m1 m2
--
-- When calling @'mergeWithKey' combine only1 only2@, a function combining two
-- 'IntMap's is created, such that
--
-- * if a key is present in both maps, it is passed with both corresponding
--   values to the @combine@ function. Depending on the result, the key is either
--   present in the result with specified value, or is left out;
--
-- * a nonempty subtree present only in the first map is passed to @only1@ and
--   the output is added to the result;
--
-- * a nonempty subtree present only in the second map is passed to @only2@ and
--   the output is added to the result.
--
-- The @only1@ and @only2@ methods /must return a map with a subset (possibly empty) of the keys of the given map/.
-- The values can be modified arbitrarily. Most common variants of @only1@ and
-- @only2@ are 'id' and @'const' 'empty'@, but for example @'map' f@ or
-- @'filterWithKey' f@ could be used for any @f@.

-- See Note [IntMap merge complexity]
mergeWithKey :: (Key -> a -> b -> Maybe c) -> (IntMap a -> IntMap c) -> (IntMap b -> IntMap c)
             -> IntMap a -> IntMap b -> IntMap c
mergeWithKey f g1 g2 = mergeWithKey' bin combine g1 g2
  where -- We use the lambda form to avoid non-exhaustive pattern matches warning.
        combine = \(Tip k1 x1) (Tip _k2 x2) ->
          case f k1 x1 x2 of
            Nothing -> Nil
            Just x -> Tip k1 x
        {-# INLINE combine #-}
{-# INLINE mergeWithKey #-}

-- Slightly more general version of mergeWithKey. It differs in the following:
--
-- * the combining function operates on maps instead of keys and values. The
--   reason is to enable sharing in union, difference and intersection.
--
-- * mergeWithKey' is given an equivalent of bin. The reason is that in union*,
--   Bin constructor can be used, because we know both subtrees are nonempty.

mergeWithKey' :: (Prefix -> IntMap c -> IntMap c -> IntMap c)
              -> (IntMap a -> IntMap b -> IntMap c) -> (IntMap a -> IntMap c) -> (IntMap b -> IntMap c)
              -> IntMap a -> IntMap b -> IntMap c
mergeWithKey' bin' f g1 g2 = go
  where
    go t1@(Bin p1 l1 r1) t2@(Bin p2 l2 r2) = case treeTreeBranch p1 p2 of
      ABL -> bin' p1 (go l1 t2) (g1 r1)
      ABR -> bin' p1 (g1 l1) (go r1 t2)
      BAL -> bin' p2 (go t1 l2) (g2 r2)
      BAR -> bin' p2 (g2 l2) (go t1 r2)
      EQL -> bin' p1 (go l1 l2) (go r1 r2)
      NOM -> maybe_link (unPrefix p1) (g1 t1) (unPrefix p2) (g2 t2)

    go t1'@(Bin _ _ _) t2'@(Tip k2' _) = merge0 t2' k2' t1'
      where
        merge0 t2 k2 t1@(Bin p1 l1 r1)
          | nomatch k2 p1 = maybe_link (unPrefix p1) (g1 t1) k2 (g2 t2)
          | left k2 p1    = bin' p1 (merge0 t2 k2 l1) (g1 r1)
          | otherwise     = bin' p1 (g1 l1) (merge0 t2 k2 r1)
        merge0 t2 k2 t1@(Tip k1 _)
          | k1 == k2 = f t1 t2
          | otherwise = maybe_link k1 (g1 t1) k2 (g2 t2)
        merge0 t2 _  Nil = g2 t2

    go t1@(Bin _ _ _) Nil = g1 t1

    go t1'@(Tip k1' _) t2' = merge0 t1' k1' t2'
      where
        merge0 t1 k1 t2@(Bin p2 l2 r2)
          | nomatch k1 p2 = maybe_link k1 (g1 t1) (unPrefix p2) (g2 t2)
          | left k1 p2    = bin' p2 (merge0 t1 k1 l2) (g2 r2)
          | otherwise     = bin' p2 (g2 l2) (merge0 t1 k1 r2)
        merge0 t1 k1 t2@(Tip k2 _)
          | k1 == k2 = f t1 t2
          | otherwise = maybe_link k1 (g1 t1) k2 (g2 t2)
        merge0 t1 _  Nil = g1 t1

    go Nil Nil = Nil

    go Nil t2 = g2 t2

    maybe_link _ Nil _ t2 = t2
    maybe_link _ t1 _ Nil = t1
    maybe_link k1 t1 k2 t2 = link k1 t1 k2 t2
    {-# INLINE maybe_link #-}
{-# INLINE mergeWithKey' #-}


{--------------------------------------------------------------------
  mergeA
--------------------------------------------------------------------}

-- | A tactic for dealing with keys present in one map but not the
-- other in 'merge' or 'mergeA'.
--
-- A tactic of type @WhenMissing f k x z@ is an abstract representation
-- of a function of type @Key -> x -> f (Maybe z)@.
--
-- @since 0.5.9

data WhenMissing f x y = WhenMissing
  { missingSubtree :: IntMap x -> f (IntMap y)
  , missingKey :: Key -> x -> f (Maybe y)}

-- | @since 0.5.9
instance (Applicative f, Monad f) => Functor (WhenMissing f x) where
  fmap = mapWhenMissing
  {-# INLINE fmap #-}


-- | @since 0.5.9
instance (Applicative f, Monad f) => Category.Category (WhenMissing f)
  where
    id = preserveMissing
    f . g =
      traverseMaybeMissing $ \ k x -> do
        y <- missingKey g k x
        case y of
          Nothing -> pure Nothing
          Just q  -> missingKey f k q
    {-# INLINE id #-}
    {-# INLINE (.) #-}


-- | Equivalent to @ReaderT k (ReaderT x (MaybeT f))@.
--
-- @since 0.5.9
instance (Applicative f, Monad f) => Applicative (WhenMissing f x) where
  pure x = mapMissing (\ _ _ -> x)
  f <*> g =
    traverseMaybeMissing $ \k x -> do
      res1 <- missingKey f k x
      case res1 of
        Nothing -> pure Nothing
        Just r  -> (pure $!) . fmap r =<< missingKey g k x
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}


-- | Equivalent to @ReaderT k (ReaderT x (MaybeT f))@.
--
-- @since 0.5.9
instance (Applicative f, Monad f) => Monad (WhenMissing f x) where
  m >>= f =
    traverseMaybeMissing $ \k x -> do
      res1 <- missingKey m k x
      case res1 of
        Nothing -> pure Nothing
        Just r  -> missingKey (f r) k x
  {-# INLINE (>>=) #-}


-- | Map covariantly over a @'WhenMissing' f x@.
--
-- @since 0.5.9
mapWhenMissing
  :: (Applicative f, Monad f)
  => (a -> b)
  -> WhenMissing f x a
  -> WhenMissing f x b
mapWhenMissing f t = WhenMissing
  { missingSubtree = \m -> missingSubtree t m >>= \m' -> pure $! fmap f m'
  , missingKey     = \k x -> missingKey t k x >>= \q -> (pure $! fmap f q) }
{-# INLINE mapWhenMissing #-}


-- | Map covariantly over a @'WhenMissing' f x@, using only a
-- 'Functor f' constraint.
mapGentlyWhenMissing
  :: Functor f
  => (a -> b)
  -> WhenMissing f x a
  -> WhenMissing f x b
mapGentlyWhenMissing f t = WhenMissing
  { missingSubtree = \m -> fmap f <$> missingSubtree t m
  , missingKey     = \k x -> fmap f <$> missingKey t k x }
{-# INLINE mapGentlyWhenMissing #-}


-- | Map covariantly over a @'WhenMatched' f k x@, using only a
-- 'Functor f' constraint.
mapGentlyWhenMatched
  :: Functor f
  => (a -> b)
  -> WhenMatched f x y a
  -> WhenMatched f x y b
mapGentlyWhenMatched f t =
  zipWithMaybeAMatched $ \k x y -> fmap f <$> runWhenMatched t k x y
{-# INLINE mapGentlyWhenMatched #-}


-- | Map contravariantly over a @'WhenMissing' f _ x@.
--
-- @since 0.5.9
lmapWhenMissing :: (b -> a) -> WhenMissing f a x -> WhenMissing f b x
lmapWhenMissing f t = WhenMissing
  { missingSubtree = \m -> missingSubtree t (fmap f m)
  , missingKey     = \k x -> missingKey t k (f x) }
{-# INLINE lmapWhenMissing #-}


-- | Map contravariantly over a @'WhenMatched' f _ y z@.
--
-- @since 0.5.9
contramapFirstWhenMatched
  :: (b -> a)
  -> WhenMatched f a y z
  -> WhenMatched f b y z
contramapFirstWhenMatched f t =
  WhenMatched $ \k x y -> runWhenMatched t k (f x) y
{-# INLINE contramapFirstWhenMatched #-}


-- | Map contravariantly over a @'WhenMatched' f x _ z@.
--
-- @since 0.5.9
contramapSecondWhenMatched
  :: (b -> a)
  -> WhenMatched f x a z
  -> WhenMatched f x b z
contramapSecondWhenMatched f t =
  WhenMatched $ \k x y -> runWhenMatched t k x (f y)
{-# INLINE contramapSecondWhenMatched #-}


-- | A tactic for dealing with keys present in one map but not the
-- other in 'merge'.
--
-- A tactic of type @SimpleWhenMissing x z@ is an abstract
-- representation of a function of type @Key -> x -> Maybe z@.
--
-- @since 0.5.9
type SimpleWhenMissing = WhenMissing Identity


-- | A tactic for dealing with keys present in both maps in 'merge'
-- or 'mergeA'.
--
-- A tactic of type @WhenMatched f x y z@ is an abstract representation
-- of a function of type @Key -> x -> y -> f (Maybe z)@.
--
-- @since 0.5.9
newtype WhenMatched f x y z = WhenMatched
  { matchedKey :: Key -> x -> y -> f (Maybe z) }


-- | Along with zipWithMaybeAMatched, witnesses the isomorphism
-- between @WhenMatched f x y z@ and @Key -> x -> y -> f (Maybe z)@.
--
-- @since 0.5.9
runWhenMatched :: WhenMatched f x y z -> Key -> x -> y -> f (Maybe z)
runWhenMatched = matchedKey
{-# INLINE runWhenMatched #-}


-- | Along with traverseMaybeMissing, witnesses the isomorphism
-- between @WhenMissing f x y@ and @Key -> x -> f (Maybe y)@.
--
-- @since 0.5.9
runWhenMissing :: WhenMissing f x y -> Key-> x -> f (Maybe y)
runWhenMissing = missingKey
{-# INLINE runWhenMissing #-}


-- | @since 0.5.9
instance Functor f => Functor (WhenMatched f x y) where
  fmap = mapWhenMatched
  {-# INLINE fmap #-}


-- | @since 0.5.9
instance (Monad f, Applicative f) => Category.Category (WhenMatched f x)
  where
    id = zipWithMatched (\_ _ y -> y)
    f . g =
      zipWithMaybeAMatched $ \k x y -> do
        res <- runWhenMatched g k x y
        case res of
          Nothing -> pure Nothing
          Just r  -> runWhenMatched f k x r
    {-# INLINE id #-}
    {-# INLINE (.) #-}


-- | Equivalent to @ReaderT Key (ReaderT x (ReaderT y (MaybeT f)))@
--
-- @since 0.5.9
instance (Monad f, Applicative f) => Applicative (WhenMatched f x y) where
  pure x = zipWithMatched (\_ _ _ -> x)
  fs <*> xs =
    zipWithMaybeAMatched $ \k x y -> do
      res <- runWhenMatched fs k x y
      case res of
        Nothing -> pure Nothing
        Just r  -> (pure $!) . fmap r =<< runWhenMatched xs k x y
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}


-- | Equivalent to @ReaderT Key (ReaderT x (ReaderT y (MaybeT f)))@
--
-- @since 0.5.9
instance (Monad f, Applicative f) => Monad (WhenMatched f x y) where
  m >>= f =
    zipWithMaybeAMatched $ \k x y -> do
      res <- runWhenMatched m k x y
      case res of
        Nothing -> pure Nothing
        Just r  -> runWhenMatched (f r) k x y
  {-# INLINE (>>=) #-}


-- | Map covariantly over a @'WhenMatched' f x y@.
--
-- @since 0.5.9
mapWhenMatched
  :: Functor f
  => (a -> b)
  -> WhenMatched f x y a
  -> WhenMatched f x y b
mapWhenMatched f (WhenMatched g) =
  WhenMatched $ \k x y -> fmap (fmap f) (g k x y)
{-# INLINE mapWhenMatched #-}


-- | A tactic for dealing with keys present in both maps in 'merge'.
--
-- A tactic of type @SimpleWhenMatched x y z@ is an abstract
-- representation of a function of type @Key -> x -> y -> Maybe z@.
--
-- @since 0.5.9
type SimpleWhenMatched = WhenMatched Identity


-- | When a key is found in both maps, apply a function to the key
-- and values and use the result in the merged map.
--
-- > zipWithMatched
-- >   :: (Key -> x -> y -> z)
-- >   -> SimpleWhenMatched x y z
--
-- @since 0.5.9
zipWithMatched
  :: Applicative f
  => (Key -> x -> y -> z)
  -> WhenMatched f x y z
zipWithMatched f = WhenMatched $ \ k x y -> pure . Just $ f k x y
{-# INLINE zipWithMatched #-}


-- | When a key is found in both maps, apply a function to the key
-- and values to produce an action and use its result in the merged
-- map.
--
-- @since 0.5.9
zipWithAMatched
  :: Applicative f
  => (Key -> x -> y -> f z)
  -> WhenMatched f x y z
zipWithAMatched f = WhenMatched $ \ k x y -> Just <$> f k x y
{-# INLINE zipWithAMatched #-}


-- | When a key is found in both maps, apply a function to the key
-- and values and maybe use the result in the merged map.
--
-- > zipWithMaybeMatched
-- >   :: (Key -> x -> y -> Maybe z)
-- >   -> SimpleWhenMatched x y z
--
-- @since 0.5.9
zipWithMaybeMatched
  :: Applicative f
  => (Key -> x -> y -> Maybe z)
  -> WhenMatched f x y z
zipWithMaybeMatched f = WhenMatched $ \ k x y -> pure $ f k x y
{-# INLINE zipWithMaybeMatched #-}


-- | When a key is found in both maps, apply a function to the key
-- and values, perform the resulting action, and maybe use the
-- result in the merged map.
--
-- This is the fundamental 'WhenMatched' tactic.
--
-- @since 0.5.9
zipWithMaybeAMatched
  :: (Key -> x -> y -> f (Maybe z))
  -> WhenMatched f x y z
zipWithMaybeAMatched f = WhenMatched $ \ k x y -> f k x y
{-# INLINE zipWithMaybeAMatched #-}


-- | Drop all the entries whose keys are missing from the other
-- map.
--
-- > dropMissing :: SimpleWhenMissing x y
--
-- prop> dropMissing = mapMaybeMissing (\_ _ -> Nothing)
--
-- but @dropMissing@ is much faster.
--
-- @since 0.5.9
dropMissing :: Applicative f => WhenMissing f x y
dropMissing = WhenMissing
  { missingSubtree = const (pure Nil)
  , missingKey     = \_ _ -> pure Nothing }
{-# INLINE dropMissing #-}


-- | Preserve, unchanged, the entries whose keys are missing from
-- the other map.
--
-- > preserveMissing :: SimpleWhenMissing x x
--
-- prop> preserveMissing = Merge.Lazy.mapMaybeMissing (\_ x -> Just x)
--
-- but @preserveMissing@ is much faster.
--
-- @since 0.5.9
preserveMissing :: Applicative f => WhenMissing f x x
preserveMissing = WhenMissing
  { missingSubtree = pure
  , missingKey     = \_ v -> pure (Just v) }
{-# INLINE preserveMissing #-}


-- | Map over the entries whose keys are missing from the other map.
--
-- > mapMissing :: (k -> x -> y) -> SimpleWhenMissing x y
--
-- prop> mapMissing f = mapMaybeMissing (\k x -> Just $ f k x)
--
-- but @mapMissing@ is somewhat faster.
--
-- @since 0.5.9
mapMissing :: Applicative f => (Key -> x -> y) -> WhenMissing f x y
mapMissing f = WhenMissing
  { missingSubtree = \m -> pure $! mapWithKey f m
  , missingKey     = \k x -> pure $ Just (f k x) }
{-# INLINE mapMissing #-}


-- | Map over the entries whose keys are missing from the other
-- map, optionally removing some. This is the most powerful
-- 'SimpleWhenMissing' tactic, but others are usually more efficient.
--
-- > mapMaybeMissing :: (Key -> x -> Maybe y) -> SimpleWhenMissing x y
--
-- prop> mapMaybeMissing f = traverseMaybeMissing (\k x -> pure (f k x))
--
-- but @mapMaybeMissing@ uses fewer unnecessary 'Applicative'
-- operations.
--
-- @since 0.5.9
mapMaybeMissing
  :: Applicative f => (Key -> x -> Maybe y) -> WhenMissing f x y
mapMaybeMissing f = WhenMissing
  { missingSubtree = \m -> pure $! mapMaybeWithKey f m
  , missingKey     = \k x -> pure $! f k x }
{-# INLINE mapMaybeMissing #-}


-- | Filter the entries whose keys are missing from the other map.
--
-- > filterMissing :: (k -> x -> Bool) -> SimpleWhenMissing x x
--
-- prop> filterMissing f = Merge.Lazy.mapMaybeMissing $ \k x -> guard (f k x) *> Just x
--
-- but this should be a little faster.
--
-- @since 0.5.9
filterMissing
  :: Applicative f => (Key -> x -> Bool) -> WhenMissing f x x
filterMissing f = WhenMissing
  { missingSubtree = \m -> pure $! filterWithKey f m
  , missingKey     = \k x -> pure $! if f k x then Just x else Nothing }
{-# INLINE filterMissing #-}


-- | Filter the entries whose keys are missing from the other map
-- using some 'Applicative' action.
--
-- > filterAMissing f = Merge.Lazy.traverseMaybeMissing $
-- >   \k x -> (\b -> guard b *> Just x) <$> f k x
--
-- but this should be a little faster.
--
-- @since 0.5.9
filterAMissing
  :: Applicative f => (Key -> x -> f Bool) -> WhenMissing f x x
filterAMissing f = WhenMissing
  { missingSubtree = \m -> filterWithKeyA f m
  , missingKey     = \k x -> bool Nothing (Just x) <$> f k x }
{-# INLINE filterAMissing #-}


-- | \(O(n)\). Filter keys and values using an 'Applicative' predicate.
filterWithKeyA
  :: Applicative f => (Key -> a -> f Bool) -> IntMap a -> f (IntMap a)
filterWithKeyA _ Nil           = pure Nil
filterWithKeyA f t@(Tip k x)   = (\b -> if b then t else Nil) <$> f k x
filterWithKeyA f (Bin p l r)
  | signBranch p = liftA2 (flip (bin p)) (filterWithKeyA f r) (filterWithKeyA f l)
  | otherwise = liftA2 (bin p) (filterWithKeyA f l) (filterWithKeyA f r)

-- | This wasn't in Data.Bool until 4.7.0, so we define it here
bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t


-- | Traverse over the entries whose keys are missing from the other
-- map.
--
-- @since 0.5.9
traverseMissing
  :: Applicative f => (Key -> x -> f y) -> WhenMissing f x y
traverseMissing f = WhenMissing
  { missingSubtree = traverseWithKey f
  , missingKey = \k x -> Just <$> f k x }
{-# INLINE traverseMissing #-}


-- | Traverse over the entries whose keys are missing from the other
-- map, optionally producing values to put in the result. This is
-- the most powerful 'WhenMissing' tactic, but others are usually
-- more efficient.
--
-- @since 0.5.9
traverseMaybeMissing
  :: Applicative f => (Key -> x -> f (Maybe y)) -> WhenMissing f x y
traverseMaybeMissing f = WhenMissing
  { missingSubtree = traverseMaybeWithKey f
  , missingKey = f }
{-# INLINE traverseMaybeMissing #-}


-- | \(O(n)\). Traverse keys\/values and collect the 'Just' results.
--
-- @since 0.6.4
traverseMaybeWithKey
  :: Applicative f => (Key -> a -> f (Maybe b)) -> IntMap a -> f (IntMap b)
traverseMaybeWithKey f = go
    where
    go Nil           = pure Nil
    go (Tip k x)     = maybe Nil (Tip k) <$> f k x
    go (Bin p l r)
      | signBranch p = liftA2 (flip (bin p)) (go r) (go l)
      | otherwise = liftA2 (bin p) (go l) (go r)


-- | Merge two maps.
--
-- 'merge' takes two 'WhenMissing' tactics, a 'WhenMatched' tactic
-- and two maps. It uses the tactics to merge the maps. Its behavior
-- is best understood via its fundamental tactics, 'mapMaybeMissing'
-- and 'zipWithMaybeMatched'.
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
-- m1 = [(0, \'a\'), (1, \'b\'), (3, \'c\'), (4, \'d\')]
-- m2 = [(1, "one"), (2, "two"), (4, "three")]
-- @
--
-- 'merge' will first \"align\" these maps by key:
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
-- maybes = [g1 0 \'a\', f 1 \'b\' "one", g2 2 "two", g1 3 \'c\', f 4 \'d\' "three"]
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
-- site. To prevent excessive inlining, you should typically use
-- 'merge' to define your custom combining functions.
--
--
-- Examples:
--
-- prop> unionWithKey f = merge preserveMissing preserveMissing (zipWithMatched f)
-- prop> intersectionWithKey f = merge dropMissing dropMissing (zipWithMatched f)
-- prop> differenceWith f = merge diffPreserve diffDrop f
-- prop> symmetricDifference = merge diffPreserve diffPreserve (\ _ _ _ -> Nothing)
-- prop> mapEachPiece f g h = merge (diffMapWithKey f) (diffMapWithKey g)
--
-- @since 0.5.9
merge
  :: SimpleWhenMissing a c -- ^ What to do with keys in @m1@ but not @m2@
  -> SimpleWhenMissing b c -- ^ What to do with keys in @m2@ but not @m1@
  -> SimpleWhenMatched a b c -- ^ What to do with keys in both @m1@ and @m2@
  -> IntMap a -- ^ Map @m1@
  -> IntMap b -- ^ Map @m2@
  -> IntMap c
merge g1 g2 f m1 m2 =
  runIdentity $ mergeA g1 g2 f m1 m2
{-# INLINE merge #-}


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
mergeA
  :: (Applicative f)
  => WhenMissing f a c -- ^ What to do with keys in @m1@ but not @m2@
  -> WhenMissing f b c -- ^ What to do with keys in @m2@ but not @m1@
  -> WhenMatched f a b c -- ^ What to do with keys in both @m1@ and @m2@
  -> IntMap a -- ^ Map @m1@
  -> IntMap b -- ^ Map @m2@
  -> f (IntMap c)
mergeA
    WhenMissing{missingSubtree = g1t, missingKey = g1k}
    WhenMissing{missingSubtree = g2t, missingKey = g2k}
    WhenMatched{matchedKey = f}
    = go
  where
    go t1  Nil = g1t t1
    go Nil t2  = g2t t2

    -- This case is already covered below.
    -- go (Tip k1 x1) (Tip k2 x2) = mergeTips k1 x1 k2 x2

    go (Tip k1 x1) t2' = merge2 t2'
      where
        merge2 t2@(Bin p2 l2 r2)
          | nomatch k1 p2 = linkA k1 (subsingletonBy g1k k1 x1) (unPrefix p2) (g2t t2)
          | left k1 p2    = binA p2 (merge2 l2) (g2t r2)
          | otherwise     = binA p2 (g2t l2) (merge2 r2)
        merge2 (Tip k2 x2)   = mergeTips k1 x1 k2 x2
        merge2 Nil           = subsingletonBy g1k k1 x1

    go t1' (Tip k2 x2) = merge1 t1'
      where
        merge1 t1@(Bin p1 l1 r1)
          | nomatch k2 p1 = linkA (unPrefix p1) (g1t t1) k2 (subsingletonBy g2k k2 x2)
          | left k2 p1    = binA p1 (merge1 l1) (g1t r1)
          | otherwise     = binA p1 (g1t l1) (merge1 r1)
        merge1 (Tip k1 x1)   = mergeTips k1 x1 k2 x2
        merge1 Nil           = subsingletonBy g2k k2 x2

    go t1@(Bin p1 l1 r1) t2@(Bin p2 l2 r2) = case treeTreeBranch p1 p2 of
      ABL -> binA p1 (go l1 t2) (g1t r1)
      ABR -> binA p1 (g1t l1) (go r1 t2)
      BAL -> binA p2 (go t1 l2) (g2t r2)
      BAR -> binA p2 (g2t l2) (go t1 r2)
      EQL -> binA p1 (go l1 l2) (go r1 r2)
      NOM -> linkA (unPrefix p1) (g1t t1) (unPrefix p2) (g2t t2)

    subsingletonBy :: Functor f => (Key -> a -> f (Maybe c)) -> Key -> a -> f (IntMap c)
    subsingletonBy gk k x = maybe Nil (Tip k) <$> gk k x
    {-# INLINE subsingletonBy #-}

    mergeTips k1 x1 k2 x2
      | k1 == k2  = maybe Nil (Tip k1) <$> f k1 x1 x2
      | k1 <  k2  = liftA2 (subdoubleton k1 k2) (g1k k1 x1) (g2k k2 x2)
        {-
        = link_ k1 k2 <$> subsingletonBy g1k k1 x1 <*> subsingletonBy g2k k2 x2
        -}
      | otherwise = liftA2 (subdoubleton k2 k1) (g2k k2 x2) (g1k k1 x1)
    {-# INLINE mergeTips #-}

    subdoubleton _ _   Nothing Nothing     = Nil
    subdoubleton _ k2  Nothing (Just y2)   = Tip k2 y2
    subdoubleton k1 _  (Just y1) Nothing   = Tip k1 y1
    subdoubleton k1 k2 (Just y1) (Just y2) = link k1 (Tip k1 y1) k2 (Tip k2 y2)
    {-# INLINE subdoubleton #-}

    -- | A variant of 'link_' which makes sure to execute side-effects
    -- in the right order.
    linkA
        :: Applicative f
        => Int -> f (IntMap a)
        -> Int -> f (IntMap a)
        -> f (IntMap a)
    linkA k1 t1 k2 t2
      | i2w k1 < i2w k2 = binA p t1 t2
      | otherwise = binA p t2 t1
      where
        p = branchPrefix k1 k2
    {-# INLINE linkA #-}

    -- A variant of 'bin' that ensures that effects for negative keys are executed
    -- first.
    binA
        :: Applicative f
        => Prefix
        -> f (IntMap a)
        -> f (IntMap a)
        -> f (IntMap a)
    binA p a b
      | signBranch p = liftA2 (flip (bin p)) b a
      | otherwise = liftA2 (bin p) a b
    {-# INLINE binA #-}
{-# INLINE mergeA #-}


{--------------------------------------------------------------------
  Min\/Max
--------------------------------------------------------------------}

-- | \(O(\min(n,W))\). Update the value at the minimal key.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMinWithKey :: (Key -> a -> Maybe a) -> IntMap a -> IntMap a
updateMinWithKey f t =
  case t of Bin p l r | signBranch p -> binCheckR p l (go f r)
            _ -> go f t
  where
    go f' (Bin p l r) = binCheckL p (go f' l) r
    go f' (Tip k y) = case f' k y of
                        Just y' -> Tip k y'
                        Nothing -> Nil
    go _ Nil =  Nil

-- | \(O(\min(n,W))\). Update the value at the maximal key.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMaxWithKey :: (Key -> a -> Maybe a) -> IntMap a -> IntMap a
updateMaxWithKey f t =
  case t of Bin p l r | signBranch p -> binCheckL p (go f l) r
            _ -> go f t
  where
    go f' (Bin p l r) = binCheckR p l (go f' r)
    go f' (Tip k y) = case f' k y of
                        Just y' -> Tip k y'
                        Nothing -> Nil
    go _ Nil = Nil


data View a = View {-# UNPACK #-} !Key a !(IntMap a)

-- | \(O(\min(n,W))\). Retrieves the maximal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
-- > maxViewWithKey empty == Nothing

maxViewWithKey :: IntMap a -> Maybe ((Key, a), IntMap a)
maxViewWithKey t = case t of
  Nil -> Nothing
  _ -> Just $ case maxViewWithKeySure t of
                View k v t' -> ((k, v), t')
{-# INLINE maxViewWithKey #-}

maxViewWithKeySure :: IntMap a -> View a
maxViewWithKeySure t =
  case t of
    Nil -> error "maxViewWithKeySure Nil"
    Bin p l r | signBranch p ->
      case go l of View k a l' -> View k a (binCheckL p l' r)
    _ -> go t
  where
    go (Bin p l r) =
        case go r of View k a r' -> View k a (binCheckR p l r')
    go (Tip k y) = View k y Nil
    go Nil = error "maxViewWithKey_go Nil"
-- See note on NOINLINE at minViewWithKeySure
{-# NOINLINE maxViewWithKeySure #-}

-- | \(O(\min(n,W))\). Retrieves the minimal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
-- > minViewWithKey empty == Nothing

minViewWithKey :: IntMap a -> Maybe ((Key, a), IntMap a)
minViewWithKey t =
  case t of
    Nil -> Nothing
    _ -> Just $ case minViewWithKeySure t of
                  View k v t' -> ((k, v), t')
-- We inline this to give GHC the best possible chance of
-- getting rid of the Maybe, pair, and Int constructors, as
-- well as a thunk under the Just. That is, we really want to
-- be certain this inlines!
{-# INLINE minViewWithKey #-}

minViewWithKeySure :: IntMap a -> View a
minViewWithKeySure t =
  case t of
    Nil -> error "minViewWithKeySure Nil"
    Bin p l r | signBranch p ->
      case go r of
        View k a r' -> View k a (binCheckR p l r')
    _ -> go t
  where
    go (Bin p l r) =
        case go l of View k a l' -> View k a (binCheckL p l' r)
    go (Tip k y) = View k y Nil
    go Nil = error "minViewWithKey_go Nil"
-- There's never anything significant to be gained by inlining
-- this. Sufficiently recent GHC versions will inline the wrapper
-- anyway, which should be good enough.
{-# NOINLINE minViewWithKeySure #-}

-- | \(O(\min(n,W))\). Update the value at the maximal key.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMax :: (a -> Maybe a) -> IntMap a -> IntMap a
updateMax f = updateMaxWithKey (const f)

-- | \(O(\min(n,W))\). Update the value at the minimal key.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMin :: (a -> Maybe a) -> IntMap a -> IntMap a
updateMin f = updateMinWithKey (const f)

-- | \(O(\min(n,W))\). Retrieves the maximal key of the map, and the map
-- stripped of that element, or 'Nothing' if passed an empty map.
maxView :: IntMap a -> Maybe (a, IntMap a)
maxView t = fmap (\((_, x), t') -> (x, t')) (maxViewWithKey t)

-- | \(O(\min(n,W))\). Retrieves the minimal key of the map, and the map
-- stripped of that element, or 'Nothing' if passed an empty map.
minView :: IntMap a -> Maybe (a, IntMap a)
minView t = fmap (\((_, x), t') -> (x, t')) (minViewWithKey t)

-- | \(O(\min(n,W))\). Delete and find the maximal element.
--
-- Calls 'error' if the map is empty.
--
-- __Note__: This function is partial. Prefer 'maxViewWithKey'.
deleteFindMax :: IntMap a -> ((Key, a), IntMap a)
deleteFindMax = fromMaybe (error "deleteFindMax: empty map has no maximal element") . maxViewWithKey

-- | \(O(\min(n,W))\). Delete and find the minimal element.
--
-- Calls 'error' if the map is empty.
--
-- __Note__: This function is partial. Prefer 'minViewWithKey'.
deleteFindMin :: IntMap a -> ((Key, a), IntMap a)
deleteFindMin = fromMaybe (error "deleteFindMin: empty map has no minimal element") . minViewWithKey

-- The KeyValue type is used when returning a key-value pair and helps with
-- GHC optimizations.
--
-- For lookupMinSure, if the return type is (Int, a), GHC compiles it to a
-- worker $wlookupMinSure :: IntMap a -> (# Int, a #). If the return type is
-- KeyValue a instead, the worker does not box the int and returns
-- (# Int#, a #).
-- For a modern enough GHC (>=9.4), this measure turns out to be unnecessary in
-- this instance. We still use it for older GHCs and to make our intent clear.

data KeyValue a = KeyValue {-# UNPACK #-} !Key a

kvToTuple :: KeyValue a -> (Key, a)
kvToTuple (KeyValue k x) = (k, x)
{-# INLINE kvToTuple #-}

lookupMinSure :: IntMap a -> KeyValue a
lookupMinSure (Tip k v)   = KeyValue k v
lookupMinSure (Bin _ l _) = lookupMinSure l
lookupMinSure Nil         = error "lookupMinSure Nil"

-- | \(O(\min(n,W))\). The minimal key of the map. Returns 'Nothing' if the map is empty.
lookupMin :: IntMap a -> Maybe (Key, a)
lookupMin Nil         = Nothing
lookupMin (Tip k v)   = Just (k,v)
lookupMin (Bin p l r) =
  Just $! kvToTuple (lookupMinSure (if signBranch p then r else l))
{-# INLINE lookupMin #-} -- See Note [Inline lookupMin] in Data.Set.Internal

-- | \(O(\min(n,W))\). The minimal key of the map. Calls 'error' if the map is empty.
--
-- __Note__: This function is partial. Prefer 'lookupMin'.
findMin :: IntMap a -> (Key, a)
findMin t
  | Just r <- lookupMin t = r
  | otherwise = error "findMin: empty map has no minimal element"

lookupMaxSure :: IntMap a -> KeyValue a
lookupMaxSure (Tip k v)   = KeyValue k v
lookupMaxSure (Bin _ _ r) = lookupMaxSure r
lookupMaxSure Nil         = error "lookupMaxSure Nil"

-- | \(O(\min(n,W))\). The maximal key of the map. Returns 'Nothing' if the map is empty.
lookupMax :: IntMap a -> Maybe (Key, a)
lookupMax Nil         = Nothing
lookupMax (Tip k v)   = Just (k,v)
lookupMax (Bin p l r) =
  Just $! kvToTuple (lookupMaxSure (if signBranch p then l else r))
{-# INLINE lookupMax #-} -- See Note [Inline lookupMin] in Data.Set.Internal

-- | \(O(\min(n,W))\). The maximal key of the map. Calls 'error' if the map is empty.
--
-- __Note__: This function is partial. Prefer 'lookupMax'.
findMax :: IntMap a -> (Key, a)
findMax t
  | Just r <- lookupMax t = r
  | otherwise = error "findMax: empty map has no maximal element"

-- | \(O(\min(n,W))\). Delete the minimal key. Returns an empty map if the map is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Map.Map' &#8211;
-- versions prior to 0.5 threw an error if the 'IntMap' was already empty.
deleteMin :: IntMap a -> IntMap a
deleteMin = maybe Nil snd . minView

-- | \(O(\min(n,W))\). Delete the maximal key. Returns an empty map if the map is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Map.Map' &#8211;
-- versions prior to 0.5 threw an error if the 'IntMap' was already empty.
deleteMax :: IntMap a -> IntMap a
deleteMax = maybe Nil snd . maxView


{--------------------------------------------------------------------
  Submap
--------------------------------------------------------------------}
-- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
-- Is this a proper submap? (ie. a submap but not equal).
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: Eq a => IntMap a -> IntMap a -> Bool
isProperSubmapOf m1 m2
  = isProperSubmapOfBy (==) m1 m2

{- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
 Is this a proper submap? (ie. a submap but not equal).
 The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
 @keys m1@ and @keys m2@ are not equal,
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':

  > isProperSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])

 But the following are all 'False':

  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
  > isProperSubmapOfBy (<)  (fromList [(1,1)])       (fromList [(1,1),(2,2)])
-}
isProperSubmapOfBy :: (a -> b -> Bool) -> IntMap a -> IntMap b -> Bool
isProperSubmapOfBy predicate t1 t2
  = case submapCmp predicate t1 t2 of
      LT -> True
      _  -> False

submapCmp :: (a -> b -> Bool) -> IntMap a -> IntMap b -> Ordering
submapCmp predicate t1@(Bin p1 l1 r1) (Bin p2 l2 r2) = case treeTreeBranch p1 p2 of
  ABL -> GT
  ABR -> GT
  BAL -> submapCmpLt l2
  BAR -> submapCmpLt r2
  EQL -> submapCmpEq
  NOM -> GT  -- disjoint
  where
    submapCmpLt t = case submapCmp predicate t1 t of
                      GT -> GT
                      _  -> LT
    submapCmpEq = case (submapCmp predicate l1 l2, submapCmp predicate r1 r2) of
                    (GT,_ ) -> GT
                    (_ ,GT) -> GT
                    (EQ,EQ) -> EQ
                    _       -> LT

submapCmp _         (Bin _ _ _) _  = GT
submapCmp predicate (Tip kx x) (Tip ky y)
  | (kx == ky) && predicate x y = EQ
  | otherwise                   = GT  -- disjoint
submapCmp predicate (Tip k x) t
  = case lookup k t of
     Just y | predicate x y -> LT
     _                      -> GT -- disjoint
submapCmp _    Nil Nil = EQ
submapCmp _    Nil _   = LT

-- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
-- Is this a submap?
-- Defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
isSubmapOf :: Eq a => IntMap a -> IntMap a -> Bool
isSubmapOf m1 m2
  = isSubmapOfBy (==) m1 m2

{- | \(O(\min(n, m \log \frac{2^W}{m})), m \leq n\).
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
isSubmapOfBy predicate t1@(Bin p1 l1 r1) (Bin p2 l2 r2) = case treeTreeBranch p1 p2 of
  ABL -> False
  ABR -> False
  BAL -> isSubmapOfBy predicate t1 l2
  BAR -> isSubmapOfBy predicate t1 r2
  EQL -> isSubmapOfBy predicate l1 l2 && isSubmapOfBy predicate r1 r2
  NOM -> False
isSubmapOfBy _         (Bin _ _ _) _ = False
isSubmapOfBy predicate (Tip k x) t     = case lookup k t of
                                         Just y  -> predicate x y
                                         Nothing -> False
isSubmapOfBy _         Nil _           = True

{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}
-- | \(O(n)\). Map a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]

map :: (a -> b) -> IntMap a -> IntMap b
map f = go
  where
    go (Bin p l r) = Bin p (go l) (go r)
    go (Tip k x)   = Tip k (f x)
    go Nil         = Nil

#ifdef __GLASGOW_HASKELL__
{-# NOINLINE [1] map #-}
{-# RULES
"map/map" forall f g xs . map f (map g xs) = map (f . g) xs
"map/coerce" map coerce = coerce
 #-}
#endif

-- | \(O(n)\). Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]

mapWithKey :: (Key -> a -> b) -> IntMap a -> IntMap b
mapWithKey f t
  = case t of
      Bin p l r -> Bin p (mapWithKey f l) (mapWithKey f r)
      Tip k x   -> Tip k (f k x)
      Nil       -> Nil

#ifdef __GLASGOW_HASKELL__
{-# NOINLINE [1] mapWithKey #-}
{-# RULES
"mapWithKey/mapWithKey" forall f g xs . mapWithKey f (mapWithKey g xs) =
  mapWithKey (\k a -> f k (g k a)) xs
"mapWithKey/map" forall f g xs . mapWithKey f (map g xs) =
  mapWithKey (\k a -> f k (g a)) xs
"map/mapWithKey" forall f g xs . map f (mapWithKey g xs) =
  mapWithKey (\k a -> f (g k a)) xs
 #-}
#endif

-- | \(O(n)\).
-- @'traverseWithKey' f s == 'fromList' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
--
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a'), (5, 'e')]) == Just (fromList [(1, 'b'), (5, 'f')])
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')])           == Nothing
traverseWithKey :: Applicative t => (Key -> a -> t b) -> IntMap a -> t (IntMap b)
traverseWithKey f = go
  where
    go Nil = pure Nil
    go (Tip k v) = Tip k <$> f k v
    go (Bin p l r)
      | signBranch p = liftA2 (flip (Bin p)) (go r) (go l)
      | otherwise = liftA2 (Bin p) (go l) (go r)
{-# INLINE traverseWithKey #-}

-- | \(O(n)\). The function @'mapAccum'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])

mapAccum :: (a -> b -> (a,c)) -> a -> IntMap b -> (a,IntMap c)
mapAccum f = mapAccumWithKey (\a' _ x -> f a' x)

-- | \(O(n)\). The function @'mapAccumWithKey'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])

mapAccumWithKey :: (a -> Key -> b -> (a,c)) -> a -> IntMap b -> (a,IntMap c)
mapAccumWithKey f a t
  = mapAccumL f a t

-- | \(O(n)\). The function @'mapAccumL'@ threads an accumulating
-- argument through the map in ascending order of keys.
mapAccumL :: (a -> Key -> b -> (a,c)) -> a -> IntMap b -> (a,IntMap c)
mapAccumL f a t
  = case t of
      Bin p l r
        | signBranch p ->
            let (a1,r') = mapAccumL f a r
                (a2,l') = mapAccumL f a1 l
            in (a2,Bin p l' r')
        | otherwise  ->
            let (a1,l') = mapAccumL f a l
                (a2,r') = mapAccumL f a1 r
            in (a2,Bin p l' r')
      Tip k x     -> let (a',x') = f a k x in (a',Tip k x')
      Nil         -> (a,Nil)

-- | \(O(n)\). The function @'mapAccumRWithKey'@ threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (a -> Key -> b -> (a,c)) -> a -> IntMap b -> (a,IntMap c)
mapAccumRWithKey f a t
  = case t of
      Bin p l r
        | signBranch p ->
            let (a1,l') = mapAccumRWithKey f a l
                (a2,r') = mapAccumRWithKey f a1 r
            in (a2,Bin p l' r')
        | otherwise  ->
            let (a1,r') = mapAccumRWithKey f a r
                (a2,l') = mapAccumRWithKey f a1 l
            in (a2,Bin p l' r')
      Tip k x     -> let (a',x') = f a k x in (a',Tip k x')
      Nil         -> (a,Nil)

-- | \(O(n \min(n,W))\).
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- If `f` is monotonically non-decreasing or monotonically non-increasing, this
-- function takes \(O(n)\) time.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the greatest of the
-- original keys is retained.
--
-- > mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        == fromList [(4, "b"), (6, "a")]
-- > mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "c"
-- > mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "c"

mapKeys :: (Key->Key) -> IntMap a -> IntMap a
mapKeys f t = finishB (foldlWithKey' (\b kx x -> insertB (f kx) x b) emptyB t)

-- | \(O(n \min(n,W))\).
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- If `f` is monotonically non-decreasing or monotonically non-increasing, this
-- function takes \(O(n)\) time.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
--
-- > mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "cdab"
-- > mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "cdab"
--
-- Also see the performance note on 'fromListWith'.

mapKeysWith :: (a -> a -> a) -> (Key->Key) -> IntMap a -> IntMap a
mapKeysWith c f t =
  finishB (foldlWithKey' (\b kx x -> insertWithB c (f kx) x b) emptyB t)

-- | \(O(n)\).
-- @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapKeysMonotonic f s == mapKeys f s
-- >     where ls = keys s
--
-- This means that @f@ maps distinct original keys to distinct resulting keys.
-- This function has slightly better performance than 'mapKeys'.
--
-- __Warning__: This function should be used only if @f@ is monotonically
-- strictly increasing. This precondition is not checked. Use 'mapKeys' if the
-- precondition may not hold.
--
-- > mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) == fromList [(6, "b"), (10, "a")]

mapKeysMonotonic :: (Key->Key) -> IntMap a -> IntMap a
mapKeysMonotonic f t =
  ascLinkAll (foldlWithKey' (\s kx x -> ascInsert s (f kx) x) MSNada t)

{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
-- | \(O(n)\). Filter all values that satisfy some predicate.
--
-- > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > filter (> "x") (fromList [(5,"a"), (3,"b")]) == empty
-- > filter (< "a") (fromList [(5,"a"), (3,"b")]) == empty

filter :: (a -> Bool) -> IntMap a -> IntMap a
filter p m
  = filterWithKey (\_ x -> p x) m

-- | \(O(n)\). Filter all keys that satisfy some predicate.
--
-- @
-- filterKeys p = 'filterWithKey' (\\k _ -> p k)
-- @
--
-- > filterKeys (> 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
--
-- @since 0.8

filterKeys :: (Key -> Bool) -> IntMap a -> IntMap a
filterKeys predicate = filterWithKey (\k _ -> predicate k)

-- | \(O(n)\). Filter all keys\/values that satisfy some predicate.
--
-- > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

filterWithKey :: (Key -> a -> Bool) -> IntMap a -> IntMap a
filterWithKey predicate = go
    where
    go Nil         = Nil
    go t@(Tip k x) = if predicate k x then t else Nil
    go (Bin p l r) = bin p (go l) (go r)

-- | \(O(n)\). Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partition :: (a -> Bool) -> IntMap a -> (IntMap a,IntMap a)
partition p m
  = partitionWithKey (\_ x -> p x) m

-- | \(O(n)\). Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
-- > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partitionWithKey :: (Key -> a -> Bool) -> IntMap a -> (IntMap a,IntMap a)
partitionWithKey predicate0 t0 = toPair $ go predicate0 t0
  where
    go predicate t =
      case t of
        Bin p l r ->
          let (l1 :*: l2) = go predicate l
              (r1 :*: r2) = go predicate r
          in bin p l1 r1 :*: bin p l2 r2
        Tip k x
          | predicate k x -> (t :*: Nil)
          | otherwise     -> (Nil :*: t)
        Nil -> (Nil :*: Nil)

-- | \(O(\min(n,W))\). Take while a predicate on the keys holds.
-- The user is responsible for ensuring that for all @Int@s, @j \< k ==\> p j \>= p k@.
-- See note at 'spanAntitone'.
--
-- @
-- takeWhileAntitone p = 'fromDistinctAscList' . 'Data.List.takeWhile' (p . fst) . 'toList'
-- takeWhileAntitone p = 'filterWithKey' (\\k _ -> p k)
-- @
--
-- @since 0.6.7
takeWhileAntitone :: (Key -> Bool) -> IntMap a -> IntMap a
takeWhileAntitone predicate t =
  case t of
    Bin p l r
      | signBranch p ->
        if predicate 0 -- handle negative numbers.
        then binCheckL p (go predicate l) r
        else go predicate r
    _ -> go predicate t
  where
    go predicate' (Bin p l r)
      | predicate' (unPrefix p) = binCheckR p l (go predicate' r)
      | otherwise               = go predicate' l
    go predicate' t'@(Tip ky _)
      | predicate' ky = t'
      | otherwise     = Nil
    go _ Nil = Nil

-- | \(O(\min(n,W))\). Drop while a predicate on the keys holds.
-- The user is responsible for ensuring that for all @Int@s, @j \< k ==\> p j \>= p k@.
-- See note at 'spanAntitone'.
--
-- @
-- dropWhileAntitone p = 'fromDistinctAscList' . 'Data.List.dropWhile' (p . fst) . 'toList'
-- dropWhileAntitone p = 'filterWithKey' (\\k _ -> not (p k))
-- @
--
-- @since 0.6.7
dropWhileAntitone :: (Key -> Bool) -> IntMap a -> IntMap a
dropWhileAntitone predicate t =
  case t of
    Bin p l r
      | signBranch p ->
        if predicate 0 -- handle negative numbers.
        then go predicate l
        else binCheckR p l (go predicate r)
    _ -> go predicate t
  where
    go predicate' (Bin p l r)
      | predicate' (unPrefix p) = go predicate' r
      | otherwise               = binCheckL p (go predicate' l) r
    go predicate' t'@(Tip ky _)
      | predicate' ky = Nil
      | otherwise     = t'
    go _ Nil = Nil

-- | \(O(\min(n,W))\). Divide a map at the point where a predicate on the keys stops holding.
-- The user is responsible for ensuring that for all @Int@s, @j \< k ==\> p j \>= p k@.
--
-- @
-- spanAntitone p xs = ('takeWhileAntitone' p xs, 'dropWhileAntitone' p xs)
-- spanAntitone p xs = 'partitionWithKey' (\\k _ -> p k) xs
-- @
--
-- Note: if @p@ is not actually antitone, then @spanAntitone@ will split the map
-- at some /unspecified/ point.
--
-- @since 0.6.7
spanAntitone :: (Key -> Bool) -> IntMap a -> (IntMap a, IntMap a)
spanAntitone predicate t =
  case t of
    Bin p l r
      | signBranch p ->
        if predicate 0 -- handle negative numbers.
        then
          case go predicate l of
            (lt :*: gt) ->
              let !lt' = binCheckL p lt r
              in (lt', gt)
        else
          case go predicate r of
            (lt :*: gt) ->
              let !gt' = binCheckR p l gt
              in (lt, gt')
    _ -> case go predicate t of
          (lt :*: gt) -> (lt, gt)
  where
    go predicate' (Bin p l r)
      | predicate' (unPrefix p)
      = case go predicate' r of (lt :*: gt) -> binCheckR p l lt :*: gt
      | otherwise
      = case go predicate' l of (lt :*: gt) -> lt :*: binCheckL p gt r
    go predicate' t'@(Tip ky _)
      | predicate' ky = (t' :*: Nil)
      | otherwise     = (Nil :*: t')
    go _ Nil = (Nil :*: Nil)

-- | \(O(n)\). Map values and collect the 'Just' results.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"

mapMaybe :: (a -> Maybe b) -> IntMap a -> IntMap b
mapMaybe f = mapMaybeWithKey (\_ x -> f x)

-- | \(O(n)\). Map keys\/values and collect the 'Just' results.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"

mapMaybeWithKey :: (Key -> a -> Maybe b) -> IntMap a -> IntMap b
mapMaybeWithKey f (Bin p l r)
  = bin p (mapMaybeWithKey f l) (mapMaybeWithKey f r)
mapMaybeWithKey f (Tip k x) = case f k x of
  Just y  -> Tip k y
  Nothing -> Nil
mapMaybeWithKey _ Nil = Nil

-- | \(O(n)\). Map values and separate the 'Left' and 'Right' results.
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- >
-- > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])

mapEither :: (a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
mapEither f m
  = mapEitherWithKey (\_ x -> f x) m

-- | \(O(n)\). Map keys\/values and separate the 'Left' and 'Right' results.
--
-- > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
-- > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
-- >
-- > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])

mapEitherWithKey :: (Key -> a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
mapEitherWithKey f0 t0 = toPair $ go f0 t0
  where
    go f (Bin p l r) =
      bin p l1 r1 :*: bin p l2 r2
      where
        (l1 :*: l2) = go f l
        (r1 :*: r2) = go f r
    go f (Tip k x) = case f k x of
      Left y  -> (Tip k y :*: Nil)
      Right z -> (Nil :*: Tip k z)
    go _ Nil = (Nil :*: Nil)

-- | \(O(\min(n,W))\). The expression (@'split' k map@) is a pair @(map1,map2)@
-- where all keys in @map1@ are lower than @k@ and all keys in
-- @map2@ larger than @k@. Any key equal to @k@ is found in neither @map1@ nor @map2@.
--
-- > split 2 (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3,"b"), (5,"a")])
-- > split 3 (fromList [(5,"a"), (3,"b")]) == (empty, singleton 5 "a")
-- > split 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > split 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", empty)
-- > split 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], empty)

split :: Key -> IntMap a -> (IntMap a, IntMap a)
split k t =
  case t of
    Bin p l r
      | signBranch p ->
        if k >= 0 -- handle negative numbers.
        then
          case go k l of
            (lt :*: gt) ->
              let !lt' = binCheckL p lt r
              in (lt', gt)
        else
          case go k r of
            (lt :*: gt) ->
              let !gt' = binCheckR p l gt
              in (lt, gt')
    _ -> case go k t of
          (lt :*: gt) -> (lt, gt)
  where
    go !k' t'@(Bin p l r)
      | nomatch k' p = if k' < unPrefix p then Nil :*: t' else t' :*: Nil
      | left k' p = case go k' l of (lt :*: gt) -> lt :*: binCheckL p gt r
      | otherwise = case go k' r of (lt :*: gt) -> binCheckR p l lt :*: gt
    go k' t'@(Tip ky _)
      | k' > ky   = (t' :*: Nil)
      | k' < ky   = (Nil :*: t')
      | otherwise = (Nil :*: Nil)
    go _ Nil = (Nil :*: Nil)


type SplitLookup a = StrictTriple (IntMap a) (Maybe a) (IntMap a)

mapLT :: (IntMap a -> IntMap a) -> SplitLookup a -> SplitLookup a
mapLT f (TripleS lt fnd gt) = TripleS (f lt) fnd gt
{-# INLINE mapLT #-}

mapGT :: (IntMap a -> IntMap a) -> SplitLookup a -> SplitLookup a
mapGT f (TripleS lt fnd gt) = TripleS lt fnd (f gt)
{-# INLINE mapGT #-}

-- | \(O(\min(n,W))\). Performs a 'split' but also returns whether the pivot
-- key was found in the original map.
--
-- > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (empty, Nothing, fromList [(3,"b"), (5,"a")])
-- > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (empty, Just "b", singleton 5 "a")
-- > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Nothing, singleton 5 "a")
-- > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", empty)
-- > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], Nothing, empty)

splitLookup :: Key -> IntMap a -> (IntMap a, Maybe a, IntMap a)
splitLookup k t =
  case
    case t of
      Bin p l r
        | signBranch p ->
          if k >= 0 -- handle negative numbers.
          then mapLT (\l' -> binCheckL p l' r) (go k l)
          else mapGT (binCheckR p l) (go k r)
      _ -> go k t
  of TripleS lt fnd gt -> (lt, fnd, gt)
  where
    go !k' t'@(Bin p l r)
      | nomatch k' p =
          if k' < unPrefix p
          then TripleS Nil Nothing t'
          else TripleS t' Nothing Nil
      | left k' p = mapGT (\l' -> binCheckL p l' r) (go k' l)
      | otherwise  = mapLT (binCheckR p l) (go k' r)
    go k' t'@(Tip ky y)
      | k' > ky   = TripleS t'  Nothing  Nil
      | k' < ky   = TripleS Nil Nothing  t'
      | otherwise = TripleS Nil (Just y) Nil
    go _ Nil      = TripleS Nil Nothing  Nil

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | \(O(n)\). Fold the values in the map using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
--
-- For example,
--
-- > elems map = foldr (:) [] map
--
-- > let f a len = len + (length a)
-- > foldr f 0 (fromList [(5,"a"), (3,"bbb")]) == 4

-- See Note [IntMap folds]
foldr :: (a -> b -> b) -> b -> IntMap a -> b
foldr f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of
    Nil -> z
    Bin p l r
      | signBranch p -> go (go z l) r -- put negative numbers before
      | otherwise -> go (go z r) l
    _ -> go z t
  where
    go _ Nil          = error "foldr.go: Nil"
    go z' (Tip _ x)   = f x z'
    go z' (Bin _ l r) = go (go z' r) l
{-# INLINE foldr #-}

-- | \(O(n)\). A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.

-- See Note [IntMap folds]
foldr' :: (a -> b -> b) -> b -> IntMap a -> b
foldr' f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of
    Nil -> z
    Bin p l r
      | signBranch p -> go (go z l) r -- put negative numbers before
      | otherwise -> go (go z r) l
    _ -> go z t
  where
    go !_ Nil         = error "foldr'.go: Nil"
    go z' (Tip _ x)   = f x z'
    go z' (Bin _ l r) = go (go z' r) l
{-# INLINE foldr' #-}

-- | \(O(n)\). Fold the values in the map using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
--
-- For example,
--
-- > elems = reverse . foldl (flip (:)) []
--
-- > let f len a = len + (length a)
-- > foldl f 0 (fromList [(5,"a"), (3,"bbb")]) == 4

-- See Note [IntMap folds]
foldl :: (a -> b -> a) -> a -> IntMap b -> a
foldl f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of
    Nil -> z
    Bin p l r
      | signBranch p -> go (go z r) l -- put negative numbers before
      | otherwise -> go (go z l) r
    _ -> go z t
  where
    go _ Nil          = error "foldl.go: Nil"
    go z' (Tip _ x)   = f z' x
    go z' (Bin _ l r) = go (go z' l) r
{-# INLINE foldl #-}

-- | \(O(n)\). A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.

-- See Note [IntMap folds]
foldl' :: (a -> b -> a) -> a -> IntMap b -> a
foldl' f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of
    Nil -> z
    Bin p l r
      | signBranch p -> go (go z r) l -- put negative numbers before
      | otherwise -> go (go z l) r
    _ -> go z t
  where
    go !_ Nil         = error "foldl'.go: Nil"
    go z' (Tip _ x)   = f z' x
    go z' (Bin _ l r) = go (go z' l) r
{-# INLINE foldl' #-}

-- See Note [IntMap folds]
foldMap :: Monoid m => (a -> m) -> IntMap a -> m
foldMap f = \t -> -- Use lambda to be inlinable with two arguments.
  case t of
    Nil -> mempty
    Bin p l r
#if MIN_VERSION_base(4,11,0)
      | signBranch p -> go r <> go l
      | otherwise -> go l <> go r
#else
      | signBranch p -> go r `mappend` go l
      | otherwise -> go l `mappend` go r
#endif
    _ -> go t
  where
    go Nil = error "foldMap.go: Nil"
    go (Tip _ x) = f x
#if MIN_VERSION_base(4,11,0)
    go (Bin _ l r) = go l <> go r
#else
    go (Bin _ l r) = go l `mappend` go r
#endif
{-# INLINE foldMap #-}

-- | \(O(n)\). Fold the keys and values in the map using the given right-associative
-- binary operator, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
--
-- For example,
--
-- > keys map = foldrWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldrWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"

-- See Note [IntMap folds]
foldrWithKey :: (Key -> a -> b -> b) -> b -> IntMap a -> b
foldrWithKey f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of
    Nil -> z
    Bin p l r
      | signBranch p -> go (go z l) r -- put negative numbers before
      | otherwise -> go (go z r) l
    _ -> go z t
  where
    go _ Nil          = error "foldrWithKey.go: Nil"
    go z' (Tip kx x)  = f kx x z'
    go z' (Bin _ l r) = go (go z' r) l
{-# INLINE foldrWithKey #-}

-- | \(O(n)\). A strict version of 'foldrWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.

-- See Note [IntMap folds]
foldrWithKey' :: (Key -> a -> b -> b) -> b -> IntMap a -> b
foldrWithKey' f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of
    Nil -> z
    Bin p l r
      | signBranch p -> go (go z l) r -- put negative numbers before
      | otherwise -> go (go z r) l
    _ -> go z t
  where
    go !_ Nil         = error "foldrWithKey'.go: Nil"
    go z' (Tip kx x)  = f kx x z'
    go z' (Bin _ l r) = go (go z' r) l
{-# INLINE foldrWithKey' #-}

-- | \(O(n)\). Fold the keys and values in the map using the given left-associative
-- binary operator, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
--
-- For example,
--
-- > keys = reverse . foldlWithKey (\ks k x -> k:ks) []
--
-- > let f result k a = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldlWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (3:b)(5:a)"

-- See Note [IntMap folds]
foldlWithKey :: (a -> Key -> b -> a) -> a -> IntMap b -> a
foldlWithKey f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of
    Nil -> z
    Bin p l r
      | signBranch p -> go (go z r) l -- put negative numbers before
      | otherwise -> go (go z l) r
    _ -> go z t
  where
    go _ Nil          = error "foldlWithKey.go: Nil"
    go z' (Tip kx x)  = f z' kx x
    go z' (Bin _ l r) = go (go z' l) r
{-# INLINE foldlWithKey #-}

-- | \(O(n)\). A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.

-- See Note [IntMap folds]
foldlWithKey' :: (a -> Key -> b -> a) -> a -> IntMap b -> a
foldlWithKey' f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of
    Nil -> z
    Bin p l r
      | signBranch p -> go (go z r) l -- put negative numbers before
      | otherwise -> go (go z l) r
    _ -> go z t
  where
    go !_ Nil         = error "foldlWithKey'.go: Nil"
    go z' (Tip kx x)  = f z' kx x
    go z' (Bin _ l r) = go (go z' l) r
{-# INLINE foldlWithKey' #-}

-- | \(O(n)\). Fold the keys and values in the map using the given monoid, such that
--
-- @'foldMapWithKey' f = 'Prelude.fold' . 'mapWithKey' f@
--
-- This can be an asymptotically faster than 'foldrWithKey' or 'foldlWithKey' for some monoids.
--
-- @since 0.5.4

-- See Note [IntMap folds]
foldMapWithKey :: Monoid m => (Key -> a -> m) -> IntMap a -> m
foldMapWithKey f = \t -> -- Use lambda to be inlinable with two arguments.
  case t of
    Nil -> mempty
    Bin p l r
#if MIN_VERSION_base(4,11,0)
      | signBranch p -> go r <> go l
      | otherwise -> go l <> go r
#else
      | signBranch p -> go r `mappend` go l
      | otherwise -> go l `mappend` go r
#endif
    _ -> go t
  where
    go Nil = error "foldMap.go: Nil"
    go (Tip kx x) = f kx x
#if MIN_VERSION_base(4,11,0)
    go (Bin _ l r) = go l <> go r
#else
    go (Bin _ l r) = go l `mappend` go r
#endif
{-# INLINE foldMapWithKey #-}

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | \(O(n)\).
-- Return all elements of the map in the ascending order of their keys.
-- Subject to list fusion.
--
-- > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
-- > elems empty == []

elems :: IntMap a -> [a]
elems = foldr (:) []

-- | \(O(n)\). Return all keys of the map in ascending order. Subject to list
-- fusion.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []

keys  :: IntMap a -> [Key]
keys = foldrWithKey (\k _ ks -> k : ks) []

-- | \(O(n)\). An alias for 'toAscList'. Returns all key\/value pairs in the
-- map in ascending key order. Subject to list fusion.
--
-- > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > assocs empty == []

assocs :: IntMap a -> [(Key,a)]
assocs = toAscList

-- | \(O(n)\). The set of all keys of the map.
--
-- > keysSet (fromList [(5,"a"), (3,"b")]) == Data.IntSet.fromList [3,5]
-- > keysSet empty == Data.IntSet.empty

keysSet :: IntMap a -> IntSet
keysSet Nil = IntSet.Nil
keysSet (Tip kx _) = IntSet.singleton kx
keysSet (Bin p l r)
  | unPrefix p .&. IntSet.suffixBitMask == 0
  = IntSet.Bin p (keysSet l) (keysSet r)
  | otherwise
  = IntSet.Tip (unPrefix p .&. IntSet.prefixBitMask) (computeBm (computeBm 0 l) r)
  where computeBm !acc (Bin _ l' r') = computeBm (computeBm acc l') r'
        computeBm acc (Tip kx _) = acc .|. IntSet.bitmapOf kx
        computeBm _   Nil = error "Data.IntSet.keysSet: Nil"

-- | \(O(n)\). Build a map from a set of keys and a function which for each key
-- computes its value.
--
-- > fromSet (\k -> replicate k 'a') (Data.IntSet.fromList [3, 5]) == fromList [(5,"aaaaa"), (3,"aaa")]
-- > fromSet undefined Data.IntSet.empty == empty

fromSet :: (Key -> a) -> IntSet -> IntMap a
#ifdef __GLASGOW_HASKELL__
fromSet f = runIdentity . fromSetA (coerce f)
#else
fromSet f = runIdentity . fromSetA (pure . f)
#endif

-- | \(O(n)\). Build a map from a set of keys and a function which for each key
-- computes its value, while within an 'Applicative' context.
--
-- > fromSetA (\k -> pure $ replicate k 'a') (Data.IntSet.fromList [3, 5]) == pure (fromList [(5,"aaaaa"), (3,"aaa")])
-- > fromSetA undefined Data.IntSet.empty == pure empty

fromSetA :: Applicative f => (Key -> f a) -> IntSet -> f (IntMap a)
fromSetA _ IntSet.Nil = pure Nil
fromSetA f (IntSet.Bin p l r)
  | signBranch p = liftA2 (flip (Bin p)) (fromSetA f r) (fromSetA f l)
  | otherwise = liftA2 (Bin p) (fromSetA f l) (fromSetA f r)
fromSetA f (IntSet.Tip kx bm) = buildTree f kx bm (IntSet.suffixBitMask + 1)
  where
    -- This is slightly complicated, as we to convert the dense
    -- representation of IntSet into tree representation of IntMap.
    --
    -- We are given a nonzero bit mask 'bmask' of 'bits' bits with
    -- prefix 'prefix'. We split bmask into halves corresponding
    -- to left and right subtree. If they are both nonempty, we
    -- create a Bin node, otherwise exactly one of them is nonempty
    -- and we construct the IntMap from that half.
    buildTree g !prefix !bmask bits = case bits of
      0 -> Tip prefix <$> (g prefix)
      _ -> case bits `iShiftRL` 1 of
        bits2
          | bmask .&. ((1 `shiftLL` bits2) - 1) == 0 ->
              buildTree g (prefix + bits2) (bmask `shiftRL` bits2) bits2
          | (bmask `shiftRL` bits2) .&. ((1 `shiftLL` bits2) - 1) == 0 ->
              buildTree g prefix bmask bits2
          | otherwise ->
              liftA2
                (Bin (Prefix (prefix .|. bits2)))
                  (buildTree g prefix bmask bits2)
                  (buildTree g (prefix + bits2) (bmask `shiftRL` bits2) bits2)
#if __GLASGOW_HASKELL__
{-# INLINABLE fromSetA #-}
#else
{-# INLINE fromSetA #-}
#endif

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

#ifdef __GLASGOW_HASKELL__
-- | @since 0.5.6.2
instance GHCExts.IsList (IntMap a) where
  type Item (IntMap a) = (Key,a)
  fromList = fromList
  toList   = toList
#endif

-- | \(O(n)\). Convert the map to a list of key\/value pairs. Subject to list
-- fusion.
--
-- > toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > toList empty == []

toList :: IntMap a -> [(Key,a)]
toList = toAscList

-- | \(O(n)\). Convert the map to a list of key\/value pairs where the
-- keys are in ascending order. Subject to list fusion.
--
-- > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]

toAscList :: IntMap a -> [(Key,a)]
toAscList = foldrWithKey (\k x xs -> (k,x):xs) []

-- | \(O(n)\). Convert the map to a list of key\/value pairs where the keys
-- are in descending order. Subject to list fusion.
--
-- > toDescList (fromList [(5,"a"), (3,"b")]) == [(5,"a"), (3,"b")]

toDescList :: IntMap a -> [(Key,a)]
toDescList = foldlWithKey (\xs k x -> (k,x):xs) []

-- List fusion for the list generating functions.
#if __GLASGOW_HASKELL__
-- The foldrFB and foldlFB are fold{r,l}WithKey equivalents, used for list fusion.
-- They are important to convert unfused methods back, see mapFB in prelude.
foldrFB :: (Key -> a -> b -> b) -> b -> IntMap a -> b
foldrFB = foldrWithKey
{-# INLINE[0] foldrFB #-}
foldlFB :: (a -> Key -> b -> a) -> a -> IntMap b -> a
foldlFB = foldlWithKey
{-# INLINE[0] foldlFB #-}

-- Inline assocs and toList, so that we need to fuse only toAscList.
{-# INLINE assocs #-}
{-# INLINE toList #-}

-- The fusion is enabled up to phase 2 included. If it does not succeed,
-- convert in phase 1 the expanded elems,keys,to{Asc,Desc}List calls back to
-- elems,keys,to{Asc,Desc}List.  In phase 0, we inline fold{lr}FB (which were
-- used in a list fusion, otherwise it would go away in phase 1), and let compiler
-- do whatever it wants with elems,keys,to{Asc,Desc}List -- it was forbidden to
-- inline it before phase 0, otherwise the fusion rules would not fire at all.
{-# NOINLINE[0] elems #-}
{-# NOINLINE[0] keys #-}
{-# NOINLINE[0] toAscList #-}
{-# NOINLINE[0] toDescList #-}
{-# RULES "IntMap.elems" [~1] forall m . elems m = build (\c n -> foldrFB (\_ x xs -> c x xs) n m) #-}
{-# RULES "IntMap.elemsBack" [1] foldrFB (\_ x xs -> x : xs) [] = elems #-}
{-# RULES "IntMap.keys" [~1] forall m . keys m = build (\c n -> foldrFB (\k _ xs -> c k xs) n m) #-}
{-# RULES "IntMap.keysBack" [1] foldrFB (\k _ xs -> k : xs) [] = keys #-}
{-# RULES "IntMap.toAscList" [~1] forall m . toAscList m = build (\c n -> foldrFB (\k x xs -> c (k,x) xs) n m) #-}
{-# RULES "IntMap.toAscListBack" [1] foldrFB (\k x xs -> (k, x) : xs) [] = toAscList #-}
{-# RULES "IntMap.toDescList" [~1] forall m . toDescList m = build (\c n -> foldlFB (\xs k x -> c (k,x) xs) n m) #-}
{-# RULES "IntMap.toDescListBack" [1] foldlFB (\xs k x -> (k, x) : xs) [] = toDescList #-}
#endif


-- | \(O(n \min(n,W))\). Create a map from a list of key\/value pairs.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
--
-- If the keys are in sorted order, ascending or descending, this function
-- takes \(O(n)\) time.
--
-- > fromList [] == empty
-- > fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
-- > fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]

fromList :: [(Key,a)] -> IntMap a
fromList xs = finishB (Foldable.foldl' (\b (kx,x) -> insertB kx x b) emptyB xs)
{-# INLINE fromList #-} -- Inline for list fusion

-- | \(O(n \min(n,W))\). Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
--
-- If the keys are in sorted order, ascending or descending, this function
-- takes \(O(n)\) time.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"x"), (5,"c")] == fromList [(3, "x"), (5, "cba")]
-- > fromListWith (++) [] == empty
--
-- Note the reverse ordering of @"cba"@ in the example.
--
-- The symmetric combining function @f@ is applied in a left-fold over the list, as @f new old@.
--
-- === Performance
--
-- You should ensure that the given @f@ is fast with this order of arguments.
--
-- Symmetric functions may be slow in one order, and fast in another.
-- For the common case of collecting values of matching keys in a list, as above:
--
-- The complexity of @(++) a b@ is \(O(a)\), so it is fast when given a short list as its first argument.
-- Thus:
--
-- > fromListWith       (++)  (replicate 1000000 (3, "x"))   -- O(n),  fast
-- > fromListWith (flip (++)) (replicate 1000000 (3, "x"))   -- O(n²), extremely slow
--
-- because they evaluate as, respectively:
--
-- > fromList [(3, "x" ++ ("x" ++ "xxxxx..xxxxx"))]   -- O(n)
-- > fromList [(3, ("xxxxx..xxxxx" ++ "x") ++ "x")]   -- O(n²)
--
-- Thus, to get good performance with an operation like @(++)@ while also preserving
-- the same order as in the input list, reverse the input:
--
-- > fromListWith (++) (reverse [(5,"a"), (5,"b"), (5,"c")]) == fromList [(5, "abc")]
--
-- and it is always fast to combine singleton-list values @[v]@ with @fromListWith (++)@, as in:
--
-- > fromListWith (++) $ reverse $ map (\(k, v) -> (k, [v])) someListOfTuples

fromListWith :: (a -> a -> a) -> [(Key,a)] -> IntMap a
fromListWith f xs
  = fromListWithKey (\_ x y -> f x y) xs
{-# INLINE fromListWith #-} -- Inline for list fusion

-- | \(O(n \min(n,W))\). Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWithKey'.
--
-- If the keys are in sorted order, ascending or descending, this function
-- takes \(O(n)\) time.
--
-- > let f key new_value old_value = show key ++ ":" ++ new_value ++ "|" ++ old_value
-- > fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")] == fromList [(3, "3:a|b"), (5, "5:c|5:b|a")]
-- > fromListWithKey f [] == empty
--
-- Also see the performance note on 'fromListWith'.

fromListWithKey :: (Key -> a -> a -> a) -> [(Key,a)] -> IntMap a
fromListWithKey f xs =
  finishB (Foldable.foldl' (\b (kx,x) -> insertWithB (f kx) kx x b) emptyB xs)
{-# INLINE fromListWithKey #-} -- Inline for list fusion

-- | \(O(n)\). Build a map from a list of key\/value pairs where
-- the keys are in ascending order.
--
-- __Warning__: This function should be used only if the keys are in
-- non-decreasing order. This precondition is not checked. Use 'fromList' if the
-- precondition may not hold.
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]

fromAscList :: [(Key,a)] -> IntMap a
fromAscList xs =
  ascLinkAll (Foldable.foldl' (\s (ky, y) -> ascInsert s ky y) MSNada xs)
{-# INLINE fromAscList #-} -- Inline for list fusion

-- | \(O(n)\). Build a map from a list of key\/value pairs where
-- the keys are in ascending order, with a combining function on equal keys.
--
-- __Warning__: This function should be used only if the keys are in
-- non-decreasing order. This precondition is not checked. Use 'fromListWith' if
-- the precondition may not hold.
--
-- > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]
--
-- Also see the performance note on 'fromListWith'.

fromAscListWith :: (a -> a -> a) -> [(Key,a)] -> IntMap a
fromAscListWith f xs = fromAscListWithKey (\_ x y -> f x y) xs
{-# INLINE fromAscListWith #-} -- Inline for list fusion

-- | \(O(n)\). Build a map from a list of key\/value pairs where
-- the keys are in ascending order, with a combining function on equal keys.
--
-- __Warning__: This function should be used only if the keys are in
-- non-decreasing order. This precondition is not checked. Use 'fromListWithKey'
-- if the precondition may not hold.
--
-- > let f key new_value old_value = show key ++ ":" ++ new_value ++ "|" ++ old_value
-- > fromAscListWithKey f [(3,"b"), (3,"a"), (5,"a"), (5,"b"), (5,"c")] == fromList [(3, "3:a|b"), (5, "5:c|5:b|a")]
-- > fromAscListWithKey f [] == empty
--
-- Also see the performance note on 'fromListWith'.

-- See Note [fromAscList implementation]
fromAscListWithKey :: (Key -> a -> a -> a) -> [(Key,a)] -> IntMap a
fromAscListWithKey f xs = ascLinkAll (Foldable.foldl' next MSNada xs)
  where
    next s (!ky, y) = case s of
      MSNada -> MSPush ky y Nada
      MSPush kx x stk
        | kx == ky -> MSPush ky (f ky y x) stk
        | otherwise -> let m = branchMask kx ky
                       in MSPush ky y (ascLinkTop stk kx (Tip kx x) m)
{-# INLINE fromAscListWithKey #-} -- Inline for list fusion

-- | \(O(n)\). Build a map from a list of key\/value pairs where
-- the keys are in ascending order and all distinct.
--
-- @fromDistinctAscList = 'fromAscList'@
--
-- See warning on 'fromAscList'.
--
-- This definition exists for backwards compatibility. It offers no advantage
-- over @fromAscList@.
fromDistinctAscList :: [(Key,a)] -> IntMap a
-- Note: There is nothing we can optimize compared to fromAscList.
-- The adjacent key equals check (kx == ky) might seem unnecessary for
-- fromDistinctAscList, but it guards branchMask which has undefined behavior
-- under that case. We could error on kx == ky instead, but that isn't any
-- better.
fromDistinctAscList = fromAscList
{-# INLINE fromDistinctAscList #-} -- Inline for list fusion

data Stack a
  = Nada
  | Push {-# UNPACK #-} !Int !(IntMap a) !(Stack a)

data MonoState a
  = MSNada
  | MSPush {-# UNPACK #-} !Key a !(Stack a)

-- Insert an entry. The key must be >= the last inserted key. If it is equal
-- to the previous key, the previous value is replaced.
ascInsert :: MonoState a -> Int -> a -> MonoState a
ascInsert s !ky y = case s of
  MSNada -> MSPush ky y Nada
  MSPush kx x stk
    | kx == ky -> MSPush ky y stk
    | otherwise -> let m = branchMask kx ky
                   in MSPush ky y (ascLinkTop stk kx (Tip kx x) m)
{-# INLINE ascInsert #-}

ascLinkTop :: Stack a -> Int -> IntMap a -> Int -> Stack a
ascLinkTop stk !rk r !rm = case stk of
  Nada -> Push rm r stk
  Push m l stk'
    | i2w m < i2w rm -> let p = mask rk m
                        in ascLinkTop stk' rk (Bin p l r) rm
    | otherwise -> Push rm r stk

ascLinkAll :: MonoState a -> IntMap a
ascLinkAll s = case s of
  MSNada -> Nil
  MSPush kx x stk -> ascLinkStack stk kx (Tip kx x)
{-# INLINABLE ascLinkAll #-}

ascLinkStack :: Stack a -> Int -> IntMap a -> IntMap a
ascLinkStack stk !rk r = case stk of
  Nada -> r
  Push m l stk'
    | signBranch p -> Bin p r l
    | otherwise -> ascLinkStack stk' rk (Bin p l r)
    where
      p = mask rk m

{--------------------------------------------------------------------
  IntMapBuilder
--------------------------------------------------------------------}

-- Note [IntMapBuilder]
-- ~~~~~~~~~~~~~~~~~~~~
-- IntMapBuilder serves as an accumulator for element-by-element construction
-- of an IntMap. It can be used in folds to construct IntMaps. This plays nicely
-- with list fusion when the structure folded over is a list, as in fromList and
-- friends.
--
-- An IntMapBuilder is either empty (BNil) or has the recently inserted Tip
-- together with a stack of trees (BTip). The structure is effectively a
-- [zipper](https://en.wikipedia.org/wiki/Zipper_(data_structure)). It always
-- has its "focus" at the last inserted entry. To insert a new entry, we need
-- to move the focus to the new entry. To do this we move up the stack to the
-- lowest common ancestor of the current position and the position of the
-- new key (implemented as moveUpB), then down to the position of the new key
-- (implemented as moveDownB).
--
-- When we are done inserting entries, we link the trees up the stack and get
-- the final result.
--
-- The advantage of this implementation is that we take the shortest path in
-- the tree from one key to the next. Unlike `insert`, we don't need to move
-- up to the root after every insertion. This is very beneficial when we have
-- runs of sorted keys, without many keys already in the tree in that range.
-- If the keys are fully sorted, inserting them all takes O(n) time instead
-- of O(n min(n,W)). But these benefits come at a small cost: when moving up
-- the tree we have to check at every point if it is time to move down. These
-- checks are absent in `insert`. So, in case we need to move up quite a lot,
-- repeated `insert` is slightly faster, but the trade-off is worthwhile since
-- such cases are pathological.

data IntMapBuilder a
  = BNil
  | BTip {-# UNPACK #-} !Int a !(BStack a)

-- BLeft: the IntMap is the left child
-- BRight: the IntMap is the right child
data BStack a
  = BNada
  | BLeft {-# UNPACK #-} !Prefix !(IntMap a) !(BStack a)
  | BRight {-# UNPACK #-} !Prefix !(IntMap a) !(BStack a)

-- Empty builder.
emptyB :: IntMapBuilder a
emptyB = BNil

-- Insert a key and value. Replaces the old value if one already exists for
-- the key.
insertB :: Key -> a -> IntMapBuilder a -> IntMapBuilder a
insertB !ky y b = case b of
  BNil -> BTip ky y BNada
  BTip kx x stk -> case moveToB ky kx x stk of
    MoveResult _ stk' -> BTip ky y stk'
{-# INLINE insertB #-}

-- Insert a key and value. The new value is combined with the old value if one
-- already exists for the key.
insertWithB :: (a -> a -> a) -> Key -> a -> IntMapBuilder a -> IntMapBuilder a
insertWithB f !ky y b = case b of
  BNil -> BTip ky y BNada
  BTip kx x stk -> case moveToB ky kx x stk of
    MoveResult m stk' -> case m of
      Nothing -> BTip ky y stk'
      Just x' -> BTip ky (f y x') stk'
{-# INLINE insertWithB #-}

-- GHC >=9.6 supports unpacking sums, so we unpack the Maybe and avoid
-- allocating Justs. GHC optimizes the workers for moveUpB and moveDownB to
-- return (# (# (# #) | a #), BStack a #).
data MoveResult a
  = MoveResult
#if __GLASGOW_HASKELL__ >= 906
      {-# UNPACK #-}
#endif
      !(Maybe a)
      !(BStack a)

moveToB :: Key -> Key -> a -> BStack a -> MoveResult a
moveToB !ky !kx x !stk
  | kx == ky = MoveResult (Just x) stk
  | otherwise = moveUpB ky kx (Tip kx x) stk
-- Don't inline this; there is no benefit according to benchmarks.
{-# NOINLINE moveToB #-}

moveUpB :: Key -> Key -> IntMap a -> BStack a -> MoveResult a
moveUpB !ky !kx !tx stk = case stk of
  BNada -> MoveResult Nothing (linkB ky kx tx BNada)
  BLeft p l stk'
    | nomatch ky p -> moveUpB ky kx (Bin p l tx) stk'
    | left ky p -> moveDownB ky l (BRight p tx stk')
    | otherwise -> MoveResult Nothing (linkB ky kx tx stk)
  BRight p r stk'
    | nomatch ky p -> moveUpB ky kx (Bin p tx r) stk'
    | left ky p -> MoveResult Nothing (linkB ky kx tx stk)
    | otherwise -> moveDownB ky r (BLeft p tx stk')

moveDownB :: Key -> IntMap a -> BStack a -> MoveResult a
moveDownB !ky tx !stk = case tx of
  Bin p l r
    | nomatch ky p -> MoveResult Nothing (linkB ky (unPrefix p) tx stk)
    | left ky p -> moveDownB ky l (BRight p r stk)
    | otherwise -> moveDownB ky r (BLeft p l stk)
  Tip kx x
    | kx == ky -> MoveResult (Just x) stk
    | otherwise -> MoveResult Nothing (linkB ky kx tx stk)
  Nil -> error "moveDownB Tip"

linkB :: Key -> Key -> IntMap a -> BStack a -> BStack a
linkB ky kx tx stk
  | i2w ky < i2w kx = BRight p tx stk
  | otherwise = BLeft p tx stk
  where
    p = branchPrefix ky kx
{-# INLINE linkB #-}

-- Finalize the builder into a Map.
finishB :: IntMapBuilder a -> IntMap a
finishB b = case b of
  BNil -> Nil
  BTip kx x stk -> finishUpB (Tip kx x) stk
{-# INLINABLE finishB #-}

finishUpB :: IntMap a -> BStack a -> IntMap a
finishUpB !t stk = case stk of
  BNada -> t
  BLeft p l stk' -> finishUpB (Bin p l t) stk'
  BRight p r stk' -> finishUpB (Bin p t r) stk'

{--------------------------------------------------------------------
  Eq
--------------------------------------------------------------------}
instance Eq a => Eq (IntMap a) where
  (==) = equal

equal :: Eq a => IntMap a -> IntMap a -> Bool
equal (Bin p1 l1 r1) (Bin p2 l2 r2)
  = (p1 == p2) && (equal l1 l2) && (equal r1 r2)
equal (Tip kx x) (Tip ky y)
  = (kx == ky) && (x==y)
equal Nil Nil = True
equal _   _   = False
{-# INLINABLE equal #-}

-- | @since 0.5.9
instance Eq1 IntMap where
  liftEq eq = go
    where
      go (Bin p1 l1 r1) (Bin p2 l2 r2) = p1 == p2 && go l1 l2 && go r1 r2
      go (Tip kx x) (Tip ky y) = kx == ky && eq x y
      go Nil Nil = True
      go _   _   = False
  {-# INLINE liftEq #-}

{--------------------------------------------------------------------
  Ord
--------------------------------------------------------------------}

instance Ord a => Ord (IntMap a) where
  compare m1 m2 = liftCmp compare m1 m2
  {-# INLINABLE compare #-}

-- | @since 0.5.9
instance Ord1 IntMap where
  liftCompare = liftCmp

liftCmp :: (a -> b -> Ordering) -> IntMap a -> IntMap b -> Ordering
liftCmp cmp m1 m2 = case (splitSign m1, splitSign m2) of
  ((l1, r1), (l2, r2)) -> case go l1 l2 of
    A_LT_B -> LT
    A_Prefix_B -> if null r1 then LT else GT
    A_EQ_B -> case go r1 r2 of
      A_LT_B -> LT
      A_Prefix_B -> LT
      A_EQ_B -> EQ
      B_Prefix_A -> GT
      A_GT_B -> GT
    B_Prefix_A -> if null r2 then GT else LT
    A_GT_B -> GT
  where
    go t1@(Bin p1 l1 r1) t2@(Bin p2 l2 r2) = case treeTreeBranch p1 p2 of
      ABL -> case go l1 t2 of
        A_Prefix_B -> A_GT_B
        A_EQ_B -> B_Prefix_A
        o -> o
      ABR -> A_LT_B
      BAL -> case go t1 l2 of
        A_EQ_B -> A_Prefix_B
        B_Prefix_A -> A_LT_B
        o -> o
      BAR -> A_GT_B
      EQL -> case go l1 l2 of
        A_Prefix_B -> A_GT_B
        A_EQ_B -> go r1 r2
        B_Prefix_A -> A_LT_B
        o -> o
      NOM -> if unPrefix p1 < unPrefix p2 then A_LT_B else A_GT_B
    go (Bin _ l1 _) (Tip k2 x2) = case lookupMinSure l1 of
      KeyValue k1 x1 -> case compare k1 k2 <> cmp x1 x2 of
        LT -> A_LT_B
        EQ -> B_Prefix_A
        GT -> A_GT_B
    go (Tip k1 x1) (Bin _ l2 _) = case lookupMinSure l2 of
      KeyValue k2 x2 -> case compare k1 k2 <> cmp x1 x2 of
        LT -> A_LT_B
        EQ -> A_Prefix_B
        GT -> A_GT_B
    go (Tip k1 x1) (Tip k2 x2) = case compare k1 k2 <> cmp x1 x2 of
      LT -> A_LT_B
      EQ -> A_EQ_B
      GT -> A_GT_B
    go Nil Nil = A_EQ_B
    go Nil _ = A_Prefix_B
    go _ Nil = B_Prefix_A
{-# INLINE liftCmp #-}

-- Split into negative and non-negative
splitSign :: IntMap a -> (IntMap a, IntMap a)
splitSign t@(Bin p l r)
  | signBranch p = (r, l)
  | unPrefix p < 0 = (t, Nil)
  | otherwise = (Nil, t)
splitSign t@(Tip k _)
  | k < 0 = (t, Nil)
  | otherwise = (Nil, t)
splitSign Nil = (Nil, Nil)
{-# INLINE splitSign #-}

{--------------------------------------------------------------------
  Functor
--------------------------------------------------------------------}

instance Functor IntMap where
    fmap = map

#ifdef __GLASGOW_HASKELL__
    a <$ Bin p l r = Bin p (a <$ l) (a <$ r)
    a <$ Tip k _   = Tip k a
    _ <$ Nil       = Nil
#endif

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}

instance Show a => Show (IntMap a) where
  showsPrec d m   = showParen (d > 10) $
    showString "fromList " . shows (toList m)

-- | @since 0.5.9
instance Show1 IntMap where
    liftShowsPrec sp sl d m =
        showsUnaryWith (liftShowsPrec sp' sl') "fromList" d (toList m)
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance (Read e) => Read (IntMap e) where
#if defined(__GLASGOW_HASKELL__) || defined(__MHS__)
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)

  readListPrec = readListPrecDefault
#else
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    return (fromList xs,t)
#endif

-- | @since 0.5.9
instance Read1 IntMap where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "fromList" fromList
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

{--------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------}
{--------------------------------------------------------------------
  Link
--------------------------------------------------------------------}

-- | Link two @IntMap@s. The maps must not be empty. The @Prefix@es of the two
-- maps must be different. @k1@ must share the prefix of @t1@. @p2@ must be the
-- prefix of @t2@.
linkKey :: Key -> IntMap a -> Prefix -> IntMap a -> IntMap a
linkKey k1 t1 p2 t2 = link k1 t1 (unPrefix p2) t2
{-# INLINE linkKey #-}

-- | Link two @IntMap@s. The maps must not be empty. The @Prefix@es of the two
-- maps must be different. @k1@ must share the prefix of @t1@ and @k2@ must
-- share the prefix of @t2@.
link :: Int -> IntMap a -> Int -> IntMap a -> IntMap a
link k1 t1 k2 t2
  | i2w k1 < i2w k2 = Bin p t1 t2
  | otherwise = Bin p t2 t1
  where
    p = branchPrefix k1 k2
{-# INLINE link #-}

{--------------------------------------------------------------------
  @bin@ assures that we never have empty trees within a tree.
--------------------------------------------------------------------}

bin :: Prefix -> IntMap a -> IntMap a -> IntMap a
bin _ l Nil = l
bin _ Nil r = r
bin p l r   = Bin p l r
{-# INLINE bin #-}

-- binCheckL only checks that the left subtree is non-empty
binCheckL :: Prefix -> IntMap a -> IntMap a -> IntMap a
binCheckL _ Nil r = r
binCheckL p l r = Bin p l r
{-# INLINE binCheckL #-}

-- binCheckR only checks that the right subtree is non-empty
binCheckR :: Prefix -> IntMap a -> IntMap a -> IntMap a
binCheckR _ l Nil = l
binCheckR p l r = Bin p l r
{-# INLINE binCheckR #-}

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

-- | \(O(1)\).  Decompose a map into pieces based on the structure
-- of the underlying tree. This function is useful for consuming a
-- map in parallel.
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
splitRoot :: IntMap a -> [IntMap a]
splitRoot orig =
  case orig of
    Nil -> []
    x@(Tip _ _) -> [x]
    Bin p l r
      | signBranch p -> [r, l]
      | otherwise -> [l, r]
{-# INLINE splitRoot #-}


{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}

-- | \(O(n \min(n,W))\). Show the tree that implements the map. The tree is shown
-- in a compressed, hanging format.
showTree :: Show a => IntMap a -> String
showTree s
  = showTreeWith True False s


{- | \(O(n \min(n,W))\). The expression (@'showTreeWith' hang wide map@) shows
 the tree that implements the map. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.
-}
showTreeWith :: Show a => Bool -> Bool -> IntMap a -> String
showTreeWith hang wide t
  | hang      = (showsTreeHang wide [] t) ""
  | otherwise = (showsTree wide [] [] t) ""

showsTree :: Show a => Bool -> [String] -> [String] -> IntMap a -> ShowS
showsTree wide lbars rbars t = case t of
  Bin p l r ->
    showsTree wide (withBar rbars) (withEmpty rbars) r .
    showWide wide rbars .
    showsBars lbars . showString (showBin p) . showString "\n" .
    showWide wide lbars .
    showsTree wide (withEmpty lbars) (withBar lbars) l
  Tip k x ->
    showsBars lbars .
    showString " " . shows k . showString ":=" . shows x . showString "\n"
  Nil -> showsBars lbars . showString "|\n"

showsTreeHang :: Show a => Bool -> [String] -> IntMap a -> ShowS
showsTreeHang wide bars t = case t of
  Bin p l r ->
    showsBars bars . showString (showBin p) . showString "\n" .
    showWide wide bars .
    showsTreeHang wide (withBar bars) l .
    showWide wide bars .
    showsTreeHang wide (withEmpty bars) r
  Tip k x ->
    showsBars bars .
    showString " " . shows k . showString ":=" . shows x . showString "\n"
  Nil -> showsBars bars . showString "|\n"

showBin :: Prefix -> String
showBin _
  = "*" -- ++ show (p,m)

showWide :: Bool -> [String] -> String -> String
showWide wide bars
  | wide      = showString (concat (reverse bars)) . showString "|\n"
  | otherwise = id

showsBars :: [String] -> ShowS
showsBars bars
  = case bars of
      [] -> id
      _ : tl -> showString (concat (reverse tl)) . showString node

node :: String
node = "+--"

withBar, withEmpty :: [String] -> [String]
withBar bars   = "|  ":bars
withEmpty bars = "   ":bars

{--------------------------------------------------------------------
  Notes
--------------------------------------------------------------------}

-- Note [Okasaki-Gill]
-- ~~~~~~~~~~~~~~~~~~~
--
-- The IntMap structure is based on the map described in the paper "Fast
-- Mergeable Integer Maps" by Chris Okasaki and Andy Gill, with some
-- differences.
--
-- The paper spends most of its time describing a little-endian tree, where the
-- branching is done first on low bits then high bits. It then briefly describes
-- a big-endian tree. The implementation here is big-endian.
--
-- The definition of Okasaki and Gill's map would be written in Haskell as
--
-- data Dict a
--   = Empty
--   | Lf !Int a
--   | Br !Int !Int !(Dict a) !(Dict a)
--
-- Empty is the same as IntMap's Nil, and Lf is the same as Tip.
--
-- In Br, the first Int is the shared prefix and the second is the mask bit by
-- itself. For the big-endian map, the paper suggests that the prefix be the
-- common prefix, followed by a 0-bit, followed by all 1-bits. This is so that
-- the prefix value can be used as a point of split for binary search.
--
-- IntMap's Bin corresponds to Br, but is different because it has only one
-- Int (newtyped as Prefix). This describes both prefix and mask, so it is not
-- necessary to store them separately. This value is, in fact, one plus the
-- value suggested for the prefix in the paper. This representation is chosen
-- because it saves one word per Bin without detriment to the efficiency of
-- operations.
--
-- The implementation of operations such as lookup, insert, union, follow
-- the described implementations on Dict and split into the same cases. For
-- instance, for insert, the three cases on a Br are whether the key belongs
-- outside the map, or it belongs in the left child, or it belongs in the
-- right child. We have the same three cases for a Bin. However, the bitwise
-- operations we use to determine the case is naturally different due to the
-- difference in representation.

-- Note [IntMap merge complexity]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The merge algorithm (used for union, intersection, etc.) is adopted from
-- Okasaki-Gill who give the complexity as O(m+n), where m and n are the sizes
-- of the two input maps. This is correct, since we visit all constructors in
-- both maps in the worst case, but we can try to find a tighter bound.
--
-- Consider that m<=n, i.e. m is the size of the smaller map and n is the size
-- of the larger. It does not matter which map is the first argument.
--
-- Now we have O(n) as one upper bound for our complexity, since O(n) is the
-- same as O(m+n) for m<=n.
--
-- Next, consider the smaller map. For this map, we will visit some
-- constructors, plus all the Bins of the larger map that lie in our way.
-- For the former, the worst case is that we visit all constructors, which is
-- O(m).
-- For the latter, the worst case is that we encounter Bins at every point
-- possible. This happens when for every key in the smaller map, the path to
-- that key's Tip in the larger map has a full length of W, with a Bin at every
-- bit position. To maximize the total number of Bins, the paths should be as
-- disjoint as possible. But even if the paths are spread out, at least O(m)
-- Bins are unavoidably shared, which extend up to a depth of lg(m) from the
-- root. Beyond this, the paths may be disjoint. This gives us a total of
-- O(m + m (W - lg m)) = O(m log (2^W / m)).
-- The number of Bins we encounter is also bounded by the total number of Bins,
-- which is n-1, but we already have O(n) as an upper bound.
--
-- Combining our bounds, we have the final complexity as
-- O(min(n, m log (2^W / m))).
--
-- Note that
-- * This is similar to the Map merge complexity, which is O(m log (n/m)).
-- * When m is a small constant the term simplifies to O(min(n, W)), which is
--   just the complexity we expect for single operations like insert and delete.

-- Note [fromAscList implementation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- fromAscList is an implementation that builds up the result bottom-up
-- in linear time. It maintains a state (MonoState) that gets updated with
-- key-value pairs from the input list one at a time. The state contains the
-- last key-value pair, and a stack of pending trees.
--
-- For a new key-value pair, the branchMask with the previous key is computed.
-- This represents the depth of the lowest common ancestor that the tree with
-- the previous key, say tl, and the tree with the new key, tr, must have in
-- the final result. Since the keys are in ascending order we expect no more
-- keys in tl, and we can build it by moving up the stack and linking trees. We
-- know when to stop by the branchMask value. We must not link higher than that
-- depth, otherwise instead of tl we will build the parent of tl prematurely
-- before tr is ready. Once the linking is done, tl will be at the top of the
-- stack.
--
-- We also store the branchMask of a tree with its future right sibling in the
-- stack. This is an optimization, benchmarks show that this is faster than
-- recomputing the branchMask values when linking trees.
--
-- In the end, we link all the trees remaining in the stack. There is a small
-- catch: negative keys appear in the input before non-negative keys (if they
-- both appear), but the tree with negative keys and the tree with non-negative
-- keys must be the right and left child of the root respectively. So we check
-- for this and link them accordingly.
--
-- The implementation is defined as a foldl' over the input list, which makes
-- it a good consumer in list fusion.

-- Note [IntMap folds]
-- ~~~~~~~~~~~~~~~~~~~
-- Folds on IntMap are defined in a particular way for a few reasons.
--
-- foldl' :: (a -> b -> a) -> a -> IntMap b -> a
-- foldl' f z = \t ->
--   case t of
--     Nil -> z
--     Bin p l r
--       | signBranch p -> go (go z r) l
--       | otherwise -> go (go z l) r
--     _ -> go z t
--   where
--     go !_ Nil         = error "foldl'.go: Nil"
--     go z' (Tip _ x)   = f z' x
--     go z' (Bin _ l r) = go (go z' l) r
-- {-# INLINE foldl' #-}
--
-- 1. We first check if the Bin separates negative and positive keys, and fold
--    over the children accordingly. This check is not inside `go` because it
--    can only happen at the top level and we don't need to check every Bin.
-- 2. We also check for Nil at the top level instead of, say, `go z Nil = z`.
--    That's because `Nil` is also allowed only at the top-level, but more
--    importantly it allows for better optimizations if the `Nil` branch errors
--    in `go`. For example, if we have
--      maximum :: Ord a => IntMap a -> Maybe a
--      maximum = foldl' (\m x -> Just $! maybe x (max x) m) Nothing
--    because `go` certainly returns a `Just` (or errors), CPR analysis will
--    optimize it to return `(# a #)` instead of `Maybe a`. This makes it
--    satisfy the conditions for SpecConstr, which generates two specializations
--    of `go` for `Nothing` and `Just` inputs. Now both `Maybe`s have been
--    optimized out of `go`.
-- 3. The `Tip` is not matched on at the top-level to avoid using `f` more than
--    once. This allows `f` to be inlined into `go` even if `f` is big, since
--    it's likely to be the only place `f` is used, and not inlining `f` means
--    missing out on optimizations. See GHC #25259 for more on this.
