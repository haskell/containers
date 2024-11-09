{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
#endif
#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Trustworthy #-}
#endif

{-# OPTIONS_HADDOCK not-home #-}

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntSet.Internal
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Joachim Breitner 2011
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
-- An efficient implementation of integer sets.
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import Data.IntSet (IntSet)
-- >  import qualified Data.IntSet as IntSet
--
-- The implementation is based on /big-endian patricia trees/.  This data
-- structure performs especially well on binary operations like 'union'
-- and 'intersection'.  However, my benchmarks show that it is also
-- (much) faster on insertions and deletions when compared to a generic
-- size-balanced set implementation (see "Data.Set").
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--      Workshop on ML, September 1998, pages 77-86,
--      <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.5452>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve Information Coded In Alphanumeric/\",
--      Journal of the ACM, 15(4), October 1968, pages 514-534.
--
-- Additionally, this implementation places bitmaps in the leaves of the tree.
-- Their size is the natural size of a machine word (32 or 64 bits) and greatly
-- reduce memory footprint and execution times for dense sets, e.g. sets where
-- it is likely that many values lie close to each other. The asymptotics are
-- not affected by this optimization.
--
-- Many operations have a worst-case complexity of \(O(\min(n,W))\).
-- This means that the operation can become linear in the number of
-- elements with a maximum of \(W\) -- the number of bits in an 'Int'
-- (32 or 64).
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
-- The order of constructors of IntSet matters when considering performance.
-- Currently in GHC 7.0, when type has 3 constructors, they are matched from
-- the first to the last -- the best performance is achieved when the
-- constructors are ordered by frequency.
-- On GHC 7.0, reordering constructors from Nil | Tip | Bin to Bin | Tip | Nil
-- improves the benchmark by circa 10%.

module Data.IntSet.Internal (
    -- * Set type
      IntSet(..) -- instance Eq,Show
    , BitMap

    -- * Operators
    , (\\)

    -- * Query
    , null
    , size
    , member
    , notMember
    , lookupLT
    , lookupGT
    , lookupLE
    , lookupGE
    , isSubsetOf
    , isProperSubsetOf
    , disjoint

    -- * Construction
    , empty
    , singleton
    , fromRange
    , insert
    , delete
    , alterF

    -- * Combine
    , union
    , unions
    , difference
    , intersection
    , intersections
    , symmetricDifference
    , Intersection(..)

    -- * Filter
    , filter
    , partition

    , takeWhileAntitone
    , dropWhileAntitone
    , spanAntitone

    , split
    , splitMember
    , splitRoot

    -- * Map
    , map
    , mapMonotonic

    -- * Folds
    , foldr
    , foldl
    , foldMap
    -- ** Strict folds
    , foldr'
    , foldl'
    -- ** Legacy folds
    , fold

    -- * Min\/Max
    , lookupMin
    , lookupMax
    , findMin
    , findMax
    , deleteMin
    , deleteMax
    , deleteFindMin
    , deleteFindMax
    , maxView
    , minView

    -- * Conversion

    -- ** List
    , elems
    , toList
    , fromList

    -- ** Ordered list
    , toAscList
    , toDescList
    , fromAscList
    , fromDistinctAscList

    -- * Debugging
    , showTree
    , showTreeWith

    -- * Internals
    , suffixBitMask
    , prefixBitMask
    , bitmapOf
    ) where

import Control.Applicative (Const(..))
import Control.DeepSeq (NFData(rnf))
import Data.Bits
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup
  (Semigroup(stimes), stimesIdempotent, stimesIdempotentMonoid)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup((<>)))
#endif
import Utils.Containers.Internal.Prelude hiding
  (filter, foldr, foldl, foldl', foldMap, null, map)
import Prelude ()

import Utils.Containers.Internal.BitUtil (iShiftRL, shiftLL, shiftRL)
import Utils.Containers.Internal.StrictPair
import Data.IntSet.Internal.IntTreeCommons
  ( Key
  , Prefix(..)
  , nomatch
  , left
  , signBranch
  , mask
  , branchMask
  , TreeTreeBranch(..)
  , treeTreeBranch
  , i2w
  )

#if __GLASGOW_HASKELL__
import Data.Data (Data(..), Constr, mkConstr, constrIndex, DataType, mkDataType)
import qualified Data.Data
import Text.Read
#endif

#if __GLASGOW_HASKELL__
import qualified GHC.Exts
#  if !(WORD_SIZE_IN_BITS==64)
import qualified GHC.Int
#  endif
import Language.Haskell.TH.Syntax (Lift)
-- See Note [ Template Haskell Dependencies ]
import Language.Haskell.TH ()
#endif

import qualified Data.Foldable as Foldable
import Data.Functor.Identity (Identity(..))

infixl 9 \\{-This comment teaches CPP correct behaviour -}

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
-- | \(O(n+m)\). See 'difference'.
(\\) :: IntSet -> IntSet -> IntSet
m1 \\ m2 = difference m1 m2

{--------------------------------------------------------------------
  Types
--------------------------------------------------------------------}

-- | A set of integers.

-- See Note: Order of constructors
data IntSet = Bin {-# UNPACK #-} !Prefix
                  !IntSet
                  !IntSet
            | Tip {-# UNPACK #-} !Int
                  {-# UNPACK #-} !BitMap
            | Nil

type BitMap = Word

--
-- Note [IntSet structure and invariants]
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
-- * The shared prefix of a Bin is never longer than
--   (WORD_SIZE - lg(WORD_SIZE) - 1) bits.
--
-- * In the context of a Tip, the highest (WORD_SIZE - lg(WORD_SIZE)) bits of
--   a key are called "prefix" and the lowest lg(WORD_SIZE) bits are called
--   "suffix". In Tip kx bm, kx is the shared prefix and bm is a bitmask of the
--   suffixes of the keys. In other words, the keys of Tip kx bm are (kx .|. i)
--   for every set bit i in bm.
--
-- * In Tip kx _, the lowest lg(WORD_SIZE) bits of kx are set to 0.
--
-- * In Tip _ bm, bm is never 0.
--

#ifdef __GLASGOW_HASKELL__
-- | @since 0.6.6
deriving instance Lift IntSet
#endif

-- | @mempty@ = 'empty'
instance Monoid IntSet where
    mempty  = empty
    mconcat = unions
    mappend = (<>)

-- | @(<>)@ = 'union'
--
-- @since 0.5.7
instance Semigroup IntSet where
    (<>)    = union
    stimes  = stimesIdempotentMonoid

#if __GLASGOW_HASKELL__

{--------------------------------------------------------------------
  A Data instance
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We provide limited reflection services for the sake of data abstraction.

instance Data IntSet where
  gfoldl f z is = z fromList `f` (toList is)
  toConstr _     = fromListConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  dataTypeOf _   = intSetDataType

fromListConstr :: Constr
fromListConstr = mkConstr intSetDataType "fromList" [] Data.Data.Prefix

intSetDataType :: DataType
intSetDataType = mkDataType "Data.IntSet.Internal.IntSet" [fromListConstr]

#endif

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | \(O(1)\). Is the set empty?
null :: IntSet -> Bool
null Nil = True
null _   = False
{-# INLINE null #-}

-- | \(O(n)\). Cardinality of the set.
size :: IntSet -> Int
size = go 0
  where
    go !acc (Bin _ l r) = go (go acc l) r
    go acc (Tip _ bm) = acc + popCount bm
    go acc Nil = acc

-- | \(O(\min(n,W))\). Is the value a member of the set?

-- See Note: Local 'go' functions and capturing.
member :: Key -> IntSet -> Bool
member !x = go
  where
    go (Bin p l r)
      | nomatch x p = False
      | left x p    = go l
      | otherwise   = go r
    go (Tip y bm) = prefixOf x == y && bitmapOf x .&. bm /= 0
    go Nil = False

-- | \(O(\min(n,W))\). Is the element not in the set?
notMember :: Key -> IntSet -> Bool
notMember k = not . member k

-- | \(O(\min(n,W))\). Find largest element smaller than the given one.
--
-- > lookupLT 3 (fromList [3, 5]) == Nothing
-- > lookupLT 5 (fromList [3, 5]) == Just 3

-- See Note: Local 'go' functions and capturing.
lookupLT :: Key -> IntSet -> Maybe Key
lookupLT !x t = case t of
    Bin p l r | signBranch p -> if x >= 0 then go r l else go Nil r
    _ -> go Nil t
  where
    go def (Bin p l r) | nomatch x p = if x < unPrefix p then unsafeFindMax def else unsafeFindMax r
                       | left x p  = go def l
                       | otherwise = go l r
    go def (Tip kx bm) | prefixOf x > kx = Just $ kx + highestBitSet bm
                       | prefixOf x == kx && maskLT /= 0 = Just $ kx + highestBitSet maskLT
                       | otherwise = unsafeFindMax def
                       where maskLT = (bitmapOf x - 1) .&. bm
    go def Nil = unsafeFindMax def


-- | \(O(\min(n,W))\). Find smallest element greater than the given one.
--
-- > lookupGT 4 (fromList [3, 5]) == Just 5
-- > lookupGT 5 (fromList [3, 5]) == Nothing

-- See Note: Local 'go' functions and capturing.
lookupGT :: Key -> IntSet -> Maybe Key
lookupGT !x t = case t of
    Bin p l r | signBranch p -> if x >= 0 then go Nil l else go l r
    _ -> go Nil t
  where
    go def (Bin p l r) | nomatch x p = if x < unPrefix p then unsafeFindMin l else unsafeFindMin def
                       | left x p  = go r l
                       | otherwise = go def r
    go def (Tip kx bm) | prefixOf x < kx = Just $ kx + lowestBitSet bm
                       | prefixOf x == kx && maskGT /= 0 = Just $ kx + lowestBitSet maskGT
                       | otherwise = unsafeFindMin def
                       where maskGT = (- ((bitmapOf x) `shiftLL` 1)) .&. bm
    go def Nil = unsafeFindMin def


-- | \(O(\min(n,W))\). Find largest element smaller or equal to the given one.
--
-- > lookupLE 2 (fromList [3, 5]) == Nothing
-- > lookupLE 4 (fromList [3, 5]) == Just 3
-- > lookupLE 5 (fromList [3, 5]) == Just 5

-- See Note: Local 'go' functions and capturing.
lookupLE :: Key -> IntSet -> Maybe Key
lookupLE !x t = case t of
    Bin p l r | signBranch p -> if x >= 0 then go r l else go Nil r
    _ -> go Nil t
  where
    go def (Bin p l r) | nomatch x p = if x < unPrefix p then unsafeFindMax def else unsafeFindMax r
                       | left x p  = go def l
                       | otherwise = go l r
    go def (Tip kx bm) | prefixOf x > kx = Just $ kx + highestBitSet bm
                       | prefixOf x == kx && maskLE /= 0 = Just $ kx + highestBitSet maskLE
                       | otherwise = unsafeFindMax def
                       where maskLE = (((bitmapOf x) `shiftLL` 1) - 1) .&. bm
    go def Nil = unsafeFindMax def


-- | \(O(\min(n,W))\). Find smallest element greater or equal to the given one.
--
-- > lookupGE 3 (fromList [3, 5]) == Just 3
-- > lookupGE 4 (fromList [3, 5]) == Just 5
-- > lookupGE 6 (fromList [3, 5]) == Nothing

-- See Note: Local 'go' functions and capturing.
lookupGE :: Key -> IntSet -> Maybe Key
lookupGE !x t = case t of
    Bin p l r | signBranch p -> if x >= 0 then go Nil l else go l r
    _ -> go Nil t
  where
    go def (Bin p l r) | nomatch x p = if x < unPrefix p then unsafeFindMin l else unsafeFindMin def
                       | left x p  = go r l
                       | otherwise = go def r
    go def (Tip kx bm) | prefixOf x < kx = Just $ kx + lowestBitSet bm
                       | prefixOf x == kx && maskGE /= 0 = Just $ kx + lowestBitSet maskGE
                       | otherwise = unsafeFindMin def
                       where maskGE = (- (bitmapOf x)) .&. bm
    go def Nil = unsafeFindMin def



-- Helper function for lookupGE and lookupGT. It assumes that if a Bin node is
-- given, it has m > 0.
unsafeFindMin :: IntSet -> Maybe Key
unsafeFindMin Nil = Nothing
unsafeFindMin (Tip kx bm) = Just $ kx + lowestBitSet bm
unsafeFindMin (Bin _ l _) = unsafeFindMin l

-- Helper function for lookupLE and lookupLT. It assumes that if a Bin node is
-- given, it has m > 0.
unsafeFindMax :: IntSet -> Maybe Key
unsafeFindMax Nil = Nothing
unsafeFindMax (Tip kx bm) = Just $ kx + highestBitSet bm
unsafeFindMax (Bin _ _ r) = unsafeFindMax r

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | \(O(1)\). The empty set.
empty :: IntSet
empty
  = Nil
{-# INLINE empty #-}

-- | \(O(1)\). A set of one element.
singleton :: Key -> IntSet
singleton x
  = Tip (prefixOf x) (bitmapOf x)
{-# INLINE singleton #-}

{--------------------------------------------------------------------
  Insert
--------------------------------------------------------------------}
-- | \(O(\min(n,W))\). Add a value to the set. There is no left- or right bias for
-- IntSets.
insert :: Key -> IntSet -> IntSet
insert !x = insertBM (prefixOf x) (bitmapOf x)

-- Helper function for insert and union.
insertBM :: Int -> BitMap -> IntSet -> IntSet
insertBM !kx !bm t@(Bin p l r)
  | nomatch kx p = linkKey kx (Tip kx bm) p t
  | left kx p    = Bin p (insertBM kx bm l) r
  | otherwise    = Bin p l (insertBM kx bm r)
insertBM kx bm t@(Tip kx' bm')
  | kx' == kx = Tip kx' (bm .|. bm')
  | otherwise = link kx (Tip kx bm) kx' t
insertBM kx bm Nil = Tip kx bm

-- | \(O(\min(n,W))\). Delete a value in the set. Returns the
-- original set when the value was not present.
delete :: Key -> IntSet -> IntSet
delete !x = deleteBM (prefixOf x) (bitmapOf x)

-- Deletes all values mentioned in the BitMap from the set.
-- Helper function for delete and difference.
deleteBM :: Int -> BitMap -> IntSet -> IntSet
deleteBM !kx !bm t@(Bin p l r)
  | nomatch kx p = t
  | left kx p    = bin p (deleteBM kx bm l) r
  | otherwise    = bin p l (deleteBM kx bm r)
deleteBM kx bm t@(Tip kx' bm')
  | kx' == kx = tip kx (bm' .&. complement bm)
  | otherwise = t
deleteBM _ _ Nil = Nil

-- | \(O(\min(n,W))\). @('alterF' f x s)@ can delete or insert @x@ in @s@ depending
-- on whether it is already present in @s@.
--
-- In short:
--
-- @
-- 'member' x \<$\> 'alterF' f x s = f ('member' x s)
-- @
--
-- Note: 'alterF' is a variant of the @at@ combinator from "Control.Lens.At".
--
-- @since 0.6.3.1
alterF :: Functor f => (Bool -> f Bool) -> Key -> IntSet -> f IntSet
alterF f k s = fmap choose (f member_)
  where
    member_ = member k s

    (inserted, deleted)
      | member_   = (s         , delete k s)
      | otherwise = (insert k s, s         )

    choose True  = inserted
    choose False = deleted
#ifndef __GLASGOW_HASKELL__
{-# INLINE alterF #-}
#else
{-# INLINABLE [2] alterF #-}

{-# RULES
"alterF/Const" forall k (f :: Bool -> Const a Bool) . alterF f k = \s -> Const . getConst . f $ member k s
 #-}
#endif

{-# SPECIALIZE alterF :: (Bool -> Identity Bool) -> Key -> IntSet -> Identity IntSet #-}

{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
-- | The union of a list of sets.
unions :: Foldable f => f IntSet -> IntSet
unions xs
  = Foldable.foldl' union empty xs


-- | \(O(n+m)\). The union of two sets.
union :: IntSet -> IntSet -> IntSet
union t1@(Bin p1 l1 r1) t2@(Bin p2 l2 r2) = case treeTreeBranch p1 p2 of
  ABL -> Bin p1 (union l1 t2) r1
  ABR -> Bin p1 l1 (union r1 t2)
  BAL -> Bin p2 (union t1 l2) r2
  BAR -> Bin p2 l2 (union t1 r2)
  EQL -> Bin p1 (union l1 l2) (union r1 r2)
  NOM -> link (unPrefix p1) t1 (unPrefix p2) t2
union t@(Bin _ _ _) (Tip kx bm) = insertBM kx bm t
union t@(Bin _ _ _) Nil = t
union (Tip kx bm) t = insertBM kx bm t
union Nil t = t


{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | \(O(n+m)\). Difference between two sets.
difference :: IntSet -> IntSet -> IntSet
difference t1@(Bin p1 l1 r1) t2@(Bin p2 l2 r2) = case treeTreeBranch p1 p2 of
  ABL -> bin p1 (difference l1 t2) r1
  ABR -> bin p1 l1 (difference r1 t2)
  BAL -> difference t1 l2
  BAR -> difference t1 r2
  EQL -> bin p1 (difference l1 l2) (difference r1 r2)
  NOM -> t1

difference t@(Bin _ _ _) (Tip kx bm) = deleteBM kx bm t
difference t@(Bin _ _ _) Nil = t

difference t1@(Tip kx bm) t2 = differenceTip t2
  where differenceTip (Bin p2 l2 r2) | nomatch kx p2 = t1
                                     | left kx p2 = differenceTip l2
                                     | otherwise = differenceTip r2
        differenceTip (Tip kx2 bm2) | kx == kx2 = tip kx (bm .&. complement bm2)
                                    | otherwise = t1
        differenceTip Nil = t1

difference Nil _     = Nil



{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | \(O(n+m)\). The intersection of two sets.
intersection :: IntSet -> IntSet -> IntSet
intersection t1@(Bin p1 l1 r1) t2@(Bin p2 l2 r2) = case treeTreeBranch p1 p2 of
  ABL -> intersection l1 t2
  ABR -> intersection r1 t2
  BAL -> intersection t1 l2
  BAR -> intersection t1 r2
  EQL -> bin p1 (intersection l1 l2) (intersection r1 r2)
  NOM -> Nil

intersection t1@(Bin _ _ _) (Tip kx2 bm2) = intersectBM t1
  where intersectBM (Bin p1 l1 r1) | nomatch kx2 p1 = Nil
                                   | left kx2 p1    = intersectBM l1
                                   | otherwise      = intersectBM r1
        intersectBM (Tip kx1 bm1) | kx1 == kx2 = tip kx1 (bm1 .&. bm2)
                                  | otherwise = Nil
        intersectBM Nil = Nil

intersection (Bin _ _ _) Nil = Nil

intersection (Tip kx1 bm1) t2 = intersectBM t2
  where intersectBM (Bin p2 l2 r2) | nomatch kx1 p2 = Nil
                                   | left kx1 p2    = intersectBM l2
                                   | otherwise      = intersectBM r2
        intersectBM (Tip kx2 bm2) | kx1 == kx2 = tip kx1 (bm1 .&. bm2)
                                  | otherwise = Nil
        intersectBM Nil = Nil

intersection Nil _ = Nil

-- | The intersection of a series of sets. Intersections are performed
-- left-to-right.
--
-- @since FIXME
intersections :: NonEmpty IntSet -> IntSet
intersections (s0 :| ss)
  | null s0 = empty
  | otherwise = List.foldr go id ss s0
  where
    go s r acc
      | null acc' = empty
      | otherwise = r acc'
      where
        acc' = intersection acc s
{-# INLINABLE intersections #-}

-- | @IntSet@s form a 'Semigroup' under 'intersection'.
--
-- A @Monoid@ instance is not defined because it would be impractical to
-- construct @mempty@, the @IntSet@ containing all @Int@s.
--
-- @since FIXME
newtype Intersection = Intersection { getIntersection :: IntSet }
  deriving (Show, Eq, Ord)

instance Semigroup Intersection where
  Intersection s1 <> Intersection s2 = Intersection (intersection s1 s2)

  stimes = stimesIdempotent
  {-# INLINABLE stimes #-}

{--------------------------------------------------------------------
  Symmetric difference
--------------------------------------------------------------------}

-- | \(O(n+m)\). The symmetric difference of two sets.
--
-- The result contains elements that appear in exactly one of the two sets.
--
-- @
-- symmetricDifference (fromList [0,2,4,6]) (fromList [0,3,6,9]) == fromList [2,3,4,9]
-- @
--
-- @since FIXME
symmetricDifference :: IntSet -> IntSet -> IntSet
symmetricDifference t1@(Bin p1 l1 r1) t2@(Bin p2 l2 r2) =
  case treeTreeBranch p1 p2 of
    ABL -> bin p1 (symmetricDifference l1 t2) r1
    ABR -> bin p1 l1 (symmetricDifference r1 t2)
    BAL -> bin p2 (symmetricDifference t1 l2) r2
    BAR -> bin p2 l2 (symmetricDifference t1 r2)
    EQL -> bin p1 (symmetricDifference l1 l2) (symmetricDifference r1 r2)
    NOM -> link (unPrefix p1) t1 (unPrefix p2) t2
symmetricDifference t1@(Bin _ _ _) t2@(Tip kx2 bm2) = symDiffTip t2 kx2 bm2 t1
symmetricDifference t1@(Bin _ _ _) Nil = t1
symmetricDifference t1@(Tip kx1 bm1) t2 = symDiffTip t1 kx1 bm1 t2
symmetricDifference Nil t2 = t2

symDiffTip :: IntSet -> Int -> BitMap -> IntSet -> IntSet
symDiffTip !t1 !kx1 !bm1 = go
  where
    go t2@(Bin p2 l2 r2)
      | nomatch kx1 p2 = linkKey kx1 t1 p2 t2
      | left kx1 p2 = bin p2 (go l2) r2
      | otherwise = bin p2 l2 (go r2)
    go t2@(Tip kx2 bm2)
      | kx1 == kx2 = tip kx1 (bm1 `xor` bm2)
      | otherwise = link kx1 t1 kx2 t2
    go Nil = t1

{--------------------------------------------------------------------
  Subset
--------------------------------------------------------------------}
-- | \(O(n+m)\). Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: IntSet -> IntSet -> Bool
isProperSubsetOf t1 t2
  = case subsetCmp t1 t2 of
      LT -> True
      _  -> False

subsetCmp :: IntSet -> IntSet -> Ordering
subsetCmp t1@(Bin p1 l1 r1) (Bin p2 l2 r2) = case treeTreeBranch p1 p2 of
  ABL -> GT
  ABR -> GT
  BAL -> case subsetCmp t1 l2 of GT -> GT ; _ -> LT
  BAR -> case subsetCmp t1 r2 of GT -> GT ; _ -> LT
  EQL -> subsetCmpEq
  NOM -> GT  -- disjoint
  where
    subsetCmpEq = case (subsetCmp l1 l2, subsetCmp r1 r2) of
                    (GT,_ ) -> GT
                    (_ ,GT) -> GT
                    (EQ,EQ) -> EQ
                    _       -> LT

subsetCmp (Bin _ _ _) _ = GT
subsetCmp (Tip kx1 bm1) (Tip kx2 bm2)
  | kx1 /= kx2                  = GT -- disjoint
  | bm1 == bm2                  = EQ
  | bm1 .&. complement bm2 == 0 = LT
  | otherwise                   = GT
subsetCmp t1@(Tip kx _) (Bin p l r)
  | nomatch kx p = GT
  | left kx p    = case subsetCmp t1 l of GT -> GT ; _ -> LT
  | otherwise    = case subsetCmp t1 r of GT -> GT ; _ -> LT
subsetCmp (Tip _ _) Nil = GT -- disjoint
subsetCmp Nil Nil = EQ
subsetCmp Nil _   = LT

-- | \(O(n+m)\). Is this a subset?
-- @(s1 \`isSubsetOf\` s2)@ tells whether @s1@ is a subset of @s2@.

isSubsetOf :: IntSet -> IntSet -> Bool
isSubsetOf t1@(Bin p1 l1 r1) (Bin p2 l2 r2) = case treeTreeBranch p1 p2 of
  ABL -> False
  ABR -> False
  BAL -> isSubsetOf t1 l2
  BAR -> isSubsetOf t1 r2
  EQL -> isSubsetOf l1 l2 && isSubsetOf r1 r2
  NOM -> False
isSubsetOf (Bin _ _ _) _ = False
isSubsetOf (Tip kx1 bm1) (Tip kx2 bm2) = kx1 == kx2 && bm1 .&. complement bm2 == 0
isSubsetOf t1@(Tip kx _) (Bin p l r)
  | nomatch kx p = False
  | left kx p    = isSubsetOf t1 l
  | otherwise    = isSubsetOf t1 r
isSubsetOf (Tip _ _) Nil = False
isSubsetOf Nil _         = True


{--------------------------------------------------------------------
  Disjoint
--------------------------------------------------------------------}
-- | \(O(n+m)\). Check whether two sets are disjoint (i.e. their intersection
--   is empty).
--
-- > disjoint (fromList [2,4,6])   (fromList [1,3])     == True
-- > disjoint (fromList [2,4,6,8]) (fromList [2,3,5,7]) == False
-- > disjoint (fromList [1,2])     (fromList [1,2,3,4]) == False
-- > disjoint (fromList [])        (fromList [])        == True
--
-- @since 0.5.11
disjoint :: IntSet -> IntSet -> Bool
disjoint t1@(Bin p1 l1 r1) t2@(Bin p2 l2 r2) = case treeTreeBranch p1 p2 of
  ABL -> disjoint l1 t2
  ABR -> disjoint r1 t2
  BAL -> disjoint t1 l2
  BAR -> disjoint t1 r2
  EQL -> disjoint l1 l2 && disjoint r1 r2
  NOM -> True

disjoint t1@(Bin _ _ _) (Tip kx2 bm2) = disjointBM t1
  where disjointBM (Bin p1 l1 r1) | nomatch kx2 p1 = True
                                  | left kx2 p1    = disjointBM l1
                                  | otherwise      = disjointBM r1
        disjointBM (Tip kx1 bm1) | kx1 == kx2 = (bm1 .&. bm2) == 0
                                 | otherwise = True
        disjointBM Nil = True

disjoint (Bin _ _ _) Nil = True

disjoint (Tip kx1 bm1) t2 = disjointBM t2
  where disjointBM (Bin p2 l2 r2) | nomatch kx1 p2 = True
                                  | left kx1 p2    = disjointBM l2
                                  | otherwise      = disjointBM r2
        disjointBM (Tip kx2 bm2) | kx1 == kx2 = (bm1 .&. bm2) == 0
                                 | otherwise = True
        disjointBM Nil = True

disjoint Nil _ = True


{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
-- | \(O(n)\). Filter all elements that satisfy some predicate.
filter :: (Key -> Bool) -> IntSet -> IntSet
filter predicate t
  = case t of
      Bin p l r
        -> bin p (filter predicate l) (filter predicate r)
      Tip kx bm
        -> tip kx (foldl'Bits 0 (bitPred kx) 0 bm)
      Nil -> Nil
  where bitPred kx bm bi | predicate (kx + bi) = bm .|. bitmapOfSuffix bi
                         | otherwise           = bm
        {-# INLINE bitPred #-}

-- | \(O(n)\). partition the set according to some predicate.
partition :: (Key -> Bool) -> IntSet -> (IntSet,IntSet)
partition predicate0 t0 = toPair $ go predicate0 t0
  where
    go predicate t
      = case t of
          Bin p l r
            -> let (l1 :*: l2) = go predicate l
                   (r1 :*: r2) = go predicate r
               in bin p l1 r1 :*: bin p l2 r2
          Tip kx bm
            -> let bm1 = foldl'Bits 0 (bitPred kx) 0 bm
               in  tip kx bm1 :*: tip kx (bm `xor` bm1)
          Nil -> (Nil :*: Nil)
      where bitPred kx bm bi | predicate (kx + bi) = bm .|. bitmapOfSuffix bi
                             | otherwise           = bm
            {-# INLINE bitPred #-}

-- | \(O(\min(n,W))\). Take while a predicate on the elements holds.
-- The user is responsible for ensuring that for all @Int@s, @j \< k ==\> p j \>= p k@.
-- See note at 'spanAntitone'.
--
-- @
-- takeWhileAntitone p = 'fromDistinctAscList' . 'Data.List.takeWhile' p . 'toList'
-- takeWhileAntitone p = 'filter' p
-- @
--
-- @since 0.6.7
takeWhileAntitone :: (Key -> Bool) -> IntSet -> IntSet
takeWhileAntitone predicate t =
  case t of
    Bin p l r
      | signBranch p ->
        if predicate 0 -- handle negative numbers.
        then bin p (go predicate l) r
        else go predicate r
    _ -> go predicate t
  where
    go predicate' (Bin p l r)
      | predicate' (unPrefix p) = bin p l (go predicate' r)
      | otherwise               = go predicate' l
    go predicate' (Tip kx bm) = tip kx (takeWhileAntitoneBits kx predicate' bm)
    go _ Nil = Nil

-- | \(O(\min(n,W))\). Drop while a predicate on the elements holds.
-- The user is responsible for ensuring that for all @Int@s, @j \< k ==\> p j \>= p k@.
-- See note at 'spanAntitone'.
--
-- @
-- dropWhileAntitone p = 'fromDistinctAscList' . 'Data.List.dropWhile' p . 'toList'
-- dropWhileAntitone p = 'filter' (not . p)
-- @
--
-- @since 0.6.7
dropWhileAntitone :: (Key -> Bool) -> IntSet -> IntSet
dropWhileAntitone predicate t =
  case t of
    Bin p l r
      | signBranch p ->
        if predicate 0 -- handle negative numbers.
        then go predicate l
        else bin p l (go predicate r)
    _ -> go predicate t
  where
    go predicate' (Bin p l r)
      | predicate' (unPrefix p) = go predicate' r
      | otherwise               = bin p (go predicate' l) r
    go predicate' (Tip kx bm) = tip kx (bm `xor` takeWhileAntitoneBits kx predicate' bm)
    go _ Nil = Nil

-- | \(O(\min(n,W))\). Divide a set at the point where a predicate on the elements stops holding.
-- The user is responsible for ensuring that for all @Int@s, @j \< k ==\> p j \>= p k@.
--
-- @
-- spanAntitone p xs = ('takeWhileAntitone' p xs, 'dropWhileAntitone' p xs)
-- spanAntitone p xs = 'partition' p xs
-- @
--
-- Note: if @p@ is not actually antitone, then @spanAntitone@ will split the set
-- at some /unspecified/ point.
--
-- @since 0.6.7
spanAntitone :: (Key -> Bool) -> IntSet -> (IntSet, IntSet)
spanAntitone predicate t =
  case t of
    Bin p l r
      | signBranch p ->
        if predicate 0 -- handle negative numbers.
        then
          case go predicate l of
            (lt :*: gt) ->
              let !lt' = bin p lt r
              in (lt', gt)
        else
          case go predicate r of
            (lt :*: gt) ->
              let !gt' = bin p l gt
              in (lt, gt')
    _ -> case go predicate t of
          (lt :*: gt) -> (lt, gt)
  where
    go predicate' (Bin p l r)
      | predicate' (unPrefix p) = case go predicate' r of (lt :*: gt) -> bin p l lt :*: gt
      | otherwise               = case go predicate' l of (lt :*: gt) -> lt :*: bin p gt r
    go predicate' (Tip kx bm) = let bm' = takeWhileAntitoneBits kx predicate' bm
                                in (tip kx bm' :*: tip kx (bm `xor` bm'))
    go _ Nil = (Nil :*: Nil)

-- | \(O(\min(n,W))\). The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
--
-- > split 3 (fromList [1..5]) == (fromList [1,2], fromList [4,5])
split :: Key -> IntSet -> (IntSet,IntSet)
split x t =
  case t of
    Bin p l r
      | signBranch p ->
        if x >= 0  -- handle negative numbers.
        then
          case go x l of
            (lt :*: gt) ->
              let !lt' = bin p lt r
              in (lt', gt)
        else
          case go x r of
            (lt :*: gt) ->
              let !gt' = bin p l gt
              in (lt, gt')
    _ -> case go x t of
          (lt :*: gt) -> (lt, gt)
  where
    go !x' t'@(Bin p l r)
        | nomatch x' p = if x' < unPrefix p then (Nil :*: t') else (t' :*: Nil)
        | left x' p    = case go x' l of (lt :*: gt) -> lt :*: bin p gt r
        | otherwise    = case go x' r of (lt :*: gt) -> bin p l lt :*: gt
    go x' t'@(Tip kx' bm)
        | kx' > x'          = (Nil :*: t')
          -- equivalent to kx' > prefixOf x'
        | kx' < prefixOf x' = (t' :*: Nil)
        | otherwise = tip kx' (bm .&. lowerBitmap) :*: tip kx' (bm .&. higherBitmap)
            where lowerBitmap = bitmapOf x' - 1
                  higherBitmap = complement (lowerBitmap + bitmapOf x')
    go _ Nil = (Nil :*: Nil)

-- | \(O(\min(n,W))\). Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
splitMember :: Key -> IntSet -> (IntSet,Bool,IntSet)
splitMember x t =
  case t of
    Bin p l r
      | signBranch p ->
        if x >= 0 -- handle negative numbers.
        then
          case go x l of
            (lt, fnd, gt) ->
              let !lt' = bin p lt r
              in (lt', fnd, gt)
        else
          case go x r of
            (lt, fnd, gt) ->
              let !gt' = bin p l gt
              in (lt, fnd, gt')
    _ -> go x t
  where
    go !x' t'@(Bin p l r)
        | nomatch x' p = if x' < unPrefix p then (Nil, False, t') else (t', False, Nil)
        | left x' p =
          case go x' l of
            (lt, fnd, gt) ->
              let !gt' = bin p gt r
              in (lt, fnd, gt')
        | otherwise =
          case go x' r of
            (lt, fnd, gt) ->
              let !lt' = bin p l lt
              in (lt', fnd, gt)
    go x' t'@(Tip kx' bm)
        | kx' > x'          = (Nil, False, t')
          -- equivalent to kx' > prefixOf x'
        | kx' < prefixOf x' = (t', False, Nil)
        | otherwise = let !lt = tip kx' (bm .&. lowerBitmap)
                          !found = (bm .&. bitmapOfx') /= 0
                          !gt = tip kx' (bm .&. higherBitmap)
                      in (lt, found, gt)
            where bitmapOfx' = bitmapOf x'
                  lowerBitmap = bitmapOfx' - 1
                  higherBitmap = complement (lowerBitmap + bitmapOfx')
    go _ Nil = (Nil, False, Nil)

{----------------------------------------------------------------------
  Min/Max
----------------------------------------------------------------------}

-- | \(O(\min(n,W))\). Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxView :: IntSet -> Maybe (Key, IntSet)
maxView t =
  case t of Nil -> Nothing
            Bin p l r | signBranch p -> case go l of (result, l') -> Just (result, bin p l' r)
            _ -> Just (go t)
  where
    go (Bin p l r) = case go r of (result, r') -> (result, bin p l r')
    go (Tip kx bm) = case highestBitSet bm of bi -> (kx + bi, tip kx (bm .&. complement (bitmapOfSuffix bi)))
    go Nil = error "maxView Nil"

-- | \(O(\min(n,W))\). Retrieves the minimal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
minView :: IntSet -> Maybe (Key, IntSet)
minView t =
  case t of Nil -> Nothing
            Bin p l r | signBranch p -> case go r of (result, r') -> Just (result, bin p l r')
            _ -> Just (go t)
  where
    go (Bin p l r) = case go l of (result, l') -> (result, bin p l' r)
    go (Tip kx bm) = case lowestBitSet bm of bi -> (kx + bi, tip kx (bm .&. complement (bitmapOfSuffix bi)))
    go Nil = error "minView Nil"

-- | \(O(\min(n,W))\). Delete and find the minimal element.
--
-- > deleteFindMin set = (findMin set, deleteMin set)
deleteFindMin :: IntSet -> (Key, IntSet)
deleteFindMin = fromMaybe (error "deleteFindMin: empty set has no minimal element") . minView

-- | \(O(\min(n,W))\). Delete and find the maximal element.
--
-- > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: IntSet -> (Key, IntSet)
deleteFindMax = fromMaybe (error "deleteFindMax: empty set has no maximal element") . maxView

lookupMinSure :: IntSet -> Key
lookupMinSure (Tip kx bm) = kx + lowestBitSet bm
lookupMinSure (Bin _ l _) = lookupMinSure l
lookupMinSure Nil         = error "lookupMin Nil"

-- | \(O(\min(n,W))\). The minimal element of the set. Returns 'Nothing' if the
-- set is empty.
--
-- @since FIXME
lookupMin :: IntSet -> Maybe Key
lookupMin Nil         = Nothing
lookupMin (Tip kx bm) = Just $! kx + lowestBitSet bm
lookupMin (Bin p l r) = Just $! lookupMinSure (if signBranch p then r else l)
{-# INLINE lookupMin #-} -- See Note [Inline lookupMin] in Data.Set.Internal

-- | \(O(\min(n,W))\). The minimal element of the set. Calls 'error' if the set
-- is empty.
findMin :: IntSet -> Key
findMin t
  | Just r <- lookupMin t = r
  | otherwise = error "findMin: empty set has no minimal element"

lookupMaxSure :: IntSet -> Key
lookupMaxSure (Tip kx bm) = kx + highestBitSet bm
lookupMaxSure (Bin _ _ r) = lookupMaxSure r
lookupMaxSure Nil         = error "lookupMax Nil"

-- | \(O(\min(n,W))\). The maximal element of the set. Returns 'Nothing' if the
-- set is empty.
--
-- @since FIXME
lookupMax :: IntSet -> Maybe Key
lookupMax Nil         = Nothing
lookupMax (Tip kx bm) = Just $! kx + highestBitSet bm
lookupMax (Bin p l r) = Just $! lookupMaxSure (if signBranch p then l else r)
{-# INLINE lookupMax #-} -- See Note [Inline lookupMin] in Data.Set.Internal

-- | \(O(\min(n,W))\). The maximal element of the set. Calls 'error' if the set
-- is empty.
findMax :: IntSet -> Key
findMax t
  | Just r <- lookupMax t = r
  | otherwise = error "findMax: empty set has no maximal element"

-- | \(O(\min(n,W))\). Delete the minimal element. Returns an empty set if the set is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Set.Set' &#8211;
-- versions prior to 0.5 threw an error if the 'IntSet' was already empty.
deleteMin :: IntSet -> IntSet
deleteMin = maybe Nil snd . minView

-- | \(O(\min(n,W))\). Delete the maximal element. Returns an empty set if the set is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Set.Set' &#8211;
-- versions prior to 0.5 threw an error if the 'IntSet' was already empty.
deleteMax :: IntSet -> IntSet
deleteMax = maybe Nil snd . maxView

{----------------------------------------------------------------------
  Map
----------------------------------------------------------------------}

-- | \(O(n \min(n,W))\).
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@

map :: (Key -> Key) -> IntSet -> IntSet
map f = fromList . List.map f . toList

-- | \(O(n)\). The
--
-- @'mapMonotonic' f s == 'map' f s@, but works only when @f@ is strictly increasing.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapMonotonic f s == map f s
-- >     where ls = toList s
--
-- @since 0.6.3.1

-- Note that for now the test is insufficient to support any fancier implementation.
mapMonotonic :: (Key -> Key) -> IntSet -> IntSet
mapMonotonic f = fromDistinctAscList . List.map f . toAscList


{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | \(O(n)\). Fold the elements in the set using the given right-associative
-- binary operator.
--
{-# DEPRECATED fold "Use Data.IntSet.foldr instead" #-}
fold :: (Key -> b -> b) -> b -> IntSet -> b
fold = foldr
{-# INLINE fold #-}

-- | \(O(n)\). Fold the elements in the set using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'toAscList'@.
--
-- For example,
--
-- > toAscList set = foldr (:) [] set
foldr :: (Key -> b -> b) -> b -> IntSet -> b
foldr f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of Bin p l r | signBranch p -> go (go z l) r -- put negative numbers before
                      | otherwise -> go (go z r) l
            _ -> go z t
  where
    go z' Nil         = z'
    go z' (Tip kx bm) = foldrBits kx f z' bm
    go z' (Bin _ l r) = go (go z' r) l
{-# INLINE foldr #-}

-- | \(O(n)\). A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (Key -> b -> b) -> b -> IntSet -> b
foldr' f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of Bin p l r | signBranch p -> go (go z l) r -- put negative numbers before
                      | otherwise -> go (go z r) l
            _ -> go z t
  where
    go !z' Nil        = z'
    go z' (Tip kx bm) = foldr'Bits kx f z' bm
    go z' (Bin _ l r) = go (go z' r) l
{-# INLINE foldr' #-}

-- | \(O(n)\). Fold the elements in the set using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'toAscList'@.
--
-- For example,
--
-- > toDescList set = foldl (flip (:)) [] set
foldl :: (a -> Key -> a) -> a -> IntSet -> a
foldl f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of Bin p l r | signBranch p -> go (go z r) l -- put negative numbers before
                      | otherwise -> go (go z l) r
            _ -> go z t
  where
    go z' Nil         = z'
    go z' (Tip kx bm) = foldlBits kx f z' bm
    go z' (Bin _ l r) = go (go z' l) r
{-# INLINE foldl #-}

-- | \(O(n)\). A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> Key -> a) -> a -> IntSet -> a
foldl' f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of Bin p l r | signBranch p -> go (go z r) l -- put negative numbers before
                      | otherwise -> go (go z l) r
            _ -> go z t
  where
    go !z' Nil        = z'
    go z' (Tip kx bm) = foldl'Bits kx f z' bm
    go z' (Bin _ l r) = go (go z' l) r
{-# INLINE foldl' #-}

-- | \(O(n))\). Map the elements in the set to a monoid and combine with @(<>)@.
foldMap :: Monoid a => (Key -> a) -> IntSet -> a
foldMap f = \t ->  -- Use lambda t to be inlinable with one argument only.
  case t of
    Bin p l r
#if MIN_VERSION_base(4,11,0)
      | signBranch p -> go r <> go l  -- handle negative numbers
      | otherwise -> go l <> go r
#else
      | signBranch p -> go r `mappend` go l  -- handle negative numbers
      | otherwise -> go l `mappend` go r
#endif
    _ -> go t
  where
#if MIN_VERSION_base(4,11,0)
    go (Bin _ l r) = go l <> go r
#else
    go (Bin _ l r) = go l `mappend` go r
#endif
    go (Tip kx bm) = foldMapBits kx f bm
    go Nil = mempty
{-# INLINE foldMap #-}

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | \(O(n)\). An alias of 'toAscList'. The elements of a set in ascending order.
-- Subject to list fusion.
elems :: IntSet -> [Key]
elems
  = toAscList

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

#ifdef __GLASGOW_HASKELL__
-- | @since 0.5.6.2
instance GHC.Exts.IsList IntSet where
  type Item IntSet = Key
  fromList = fromList
  toList   = toList
#endif

-- | \(O(n)\). Convert the set to a list of elements. Subject to list fusion.
toList :: IntSet -> [Key]
toList
  = toAscList

-- | \(O(n)\). Convert the set to an ascending list of elements. Subject to list
-- fusion.
toAscList :: IntSet -> [Key]
toAscList = foldr (:) []

-- | \(O(n)\). Convert the set to a descending list of elements. Subject to list
-- fusion.
toDescList :: IntSet -> [Key]
toDescList = foldl (flip (:)) []

-- List fusion for the list generating functions.
#if __GLASGOW_HASKELL__
-- The foldrFB and foldlFB are foldr and foldl equivalents, used for list fusion.
-- They are important to convert unfused to{Asc,Desc}List back, see mapFB in prelude.
foldrFB :: (Key -> b -> b) -> b -> IntSet -> b
foldrFB = foldr
{-# INLINE[0] foldrFB #-}
foldlFB :: (a -> Key -> a) -> a -> IntSet -> a
foldlFB = foldl
{-# INLINE[0] foldlFB #-}

-- Inline elems and toList, so that we need to fuse only toAscList.
{-# INLINE elems #-}
{-# INLINE toList #-}

-- The fusion is enabled up to phase 2 included. If it does not succeed,
-- convert in phase 1 the expanded to{Asc,Desc}List calls back to
-- to{Asc,Desc}List.  In phase 0, we inline fold{lr}FB (which were used in
-- a list fusion, otherwise it would go away in phase 1), and let compiler do
-- whatever it wants with to{Asc,Desc}List -- it was forbidden to inline it
-- before phase 0, otherwise the fusion rules would not fire at all.
{-# NOINLINE[0] toAscList #-}
{-# NOINLINE[0] toDescList #-}
{-# RULES "IntSet.toAscList" [~1] forall s . toAscList s = GHC.Exts.build (\c n -> foldrFB c n s) #-}
{-# RULES "IntSet.toAscListBack" [1] foldrFB (:) [] = toAscList #-}
{-# RULES "IntSet.toDescList" [~1] forall s . toDescList s = GHC.Exts.build (\c n -> foldlFB (\xs x -> c x xs) n s) #-}
{-# RULES "IntSet.toDescListBack" [1] foldlFB (\xs x -> x : xs) [] = toDescList #-}
#endif


-- | \(O(n \min(n,W))\). Create a set from a list of integers.
fromList :: [Key] -> IntSet
fromList xs
  = Foldable.foldl' ins empty xs
  where
    ins t x  = insert x t

-- | \(O(n / W)\). Create a set from a range of integers.
--
-- > fromRange (low, high) == fromList [low..high]
--
-- @since 0.7
fromRange :: (Key, Key) -> IntSet
fromRange (lx,rx)
  | lx > rx  = empty
  | lp == rp = Tip lp (bitmapOf rx `shiftLL` 1 - bitmapOf lx)
  | otherwise =
      let m = branchMask lx rx
          p = Prefix (mask lx m .|. m)
      in if signBranch p  -- handle negative numbers
         then Bin p (goR 0) (goL 0)
         else Bin p (goL (unPrefix p)) (goR (unPrefix p))
  where
    lp = prefixOf lx
    rp = prefixOf rx
    -- goL p0 = fromList [lx .. p0-1]
    -- Expected: p0 is lx where one 0-bit is flipped to 1 and all bits lower than that are 0.
    --           p0 can be 0 (pretend that bit WORD_SIZE is flipped to 1).
    goL :: Int -> IntSet
    goL !p0 = go (Tip lp (- bitmapOf lx)) (lp + lbm prefixBitMask)
      where
        go !l p | p == p0 = l
        go l p =
          let m = lbm p
              l' = Bin (Prefix p) l (goFull p (shr1 m))
          in go l' (p + m)
    -- goR p0 = fromList [p0 .. rx]
    -- Expected: p0 is a prefix of rx
    goR :: Int -> IntSet
    goR !p0 = go (Tip rp (bitmapOf rx `shiftLL` 1 - 1)) rp
      where
        go !r p | p == p0 = r
        go r p =
          let m = lbm p
              p' = p `xor` m
              r' = Bin (Prefix p) (goFull p' (shr1 m)) r
          in go r' p'
    -- goFull p m = fromList [p .. p+2*m-1]
    -- Expected: popCount m == 1, p == mask p m
    goFull :: Int -> Int -> IntSet
    goFull p m
      | m < suffixBitMask = Tip p (complement 0)
      | otherwise         = Bin (Prefix (p .|. m)) (goFull p (shr1 m)) (goFull (p .|. m) (shr1 m))
    lbm :: Int -> Int
    lbm p = p .&. negate p -- lowest bit mask
    {-# INLINE lbm #-}
    shr1 :: Int -> Int
    shr1 m = m `iShiftRL` 1
    {-# INLINE shr1 #-}

-- | \(O(n)\). Build a set from an ascending list of elements.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: [Key] -> IntSet
fromAscList = fromMonoList
{-# NOINLINE fromAscList #-}

-- | \(O(n)\). Build a set from an ascending list of distinct elements.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscList :: [Key] -> IntSet
fromDistinctAscList = fromAscList
{-# INLINE fromDistinctAscList #-}

-- | \(O(n)\). Build a set from a monotonic list of elements.
--
-- The precise conditions under which this function works are subtle:
-- For any branch mask, keys with the same prefix w.r.t. the branch
-- mask must occur consecutively in the list.
fromMonoList :: [Key] -> IntSet
fromMonoList []         = Nil
fromMonoList (kx : zs1) = addAll' (prefixOf kx) (bitmapOf kx) zs1
  where
    -- `addAll'` collects all keys with the prefix `px` into a single
    -- bitmap, and then proceeds with `addAll`.
    addAll' !px !bm []
        = Tip px bm
    addAll' !px !bm (ky : zs)
        | px == prefixOf ky
        = addAll' px (bm .|. bitmapOf ky) zs
        -- inlined: | otherwise = addAll px (Tip px bm) (ky : zs)
        | py <- prefixOf ky
        , m <- branchMask px py
        , Inserted ty zs' <- addMany' m py (bitmapOf ky) zs
        = addAll px (linkWithMask m py ty px (Tip px bm)) zs'

    -- for `addAll` and `addMany`, px is /a/ prefix inside the tree `tx`
    -- `addAll` consumes the rest of the list, adding to the tree `tx`
    addAll !_px !tx []
        = tx
    addAll !px !tx (ky : zs)
        | py <- prefixOf ky
        , m <- branchMask px py
        , Inserted ty zs' <- addMany' m py (bitmapOf ky) zs
        = addAll px (linkWithMask m py ty px tx) zs'

    -- `addMany'` is similar to `addAll'`, but proceeds with `addMany'`.
    addMany' !_m !px !bm []
        = Inserted (Tip px bm) []
    addMany' !m !px !bm zs0@(ky : zs)
        | px == prefixOf ky
        = addMany' m px (bm .|. bitmapOf ky) zs
        -- inlined: | otherwise = addMany m px (Tip px bm) (ky : zs)
        | mask px m /= mask ky m
        = Inserted (Tip (prefixOf px) bm) zs0
        | py <- prefixOf ky
        , mxy <- branchMask px py
        , Inserted ty zs' <- addMany' mxy py (bitmapOf ky) zs
        = addMany m px (linkWithMask mxy py ty px (Tip px bm)) zs'

    -- `addAll` adds to `tx` all keys whose prefix w.r.t. `m` agrees with `px`.
    addMany !_m !_px tx []
        = Inserted tx []
    addMany !m !px tx zs0@(ky : zs)
        | mask px m /= mask ky m
        = Inserted tx zs0
        | py <- prefixOf ky
        , mxy <- branchMask px py
        , Inserted ty zs' <- addMany' mxy py (bitmapOf ky) zs
        = addMany m px (linkWithMask mxy py ty px tx) zs'
{-# INLINE fromMonoList #-}

data Inserted = Inserted !IntSet ![Key]

{--------------------------------------------------------------------
  Eq
--------------------------------------------------------------------}
instance Eq IntSet where
  (==) = equal

equal :: IntSet -> IntSet -> Bool
equal (Bin p1 l1 r1) (Bin p2 l2 r2)
  = (p1 == p2) && (equal l1 l2) && (equal r1 r2)
equal (Tip kx1 bm1) (Tip kx2 bm2)
  = kx1 == kx2 && bm1 == bm2
equal Nil Nil = True
equal _   _   = False

{--------------------------------------------------------------------
  Ord
--------------------------------------------------------------------}

instance Ord IntSet where
    compare s1 s2 = compare (toAscList s1) (toAscList s2)
    -- tentative implementation. See if more efficient exists.

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance Show IntSet where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (toList xs)

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance Read IntSet where
#ifdef __GLASGOW_HASKELL__
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

{--------------------------------------------------------------------
  NFData
--------------------------------------------------------------------}

-- The IntSet constructors consist only of strict fields of Ints and
-- IntSets, thus the default NFData instance which evaluates to whnf
-- should suffice
instance NFData IntSet where rnf x = seq x ()

{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}
-- | \(O(n \min(n,W))\). Show the tree that implements the set. The tree is shown
-- in a compressed, hanging format.
showTree :: IntSet -> String
showTree s
  = showTreeWith True False s


{- | \(O(n \min(n,W))\). The expression (@'showTreeWith' hang wide map@) shows
 the tree that implements the set. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.
-}
showTreeWith :: Bool -> Bool -> IntSet -> String
showTreeWith hang wide t
  | hang      = (showsTreeHang wide [] t) ""
  | otherwise = (showsTree wide [] [] t) ""

showsTree :: Bool -> [String] -> [String] -> IntSet -> ShowS
showsTree wide lbars rbars t
  = case t of
      Bin p l r
          -> showsTree wide (withBar rbars) (withEmpty rbars) r .
             showWide wide rbars .
             showsBars lbars . showString (showBin p) . showString "\n" .
             showWide wide lbars .
             showsTree wide (withEmpty lbars) (withBar lbars) l
      Tip kx bm
          -> showsBars lbars . showString " " . shows kx . showString " + " .
                                                showsBitMap bm . showString "\n"
      Nil -> showsBars lbars . showString "|\n"

showsTreeHang :: Bool -> [String] -> IntSet -> ShowS
showsTreeHang wide bars t
  = case t of
      Bin p l r
          -> showsBars bars . showString (showBin p) . showString "\n" .
             showWide wide bars .
             showsTreeHang wide (withBar bars) l .
             showWide wide bars .
             showsTreeHang wide (withEmpty bars) r
      Tip kx bm
          -> showsBars bars . showString " " . shows kx . showString " + " .
                                               showsBitMap bm . showString "\n"
      Nil -> showsBars bars . showString "|\n"

showBin :: Prefix -> String
showBin _
  = "*" -- ++ show (p,m)

showWide :: Bool -> [String] -> String -> String
showWide wide bars
  | wide      = showString (concat (reverse bars)) . showString "|\n"
  | otherwise = id

showsBars :: [String] -> ShowS
showsBars [] = id
showsBars (_ : tl) = showString (concat (reverse tl)) . showString node

showsBitMap :: Word -> ShowS
showsBitMap = showString . showBitMap

showBitMap :: Word -> String
showBitMap w = show $ foldrBits 0 (:) [] w

node :: String
node           = "+--"

withBar, withEmpty :: [String] -> [String]
withBar bars   = "|  ":bars
withEmpty bars = "   ":bars


{--------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------}
{--------------------------------------------------------------------
  Link
--------------------------------------------------------------------}

-- | Link two @IntSet@s. The sets must not be empty. The @Prefix@es of the two
-- sets must be different. @k1@ must share the prefix of @t1@. @p2@ must be the
-- prefix of @t2@.
linkKey :: Key -> IntSet -> Prefix -> IntSet -> IntSet
linkKey k1 t1 p2 t2 = link k1 t1 (unPrefix p2) t2
{-# INLINE linkKey #-}

-- | Link two @IntSets. The sets must not be empty. The @Prefix@es of the two
-- sets must be different. @k1@ must share the prefix of @t1@ and @k2@ must
-- share the prefix of @t2@.
link :: Int -> IntSet -> Int -> IntSet -> IntSet
link k1 t1 k2 t2 = linkWithMask (branchMask k1 k2) k1 t1 k2 t2
{-# INLINE link #-}

-- `linkWithMask` is useful when the `branchMask` has already been computed
linkWithMask :: Int -> Key -> IntSet -> Key -> IntSet -> IntSet
linkWithMask m k1 t1 k2 t2
  | i2w k1 < i2w k2 = Bin p t1 t2
  | otherwise = Bin p t2 t1
  where
    p = Prefix (mask k1 m .|. m)
{-# INLINE linkWithMask #-}

{--------------------------------------------------------------------
  @bin@ assures that we never have empty trees within a tree.
--------------------------------------------------------------------}
bin :: Prefix -> IntSet -> IntSet -> IntSet
bin _ l Nil = l
bin _ Nil r = r
bin p l r   = Bin p l r
{-# INLINE bin #-}

{--------------------------------------------------------------------
  @tip@ assures that we never have empty bitmaps within a tree.
--------------------------------------------------------------------}
tip :: Int -> BitMap -> IntSet
tip _ 0 = Nil
tip kx bm = Tip kx bm
{-# INLINE tip #-}


{----------------------------------------------------------------------
  Functions that generate Prefix and BitMap of a Key or a Suffix.
----------------------------------------------------------------------}

suffixBitMask :: Int
suffixBitMask = finiteBitSize (undefined::Word) - 1
{-# INLINE suffixBitMask #-}

prefixBitMask :: Int
prefixBitMask = complement suffixBitMask
{-# INLINE prefixBitMask #-}

prefixOf :: Int -> Int
prefixOf x = x .&. prefixBitMask
{-# INLINE prefixOf #-}

suffixOf :: Int -> Int
suffixOf x = x .&. suffixBitMask
{-# INLINE suffixOf #-}

bitmapOfSuffix :: Int -> BitMap
bitmapOfSuffix s = 1 `shiftLL` s
{-# INLINE bitmapOfSuffix #-}

bitmapOf :: Int -> BitMap
bitmapOf x = bitmapOfSuffix (suffixOf x)
{-# INLINE bitmapOf #-}


{----------------------------------------------------------------------
  To get best performance, we provide fast implementations of
  lowestBitSet, highestBitSet and fold[lr][l]Bits for GHC.
  If the intel bsf and bsr instructions ever become GHC primops,
  this code should be reimplemented using these.

  Performance of this code is crucial for folds, toList, filter, partition.

  The signatures of methods in question are placed after this comment.
----------------------------------------------------------------------}

lowestBitSet :: Word -> Int
highestBitSet :: Word -> Int
foldlBits :: Int -> (a -> Int -> a) -> a -> Word -> a
foldl'Bits :: Int -> (a -> Int -> a) -> a -> Word -> a
foldrBits :: Int -> (Int -> a -> a) -> a -> Word -> a
foldr'Bits :: Int -> (Int -> a -> a) -> a -> Word -> a
#if MIN_VERSION_base(4,11,0)
foldMapBits :: Semigroup a => Int -> (Int -> a) -> Word -> a
#else
foldMapBits :: Monoid a => Int -> (Int -> a) -> Word -> a
#endif
takeWhileAntitoneBits :: Int -> (Int -> Bool) -> Word -> Word

{-# INLINE lowestBitSet #-}
{-# INLINE highestBitSet #-}
{-# INLINE foldlBits #-}
{-# INLINE foldl'Bits #-}
{-# INLINE foldrBits #-}
{-# INLINE foldr'Bits #-}
{-# INLINE foldMapBits #-}
{-# INLINE takeWhileAntitoneBits #-}

#if defined(__GLASGOW_HASKELL__)

lowestBitMask :: Word -> Word
lowestBitMask x = x .&. negate x
{-# INLINE lowestBitMask #-}

lowestBitSet x = countTrailingZeros x

highestBitSet x = WORD_SIZE_IN_BITS - 1 - countLeadingZeros x

-- Reverse the order of bits in the Word.
revWord :: Word -> Word
#if WORD_SIZE_IN_BITS==32
revWord x1 = case ((x1 `shiftRL` 1) .&. 0x55555555) .|. ((x1 .&. 0x55555555) `shiftLL` 1) of
              x2 -> case ((x2 `shiftRL` 2) .&. 0x33333333) .|. ((x2 .&. 0x33333333) `shiftLL` 2) of
                 x3 -> case ((x3 `shiftRL` 4) .&. 0x0F0F0F0F) .|. ((x3 .&. 0x0F0F0F0F) `shiftLL` 4) of
                   x4 -> case ((x4 `shiftRL` 8) .&. 0x00FF00FF) .|. ((x4 .&. 0x00FF00FF) `shiftLL` 8) of
                     x5 -> ( x5 `shiftRL` 16             ) .|. ( x5               `shiftLL` 16);
#else
revWord x1 = case ((x1 `shiftRL` 1) .&. 0x5555555555555555) .|. ((x1 .&. 0x5555555555555555) `shiftLL` 1) of
              x2 -> case ((x2 `shiftRL` 2) .&. 0x3333333333333333) .|. ((x2 .&. 0x3333333333333333) `shiftLL` 2) of
                 x3 -> case ((x3 `shiftRL` 4) .&. 0x0F0F0F0F0F0F0F0F) .|. ((x3 .&. 0x0F0F0F0F0F0F0F0F) `shiftLL` 4) of
                   x4 -> case ((x4 `shiftRL` 8) .&. 0x00FF00FF00FF00FF) .|. ((x4 .&. 0x00FF00FF00FF00FF) `shiftLL` 8) of
                     x5 -> case ((x5 `shiftRL` 16) .&. 0x0000FFFF0000FFFF) .|. ((x5 .&. 0x0000FFFF0000FFFF) `shiftLL` 16) of
                       x6 -> ( x6 `shiftRL` 32             ) .|. ( x6               `shiftLL` 32);
#endif

foldlBits prefix f z bitmap = go bitmap z
  where go 0 acc = acc
        go bm acc = go (bm `xor` bitmask) ((f acc) $! (prefix+bi))
          where
            !bitmask = lowestBitMask bm
            !bi = countTrailingZeros bitmask

foldl'Bits prefix f z bitmap = go bitmap z
  where go 0 acc = acc
        go bm !acc = go (bm `xor` bitmask) ((f acc) $! (prefix+bi))
          where !bitmask = lowestBitMask bm
                !bi = countTrailingZeros bitmask

foldrBits prefix f z bitmap = go (revWord bitmap) z
  where go 0 acc = acc
        go bm acc = go (bm `xor` bitmask) ((f $! (prefix+(WORD_SIZE_IN_BITS-1)-bi)) acc)
          where !bitmask = lowestBitMask bm
                !bi = countTrailingZeros bitmask


foldr'Bits prefix f z bitmap = go (revWord bitmap) z
  where go 0 acc = acc
        go bm !acc = go (bm `xor` bitmask) ((f $! (prefix+(WORD_SIZE_IN_BITS-1)-bi)) acc)
          where !bitmask = lowestBitMask bm
                !bi = countTrailingZeros bitmask

foldMapBits prefix f bitmap = go (prefix + bi0) (bitmap `xor` bitmask0)
  where
    bitmask0 = lowestBitMask bitmap
    bi0 = countTrailingZeros bitmask0
    go !x 0 = f x
#if MIN_VERSION_base(4,11,0)
    go !x bm = f x <> go (prefix + bi) (bm `xor` bitmask)
#else
    go !x bm = f x `mappend` go (prefix + bi) (bm `xor` bitmask)
#endif
      where
        bitmask = lowestBitMask bm
        bi = countTrailingZeros bitmask

takeWhileAntitoneBits prefix predicate bitmap =
  -- Binary search for the first index where the predicate returns false, but skip a predicate
  -- call if the high half of the current range is empty. This ensures
  -- min (log2 WORD_SIZE_IN_BITS + 1) (popcount bitmap) predicate calls.
  let next d h (n',b') =
        if n' .&. h /= 0 && (predicate $! prefix+b'+d) then (n' `shiftRL` d, b'+d) else (n',b')
      {-# INLINE next #-}
      (_,b) = next 1  0x2 $
              next 2  0xC $
              next 4  0xF0 $
              next 8  0xFF00 $
              next 16 0xFFFF0000 $
#if WORD_SIZE_IN_BITS==64
              next 32 0xFFFFFFFF00000000 $
#endif
              (bitmap,0)
      m = if b /= 0 || (bitmap .&. 0x1 /= 0 && predicate prefix)
          then ((2 `shiftLL` b) - 1)
          else ((1 `shiftLL` b) - 1)
  in bitmap .&. m

#else
{----------------------------------------------------------------------
  In general case we use logarithmic implementation of
  lowestBitSet and highestBitSet, which works up to bit sizes of 64.

  Folds are linear scans.
----------------------------------------------------------------------}

lowestBitSet n0 =
    let (n1,b1) = if n0 .&. 0xFFFFFFFF /= 0 then (n0,0)  else (n0 `shiftRL` 32, 32)
        (n2,b2) = if n1 .&. 0xFFFF /= 0     then (n1,b1) else (n1 `shiftRL` 16, 16+b1)
        (n3,b3) = if n2 .&. 0xFF /= 0       then (n2,b2) else (n2 `shiftRL` 8,  8+b2)
        (n4,b4) = if n3 .&. 0xF /= 0        then (n3,b3) else (n3 `shiftRL` 4,  4+b3)
        (n5,b5) = if n4 .&. 0x3 /= 0        then (n4,b4) else (n4 `shiftRL` 2,  2+b4)
        b6      = if n5 .&. 0x1 /= 0        then     b5  else                   1+b5
    in b6

highestBitSet n0 =
    let (n1,b1) = if n0 .&. 0xFFFFFFFF00000000 /= 0 then (n0 `shiftRL` 32, 32)    else (n0,0)
        (n2,b2) = if n1 .&. 0xFFFF0000 /= 0         then (n1 `shiftRL` 16, 16+b1) else (n1,b1)
        (n3,b3) = if n2 .&. 0xFF00 /= 0             then (n2 `shiftRL` 8,  8+b2)  else (n2,b2)
        (n4,b4) = if n3 .&. 0xF0 /= 0               then (n3 `shiftRL` 4,  4+b3)  else (n3,b3)
        (n5,b5) = if n4 .&. 0xC /= 0                then (n4 `shiftRL` 2,  2+b4)  else (n4,b4)
        b6      = if n5 .&. 0x2 /= 0                then                   1+b5   else     b5
    in b6

foldlBits prefix f z bm = let lb = lowestBitSet bm
                          in  go (prefix+lb) z (bm `shiftRL` lb)
  where go !_ acc 0 = acc
        go bi acc n | n `testBit` 0 = go (bi + 1) (f acc bi) (n `shiftRL` 1)
                    | otherwise     = go (bi + 1)    acc     (n `shiftRL` 1)

foldl'Bits prefix f z bm = let lb = lowestBitSet bm
                           in  go (prefix+lb) z (bm `shiftRL` lb)
  where go !_ !acc 0 = acc
        go bi acc n | n `testBit` 0 = go (bi + 1) (f acc bi) (n `shiftRL` 1)
                    | otherwise     = go (bi + 1)    acc     (n `shiftRL` 1)

foldrBits prefix f z bm = let lb = lowestBitSet bm
                          in  go (prefix+lb) (bm `shiftRL` lb)
  where go !_ 0 = z
        go bi n | n `testBit` 0 = f bi (go (bi + 1) (n `shiftRL` 1))
                | otherwise     =       go (bi + 1) (n `shiftRL` 1)

foldr'Bits prefix f z bm = let lb = lowestBitSet bm
                           in  go (prefix+lb) (bm `shiftRL` lb)
  where
        go !_ 0 = z
        go bi n | n `testBit` 0 = f bi $! go (bi + 1) (n `shiftRL` 1)
                | otherwise     =         go (bi + 1) (n `shiftRL` 1)

foldMapBits prefix f bm = go x0 (x0 + 1) ((bm `shiftRL` lb) `shiftRL` 1)
  where
    lb = lowestBitSet bm
    x0 = prefix + lb
    go !x !_ 0 = f x
    go !x !bi n
#if MIN_VERSION_base(4,11,0)
      | n `testBit` 0 = f x <> go bi (bi + 1) (n `shiftRL` 1)
#else
      | n `testBit` 0 = f x `mappend` go bi (bi + 1) (n `shiftRL` 1)
#endif
      | otherwise = go x (bi + 1) (n `shiftRL` 1)

takeWhileAntitoneBits prefix predicate = foldl'Bits prefix f 0 -- Does not use antitone property
  where
    f acc bi | predicate bi = acc .|. bitmapOf bi
             | otherwise    = acc

#endif


{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

-- | \(O(1)\).  Decompose a set into pieces based on the structure of the underlying
-- tree.  This function is useful for consuming a set in parallel.
--
-- No guarantee is made as to the sizes of the pieces; an internal, but
-- deterministic process determines this.  However, it is guaranteed that the
-- pieces returned will be in ascending order (all elements in the first submap
-- less than all elements in the second, and so on).
--
-- Examples:
--
-- > splitRoot (fromList [1..120]) == [fromList [1..63],fromList [64..120]]
-- > splitRoot empty == []
--
--  Note that the current implementation does not return more than two subsets,
--  but you should not depend on this behaviour because it can change in the
--  future without notice. Also, the current version does not continue
--  splitting all the way to individual singleton sets -- it stops at some
--  point.
splitRoot :: IntSet -> [IntSet]
splitRoot Nil = []
-- NOTE: we don't currently split below Tip, but we could.
splitRoot x@(Tip _ _) = [x]
splitRoot (Bin p l r) | signBranch p = [r, l]
                      | otherwise = [l, r]
{-# INLINE splitRoot #-}
