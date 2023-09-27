{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Trustworthy #-}
#endif
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
#endif

{-# OPTIONS_HADDOCK not-home #-}

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Set.Internal
-- Copyright   :  (c) Daan Leijen 2002
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
-- An efficient implementation of sets.
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import Data.Set (Set)
-- >  import qualified Data.Set as Set
--
-- The implementation of 'Set' is based on /size balanced/ binary trees (or
-- trees of /bounded balance/) as described by:
--
--    * Stephen Adams, \"/Efficient sets: a balancing act/\",
--      Journal of Functional Programming 3(4):553-562, October 1993,
--      <http://www.swiss.ai.mit.edu/~adams/BB/>.
--    * J. Nievergelt and E.M. Reingold,
--      \"/Binary search trees of bounded balance/\",
--      SIAM journal of computing 2(1), March 1973.
--
--  Bounds for 'union', 'intersection', and 'difference' are as given
--  by
--
--    * Guy Blelloch, Daniel Ferizovic, and Yihan Sun,
--      \"/Just Join for Parallel Ordered Sets/\",
--      <https://arxiv.org/abs/1602.02120v3>.
--
-- Note that the implementation is /left-biased/ -- the elements of a
-- first argument are always preferred to the second, for example in
-- 'union' or 'insert'.  Of course, left-biasing can only be observed
-- when equality is an equivalence relation instead of structural
-- equality.
--
-- /Warning/: The size of the set must not exceed @maxBound::Int@. Violation of
-- this condition is not detected and if the size limit is exceeded, the
-- behavior of the set is completely undefined.
--
-- @since 0.5.9
-----------------------------------------------------------------------------

-- [Note: Using INLINABLE]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- It is crucial to the performance that the functions specialize on the Ord
-- type when possible. GHC 7.0 and higher does this by itself when it sees th
-- unfolding of a function -- that is why all public functions are marked
-- INLINABLE (that exposes the unfolding).


-- [Note: Using INLINE]
-- ~~~~~~~~~~~~~~~~~~~~
-- For other compilers and GHC pre 7.0, we mark some of the functions INLINE.
-- We mark the functions that just navigate down the tree (lookup, insert,
-- delete and similar). That navigation code gets inlined and thus specialized
-- when possible. There is a price to pay -- code growth. The code INLINED is
-- therefore only the tree navigation, all the real work (rebalancing) is not
-- INLINED by using a NOINLINE.
--
-- All methods marked INLINE have to be nonrecursive -- a 'go' function doing
-- the real work is provided.


-- [Note: Type of local 'go' function]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- If the local 'go' function uses an Ord class, it sometimes heap-allocates
-- the Ord dictionary when the 'go' function does not have explicit type.
-- In that case we give 'go' explicit type. But this slightly decrease
-- performance, as the resulting 'go' function can float out to top level.


-- [Note: Local 'go' functions and capturing]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- As opposed to IntSet, when 'go' function captures an argument, increased
-- heap-allocation can occur: sometimes in a polymorphic function, the 'go'
-- floats out of its enclosing function and then it heap-allocates the
-- dictionary and the argument. Maybe it floats out too late and strictness
-- analyzer cannot see that these could be passed on stack.

-- [Note: Order of constructors]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The order of constructors of Set matters when considering performance.
-- Currently in GHC 7.0, when type has 2 constructors, a forward conditional
-- jump is made when successfully matching second constructor. Successful match
-- of first constructor results in the forward jump not taken.
-- On GHC 7.0, reordering constructors from Tip | Bin to Bin | Tip
-- improves the benchmark by up to 10% on x86.

module Data.Set.Internal (
            -- * Set type
              Set(..)       -- instance Eq,Ord,Show,Read,Data
            , Size

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
            , insert
            , delete
            , alterF
            , powerSet

            -- * Combine
            , union
            , unions
            , difference
            , intersection
            , intersections
            , cartesianProduct
            , disjointUnion
            , Intersection(..)


            -- * Filter
            , filter
            , takeWhileAntitone
            , dropWhileAntitone
            , spanAntitone
            , partition
            , split
            , splitMember
            , splitRoot

            -- * Indexed
            , lookupIndex
            , findIndex
            , elemAt
            , deleteAt
            , take
            , drop
            , splitAt

            -- * Map
            , map
            , mapMonotonic

            -- * Folds
            , foldr
            , foldl
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
            , fromDescList
            , fromDistinctDescList

            -- * Debugging
            , showTree
            , showTreeWith
            , valid

            -- Internals (for testing)
            , bin
            , balanced
            , link
            , merge
            ) where

import Utils.Containers.Internal.Prelude hiding
  (filter,foldl,foldl',foldr,null,map,take,drop,splitAt)
import Prelude ()
import Control.Applicative (Const(..))
import qualified Data.List as List
import Data.Bits (shiftL, shiftR)
import Data.Semigroup (Semigroup(stimes))
import Data.List.NonEmpty (NonEmpty(..))
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.Semigroup (stimesIdempotentMonoid, stimesIdempotent)
import Data.Functor.Classes
import Data.Functor.Identity (Identity)
import qualified Data.Foldable as Foldable
import Control.DeepSeq (NFData(rnf))

import Utils.Containers.Internal.StrictPair
import Utils.Containers.Internal.PtrEquality

#if __GLASGOW_HASKELL__
import GHC.Exts ( build, lazy )
import qualified GHC.Exts as GHCExts
import Text.Read ( readPrec, Read (..), Lexeme (..), parens, prec
                 , lexP, readListPrecDefault )
import Data.Data
import Language.Haskell.TH.Syntax (Lift)
-- See Note [ Template Haskell Dependencies ]
import Language.Haskell.TH ()
#endif


{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 \\ --

-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\). See 'difference'.
(\\) :: Ord a => Set a -> Set a -> Set a
m1 \\ m2 = difference m1 m2
#if __GLASGOW_HASKELL__
{-# INLINABLE (\\) #-}
#endif

{--------------------------------------------------------------------
  Sets are size balanced trees
--------------------------------------------------------------------}
-- | A set of values @a@.

-- See Note: Order of constructors
data Set a    = Bin {-# UNPACK #-} !Size !a !(Set a) !(Set a)
              | Tip

type Size     = Int

#ifdef __GLASGOW_HASKELL__
type role Set nominal
#endif

-- | @since 0.6.6
deriving instance Lift a => Lift (Set a)

instance Ord a => Monoid (Set a) where
    mempty  = empty
    mconcat = unions
    mappend = (<>)

-- | @since 0.5.7
instance Ord a => Semigroup (Set a) where
    (<>)    = union
    stimes  = stimesIdempotentMonoid

-- | Folds in order of increasing key.
instance Foldable.Foldable Set where
    fold = go
      where go Tip = mempty
            go (Bin 1 k _ _) = k
            go (Bin _ k l r) = go l `mappend` (k `mappend` go r)
    {-# INLINABLE fold #-}
    foldr = foldr
    {-# INLINE foldr #-}
    foldl = foldl
    {-# INLINE foldl #-}
    foldMap f t = go t
      where go Tip = mempty
            go (Bin 1 k _ _) = f k
            go (Bin _ k l r) = go l `mappend` (f k `mappend` go r)
    {-# INLINE foldMap #-}
    foldl' = foldl'
    {-# INLINE foldl' #-}
    foldr' = foldr'
    {-# INLINE foldr' #-}
    length = size
    {-# INLINE length #-}
    null   = null
    {-# INLINE null #-}
    toList = toList
    {-# INLINE toList #-}
    elem = go
      where go !_ Tip = False
            go x (Bin _ y l r) = x == y || go x l || go x r
    {-# INLINABLE elem #-}
    minimum = findMin
    {-# INLINE minimum #-}
    maximum = findMax
    {-# INLINE maximum #-}
    sum = foldl' (+) 0
    {-# INLINABLE sum #-}
    product = foldl' (*) 1
    {-# INLINABLE product #-}

#if __GLASGOW_HASKELL__

{--------------------------------------------------------------------
  A Data instance
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We provide limited reflection services for the sake of data abstraction.

instance (Data a, Ord a) => Data (Set a) where
  gfoldl f z set = z fromList `f` (toList set)
  toConstr _     = fromListConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  dataTypeOf _   = setDataType
  dataCast1 f    = gcast1 f

fromListConstr :: Constr
fromListConstr = mkConstr setDataType "fromList" [] Prefix

setDataType :: DataType
setDataType = mkDataType "Data.Set.Internal.Set" [fromListConstr]

#endif

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | \(O(1)\). Is this the empty set?
null :: Set a -> Bool
null Tip      = True
null (Bin {}) = False
{-# INLINE null #-}

-- | \(O(1)\). The number of elements in the set.
size :: Set a -> Int
size Tip = 0
size (Bin sz _ _ _) = sz
{-# INLINE size #-}

-- | \(O(\log n)\). Is the element in the set?
member :: Ord a => a -> Set a -> Bool
member = go
  where
    go !_ Tip = False
    go x (Bin _ y l r) = case compare x y of
      LT -> go x l
      GT -> go x r
      EQ -> True
#if __GLASGOW_HASKELL__
{-# INLINABLE member #-}
#else
{-# INLINE member #-}
#endif

-- | \(O(\log n)\). Is the element not in the set?
notMember :: Ord a => a -> Set a -> Bool
notMember a t = not $ member a t
#if __GLASGOW_HASKELL__
{-# INLINABLE notMember #-}
#else
{-# INLINE notMember #-}
#endif

-- | \(O(\log n)\). Find largest element smaller than the given one.
--
-- > lookupLT 3 (fromList [3, 5]) == Nothing
-- > lookupLT 5 (fromList [3, 5]) == Just 3
lookupLT :: Ord a => a -> Set a -> Maybe a
lookupLT = goNothing
  where
    goNothing !_ Tip = Nothing
    goNothing x (Bin _ y l r) | x <= y = goNothing x l
                              | otherwise = goJust x y r

    goJust !_ best Tip = Just best
    goJust x best (Bin _ y l r) | x <= y = goJust x best l
                                | otherwise = goJust x y r
#if __GLASGOW_HASKELL__
{-# INLINABLE lookupLT #-}
#else
{-# INLINE lookupLT #-}
#endif

-- | \(O(\log n)\). Find smallest element greater than the given one.
--
-- > lookupGT 4 (fromList [3, 5]) == Just 5
-- > lookupGT 5 (fromList [3, 5]) == Nothing
lookupGT :: Ord a => a -> Set a -> Maybe a
lookupGT = goNothing
  where
    goNothing !_ Tip = Nothing
    goNothing x (Bin _ y l r) | x < y = goJust x y l
                              | otherwise = goNothing x r

    goJust !_ best Tip = Just best
    goJust x best (Bin _ y l r) | x < y = goJust x y l
                                | otherwise = goJust x best r
#if __GLASGOW_HASKELL__
{-# INLINABLE lookupGT #-}
#else
{-# INLINE lookupGT #-}
#endif

-- | \(O(\log n)\). Find largest element smaller or equal to the given one.
--
-- > lookupLE 2 (fromList [3, 5]) == Nothing
-- > lookupLE 4 (fromList [3, 5]) == Just 3
-- > lookupLE 5 (fromList [3, 5]) == Just 5
lookupLE :: Ord a => a -> Set a -> Maybe a
lookupLE = goNothing
  where
    goNothing !_ Tip = Nothing
    goNothing x (Bin _ y l r) = case compare x y of LT -> goNothing x l
                                                    EQ -> Just y
                                                    GT -> goJust x y r

    goJust !_ best Tip = Just best
    goJust x best (Bin _ y l r) = case compare x y of LT -> goJust x best l
                                                      EQ -> Just y
                                                      GT -> goJust x y r
#if __GLASGOW_HASKELL__
{-# INLINABLE lookupLE #-}
#else
{-# INLINE lookupLE #-}
#endif

-- | \(O(\log n)\). Find smallest element greater or equal to the given one.
--
-- > lookupGE 3 (fromList [3, 5]) == Just 3
-- > lookupGE 4 (fromList [3, 5]) == Just 5
-- > lookupGE 6 (fromList [3, 5]) == Nothing
lookupGE :: Ord a => a -> Set a -> Maybe a
lookupGE = goNothing
  where
    goNothing !_ Tip = Nothing
    goNothing x (Bin _ y l r) = case compare x y of LT -> goJust x y l
                                                    EQ -> Just y
                                                    GT -> goNothing x r

    goJust !_ best Tip = Just best
    goJust x best (Bin _ y l r) = case compare x y of LT -> goJust x y l
                                                      EQ -> Just y
                                                      GT -> goJust x best r
#if __GLASGOW_HASKELL__
{-# INLINABLE lookupGE #-}
#else
{-# INLINE lookupGE #-}
#endif

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | \(O(1)\). The empty set.
empty  :: Set a
empty = Tip
{-# INLINE empty #-}

-- | \(O(1)\). Create a singleton set.
singleton :: a -> Set a
singleton x = Bin 1 x Tip Tip
{-# INLINE singleton #-}

{--------------------------------------------------------------------
  Insertion, Deletion
--------------------------------------------------------------------}
-- | \(O(\log n)\). Insert an element in a set.
-- If the set already contains an element equal to the given value,
-- it is replaced with the new value.

-- See Note: Type of local 'go' function
-- See Note: Avoiding worker/wrapper (in Data.Map.Internal)
insert :: Ord a => a -> Set a -> Set a
insert x0 = go x0 x0
  where
    go :: Ord a => a -> a -> Set a -> Set a
    go orig !_ Tip = singleton (lazy orig)
    go orig !x t@(Bin sz y l r) = case compare x y of
        LT | l' `ptrEq` l -> t
           | otherwise -> balanceL y l' r
           where !l' = go orig x l
        GT | r' `ptrEq` r -> t
           | otherwise -> balanceR y l r'
           where !r' = go orig x r
        EQ | lazy orig `seq` (orig `ptrEq` y) -> t
           | otherwise -> Bin sz (lazy orig) l r
#if __GLASGOW_HASKELL__
{-# INLINABLE insert #-}
#else
{-# INLINE insert #-}
#endif

#ifndef __GLASGOW_HASKELL__
lazy :: a -> a
lazy a = a
#endif

-- Insert an element to the set only if it is not in the set.
-- Used by `union`.

-- See Note: Type of local 'go' function
-- See Note: Avoiding worker/wrapper (in Data.Map.Internal)
insertR :: Ord a => a -> Set a -> Set a
insertR x0 = go x0 x0
  where
    go :: Ord a => a -> a -> Set a -> Set a
    go orig !_ Tip = singleton (lazy orig)
    go orig !x t@(Bin _ y l r) = case compare x y of
        LT | l' `ptrEq` l -> t
           | otherwise -> balanceL y l' r
           where !l' = go orig x l
        GT | r' `ptrEq` r -> t
           | otherwise -> balanceR y l r'
           where !r' = go orig x r
        EQ -> t
#if __GLASGOW_HASKELL__
{-# INLINABLE insertR #-}
#else
{-# INLINE insertR #-}
#endif

-- | \(O(\log n)\). Delete an element from a set.

-- See Note: Type of local 'go' function
delete :: Ord a => a -> Set a -> Set a
delete = go
  where
    go :: Ord a => a -> Set a -> Set a
    go !_ Tip = Tip
    go x t@(Bin _ y l r) = case compare x y of
        LT | l' `ptrEq` l -> t
           | otherwise -> balanceR y l' r
           where !l' = go x l
        GT | r' `ptrEq` r -> t
           | otherwise -> balanceL y l r'
           where !r' = go x r
        EQ -> glue l r
#if __GLASGOW_HASKELL__
{-# INLINABLE delete #-}
#else
{-# INLINE delete #-}
#endif

-- | \(O(\log n)\) @('alterF' f x s)@ can delete or insert @x@ in @s@ depending on
-- whether an equal element is found in @s@.
--
-- In short:
--
-- @
-- 'member' x \<$\> 'alterF' f x s = f ('member' x s)
-- @
--
-- Note that unlike 'insert', 'alterF' will /not/ replace an element equal to
-- the given value.
--
-- Note: 'alterF' is a variant of the @at@ combinator from "Control.Lens.At".
--
-- @since 0.6.3.1
alterF :: (Ord a, Functor f) => (Bool -> f Bool) -> a -> Set a -> f (Set a)
alterF f k s = fmap choose (f member_)
  where
    (member_, inserted, deleted) = case alteredSet k s of
        Deleted d           -> (True , s, d)
        Inserted i          -> (False, i, s)

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

{-# SPECIALIZE alterF :: Ord a => (Bool -> Identity Bool) -> a -> Set a -> Identity (Set a) #-}

data AlteredSet a
      -- | The needle is present in the original set.
      -- We return the set where the needle is deleted.
    = Deleted !(Set a)

      -- | The needle is not present in the original set.
      -- We return the set with the needle inserted.
    | Inserted !(Set a)

alteredSet :: Ord a => a -> Set a -> AlteredSet a
alteredSet x0 s0 = go x0 s0
  where
    go :: Ord a => a -> Set a -> AlteredSet a
    go x Tip           = Inserted (singleton x)
    go x (Bin _ y l r) = case compare x y of
        LT -> case go x l of
            Deleted d           -> Deleted (balanceR y d r)
            Inserted i          -> Inserted (balanceL y i r)
        GT -> case go x r of
            Deleted d           -> Deleted (balanceL y l d)
            Inserted i          -> Inserted (balanceR y l i)
        EQ -> Deleted (glue l r)
#if __GLASGOW_HASKELL__
{-# INLINABLE alteredSet #-}
#else
{-# INLINE alteredSet #-}
#endif

{--------------------------------------------------------------------
  Subset
--------------------------------------------------------------------}
-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\).
-- @(s1 \`isProperSubsetOf\` s2)@ indicates whether @s1@ is a
-- proper subset of @s2@.
--
-- @
-- s1 \`isProperSubsetOf\` s2 = s1 ``isSubsetOf`` s2 && s1 /= s2
-- @
isProperSubsetOf :: Ord a => Set a -> Set a -> Bool
isProperSubsetOf s1 s2
    = size s1 < size s2 && isSubsetOfX s1 s2
#if __GLASGOW_HASKELL__
{-# INLINABLE isProperSubsetOf #-}
#endif


-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\).
-- @(s1 \`isSubsetOf\` s2)@ indicates whether @s1@ is a subset of @s2@.
--
-- @
-- s1 \`isSubsetOf\` s2 = all (``member`` s2) s1
-- s1 \`isSubsetOf\` s2 = null (s1 ``difference`` s2)
-- s1 \`isSubsetOf\` s2 = s1 ``union`` s2 == s2
-- s1 \`isSubsetOf\` s2 = s1 ``intersection`` s2 == s1
-- @
isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf t1 t2
  = size t1 <= size t2 && isSubsetOfX t1 t2
#if __GLASGOW_HASKELL__
{-# INLINABLE isSubsetOf #-}
#endif

-- Test whether a set is a subset of another without the *initial*
-- size test.
--
-- This function is structured very much like `difference`, `union`,
-- and `intersection`. Whereas the bounds proofs for those in Blelloch
-- et al needed to account for both "split work" and "merge work", we
-- only have to worry about split work here, which is the same as in
-- those functions.
isSubsetOfX :: Ord a => Set a -> Set a -> Bool
isSubsetOfX Tip _ = True
isSubsetOfX _ Tip = False
-- Skip the final split when we hit a singleton.
isSubsetOfX (Bin 1 x _ _) t = member x t
isSubsetOfX (Bin _ x l r) t
  = found &&
    -- Cheap size checks can sometimes save expensive recursive calls when the
    -- result will be False. Suppose we check whether [1..10] (with root 4) is
    -- a subset of [0..9]. After the first split, we have to check if [1..3] is
    -- a subset of [0..3] and if [5..10] is a subset of [5..9]. But we can bail
    -- immediately because size [5..10] > size [5..9].
    --
    -- Why not just call `isSubsetOf` on each side to do the size checks?
    -- Because that could make a recursive call on the left even though the
    -- size check would fail on the right. In principle, we could take this to
    -- extremes by maintaining a queue of pairs of sets to be checked, working
    -- through the tree level-wise. But that would impose higher administrative
    -- costs without obvious benefits. It might be worth considering if we find
    -- a way to use it to tighten the bounds in some useful/comprehensible way.
    size l <= size lt && size r <= size gt &&
    isSubsetOfX l lt && isSubsetOfX r gt
  where
    (lt,found,gt) = splitMember x t
#if __GLASGOW_HASKELL__
{-# INLINABLE isSubsetOfX #-}
#endif

{--------------------------------------------------------------------
  Disjoint
--------------------------------------------------------------------}
-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\). Check whether two sets are disjoint
-- (i.e., their intersection is empty).
--
-- > disjoint (fromList [2,4,6])   (fromList [1,3])     == True
-- > disjoint (fromList [2,4,6,8]) (fromList [2,3,5,7]) == False
-- > disjoint (fromList [1,2])     (fromList [1,2,3,4]) == False
-- > disjoint (fromList [])        (fromList [])        == True
--
-- @
-- xs ``disjoint`` ys = null (xs ``intersection`` ys)
-- @
--
-- @since 0.5.11

disjoint :: Ord a => Set a -> Set a -> Bool
disjoint Tip _ = True
disjoint _ Tip = True
-- Avoid a split for the singleton case.
disjoint (Bin 1 x _ _) t = x `notMember` t
disjoint (Bin _ x l r) t
  -- Analogous implementation to `subsetOfX`
  = not found && disjoint l lt && disjoint r gt
  where
    (lt,found,gt) = splitMember x t

{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}

-- We perform call-pattern specialization manually on lookupMin
-- and lookupMax. Otherwise, GHC doesn't seem to do it, which is
-- unfortunate if, for example, someone uses findMin or findMax.

lookupMinSure :: a -> Set a -> a
lookupMinSure x Tip = x
lookupMinSure _ (Bin _ x l _) = lookupMinSure x l

-- | \(O(\log n)\). The minimal element of a set.
--
-- @since 0.5.9

lookupMin :: Set a -> Maybe a
lookupMin Tip = Nothing
lookupMin (Bin _ x l _) = Just $! lookupMinSure x l

-- | \(O(\log n)\). The minimal element of a set.
findMin :: Set a -> a
findMin t
  | Just r <- lookupMin t = r
  | otherwise = error "Set.findMin: empty set has no minimal element"

lookupMaxSure :: a -> Set a -> a
lookupMaxSure x Tip = x
lookupMaxSure _ (Bin _ x _ r) = lookupMaxSure x r

-- | \(O(\log n)\). The maximal element of a set.
--
-- @since 0.5.9

lookupMax :: Set a -> Maybe a
lookupMax Tip = Nothing
lookupMax (Bin _ x _ r) = Just $! lookupMaxSure x r

-- | \(O(\log n)\). The maximal element of a set.
findMax :: Set a -> a
findMax t
  | Just r <- lookupMax t = r
  | otherwise = error "Set.findMax: empty set has no maximal element"

-- | \(O(\log n)\). Delete the minimal element. Returns an empty set if the set is empty.
deleteMin :: Set a -> Set a
deleteMin (Bin _ _ Tip r) = r
deleteMin (Bin _ x l r)   = balanceR x (deleteMin l) r
deleteMin Tip             = Tip

-- | \(O(\log n)\). Delete the maximal element. Returns an empty set if the set is empty.
deleteMax :: Set a -> Set a
deleteMax (Bin _ _ l Tip) = l
deleteMax (Bin _ x l r)   = balanceL x l (deleteMax r)
deleteMax Tip             = Tip

{--------------------------------------------------------------------
  Union.
--------------------------------------------------------------------}
-- | The union of the sets in a Foldable structure : (@'unions' == 'foldl' 'union' 'empty'@).
unions :: (Foldable f, Ord a) => f (Set a) -> Set a
unions = Foldable.foldl' union empty
#if __GLASGOW_HASKELL__
{-# INLINABLE unions #-}
#endif

-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\). The union of two sets, preferring the first set when
-- equal elements are encountered.
union :: Ord a => Set a -> Set a -> Set a
union t1 Tip  = t1
union t1 (Bin 1 x _ _) = insertR x t1
union (Bin 1 x _ _) t2 = insert x t2
union Tip t2  = t2
union t1@(Bin _ x l1 r1) t2 = case splitS x t2 of
  (l2 :*: r2)
    | l1l2 `ptrEq` l1 && r1r2 `ptrEq` r1 -> t1
    | otherwise -> link x l1l2 r1r2
    where !l1l2 = union l1 l2
          !r1r2 = union r1 r2
#if __GLASGOW_HASKELL__
{-# INLINABLE union #-}
#endif

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\). Difference of two sets.
--
-- Return elements of the first set not existing in the second set.
--
-- > difference (fromList [5, 3]) (fromList [5, 7]) == singleton 3
difference :: Ord a => Set a -> Set a -> Set a
difference Tip _   = Tip
difference t1 Tip  = t1
difference t1 (Bin _ x l2 r2) = case split x t1 of
   (l1, r1)
     | size l1l2 + size r1r2 == size t1 -> t1
     | otherwise -> merge l1l2 r1r2
     where !l1l2 = difference l1 l2
           !r1r2 = difference r1 r2
#if __GLASGOW_HASKELL__
{-# INLINABLE difference #-}
#endif

{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr), \; 0 < m \leq n\). The intersection of two sets.
-- Elements of the result come from the first set, so for example
--
-- > import qualified Data.Set as S
-- > data AB = A | B deriving Show
-- > instance Ord AB where compare _ _ = EQ
-- > instance Eq AB where _ == _ = True
-- > main = print (S.singleton A `S.intersection` S.singleton B,
-- >               S.singleton B `S.intersection` S.singleton A)
--
-- prints @(fromList [A],fromList [B])@.
intersection :: Ord a => Set a -> Set a -> Set a
intersection Tip _ = Tip
intersection _ Tip = Tip
intersection t1@(Bin _ x l1 r1) t2
  | b = if l1l2 `ptrEq` l1 && r1r2 `ptrEq` r1
        then t1
        else link x l1l2 r1r2
  | otherwise = merge l1l2 r1r2
  where
    !(l2, b, r2) = splitMember x t2
    !l1l2 = intersection l1 l2
    !r1r2 = intersection r1 r2
#if __GLASGOW_HASKELL__
{-# INLINABLE intersection #-}
#endif

-- | The intersection of a series of sets. Intersections are performed left-to-right.
intersections :: Ord a => NonEmpty (Set a) -> Set a
intersections (s0 :| ss) = List.foldr go id ss s0
    where
      go s r acc
          | null acc = empty
          | otherwise = r (intersection acc s)

-- | Sets form a 'Semigroup' under 'intersection'.
newtype Intersection a = Intersection { getIntersection :: Set a }
    deriving (Show, Eq, Ord)

instance (Ord a) => Semigroup (Intersection a) where
    (Intersection a) <> (Intersection b) = Intersection $ intersection a b
    stimes = stimesIdempotent

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}
-- | \(O(n)\). Filter all elements that satisfy the predicate.
filter :: (a -> Bool) -> Set a -> Set a
filter _ Tip = Tip
filter p t@(Bin _ x l r)
    | p x = if l `ptrEq` l' && r `ptrEq` r'
            then t
            else link x l' r'
    | otherwise = merge l' r'
    where
      !l' = filter p l
      !r' = filter p r

-- | \(O(n)\). Partition the set into two sets, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
-- See also 'split'.
partition :: (a -> Bool) -> Set a -> (Set a,Set a)
partition p0 t0 = toPair $ go p0 t0
  where
    go _ Tip = (Tip :*: Tip)
    go p t@(Bin _ x l r) = case (go p l, go p r) of
      ((l1 :*: l2), (r1 :*: r2))
        | p x       -> (if l1 `ptrEq` l && r1 `ptrEq` r
                        then t
                        else link x l1 r1) :*: merge l2 r2
        | otherwise -> merge l1 r1 :*:
                       (if l2 `ptrEq` l && r2 `ptrEq` r
                        then t
                        else link x l2 r2)

{----------------------------------------------------------------------
  Map
----------------------------------------------------------------------}

-- | \(O(n \log n)\).
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@

map :: Ord b => (a->b) -> Set a -> Set b
map f = fromList . List.map f . toList
#if __GLASGOW_HASKELL__
{-# INLINABLE map #-}
#endif

-- | \(O(n)\). The
--
-- @'mapMonotonic' f s == 'map' f s@, but works only when @f@ is strictly increasing.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapMonotonic f s == map f s
-- >     where ls = toList s

mapMonotonic :: (a->b) -> Set a -> Set b
mapMonotonic _ Tip = Tip
mapMonotonic f (Bin sz x l r) = Bin sz (f x) (mapMonotonic f l) (mapMonotonic f r)

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | \(O(n)\). Fold the elements in the set using the given right-associative
-- binary operator. This function is an equivalent of 'foldr' and is present
-- for compatibility only.
--
-- /Please note that fold will be deprecated in the future and removed./
fold :: (a -> b -> b) -> b -> Set a -> b
fold = foldr
{-# INLINE fold #-}

-- | \(O(n)\). Fold the elements in the set using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'toAscList'@.
--
-- For example,
--
-- > toAscList set = foldr (:) [] set
foldr :: (a -> b -> b) -> b -> Set a -> b
foldr f z = go z
  where
    go z' Tip           = z'
    go z' (Bin _ x l r) = go (f x (go z' r)) l
{-# INLINE foldr #-}

-- | \(O(n)\). A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> Set a -> b
foldr' f z = go z
  where
    go !z' Tip           = z'
    go z' (Bin _ x l r) = go (f x $! go z' r) l
{-# INLINE foldr' #-}

-- | \(O(n)\). Fold the elements in the set using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'toAscList'@.
--
-- For example,
--
-- > toDescList set = foldl (flip (:)) [] set
foldl :: (a -> b -> a) -> a -> Set b -> a
foldl f z = go z
  where
    go z' Tip           = z'
    go z' (Bin _ x l r) = go (f (go z' l) x) r
{-# INLINE foldl #-}

-- | \(O(n)\). A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> b -> a) -> a -> Set b -> a
foldl' f z = go z
  where
    go !z' Tip           = z'
    go z' (Bin _ x l r) =
      let !z'' = go z' l
      in go (f z'' x) r
{-# INLINE foldl' #-}

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | \(O(n)\). An alias of 'toAscList'. The elements of a set in ascending order.
-- Subject to list fusion.
elems :: Set a -> [a]
elems = toAscList

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

#ifdef __GLASGOW_HASKELL__
-- | @since 0.5.6.2
instance (Ord a) => GHCExts.IsList (Set a) where
  type Item (Set a) = a
  fromList = fromList
  toList   = toList
#endif

-- | \(O(n)\). Convert the set to a list of elements. Subject to list fusion.
toList :: Set a -> [a]
toList = toAscList

-- | \(O(n)\). Convert the set to an ascending list of elements. Subject to list fusion.
toAscList :: Set a -> [a]
toAscList = foldr (:) []

-- | \(O(n)\). Convert the set to a descending list of elements. Subject to list
-- fusion.
toDescList :: Set a -> [a]
toDescList = foldl (flip (:)) []

-- List fusion for the list generating functions.
#if __GLASGOW_HASKELL__
-- The foldrFB and foldlFB are foldr and foldl equivalents, used for list fusion.
-- They are important to convert unfused to{Asc,Desc}List back, see mapFB in prelude.
foldrFB :: (a -> b -> b) -> b -> Set a -> b
foldrFB = foldr
{-# INLINE[0] foldrFB #-}
foldlFB :: (a -> b -> a) -> a -> Set b -> a
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
{-# RULES "Set.toAscList" [~1] forall s . toAscList s = build (\c n -> foldrFB c n s) #-}
{-# RULES "Set.toAscListBack" [1] foldrFB (:) [] = toAscList #-}
{-# RULES "Set.toDescList" [~1] forall s . toDescList s = build (\c n -> foldlFB (\xs x -> c x xs) n s) #-}
{-# RULES "Set.toDescListBack" [1] foldlFB (\xs x -> x : xs) [] = toDescList #-}
#endif

-- | \(O(n \log n)\). Create a set from a list of elements.
--
-- If the elements are ordered, a linear-time implementation is used.

-- For some reason, when 'singleton' is used in fromList or in
-- create, it is not inlined, so we inline it manually.
fromList :: Ord a => [a] -> Set a
fromList [] = Tip
fromList [x] = Bin 1 x Tip Tip
fromList (x0 : xs0) | not_ordered x0 xs0 = fromList' (Bin 1 x0 Tip Tip) xs0
                    | otherwise = go (1::Int) (Bin 1 x0 Tip Tip) xs0
  where
    not_ordered _ [] = False
    not_ordered x (y : _) = x >= y
    {-# INLINE not_ordered #-}

    fromList' t0 xs = Foldable.foldl' ins t0 xs
      where ins t x = insert x t

    go !_ t [] = t
    go _ t [x] = insertMax x t
    go s l xs@(x : xss) | not_ordered x xss = fromList' l xs
                        | otherwise = case create s xss of
                            (r, ys, []) -> go (s `shiftL` 1) (link x l r) ys
                            (r, _,  ys) -> fromList' (link x l r) ys

    -- The create is returning a triple (tree, xs, ys). Both xs and ys
    -- represent not yet processed elements and only one of them can be nonempty.
    -- If ys is nonempty, the keys in ys are not ordered with respect to tree
    -- and must be inserted using fromList'. Otherwise the keys have been
    -- ordered so far.
    create !_ [] = (Tip, [], [])
    create s xs@(x : xss)
      | s == 1 = if not_ordered x xss then (Bin 1 x Tip Tip, [], xss)
                                      else (Bin 1 x Tip Tip, xss, [])
      | otherwise = case create (s `shiftR` 1) xs of
                      res@(_, [], _) -> res
                      (l, [y], zs) -> (insertMax y l, [], zs)
                      (l, ys@(y:yss), _) | not_ordered y yss -> (l, [], ys)
                                         | otherwise -> case create (s `shiftR` 1) yss of
                                                   (r, zs, ws) -> (link y l r, zs, ws)
#if __GLASGOW_HASKELL__
{-# INLINABLE fromList #-}
#endif

{--------------------------------------------------------------------
  Building trees from ascending/descending lists can be done in linear time.

  Note that if [xs] is ascending that:
    fromAscList xs == fromList xs
--------------------------------------------------------------------}
-- | \(O(n)\). Build a set from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: Eq a => [a] -> Set a
fromAscList xs = fromDistinctAscList (combineEq xs)
#if __GLASGOW_HASKELL__
{-# INLINABLE fromAscList #-}
#endif

-- | \(O(n)\). Build a set from a descending list in linear time.
-- /The precondition (input list is descending) is not checked./
--
-- @since 0.5.8
fromDescList :: Eq a => [a] -> Set a
fromDescList xs = fromDistinctDescList (combineEq xs)
#if __GLASGOW_HASKELL__
{-# INLINABLE fromDescList #-}
#endif

-- [combineEq xs] combines equal elements with [const] in an ordered list [xs]
--
-- TODO: combineEq allocates an intermediate list. It *should* be better to
-- make fromAscListBy and fromDescListBy the fundamental operations, and to
-- implement the rest using those.
combineEq :: Eq a => [a] -> [a]
combineEq [] = []
combineEq (x : xs) = combineEq' x xs
  where
    combineEq' z [] = [z]
    combineEq' z (y:ys)
      | z == y = combineEq' z ys
      | otherwise = z : combineEq' y ys

-- | \(O(n)\). Build a set from an ascending list of distinct elements in linear time.
-- /The precondition (input list is strictly ascending) is not checked./

-- For some reason, when 'singleton' is used in fromDistinctAscList or in
-- create, it is not inlined, so we inline it manually.

-- See Note [fromDistinctAscList implementation]
fromDistinctAscList :: [a] -> Set a
fromDistinctAscList = fromDistinctAscList_linkAll . Foldable.foldl' next (State0 Nada)
  where
    next :: FromDistinctMonoState a -> a -> FromDistinctMonoState a
    next (State0 stk) !x = fromDistinctAscList_linkTop (Bin 1 x Tip Tip) stk
    next (State1 l stk) x = State0 (Push x l stk)
{-# INLINE fromDistinctAscList #-}  -- INLINE for fusion

fromDistinctAscList_linkTop :: Set a -> Stack a -> FromDistinctMonoState a
fromDistinctAscList_linkTop r@(Bin rsz _ _ _) (Push x l@(Bin lsz _ _ _) stk)
  | rsz == lsz = fromDistinctAscList_linkTop (bin x l r) stk
fromDistinctAscList_linkTop l stk = State1 l stk
{-# INLINABLE fromDistinctAscList_linkTop #-}

fromDistinctAscList_linkAll :: FromDistinctMonoState a -> Set a
fromDistinctAscList_linkAll (State0 stk)    = foldl'Stack (\r x l -> link x l r) Tip stk
fromDistinctAscList_linkAll (State1 r0 stk) = foldl'Stack (\r x l -> link x l r) r0 stk
{-# INLINABLE fromDistinctAscList_linkAll #-}

-- | \(O(n)\). Build a set from a descending list of distinct elements in linear time.
-- /The precondition (input list is strictly descending) is not checked./
--
-- @since 0.5.8

-- For some reason, when 'singleton' is used in fromDistinctDescList or in
-- create, it is not inlined, so we inline it manually.

-- See Note [fromDistinctAscList implementation]
fromDistinctDescList :: [a] -> Set a
fromDistinctDescList = fromDistinctDescList_linkAll . Foldable.foldl' next (State0 Nada)
  where
    next :: FromDistinctMonoState a -> a -> FromDistinctMonoState a
    next (State0 stk) !x = fromDistinctDescList_linkTop (Bin 1 x Tip Tip) stk
    next (State1 r stk) x = State0 (Push x r stk)
{-# INLINE fromDistinctDescList #-}  -- INLINE for fusion

fromDistinctDescList_linkTop :: Set a -> Stack a -> FromDistinctMonoState a
fromDistinctDescList_linkTop l@(Bin lsz _ _ _) (Push x r@(Bin rsz _ _ _) stk)
  | lsz == rsz = fromDistinctDescList_linkTop (bin x l r) stk
fromDistinctDescList_linkTop r stk = State1 r stk
{-# INLINABLE fromDistinctDescList_linkTop #-}

fromDistinctDescList_linkAll :: FromDistinctMonoState a -> Set a
fromDistinctDescList_linkAll (State0 stk)    = foldl'Stack (\l x r -> link x l r) Tip stk
fromDistinctDescList_linkAll (State1 l0 stk) = foldl'Stack (\l x r -> link x l r) l0 stk
{-# INLINABLE fromDistinctDescList_linkAll #-}

data FromDistinctMonoState a
  = State0 !(Stack a)
  | State1 !(Set a) !(Stack a)

data Stack a = Push !a !(Set a) !(Stack a) | Nada

foldl'Stack :: (b -> a -> Set a -> b) -> b -> Stack a -> b
foldl'Stack f = go
  where
    go !z Nada = z
    go z (Push x t stk) = go (f z x t) stk
{-# INLINE foldl'Stack #-}

{--------------------------------------------------------------------
  Eq converts the set to a list. In a lazy setting, this
  actually seems one of the faster methods to compare two trees
  and it is certainly the simplest :-)
--------------------------------------------------------------------}
instance Eq a => Eq (Set a) where
  t1 == t2  = (size t1 == size t2) && (toAscList t1 == toAscList t2)

{--------------------------------------------------------------------
  Ord
--------------------------------------------------------------------}

instance Ord a => Ord (Set a) where
    compare s1 s2 = compare (toAscList s1) (toAscList s2)

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance Show a => Show (Set a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (toList xs)

-- | @since 0.5.9
instance Eq1 Set where
    liftEq eq m n =
        size m == size n && liftEq eq (toList m) (toList n)

-- | @since 0.5.9
instance Ord1 Set where
    liftCompare cmp m n =
        liftCompare cmp (toList m) (toList n)

-- | @since 0.5.9
instance Show1 Set where
    liftShowsPrec sp sl d m =
        showsUnaryWith (liftShowsPrec sp sl) "fromList" d (toList m)

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance (Read a, Ord a) => Read (Set a) where
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

instance NFData a => NFData (Set a) where
    rnf Tip           = ()
    rnf (Bin _ y l r) = rnf y `seq` rnf l `seq` rnf r

{--------------------------------------------------------------------
  Split
--------------------------------------------------------------------}
-- | \(O(\log n)\). The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
split :: Ord a => a -> Set a -> (Set a,Set a)
split x t = toPair $ splitS x t
{-# INLINABLE split #-}

splitS :: Ord a => a -> Set a -> StrictPair (Set a) (Set a)
splitS _ Tip = (Tip :*: Tip)
splitS x (Bin _ y l r)
      = case compare x y of
          LT -> let (lt :*: gt) = splitS x l in (lt :*: link y gt r)
          GT -> let (lt :*: gt) = splitS x r in (link y l lt :*: gt)
          EQ -> (l :*: r)
{-# INLINABLE splitS #-}

-- | \(O(\log n)\). Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
splitMember :: Ord a => a -> Set a -> (Set a,Bool,Set a)
splitMember _ Tip = (Tip, False, Tip)
splitMember x (Bin _ y l r)
   = case compare x y of
       LT -> let (lt, found, gt) = splitMember x l
                 !gt' = link y gt r
             in (lt, found, gt')
       GT -> let (lt, found, gt) = splitMember x r
                 !lt' = link y l lt
             in (lt', found, gt)
       EQ -> (l, True, r)
#if __GLASGOW_HASKELL__
{-# INLINABLE splitMember #-}
#endif

{--------------------------------------------------------------------
  Indexing
--------------------------------------------------------------------}

-- | \(O(\log n)\). Return the /index/ of an element, which is its zero-based
-- index in the sorted sequence of elements. The index is a number from /0/ up
-- to, but not including, the 'size' of the set. Calls 'error' when the element
-- is not a 'member' of the set.
--
-- > findIndex 2 (fromList [5,3])    Error: element is not in the set
-- > findIndex 3 (fromList [5,3]) == 0
-- > findIndex 5 (fromList [5,3]) == 1
-- > findIndex 6 (fromList [5,3])    Error: element is not in the set
--
-- @since 0.5.4

-- See Note: Type of local 'go' function
findIndex :: Ord a => a -> Set a -> Int
findIndex = go 0
  where
    go :: Ord a => Int -> a -> Set a -> Int
    go !_ !_ Tip  = error "Set.findIndex: element is not in the set"
    go idx x (Bin _ kx l r) = case compare x kx of
      LT -> go idx x l
      GT -> go (idx + size l + 1) x r
      EQ -> idx + size l
#if __GLASGOW_HASKELL__
{-# INLINABLE findIndex #-}
#endif

-- | \(O(\log n)\). Lookup the /index/ of an element, which is its zero-based index in
-- the sorted sequence of elements. The index is a number from /0/ up to, but not
-- including, the 'size' of the set.
--
-- > isJust   (lookupIndex 2 (fromList [5,3])) == False
-- > fromJust (lookupIndex 3 (fromList [5,3])) == 0
-- > fromJust (lookupIndex 5 (fromList [5,3])) == 1
-- > isJust   (lookupIndex 6 (fromList [5,3])) == False
--
-- @since 0.5.4

-- See Note: Type of local 'go' function
lookupIndex :: Ord a => a -> Set a -> Maybe Int
lookupIndex = go 0
  where
    go :: Ord a => Int -> a -> Set a -> Maybe Int
    go !_ !_ Tip  = Nothing
    go idx x (Bin _ kx l r) = case compare x kx of
      LT -> go idx x l
      GT -> go (idx + size l + 1) x r
      EQ -> Just $! idx + size l
#if __GLASGOW_HASKELL__
{-# INLINABLE lookupIndex #-}
#endif

-- | \(O(\log n)\). Retrieve an element by its /index/, i.e. by its zero-based
-- index in the sorted sequence of elements. If the /index/ is out of range (less
-- than zero, greater or equal to 'size' of the set), 'error' is called.
--
-- > elemAt 0 (fromList [5,3]) == 3
-- > elemAt 1 (fromList [5,3]) == 5
-- > elemAt 2 (fromList [5,3])    Error: index out of range
--
-- @since 0.5.4

elemAt :: Int -> Set a -> a
elemAt !_ Tip = error "Set.elemAt: index out of range"
elemAt i (Bin _ x l r)
  = case compare i sizeL of
      LT -> elemAt i l
      GT -> elemAt (i-sizeL-1) r
      EQ -> x
  where
    sizeL = size l

-- | \(O(\log n)\). Delete the element at /index/, i.e. by its zero-based index in
-- the sorted sequence of elements. If the /index/ is out of range (less than zero,
-- greater or equal to 'size' of the set), 'error' is called.
--
-- > deleteAt 0    (fromList [5,3]) == singleton 5
-- > deleteAt 1    (fromList [5,3]) == singleton 3
-- > deleteAt 2    (fromList [5,3])    Error: index out of range
-- > deleteAt (-1) (fromList [5,3])    Error: index out of range
--
-- @since 0.5.4

deleteAt :: Int -> Set a -> Set a
deleteAt !i t =
  case t of
    Tip -> error "Set.deleteAt: index out of range"
    Bin _ x l r -> case compare i sizeL of
      LT -> balanceR x (deleteAt i l) r
      GT -> balanceL x l (deleteAt (i-sizeL-1) r)
      EQ -> glue l r
      where
        sizeL = size l

-- | \(O(\log n)\). Take a given number of elements in order, beginning
-- with the smallest ones.
--
-- @
-- take n = 'fromDistinctAscList' . 'Prelude.take' n . 'toAscList'
-- @
--
-- @since 0.5.8
take :: Int -> Set a -> Set a
take i m | i >= size m = m
take i0 m0 = go i0 m0
  where
    go i !_ | i <= 0 = Tip
    go !_ Tip = Tip
    go i (Bin _ x l r) =
      case compare i sizeL of
        LT -> go i l
        GT -> link x l (go (i - sizeL - 1) r)
        EQ -> l
      where sizeL = size l

-- | \(O(\log n)\). Drop a given number of elements in order, beginning
-- with the smallest ones.
--
-- @
-- drop n = 'fromDistinctAscList' . 'Prelude.drop' n . 'toAscList'
-- @
--
-- @since 0.5.8
drop :: Int -> Set a -> Set a
drop i m | i >= size m = Tip
drop i0 m0 = go i0 m0
  where
    go i m | i <= 0 = m
    go !_ Tip = Tip
    go i (Bin _ x l r) =
      case compare i sizeL of
        LT -> link x (go i l) r
        GT -> go (i - sizeL - 1) r
        EQ -> insertMin x r
      where sizeL = size l

-- | \(O(\log n)\). Split a set at a particular index.
--
-- @
-- splitAt !n !xs = ('take' n xs, 'drop' n xs)
-- @
splitAt :: Int -> Set a -> (Set a, Set a)
splitAt i0 m0
  | i0 >= size m0 = (m0, Tip)
  | otherwise = toPair $ go i0 m0
  where
    go i m | i <= 0 = Tip :*: m
    go !_ Tip = Tip :*: Tip
    go i (Bin _ x l r)
      = case compare i sizeL of
          LT -> case go i l of
                  ll :*: lr -> ll :*: link x lr r
          GT -> case go (i - sizeL - 1) r of
                  rl :*: rr -> link x l rl :*: rr
          EQ -> l :*: insertMin x r
      where sizeL = size l

-- | \(O(\log n)\). Take while a predicate on the elements holds.
-- The user is responsible for ensuring that for all elements @j@ and @k@ in the set,
-- @j \< k ==\> p j \>= p k@. See note at 'spanAntitone'.
--
-- @
-- takeWhileAntitone p = 'fromDistinctAscList' . 'Data.List.takeWhile' p . 'toList'
-- takeWhileAntitone p = 'filter' p
-- @
--
-- @since 0.5.8

takeWhileAntitone :: (a -> Bool) -> Set a -> Set a
takeWhileAntitone _ Tip = Tip
takeWhileAntitone p (Bin _ x l r)
  | p x = link x l (takeWhileAntitone p r)
  | otherwise = takeWhileAntitone p l

-- | \(O(\log n)\). Drop while a predicate on the elements holds.
-- The user is responsible for ensuring that for all elements @j@ and @k@ in the set,
-- @j \< k ==\> p j \>= p k@. See note at 'spanAntitone'.
--
-- @
-- dropWhileAntitone p = 'fromDistinctAscList' . 'Data.List.dropWhile' p . 'toList'
-- dropWhileAntitone p = 'filter' (not . p)
-- @
--
-- @since 0.5.8

dropWhileAntitone :: (a -> Bool) -> Set a -> Set a
dropWhileAntitone _ Tip = Tip
dropWhileAntitone p (Bin _ x l r)
  | p x = dropWhileAntitone p r
  | otherwise = link x (dropWhileAntitone p l) r

-- | \(O(\log n)\). Divide a set at the point where a predicate on the elements stops holding.
-- The user is responsible for ensuring that for all elements @j@ and @k@ in the set,
-- @j \< k ==\> p j \>= p k@.
--
-- @
-- spanAntitone p xs = ('takeWhileAntitone' p xs, 'dropWhileAntitone' p xs)
-- spanAntitone p xs = partition p xs
-- @
--
-- Note: if @p@ is not actually antitone, then @spanAntitone@ will split the set
-- at some /unspecified/ point where the predicate switches from holding to not
-- holding (where the predicate is seen to hold before the first element and to fail
-- after the last element).
--
-- @since 0.5.8

spanAntitone :: (a -> Bool) -> Set a -> (Set a, Set a)
spanAntitone p0 m = toPair (go p0 m)
  where
    go _ Tip = Tip :*: Tip
    go p (Bin _ x l r)
      | p x = let u :*: v = go p r in link x l u :*: v
      | otherwise = let u :*: v = go p l in u :*: link x v r


{--------------------------------------------------------------------
  Utility functions that maintain the balance properties of the tree.
  All constructors assume that all values in [l] < [x] and all values
  in [r] > [x], and that [l] and [r] are valid trees.

  In order of sophistication:
    [Bin sz x l r]    The type constructor.
    [bin x l r]       Maintains the correct size, assumes that both [l]
                      and [r] are balanced with respect to each other.
    [balance x l r]   Restores the balance and size.
                      Assumes that the original tree was balanced and
                      that [l] or [r] has changed by at most one element.
    [link x l r]      Restores balance and size.

  Furthermore, we can construct a new tree from two trees. Both operations
  assume that all values in [l] < all values in [r] and that [l] and [r]
  are valid:
    [glue l r]        Glues [l] and [r] together. Assumes that [l] and
                      [r] are already balanced with respect to each other.
    [merge l r]       Merges two trees and restores balance.
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  Link
--------------------------------------------------------------------}
link :: a -> Set a -> Set a -> Set a
link x Tip r  = insertMin x r
link x l Tip  = insertMax x l
link x l@(Bin sizeL y ly ry) r@(Bin sizeR z lz rz)
  | delta*sizeL < sizeR  = balanceL z (link x l lz) rz
  | delta*sizeR < sizeL  = balanceR y ly (link x ry r)
  | otherwise            = bin x l r


-- insertMin and insertMax don't perform potentially expensive comparisons.
insertMax,insertMin :: a -> Set a -> Set a
insertMax x t
  = case t of
      Tip -> singleton x
      Bin _ y l r
          -> balanceR y l (insertMax x r)

insertMin x t
  = case t of
      Tip -> singleton x
      Bin _ y l r
          -> balanceL y (insertMin x l) r

{--------------------------------------------------------------------
  [merge l r]: merges two trees.
--------------------------------------------------------------------}
merge :: Set a -> Set a -> Set a
merge Tip r   = r
merge l Tip   = l
merge l@(Bin sizeL x lx rx) r@(Bin sizeR y ly ry)
  | delta*sizeL < sizeR = balanceL y (merge l ly) ry
  | delta*sizeR < sizeL = balanceR x lx (merge rx r)
  | otherwise           = glue l r

{--------------------------------------------------------------------
  [glue l r]: glues two trees together.
  Assumes that [l] and [r] are already balanced with respect to each other.
--------------------------------------------------------------------}
glue :: Set a -> Set a -> Set a
glue Tip r = r
glue l Tip = l
glue l@(Bin sl xl ll lr) r@(Bin sr xr rl rr)
  | sl > sr = let !(m :*: l') = maxViewSure xl ll lr in balanceR m l' r
  | otherwise = let !(m :*: r') = minViewSure xr rl rr in balanceL m l r'

-- | \(O(\log n)\). Delete and find the minimal element.
--
-- > deleteFindMin set = (findMin set, deleteMin set)

deleteFindMin :: Set a -> (a,Set a)
deleteFindMin t
  | Just r <- minView t = r
  | otherwise = (error "Set.deleteFindMin: can not return the minimal element of an empty set", Tip)

-- | \(O(\log n)\). Delete and find the maximal element.
--
-- > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: Set a -> (a,Set a)
deleteFindMax t
  | Just r <- maxView t = r
  | otherwise = (error "Set.deleteFindMax: can not return the maximal element of an empty set", Tip)

minViewSure :: a -> Set a -> Set a -> StrictPair a (Set a)
minViewSure = go
  where
    go x Tip r = x :*: r
    go x (Bin _ xl ll lr) r =
      case go xl ll lr of
        xm :*: l' -> xm :*: balanceR x l' r

-- | \(O(\log n)\). Retrieves the minimal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
minView :: Set a -> Maybe (a, Set a)
minView Tip = Nothing
minView (Bin _ x l r) = Just $! toPair $ minViewSure x l r

maxViewSure :: a -> Set a -> Set a -> StrictPair a (Set a)
maxViewSure = go
  where
    go x l Tip = x :*: l
    go x l (Bin _ xr rl rr) =
      case go xr rl rr of
        xm :*: r' -> xm :*: balanceL x l r'

-- | \(O(\log n)\). Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxView :: Set a -> Maybe (a, Set a)
maxView Tip = Nothing
maxView (Bin _ x l r) = Just $! toPair $ maxViewSure x l r

{--------------------------------------------------------------------
  [balance x l r] balances two trees with value x.
  The sizes of the trees should balance after decreasing the
  size of one of them. (a rotation).

  [delta] is the maximal relative difference between the sizes of
          two trees, it corresponds with the [w] in Adams' paper.
  [ratio] is the ratio between an outer and inner sibling of the
          heavier subtree in an unbalanced setting. It determines
          whether a double or single rotation should be performed
          to restore balance. It is corresponds with the inverse
          of $\alpha$ in Adam's article.

  Note that according to the Adam's paper:
  - [delta] should be larger than 4.646 with a [ratio] of 2.
  - [delta] should be larger than 3.745 with a [ratio] of 1.534.

  But the Adam's paper is erroneous:
  - it can be proved that for delta=2 and delta>=5 there does
    not exist any ratio that would work
  - delta=4.5 and ratio=2 does not work

  That leaves two reasonable variants, delta=3 and delta=4,
  both with ratio=2.

  - A lower [delta] leads to a more 'perfectly' balanced tree.
  - A higher [delta] performs less rebalancing.

  In the benchmarks, delta=3 is faster on insert operations,
  and delta=4 has slightly better deletes. As the insert speedup
  is larger, we currently use delta=3.

--------------------------------------------------------------------}
delta,ratio :: Int
delta = 3
ratio = 2

-- The balance function is equivalent to the following:
--
--   balance :: a -> Set a -> Set a -> Set a
--   balance x l r
--     | sizeL + sizeR <= 1   = Bin sizeX x l r
--     | sizeR > delta*sizeL  = rotateL x l r
--     | sizeL > delta*sizeR  = rotateR x l r
--     | otherwise            = Bin sizeX x l r
--     where
--       sizeL = size l
--       sizeR = size r
--       sizeX = sizeL + sizeR + 1
--
--   rotateL :: a -> Set a -> Set a -> Set a
--   rotateL x l r@(Bin _ _ ly ry) | size ly < ratio*size ry = singleL x l r
--                                 | otherwise               = doubleL x l r
--   rotateR :: a -> Set a -> Set a -> Set a
--   rotateR x l@(Bin _ _ ly ry) r | size ry < ratio*size ly = singleR x l r
--                                 | otherwise               = doubleR x l r
--
--   singleL, singleR :: a -> Set a -> Set a -> Set a
--   singleL x1 t1 (Bin _ x2 t2 t3)  = bin x2 (bin x1 t1 t2) t3
--   singleR x1 (Bin _ x2 t1 t2) t3  = bin x2 t1 (bin x1 t2 t3)
--
--   doubleL, doubleR :: a -> Set a -> Set a -> Set a
--   doubleL x1 t1 (Bin _ x2 (Bin _ x3 t2 t3) t4) = bin x3 (bin x1 t1 t2) (bin x2 t3 t4)
--   doubleR x1 (Bin _ x2 t1 (Bin _ x3 t2 t3)) t4 = bin x3 (bin x2 t1 t2) (bin x1 t3 t4)
--
-- It is only written in such a way that every node is pattern-matched only once.
--
-- Only balanceL and balanceR are needed at the moment, so balance is not here anymore.
-- In case it is needed, it can be found in Data.Map.

-- Functions balanceL and balanceR are specialised versions of balance.
-- balanceL only checks whether the left subtree is too big,
-- balanceR only checks whether the right subtree is too big.

-- balanceL is called when left subtree might have been inserted to or when
-- right subtree might have been deleted from.
balanceL :: a -> Set a -> Set a -> Set a
balanceL x l r = case r of
  Tip -> case l of
           Tip -> Bin 1 x Tip Tip
           (Bin _ _ Tip Tip) -> Bin 2 x l Tip
           (Bin _ lx Tip (Bin _ lrx _ _)) -> Bin 3 lrx (Bin 1 lx Tip Tip) (Bin 1 x Tip Tip)
           (Bin _ lx ll@(Bin _ _ _ _) Tip) -> Bin 3 lx ll (Bin 1 x Tip Tip)
           (Bin ls lx ll@(Bin lls _ _ _) lr@(Bin lrs lrx lrl lrr))
             | lrs < ratio*lls -> Bin (1+ls) lx ll (Bin (1+lrs) x lr Tip)
             | otherwise -> Bin (1+ls) lrx (Bin (1+lls+size lrl) lx ll lrl) (Bin (1+size lrr) x lrr Tip)

  (Bin rs _ _ _) -> case l of
           Tip -> Bin (1+rs) x Tip r

           (Bin ls lx ll lr)
              | ls > delta*rs  -> case (ll, lr) of
                   (Bin lls _ _ _, Bin lrs lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lx ll (Bin (1+rs+lrs) x lr r)
                     | otherwise -> Bin (1+ls+rs) lrx (Bin (1+lls+size lrl) lx ll lrl) (Bin (1+rs+size lrr) x lrr r)
                   (_, _) -> error "Failure in Data.Set.balanceL"
              | otherwise -> Bin (1+ls+rs) x l r
{-# NOINLINE balanceL #-}

-- balanceR is called when right subtree might have been inserted to or when
-- left subtree might have been deleted from.
balanceR :: a -> Set a -> Set a -> Set a
balanceR x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 x Tip Tip
           (Bin _ _ Tip Tip) -> Bin 2 x Tip r
           (Bin _ rx Tip rr@(Bin _ _ _ _)) -> Bin 3 rx (Bin 1 x Tip Tip) rr
           (Bin _ rx (Bin _ rlx _ _) Tip) -> Bin 3 rlx (Bin 1 x Tip Tip) (Bin 1 rx Tip Tip)
           (Bin rs rx rl@(Bin rls rlx rll rlr) rr@(Bin rrs _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rx (Bin (1+rls) x Tip rl) rr
             | otherwise -> Bin (1+rs) rlx (Bin (1+size rll) x Tip rll) (Bin (1+rrs+size rlr) rx rlr rr)

  (Bin ls _ _ _) -> case r of
           Tip -> Bin (1+ls) x l Tip

           (Bin rs rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (Bin rls rlx rll rlr, Bin rrs _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rx (Bin (1+ls+rls) x l rl) rr
                     | otherwise -> Bin (1+ls+rs) rlx (Bin (1+ls+size rll) x l rll) (Bin (1+rrs+size rlr) rx rlr rr)
                   (_, _) -> error "Failure in Data.Set.balanceR"
              | otherwise -> Bin (1+ls+rs) x l r
{-# NOINLINE balanceR #-}

{--------------------------------------------------------------------
  The bin constructor maintains the size of the tree
--------------------------------------------------------------------}
bin :: a -> Set a -> Set a -> Set a
bin x l r
  = Bin (size l + size r + 1) x l r
{-# INLINE bin #-}


{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

-- | \(O(1)\).  Decompose a set into pieces based on the structure of the underlying
-- tree.  This function is useful for consuming a set in parallel.
--
-- No guarantee is made as to the sizes of the pieces; an internal, but
-- deterministic process determines this.  However, it is guaranteed that the pieces
-- returned will be in ascending order (all elements in the first subset less than all
-- elements in the second, and so on).
--
-- Examples:
--
-- > splitRoot (fromList [1..6]) ==
-- >   [fromList [1,2,3],fromList [4],fromList [5,6]]
--
-- > splitRoot empty == []
--
--  Note that the current implementation does not return more than three subsets,
--  but you should not depend on this behaviour because it can change in the
--  future without notice.
--
-- @since 0.5.4
splitRoot :: Set a -> [Set a]
splitRoot orig =
  case orig of
    Tip           -> []
    Bin _ v l r -> [l, singleton v, r]
{-# INLINE splitRoot #-}


-- | \(O(2^n \log n)\). Calculate the power set of a set: the set of all its subsets.
--
-- @
-- t ``member`` powerSet s == t ``isSubsetOf`` s
-- @
--
-- Example:
--
-- @
-- powerSet (fromList [1,2,3]) =
--   fromList $ map fromList [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]
-- @
--
-- @since 0.5.11

-- Proof of complexity: step executes n times. At the ith step,
-- "insertMin x `mapMonotonic` pxs" takes O(2^i log i) time since pxs has size
-- 2^i - 1 and we insertMin into its elements which are sets of size <= i.
-- "insertMin (singleton x)" and "`glue` pxs" are cheaper operations that both
-- take O(i) time. Over n steps, we have a total cost of
--
--   O(\sum_{i=1}^{n-1} 2^i log i)
-- = O(log n * \sum_{i=1}^{n-1} 2^i)
-- = O(2^n log n)

powerSet :: Set a -> Set (Set a)
powerSet xs0 = insertMin empty (foldr' step Tip xs0) where
  step x pxs = insertMin (singleton x) (insertMin x `mapMonotonic` pxs) `glue` pxs

-- | \(O(nm)\). Calculate the Cartesian product of two sets.
--
-- @
-- cartesianProduct xs ys = fromList $ liftA2 (,) (toList xs) (toList ys)
-- @
--
-- Example:
--
-- @
-- cartesianProduct (fromList [1,2]) (fromList [\'a\',\'b\']) =
--   fromList [(1,\'a\'), (1,\'b\'), (2,\'a\'), (2,\'b\')]
-- @
--
-- @since 0.5.11
cartesianProduct :: Set a -> Set b -> Set (a, b)
-- The obvious big-O optimal (O(nm)) implementation would be
--
--   cartesianProduct _as Tip = Tip
--   cartesianProduct as bs = fromDistinctAscList
--     [(a,b) | a <- toList as, b <- toList bs]
--
-- Unfortunately, this is much slower in practice, at least when the sets are
-- constructed from ascending lists. I tried doing the same thing using a
-- known-length (perfect balancing) variant of fromDistinctAscList, but it
-- still didn't come close to the performance of the implementation we use in my
-- very informal tests.
--
-- The implementation we use (slightly modified from one that Edward Kmett
-- hacked together) is also optimal but performs better in practice. We map
-- each element a in as to a set made up of (a,b) for every element b in bs,
-- taking O(nm) overall. Then we merge these sets up the tree of as, which takes
-- O(n log m). A brief sketch of proof for the latter:
--
-- Consider all nodes in the tree at the same distance from the root to be at
-- the same "level". The nodes farthest from the root are at level 0, with
-- levels increasing by 1 towards the root. Being a balanced tree, there are
-- O(n/2^i) nodes at level i. At every node at level i, we merge the merged left
-- set, current set, and merged right set into a set of size O(2^i*m) in
-- O(log (2^i*m)) = O(i + log m) time. Over all levels, we do a total work of
--
--   O(\sum_{i=0}^{root_level} n * (i + log m) / 2^i)
-- = O(  \sum_{i=0}^{root_level} n * i / 2^i
--     + \sum_{i=0}^{root_level} n * log m / 2^i)
-- = O(  n * \sum_{i=0}^{root_level} i/2^i
--     + n * log m * \sum_{i=0}^{root_level} 1/2^i)
-- = O(  n * \sum_{i=0}^{inf} i/2^i
--     + n * log m * \sum_{i=0}^{inf} 1/2^i)
--
-- The sum terms converge, and we get O(n log m).

-- When the second argument has at most one element, we can be a little
-- clever.
cartesianProduct !_as Tip = Tip
cartesianProduct as (Bin 1 b _ _) = mapMonotonic (flip (,) b) as
cartesianProduct as bs =
  getMergeSet $ foldMap (\a -> MergeSet $ mapMonotonic ((,) a) bs) as

-- A version of Set with peculiar Semigroup and Monoid instances.
-- The result of xs <> ys will only be a valid set if the greatest
-- element of xs is strictly less than the least element of ys.
-- This is used to define cartesianProduct.
newtype MergeSet a = MergeSet { getMergeSet :: Set a }

instance Semigroup (MergeSet a) where
  MergeSet xs <> MergeSet ys = MergeSet (merge xs ys)

instance Monoid (MergeSet a) where
  mempty = MergeSet empty

  mappend = (<>)

-- | \(O(n+m)\). Calculate the disjoint union of two sets.
--
-- @ disjointUnion xs ys = map Left xs ``union`` map Right ys @
--
-- Example:
--
-- @
-- disjointUnion (fromList [1,2]) (fromList ["hi", "bye"]) =
--   fromList [Left 1, Left 2, Right "hi", Right "bye"]
-- @
--
-- @since 0.5.11
disjointUnion :: Set a -> Set b -> Set (Either a b)
disjointUnion as bs = merge (mapMonotonic Left as) (mapMonotonic Right bs)

{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}
-- | \(O(n \log n)\). Show the tree that implements the set. The tree is shown
-- in a compressed, hanging format.
showTree :: Show a => Set a -> String
showTree s
  = showTreeWith True False s


{- | \(O(n \log n)\). The expression (@showTreeWith hang wide map@) shows
 the tree that implements the set. If @hang@ is
 @True@, a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.

> Set> putStrLn $ showTreeWith True False $ fromDistinctAscList [1..5]
> 4
> +--2
> |  +--1
> |  +--3
> +--5
>
> Set> putStrLn $ showTreeWith True True $ fromDistinctAscList [1..5]
> 4
> |
> +--2
> |  |
> |  +--1
> |  |
> |  +--3
> |
> +--5
>
> Set> putStrLn $ showTreeWith False True $ fromDistinctAscList [1..5]
> +--5
> |
> 4
> |
> |  +--3
> |  |
> +--2
>    |
>    +--1

-}
showTreeWith :: Show a => Bool -> Bool -> Set a -> String
showTreeWith hang wide t
  | hang      = (showsTreeHang wide [] t) ""
  | otherwise = (showsTree wide [] [] t) ""

showsTree :: Show a => Bool -> [String] -> [String] -> Set a -> ShowS
showsTree wide lbars rbars t
  = case t of
      Tip -> showsBars lbars . showString "|\n"
      Bin _ x Tip Tip
          -> showsBars lbars . shows x . showString "\n"
      Bin _ x l r
          -> showsTree wide (withBar rbars) (withEmpty rbars) r .
             showWide wide rbars .
             showsBars lbars . shows x . showString "\n" .
             showWide wide lbars .
             showsTree wide (withEmpty lbars) (withBar lbars) l

showsTreeHang :: Show a => Bool -> [String] -> Set a -> ShowS
showsTreeHang wide bars t
  = case t of
      Tip -> showsBars bars . showString "|\n"
      Bin _ x Tip Tip
          -> showsBars bars . shows x . showString "\n"
      Bin _ x l r
          -> showsBars bars . shows x . showString "\n" .
             showWide wide bars .
             showsTreeHang wide (withBar bars) l .
             showWide wide bars .
             showsTreeHang wide (withEmpty bars) r

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
node           = "+--"

withBar, withEmpty :: [String] -> [String]
withBar bars   = "|  ":bars
withEmpty bars = "   ":bars

{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}
-- | \(O(n)\). Test if the internal set structure is valid.
valid :: Ord a => Set a -> Bool
valid t
  = balanced t && ordered t && validsize t

ordered :: Ord a => Set a -> Bool
ordered t
  = bounded (const True) (const True) t
  where
    bounded lo hi t'
      = case t' of
          Tip         -> True
          Bin _ x l r -> (lo x) && (hi x) && bounded lo (<x) l && bounded (>x) hi r

balanced :: Set a -> Bool
balanced t
  = case t of
      Tip         -> True
      Bin _ _ l r -> (size l + size r <= 1 || (size l <= delta*size r && size r <= delta*size l)) &&
                     balanced l && balanced r

validsize :: Set a -> Bool
validsize t
  = (realsize t == Just (size t))
  where
    realsize t'
      = case t' of
          Tip          -> Just 0
          Bin sz _ l r -> case (realsize l,realsize r) of
                            (Just n,Just m)  | n+m+1 == sz  -> Just sz
                            _                -> Nothing

--------------------------------------------------------------------

-- Note [fromDistinctAscList implementation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- fromDistinctAscList is implemented by building up perfectly balanced trees
-- while we consume elements from the list one by one. A stack of
-- (root, perfectly balanced left branch) pairs is maintained, in increasing
-- order of size from top to bottom.
--
-- When we get an element from the list, we attempt to link it as the right
-- branch with the top (root, perfect left branch) of the stack to create a new
-- perfect tree. We can only do this if the left branch has size 1. If we link
-- it, we get a perfect tree of size 3. We repeat this process, merging with the
-- top of the stack as long as the sizes match. When we can't link any more, the
-- perfect tree we built so far is a potential left branch. The next element
-- we find becomes the root, and we push this new (root, left branch) on the
-- stack.
--
-- When we are out of elements, we link the (root, left branch)s in the stack
-- top to bottom to get the final tree.
--
-- How long does this take? We do O(1) work per element excluding the links.
-- Over n elements, we build trees with at most n nodes total, and each link is
-- done in O(1) using `bin`. The final linking of the stack is done in O(log n)
-- using `link`  (proof below). The total time is thus O(n).
--
-- Additionally, the implemention is written using foldl' over the input list,
-- which makes it participate as a good consumer in list fusion.
--
-- fromDistinctDescList is implemented similarly, adapted for left and right
-- sides being swapped.
--
-- ~~~
--
-- A `link` operation links trees L and R with a root in
-- O(|log(size(L)) - log(size(R))|). Let's say there are m (root, tree) in the
-- stack, the size of the ith tree being 2^{k_i} - 1. We also know that
-- k_i > k_j for i > j, and n = \sum_{i=1}^m 2^{k_i}. With this information, we
-- can calculate the total time to link everything on the stack:
--
--   O(\sum_{i=2}^m |log(2^{k_i} - 1) - log(\sum_{j=1}^{i-1} 2^{k_j})|)
-- = O(\sum_{i=2}^m log(2^{k_i} - 1) - log(\sum_{j=1}^{i-1} 2^{k_j}))
-- = O(\sum_{i=2}^m log(2^{k_i} - 1) - log(2^{k_{i-1}}))
-- = O(\sum_{i=2}^m k_i - k_{i-1})
-- = O(k_m - k_1)
-- = O(log n)
