{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#if __GLASGOW_HASKELL__ >= 708
#define DEFINE_PATTERN_SYNONYMS 1
#endif
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
#endif
#if __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Trustworthy #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE TypeFamilies #-}
#endif
#ifdef DEFINE_PATTERN_SYNONYMS
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence
-- Copyright   :  (c) Ross Paterson 2005
--                (c) Louis Wasserman 2009
--                (c) Bertram Felgenhauer, David Feuer, Ross Paterson, and
--                    Milan Straka 2014
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- General purpose finite sequences.
-- Apart from being finite and having strict operations, sequences
-- also differ from lists in supporting a wider variety of operations
-- efficiently.
--
-- An amortized running time is given for each operation, with /n/ referring
-- to the length of the sequence and /i/ being the integral index used by
-- some operations. These bounds hold even in a persistent (shared) setting.
--
-- The implementation uses 2-3 finger trees annotated with sizes,
-- as described in section 4.2 of
--
--    * Ralf Hinze and Ross Paterson,
--      \"Finger trees: a simple general-purpose data structure\",
--      /Journal of Functional Programming/ 16:2 (2006) pp 197-217.
--      <http://staff.city.ac.uk/~ross/papers/FingerTree.html>
--
-- /Note/: Many of these operations have the same names as similar
-- operations on lists in the "Prelude". The ambiguity may be resolved
-- using either qualification or the @hiding@ clause.
--
-- /Warning/: The size of a 'Seq' must not exceed @maxBound::Int@.  Violation
-- of this condition is not detected and if the size limit is exceeded, the
-- behaviour of the sequence is undefined.  This is unlikely to occur in most
-- applications, but some care may be required when using '><', '<*>', '*>', or
-- '>>', particularly repeatedly and particularly in combination with
-- 'replicate' or 'fromFunction'.
--
-----------------------------------------------------------------------------

module Data.Sequence (
#if defined(TESTING)
    Elem(..), FingerTree(..), Node(..), Digit(..),
#if __GLASGOW_HASKELL__ >= 800
    Seq (.., Empty, (:<|), (:|>)),
#else
    Seq (..),
#endif

#elif __GLASGOW_HASKELL__ >= 800
    Seq (Empty, (:<|), (:|>)),
#else
    Seq,
#if defined(DEFINE_PATTERN_SYNONYMS)
    -- * Pattern synonyms
    pattern Empty,  -- :: Seq a
    pattern (:<|),  -- :: a -> Seq a -> Seq a
    pattern (:|>),  -- :: Seq a -> a -> Seq a
#endif
#endif
    -- * Construction
    empty,          -- :: Seq a
    singleton,      -- :: a -> Seq a
    (<|),           -- :: a -> Seq a -> Seq a
    (|>),           -- :: Seq a -> a -> Seq a
    (><),           -- :: Seq a -> Seq a -> Seq a
    fromList,       -- :: [a] -> Seq a
    fromFunction,   -- :: Int -> (Int -> a) -> Seq a
    fromArray,      -- :: Ix i => Array i a -> Seq a
    -- ** Repetition
    replicate,      -- :: Int -> a -> Seq a
    replicateA,     -- :: Applicative f => Int -> f a -> f (Seq a)
    replicateM,     -- :: Monad m => Int -> m a -> m (Seq a)
    -- ** Iterative construction
    iterateN,       -- :: Int -> (a -> a) -> a -> Seq a
    unfoldr,        -- :: (b -> Maybe (a, b)) -> b -> Seq a
    unfoldl,        -- :: (b -> Maybe (b, a)) -> b -> Seq a
    -- * Deconstruction
    -- | Additional functions for deconstructing sequences are available
    -- via the 'Foldable' instance of 'Seq'.

    -- ** Queries
    null,           -- :: Seq a -> Bool
    length,         -- :: Seq a -> Int
    -- ** Views
    ViewL(..),
    viewl,          -- :: Seq a -> ViewL a
    ViewR(..),
    viewr,          -- :: Seq a -> ViewR a
    -- * Scans
    scanl,          -- :: (a -> b -> a) -> a -> Seq b -> Seq a
    scanl1,         -- :: (a -> a -> a) -> Seq a -> Seq a
    scanr,          -- :: (a -> b -> b) -> b -> Seq a -> Seq b
    scanr1,         -- :: (a -> a -> a) -> Seq a -> Seq a
    -- * Sublists
    tails,          -- :: Seq a -> Seq (Seq a)
    inits,          -- :: Seq a -> Seq (Seq a)
    -- ** Sequential searches
    takeWhileL,     -- :: (a -> Bool) -> Seq a -> Seq a
    takeWhileR,     -- :: (a -> Bool) -> Seq a -> Seq a
    dropWhileL,     -- :: (a -> Bool) -> Seq a -> Seq a
    dropWhileR,     -- :: (a -> Bool) -> Seq a -> Seq a
    spanl,          -- :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    spanr,          -- :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    breakl,         -- :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    breakr,         -- :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    partition,      -- :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    filter,         -- :: (a -> Bool) -> Seq a -> Seq a
    -- * Sorting
    sort,           -- :: Ord a => Seq a -> Seq a
    sortBy,         -- :: (a -> a -> Ordering) -> Seq a -> Seq a
    unstableSort,   -- :: Ord a => Seq a -> Seq a
    unstableSortBy, -- :: (a -> a -> Ordering) -> Seq a -> Seq a
    -- * Indexing
    index,          -- :: Seq a -> Int -> a
    adjust,         -- :: (a -> a) -> Int -> Seq a -> Seq a
    update,         -- :: Int -> a -> Seq a -> Seq a
    take,           -- :: Int -> Seq a -> Seq a
    drop,           -- :: Int -> Seq a -> Seq a
    splitAt,        -- :: Int -> Seq a -> (Seq a, Seq a)
    -- ** Indexing with predicates
    -- | These functions perform sequential searches from the left
    -- or right ends of the sequence, returning indices of matching
    -- elements.
    elemIndexL,     -- :: Eq a => a -> Seq a -> Maybe Int
    elemIndicesL,   -- :: Eq a => a -> Seq a -> [Int]
    elemIndexR,     -- :: Eq a => a -> Seq a -> Maybe Int
    elemIndicesR,   -- :: Eq a => a -> Seq a -> [Int]
    findIndexL,     -- :: (a -> Bool) -> Seq a -> Maybe Int
    findIndicesL,   -- :: (a -> Bool) -> Seq a -> [Int]
    findIndexR,     -- :: (a -> Bool) -> Seq a -> Maybe Int
    findIndicesR,   -- :: (a -> Bool) -> Seq a -> [Int]
    -- * Folds
    -- | General folds are available via the 'Foldable' instance of 'Seq'.
    foldlWithIndex, -- :: (b -> Int -> a -> b) -> b -> Seq a -> b
    foldrWithIndex, -- :: (Int -> a -> b -> b) -> b -> Seq a -> b
    -- * Transformations
    mapWithIndex,   -- :: (Int -> a -> b) -> Seq a -> Seq b
    traverseWithIndex, -- :: Applicative f => (Int -> a -> f b) -> Seq a -> f (Seq b)
    reverse,        -- :: Seq a -> Seq a
    intersperse,    -- :: a -> Seq a -> Seq a
    -- ** Zips
    zip,            -- :: Seq a -> Seq b -> Seq (a, b)
    zipWith,        -- :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
    zip3,           -- :: Seq a -> Seq b -> Seq c -> Seq (a, b, c)
    zipWith3,       -- :: (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d
    zip4,           -- :: Seq a -> Seq b -> Seq c -> Seq d -> Seq (a, b, c, d)
    zipWith4,       -- :: (a -> b -> c -> d -> e) -> Seq a -> Seq b -> Seq c -> Seq d -> Seq e
#if TESTING
    Sized(..),
    deep,
    node2,
    node3,
#endif
    ) where

import Prelude hiding (
    Functor(..),
#if MIN_VERSION_base(4,8,0)
    Applicative, (<$>), foldMap, Monoid,
#endif
    null, length, take, drop, splitAt, foldl, foldl1, foldr, foldr1,
    scanl, scanl1, scanr, scanr1, replicate, zip, zipWith, zip3, zipWith3,
    takeWhile, dropWhile, iterate, reverse, filter, mapM, sum, all)
import qualified Data.List
import Control.Applicative (Applicative(..), (<$>), (<**>),  Alternative,
                            WrappedMonad(..), liftA, liftA2, liftA3)
import qualified Control.Applicative as Applicative (Alternative(..))
import Control.DeepSeq (NFData(rnf))
import Control.Monad (MonadPlus(..), ap)
import Data.Monoid (Monoid(..))
import Data.Functor (Functor(..))
import Data.Foldable (Foldable(foldl, foldl1, foldr, foldr1, foldMap), foldl', toList)
#if MIN_VERSION_base(4,8,0)
import Data.Foldable (foldr')
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.Traversable
import Data.Typeable

-- GHC specific stuff
#ifdef __GLASGOW_HASKELL__
import GHC.Exts (build)
import Text.Read (Lexeme(Ident), lexP, parens, prec,
    readPrec, readListPrec, readListPrecDefault)
import Data.Data
import Data.String (IsString(..))
#endif

-- Array stuff, with GHC.Arr on GHC
import Data.Array (Ix, Array)
import qualified Data.Array
#ifdef __GLASGOW_HASKELL__
import qualified GHC.Arr
#endif

-- Coercion on GHC 7.8+
#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce
import qualified GHC.Exts
#else
#endif

-- Identity functor on base 4.8 (GHC 7.10+)
#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity (Identity(..))
#endif

default ()

infixr 5 `consTree`
infixl 5 `snocTree`
infixr 5 `appendTree0`

infixr 5 ><
infixr 5 <|, :<
infixl 5 |>, :>

#ifdef DEFINE_PATTERN_SYNONYMS
infixr 5 :<|
infixl 5 :|>

-- TODO: Once GHC implements some way to prevent non-exhaustive
-- pattern match warnings for pattern synonyms, we should be
-- sure to take advantage of that.

-- Unfortunately, there's some extra noise here because
-- pattern synonyms could not have signatures until 7.10,
-- but 8.0 at least will warn if they're missing.
#if __GLASGOW_HASKELL__ >= 710
pattern Empty :: Seq a
#endif
pattern Empty = Seq EmptyT

-- Non-trivial bidirectional pattern synonyms are only
-- available in GHC >= 7.10. In earlier versions, these
-- can be used to match, but not to construct.

#if __GLASGOW_HASKELL__ >= 710
pattern (:<|) :: a -> Seq a -> Seq a
#endif
pattern x :<| xs <- (viewl -> x :< xs)
#if __GLASGOW_HASKELL__ >= 710
  where
    x :<| xs = x <| xs
#endif

#if __GLASGOW_HASKELL__ >= 710
pattern (:|>) :: Seq a -> a -> Seq a
#endif
pattern xs :|> x <- (viewr -> xs :> x)
#if __GLASGOW_HASKELL__ >= 710
  where
    xs :|> x = xs |> x
#endif
#endif

class Sized a where
    size :: a -> Int

-- | General-purpose finite sequences.
newtype Seq a = Seq (FingerTree (Elem a))

instance Functor Seq where
    fmap = fmapSeq
#ifdef __GLASGOW_HASKELL__
    x <$ s = replicate (length s) x
#endif

fmapSeq :: (a -> b) -> Seq a -> Seq b
fmapSeq f (Seq xs) = Seq (fmap (fmap f) xs)
#ifdef __GLASGOW_HASKELL__
{-# NOINLINE [1] fmapSeq #-}
{-# RULES
"fmapSeq/fmapSeq" forall f g xs . fmapSeq f (fmapSeq g xs) = fmapSeq (f . g) xs
 #-}
#endif
#if __GLASGOW_HASKELL__ >= 709
-- Safe coercions were introduced in 7.8, but did not work well with RULES yet.
{-# RULES
"fmapSeq/coerce" fmapSeq coerce = coerce
 #-}
#endif

instance Foldable Seq where
    foldMap f (Seq xs) = foldMap (foldMap f) xs
    foldr f z (Seq xs) = foldr (flip (foldr f)) z xs
    foldl f z (Seq xs) = foldl (foldl f) z xs

    foldr1 f (Seq xs) = getElem (foldr1 f' xs)
      where f' (Elem x) (Elem y) = Elem (f x y)

    foldl1 f (Seq xs) = getElem (foldl1 f' xs)
      where f' (Elem x) (Elem y) = Elem (f x y)

#if MIN_VERSION_base(4,8,0)
    length = length
    {-# INLINE length #-}
    null   = null
    {-# INLINE null #-}
#endif

instance Traversable Seq where
    traverse f (Seq xs) = Seq <$> traverse (traverse f) xs

instance NFData a => NFData (Seq a) where
    rnf (Seq xs) = rnf xs

instance Monad Seq where
    return = pure
    xs >>= f = foldl' add empty xs
      where add ys x = ys >< f x
    (>>) = (*>)

instance Applicative Seq where
    pure = singleton
    xs *> ys = cycleN (length xs) ys

    fs <*> xs@(Seq xsFT) = case viewl fs of
      EmptyL -> empty
      firstf :< fs' -> case viewr fs' of
        EmptyR -> fmap firstf xs
        Seq fs''FT :> lastf -> case rigidify xsFT of
             RigidEmpty -> empty
             RigidOne (Elem x) -> fmap ($x) fs
             RigidTwo (Elem x1) (Elem x2) ->
                Seq $ ap2FT firstf fs''FT lastf (x1, x2)
             RigidThree (Elem x1) (Elem x2) (Elem x3) ->
                Seq $ ap3FT firstf fs''FT lastf (x1, x2, x3)
             RigidFull r@(Rigid s pr _m sf) -> Seq $
                   Deep (s * length fs)
                        (fmap (fmap firstf) (nodeToDigit pr))
                        (aptyMiddle (fmap firstf) (fmap lastf) fmap fs''FT r)
                        (fmap (fmap lastf) (nodeToDigit sf))


ap2FT :: (a -> b) -> FingerTree (Elem (a->b)) -> (a -> b) -> (a,a) -> FingerTree (Elem b)
ap2FT firstf fs lastf (x,y) =
                 Deep (size fs * 2 + 4)
                      (Two (Elem $ firstf x) (Elem $ firstf y))
                      (mapMulFT 2 (\(Elem f) -> Node2 2 (Elem (f x)) (Elem (f y))) fs)
                      (Two (Elem $ lastf x) (Elem $ lastf y))

ap3FT :: (a -> b) -> FingerTree (Elem (a->b)) -> (a -> b) -> (a,a,a) -> FingerTree (Elem b)
ap3FT firstf fs lastf (x,y,z) = Deep (size fs * 3 + 6)
                        (Three (Elem $ firstf x) (Elem $ firstf y) (Elem $ firstf z))
                        (mapMulFT 3 (\(Elem f) -> Node3 3 (Elem (f x)) (Elem (f y)) (Elem (f z))) fs)
                        (Three (Elem $ lastf x) (Elem $ lastf y) (Elem $ lastf z))


data Rigidified a = RigidEmpty
                  | RigidOne a
                  | RigidTwo a a
                  | RigidThree a a a
                  | RigidFull (Rigid a)
#ifdef TESTING
                  deriving Show
#endif

-- | A finger tree whose top level has only Two and/or Three digits, and whose
-- other levels have only One and Two digits. A Rigid tree is precisely what one
-- gets by unzipping/inverting a 2-3 tree, so it is precisely what we need to
-- turn a finger tree into in order to transform it into a 2-3 tree.
data Rigid a = Rigid {-# UNPACK #-} !Int !(Digit23 a) (Thin (Node a)) !(Digit23 a)
#ifdef TESTING
             deriving Show
#endif

-- | A finger tree whose digits are all ones and twos
data Thin a = EmptyTh
            | SingleTh a
            | DeepTh {-# UNPACK #-} !Int !(Digit12 a) (Thin (Node a)) !(Digit12 a)
#ifdef TESTING
            deriving Show
#endif

data Digit12 a = One12 a | Two12 a a
#ifdef TESTING
        deriving Show
#endif

-- | Sometimes, we want to emphasize that we are viewing a node as a top-level
-- digit of a 'Rigid' tree.
type Digit23 a = Node a

-- | 'aptyMiddle' does most of the hard work of computing @fs<*>xs@.  It
-- produces the center part of a finger tree, with a prefix corresponding to
-- the prefix of @xs@ and a suffix corresponding to the suffix of @xs@ omitted;
-- the missing suffix and prefix are added by the caller.  For the recursive
-- call, it squashes the prefix and the suffix into the center tree. Once it
-- gets to the bottom, it turns the tree into a 2-3 tree, applies 'mapMulFT' to
-- produce the main body, and glues all the pieces together.
--
-- 'map23' itself is a bit horrifying because of the nested types involved. Its
-- job is to map over the *elements* of a 2-3 tree, rather than the subtrees.
-- If we used a higher-order nested type with MPTC, we could probably use a
-- class, but as it is we have to build up 'map23' explicitly through the
-- recursion.
aptyMiddle
  :: (c -> d)
     -> (c -> d)
     -> ((a -> b) -> c -> d)
     -> FingerTree (Elem (a -> b))
     -> Rigid c
     -> FingerTree (Node d)

-- Not at the bottom yet

aptyMiddle firstf
           lastf
           map23
           fs
           (Rigid s pr (DeepTh sm prm mm sfm) sf)
    = Deep (sm + s * (size fs + 1)) -- note: sm = s - size pr - size sf
           (fmap (fmap firstf) (digit12ToDigit prm))
           (aptyMiddle (fmap firstf)
                       (fmap lastf)
                       (fmap . map23)
                       fs
                       (Rigid s (squashL pr prm) mm (squashR sfm sf)))
           (fmap (fmap lastf) (digit12ToDigit sfm))

-- At the bottom

aptyMiddle firstf
           lastf
           map23
           fs
           (Rigid s pr EmptyTh sf)
     = deep
            (One (fmap firstf sf))
            (mapMulFT s (\(Elem f) -> fmap (fmap (map23 f)) converted) fs)
            (One (fmap lastf pr))
   where converted = node2 pr sf

aptyMiddle firstf
           lastf
           map23
           fs
           (Rigid s pr (SingleTh q) sf)
     = deep
            (Two (fmap firstf q) (fmap firstf sf))
            (mapMulFT s (\(Elem f) -> fmap (fmap (map23 f)) converted) fs)
            (Two (fmap lastf pr) (fmap lastf q))
   where converted = node3 pr q sf

digit12ToDigit :: Digit12 a -> Digit a
digit12ToDigit (One12 a) = One a
digit12ToDigit (Two12 a b) = Two a b

-- Squash the first argument down onto the left side of the second.
squashL :: Digit23 a -> Digit12 (Node a) -> Digit23 (Node a)
squashL m (One12 n) = node2 m n
squashL m (Two12 n1 n2) = node3 m n1 n2

-- Squash the second argument down onto the right side of the first
squashR :: Digit12 (Node a) -> Digit23 a -> Digit23 (Node a)
squashR (One12 n) m = node2 n m
squashR (Two12 n1 n2) m = node3 n1 n2 m


-- | /O(m*n)/ (incremental) Takes an /O(m)/ function and a finger tree of size
-- /n/ and maps the function over the tree leaves. Unlike the usual 'fmap', the
-- function is applied to the "leaves" of the 'FingerTree' (i.e., given a
-- @FingerTree (Elem a)@, it applies the function to elements of type @Elem
-- a@), replacing the leaves with subtrees of at least the same height, e.g.,
-- @Node(Node(Elem y))@. The multiplier argument serves to make the annotations
-- match up properly.
mapMulFT :: Int -> (a -> b) -> FingerTree a -> FingerTree b
mapMulFT _ _ EmptyT = EmptyT
mapMulFT _mul f (Single a) = Single (f a)
mapMulFT mul f (Deep s pr m sf) = Deep (mul * s) (fmap f pr) (mapMulFT mul (mapMulNode mul f) m) (fmap f sf)

mapMulNode :: Int -> (a -> b) -> Node a -> Node b
mapMulNode mul f (Node2 s a b)   = Node2 (mul * s) (f a) (f b)
mapMulNode mul f (Node3 s a b c) = Node3 (mul * s) (f a) (f b) (f c)

-- | /O(log n)/ (incremental) Takes the extra flexibility out of a 'FingerTree'
-- to make it a genuine 2-3 finger tree. The result of 'rigidify' will have
-- only two and three digits at the top level and only one and two
-- digits elsewhere. If the tree has fewer than four elements, 'rigidify'
-- will simply extract them, and will not build a tree.
rigidify :: FingerTree (Elem a) -> Rigidified (Elem a)
-- The patterns below just fix up the top level of the tree; 'rigidify'
-- delegates the hard work to 'thin'.

rigidify EmptyT = RigidEmpty

rigidify (Single q) = RigidOne q

-- The left digit is Two or Three
rigidify (Deep s (Two a b) m sf) = rigidifyRight s (node2 a b) m sf
rigidify (Deep s (Three a b c) m sf) = rigidifyRight s (node3 a b c) m sf

-- The left digit is Four
rigidify (Deep s (Four a b c d) m sf) = rigidifyRight s (node2 a b) (node2 c d `consTree` m) sf

-- The left digit is One
rigidify (Deep s (One a) m sf) = case viewLTree m of
   Just2 (Node2 _ b c) m' -> rigidifyRight s (node3 a b c) m' sf
   Just2 (Node3 _ b c d) m' -> rigidifyRight s (node2 a b) (node2 c d `consTree` m') sf
   Nothing2 -> case sf of
     One b -> RigidTwo a b
     Two b c -> RigidThree a b c
     Three b c d -> RigidFull $ Rigid s (node2 a b) EmptyTh (node2 c d)
     Four b c d e -> RigidFull $ Rigid s (node3 a b c) EmptyTh (node2 d e)

-- | /O(log n)/ (incremental) Takes a tree whose left side has been rigidified
-- and finishes the job.
rigidifyRight :: Int -> Digit23 (Elem a) -> FingerTree (Node (Elem a)) -> Digit (Elem a) -> Rigidified (Elem a)

-- The right digit is Two, Three, or Four
rigidifyRight s pr m (Two a b) = RigidFull $ Rigid s pr (thin m) (node2 a b)
rigidifyRight s pr m (Three a b c) = RigidFull $ Rigid s pr (thin m) (node3 a b c)
rigidifyRight s pr m (Four a b c d) = RigidFull $ Rigid s pr (thin $ m `snocTree` node2 a b) (node2 c d)

-- The right digit is One
rigidifyRight s pr m (One e) = case viewRTree m of
    Just2 m' (Node2 _ a b) -> RigidFull $ Rigid s pr (thin m') (node3 a b e)
    Just2 m' (Node3 _ a b c) -> RigidFull $ Rigid s pr (thin $ m' `snocTree` node2 a b) (node2 c e)
    Nothing2 -> case pr of
      Node2 _ a b -> RigidThree a b e
      Node3 _ a b c -> RigidFull $ Rigid s (node2 a b) EmptyTh (node2 c e)

-- | /O(log n)/ (incremental) Rejigger a finger tree so the digits are all ones
-- and twos.
thin :: Sized a => FingerTree a -> Thin a
-- Note that 'thin12' will produce a 'DeepTh' constructor immediately before
-- recursively calling 'thin'.
thin EmptyT = EmptyTh
thin (Single a) = SingleTh a
thin (Deep s pr m sf) =
  case pr of
    One a -> thin12 s (One12 a) m sf
    Two a b -> thin12 s (Two12 a b) m sf
    Three a b c  -> thin12 s (One12 a) (node2 b c `consTree` m) sf
    Four a b c d -> thin12 s (Two12 a b) (node2 c d `consTree` m) sf

thin12 :: Sized a => Int -> Digit12 a -> FingerTree (Node a) -> Digit a -> Thin a
thin12 s pr m (One a) = DeepTh s pr (thin m) (One12 a)
thin12 s pr m (Two a b) = DeepTh s pr (thin m) (Two12 a b)
thin12 s pr m (Three a b c) = DeepTh s pr (thin $ m `snocTree` node2 a b) (One12 c)
thin12 s pr m (Four a b c d) = DeepTh s pr (thin $ m `snocTree` node2 a b) (Two12 c d)

-- | Intersperse an element between the elements of a sequence.
-- > intersperse a empty = empty
-- > intersperse a (singleton x) = singleton x
-- > intersperse a (fromList [x,y]) = fromList [x,a,y]
-- > intersperse a (fromList [x,y,z]) = fromList [x,a,y,a,z]
--
-- @since 0.5.8
intersperse :: a -> Seq a -> Seq a
intersperse y xs = drop 1 $ xs <**> (const y <| singleton id)

instance MonadPlus Seq where
    mzero = empty
    mplus = (><)

instance Alternative Seq where
    empty = empty
    (<|>) = (><)

instance Eq a => Eq (Seq a) where
    xs == ys = length xs == length ys && toList xs == toList ys

instance Ord a => Ord (Seq a) where
    compare xs ys = compare (toList xs) (toList ys)

#if TESTING
instance Show a => Show (Seq a) where
    showsPrec p (Seq x) = showsPrec p x
#else
instance Show a => Show (Seq a) where
    showsPrec p xs = showParen (p > 10) $
        showString "fromList " . shows (toList xs)
#endif

instance Read a => Read (Seq a) where
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

instance Monoid (Seq a) where
    mempty = empty
#if !(MIN_VERSION_base(4,9,0))
    mappend = (><)
#else
    mappend = (<>)

instance Semigroup (Seq a) where
    (<>)    = (><)
#endif

INSTANCE_TYPEABLE1(Seq,seqTc,"Seq")

#if __GLASGOW_HASKELL__
instance Data a => Data (Seq a) where
    gfoldl f z s    = case viewl s of
        EmptyL  -> z empty
        x :< xs -> z (<|) `f` x `f` xs

    gunfold k z c   = case constrIndex c of
        1 -> z empty
        2 -> k (k (z (<|)))
        _ -> error "gunfold"

    toConstr xs
      | null xs     = emptyConstr
      | otherwise   = consConstr

    dataTypeOf _    = seqDataType

    dataCast1 f     = gcast1 f

emptyConstr, consConstr :: Constr
emptyConstr = mkConstr seqDataType "empty" [] Prefix
consConstr  = mkConstr seqDataType "<|" [] Infix

seqDataType :: DataType
seqDataType = mkDataType "Data.Sequence.Seq" [emptyConstr, consConstr]
#endif

-- Finger trees

data FingerTree a
    = EmptyT
    | Single a
    | Deep {-# UNPACK #-} !Int !(Digit a) (FingerTree (Node a)) !(Digit a)
#if TESTING
    deriving Show
#endif

instance Sized a => Sized (FingerTree a) where
    {-# SPECIALIZE instance Sized (FingerTree (Elem a)) #-}
    {-# SPECIALIZE instance Sized (FingerTree (Node a)) #-}
    size EmptyT             = 0
    size (Single x)         = size x
    size (Deep v _ _ _)     = v

instance Foldable FingerTree where
    foldMap _ EmptyT = mempty
    foldMap f (Single x) = f x
    foldMap f (Deep _ pr m sf) =
        foldMap f pr `mappend` (foldMap (foldMap f) m `mappend` foldMap f sf)

    foldr _ z EmptyT = z
    foldr f z (Single x) = x `f` z
    foldr f z (Deep _ pr m sf) =
        foldr f (foldr (flip (foldr f)) (foldr f z sf) m) pr

    foldl _ z EmptyT = z
    foldl f z (Single x) = z `f` x
    foldl f z (Deep _ pr m sf) =
        foldl f (foldl (foldl f) (foldl f z pr) m) sf

    foldr1 _ EmptyT = error "foldr1: empty sequence"
    foldr1 _ (Single x) = x
    foldr1 f (Deep _ pr m sf) =
        foldr f (foldr (flip (foldr f)) (foldr1 f sf) m) pr

    foldl1 _ EmptyT = error "foldl1: empty sequence"
    foldl1 _ (Single x) = x
    foldl1 f (Deep _ pr m sf) =
        foldl f (foldl (foldl f) (foldl1 f pr) m) sf

instance Functor FingerTree where
    fmap _ EmptyT = EmptyT
    fmap f (Single x) = Single (f x)
    fmap f (Deep v pr m sf) =
        Deep v (fmap f pr) (fmap (fmap f) m) (fmap f sf)

instance Traversable FingerTree where
    traverse _ EmptyT = pure EmptyT
    traverse f (Single x) = Single <$> f x
    traverse f (Deep v pr m sf) =
        deep' v <$> traverse f pr <*> traverse (traverse f) m <*>
            traverse f sf

instance NFData a => NFData (FingerTree a) where
    rnf EmptyT = ()
    rnf (Single x) = rnf x
    rnf (Deep _ pr m sf) = rnf pr `seq` rnf sf `seq` rnf m

{-# INLINE deep #-}
deep            :: Sized a => Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
deep pr m sf    =  Deep (size pr + size m + size sf) pr m sf

{-# INLINE pullL #-}
pullL :: Int -> FingerTree (Node a) -> Digit a -> FingerTree a
pullL s m sf = case viewLTree m of
    Nothing2        -> digitToTree' s sf
    Just2 pr m'     -> Deep s (nodeToDigit pr) m' sf

{-# INLINE pullR #-}
pullR :: Int -> Digit a -> FingerTree (Node a) -> FingerTree a
pullR s pr m = case viewRTree m of
    Nothing2        -> digitToTree' s pr
    Just2 m' sf     -> Deep s pr m' (nodeToDigit sf)

{-# SPECIALIZE deepL :: Maybe (Digit (Elem a)) -> FingerTree (Node (Elem a)) -> Digit (Elem a) -> FingerTree (Elem a) #-}
{-# SPECIALIZE deepL :: Maybe (Digit (Node a)) -> FingerTree (Node (Node a)) -> Digit (Node a) -> FingerTree (Node a) #-}
deepL :: Sized a => Maybe (Digit a) -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL Nothing m sf      = pullL (size m + size sf) m sf
deepL (Just pr) m sf    = deep pr m sf

{-# SPECIALIZE deepR :: Digit (Elem a) -> FingerTree (Node (Elem a)) -> Maybe (Digit (Elem a)) -> FingerTree (Elem a) #-}
{-# SPECIALIZE deepR :: Digit (Node a) -> FingerTree (Node (Node a)) -> Maybe (Digit (Node a)) -> FingerTree (Node a) #-}
deepR :: Sized a => Digit a -> FingerTree (Node a) -> Maybe (Digit a) -> FingerTree a
deepR pr m Nothing      = pullR (size m + size pr) pr m
deepR pr m (Just sf)    = deep pr m sf

-- Digits

data Digit a
    = One a
    | Two a a
    | Three a a a
    | Four a a a a
#if TESTING
    deriving Show
#endif

instance Foldable Digit where
    foldMap f (One a) = f a
    foldMap f (Two a b) = f a `mappend` f b
    foldMap f (Three a b c) = f a `mappend` (f b `mappend` f c)
    foldMap f (Four a b c d) = f a `mappend` (f b `mappend` (f c `mappend` f d))

    foldr f z (One a) = a `f` z
    foldr f z (Two a b) = a `f` (b `f` z)
    foldr f z (Three a b c) = a `f` (b `f` (c `f` z))
    foldr f z (Four a b c d) = a `f` (b `f` (c `f` (d `f` z)))

    foldl f z (One a) = z `f` a
    foldl f z (Two a b) = (z `f` a) `f` b
    foldl f z (Three a b c) = ((z `f` a) `f` b) `f` c
    foldl f z (Four a b c d) = (((z `f` a) `f` b) `f` c) `f` d

    foldr1 _ (One a) = a
    foldr1 f (Two a b) = a `f` b
    foldr1 f (Three a b c) = a `f` (b `f` c)
    foldr1 f (Four a b c d) = a `f` (b `f` (c `f` d))

    foldl1 _ (One a) = a
    foldl1 f (Two a b) = a `f` b
    foldl1 f (Three a b c) = (a `f` b) `f` c
    foldl1 f (Four a b c d) = ((a `f` b) `f` c) `f` d

instance Functor Digit where
    {-# INLINE fmap #-}
    fmap f (One a) = One (f a)
    fmap f (Two a b) = Two (f a) (f b)
    fmap f (Three a b c) = Three (f a) (f b) (f c)
    fmap f (Four a b c d) = Four (f a) (f b) (f c) (f d)

instance Traversable Digit where
    {-# INLINE traverse #-}
    traverse f (One a) = One <$> f a
    traverse f (Two a b) = Two <$> f a <*> f b
    traverse f (Three a b c) = Three <$> f a <*> f b <*> f c
    traverse f (Four a b c d) = Four <$> f a <*> f b <*> f c <*> f d

instance NFData a => NFData (Digit a) where
    rnf (One a) = rnf a
    rnf (Two a b) = rnf a `seq` rnf b
    rnf (Three a b c) = rnf a `seq` rnf b `seq` rnf c
    rnf (Four a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

instance Sized a => Sized (Digit a) where
    {-# INLINE size #-}
    size = foldl1 (+) . fmap size

{-# SPECIALIZE digitToTree :: Digit (Elem a) -> FingerTree (Elem a) #-}
{-# SPECIALIZE digitToTree :: Digit (Node a) -> FingerTree (Node a) #-}
digitToTree     :: Sized a => Digit a -> FingerTree a
digitToTree (One a) = Single a
digitToTree (Two a b) = deep (One a) EmptyT (One b)
digitToTree (Three a b c) = deep (Two a b) EmptyT (One c)
digitToTree (Four a b c d) = deep (Two a b) EmptyT (Two c d)

-- | Given the size of a digit and the digit itself, efficiently converts
-- it to a FingerTree.
digitToTree' :: Int -> Digit a -> FingerTree a
digitToTree' n (Four a b c d) = Deep n (Two a b) EmptyT (Two c d)
digitToTree' n (Three a b c) = Deep n (Two a b) EmptyT (One c)
digitToTree' n (Two a b) = Deep n (One a) EmptyT (One b)
digitToTree' !_n (One a) = Single a

-- Nodes

data Node a
    = Node2 {-# UNPACK #-} !Int a a
    | Node3 {-# UNPACK #-} !Int a a a
#if TESTING
    deriving Show
#endif

-- Sometimes, we need to apply a Node2, Node3, or Deep constructor
-- to a size and pass the result to a function. If we calculate,
-- say, `Node2 n <$> x <*> y`, then according to -ddump-simpl,
-- GHC boxes up `n`, passes it to the strict constructor for `Node2`,
-- and passes the result to `fmap`. Using `node2'` instead prevents
-- this, forming a closure with the unboxed size.
{-# INLINE node2' #-}
node2' :: Int -> a -> a -> Node a
node2' !s = \a b -> Node2 s a b

{-# INLINE node3' #-}
node3' :: Int -> a -> a -> a -> Node a
node3' !s = \a b c -> Node3 s a b c

{-# INLINE deep' #-}
deep' :: Int -> Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
deep' !s = \pr m sf -> Deep s pr m sf

instance Foldable Node where
    foldMap f (Node2 _ a b) = f a `mappend` f b
    foldMap f (Node3 _ a b c) = f a `mappend` (f b `mappend` f c)

    foldr f z (Node2 _ a b) = a `f` (b `f` z)
    foldr f z (Node3 _ a b c) = a `f` (b `f` (c `f` z))

    foldl f z (Node2 _ a b) = (z `f` a) `f` b
    foldl f z (Node3 _ a b c) = ((z `f` a) `f` b) `f` c

instance Functor Node where
    {-# INLINE fmap #-}
    fmap f (Node2 v a b) = Node2 v (f a) (f b)
    fmap f (Node3 v a b c) = Node3 v (f a) (f b) (f c)

instance Traversable Node where
    {-# INLINE traverse #-}
    traverse f (Node2 v a b) = node2' v <$> f a <*> f b
    traverse f (Node3 v a b c) = node3' v <$> f a <*> f b <*> f c

instance NFData a => NFData (Node a) where
    rnf (Node2 _ a b) = rnf a `seq` rnf b
    rnf (Node3 _ a b c) = rnf a `seq` rnf b `seq` rnf c

instance Sized (Node a) where
    size (Node2 v _ _)      = v
    size (Node3 v _ _ _)    = v

{-# INLINE node2 #-}
node2           :: Sized a => a -> a -> Node a
node2 a b       =  Node2 (size a + size b) a b

{-# INLINE node3 #-}
node3           :: Sized a => a -> a -> a -> Node a
node3 a b c     =  Node3 (size a + size b + size c) a b c

nodeToDigit :: Node a -> Digit a
nodeToDigit (Node2 _ a b) = Two a b
nodeToDigit (Node3 _ a b c) = Three a b c

-- Elements

newtype Elem a  =  Elem { getElem :: a }
#if TESTING
    deriving Show
#endif

instance Sized (Elem a) where
    size _ = 1

instance Functor Elem where
#if __GLASGOW_HASKELL__ >= 708
-- This cuts the time for <*> by around a fifth.
    fmap = coerce
#else
    fmap f (Elem x) = Elem (f x)
#endif

instance Foldable Elem where
    foldMap f (Elem x) = f x
    foldr f z (Elem x) = f x z
    foldl f z (Elem x) = f z x

instance Traversable Elem where
    traverse f (Elem x) = Elem <$> f x

instance NFData a => NFData (Elem a) where
    rnf (Elem x) = rnf x

-------------------------------------------------------
-- Applicative construction
-------------------------------------------------------
#if !MIN_VERSION_base(4,8,0)
newtype Identity a = Identity {runIdentity :: a}

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity x = Identity (f x)
#endif

-- | This is essentially a clone of Control.Monad.State.Strict.
newtype State s a = State {runState :: s -> (s, a)}

instance Functor (State s) where
    fmap = liftA

instance Monad (State s) where
    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    return = pure
    m >>= k = State $ \ s -> case runState m s of
        (s', x) -> runState (k x) s'

instance Applicative (State s) where
    {-# INLINE pure #-}
    pure x = State $ \ s -> (s, x)
    (<*>) = ap

execState :: State s a -> s -> a
execState m x = snd (runState m x)

-- | 'applicativeTree' takes an Applicative-wrapped construction of a
-- piece of a FingerTree, assumed to always have the same size (which
-- is put in the second argument), and replicates it as many times as
-- specified.  This is a generalization of 'replicateA', which itself
-- is a generalization of many Data.Sequence methods.
{-# SPECIALIZE applicativeTree :: Int -> Int -> State s a -> State s (FingerTree a) #-}
{-# SPECIALIZE applicativeTree :: Int -> Int -> Identity a -> Identity (FingerTree a) #-}
-- Special note: the Identity specialization automatically does node sharing,
-- reducing memory usage of the resulting tree to /O(log n)/.
applicativeTree :: Applicative f => Int -> Int -> f a -> f (FingerTree a)
applicativeTree n !mSize m = case n of
    0 -> pure EmptyT
    1 -> fmap Single m
    2 -> deepA one emptyTree one
    3 -> deepA two emptyTree one
    4 -> deepA two emptyTree two
    5 -> deepA three emptyTree two
    6 -> deepA three emptyTree three
    _ -> case n `quotRem` 3 of
           (q,0) -> deepA three (applicativeTree (q - 2) mSize' n3) three
           (q,1) -> deepA two (applicativeTree (q - 1) mSize' n3) two
           (q,_) -> deepA three (applicativeTree (q - 1) mSize' n3) two
      where !mSize' = 3 * mSize
            n3 = liftA3 (node3' mSize') m m m
  where
    one = fmap One m
    two = liftA2 Two m m
    three = liftA3 Three m m m
    deepA = liftA3 (deep' (n * mSize))
    emptyTree = pure EmptyT

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

-- | /O(1)/. The empty sequence.
empty           :: Seq a
empty           =  Seq EmptyT

-- | /O(1)/. A singleton sequence.
singleton       :: a -> Seq a
singleton x     =  Seq (Single (Elem x))

-- | /O(log n)/. @replicate n x@ is a sequence consisting of @n@ copies of @x@.
replicate       :: Int -> a -> Seq a
replicate n x
  | n >= 0      = runIdentity (replicateA n (Identity x))
  | otherwise   = error "replicate takes a nonnegative integer argument"

-- | 'replicateA' is an 'Applicative' version of 'replicate', and makes
-- /O(log n)/ calls to '<*>' and 'pure'.
--
-- > replicateA n x = sequenceA (replicate n x)
replicateA :: Applicative f => Int -> f a -> f (Seq a)
replicateA n x
  | n >= 0      = Seq <$> applicativeTree n 1 (Elem <$> x)
  | otherwise   = error "replicateA takes a nonnegative integer argument"

-- | 'replicateM' is a sequence counterpart of 'Control.Monad.replicateM'.
--
-- > replicateM n x = sequence (replicate n x)
replicateM :: Monad m => Int -> m a -> m (Seq a)
replicateM n x
  | n >= 0      = unwrapMonad (replicateA n (WrapMonad x))
  | otherwise   = error "replicateM takes a nonnegative integer argument"

-- | @'cycleN' n xs@ concatenates @n@ copies of @xs@.
cycleN :: Int -> Seq a -> Seq a
cycleN n xs
  | n < 0     = error "cycleN takes a nonnegative integer argument"
  | n == 0    = empty
  | n == 1    = xs
cycleN n (Seq xsFT) = case rigidify xsFT of
             RigidEmpty -> empty
             RigidOne (Elem x) -> replicate n x
             RigidTwo x1 x2 -> Seq $
               Deep (n*2) pair
                    (runIdentity $ applicativeTree (n-2) 2 (Identity (node2 x1 x2)))
                    pair
               where pair = Two x1 x2
             RigidThree x1 x2 x3 -> Seq $
               Deep (n*3) triple
                    (runIdentity $ applicativeTree (n-2) 3 (Identity (node3 x1 x2 x3)))
                    triple
               where triple = Three x1 x2 x3
             RigidFull r@(Rigid s pr _m sf) -> Seq $
                   Deep (n*s)
                        (nodeToDigit pr)
                        (cycleNMiddle (n-2) r)
                        (nodeToDigit sf)

cycleNMiddle
  :: Int
     -> Rigid c
     -> FingerTree (Node c)

-- Not at the bottom yet

cycleNMiddle !n
           (Rigid s pr (DeepTh sm prm mm sfm) sf)
    = Deep (sm + s * (n + 1)) -- note: sm = s - size pr - size sf
           (digit12ToDigit prm)
           (cycleNMiddle n
                       (Rigid s (squashL pr prm) mm (squashR sfm sf)))
           (digit12ToDigit sfm)

-- At the bottom

cycleNMiddle n
           (Rigid s pr EmptyTh sf)
     = deep
            (One sf)
            (runIdentity $ applicativeTree n s (Identity converted))
            (One pr)
   where converted = node2 pr sf

cycleNMiddle n
           (Rigid s pr (SingleTh q) sf)
     = deep
            (Two q sf)
            (runIdentity $ applicativeTree n s (Identity converted))
            (Two pr q)
   where converted = node3 pr q sf


-- | /O(1)/. Add an element to the left end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(<|)            :: a -> Seq a -> Seq a
x <| Seq xs     =  Seq (Elem x `consTree` xs)

{-# SPECIALIZE consTree :: Elem a -> FingerTree (Elem a) -> FingerTree (Elem a) #-}
{-# SPECIALIZE consTree :: Node a -> FingerTree (Node a) -> FingerTree (Node a) #-}
consTree        :: Sized a => a -> FingerTree a -> FingerTree a
consTree a EmptyT       = Single a
consTree a (Single b)   = deep (One a) EmptyT (One b)
-- As described in the paper, we force the middle of a tree
-- *before* consing onto it; this preserves the amortized
-- bounds but prevents repeated consing from building up
-- gigantic suspensions.
consTree a (Deep s (Four b c d e) m sf) = m `seq`
    Deep (size a + s) (Two a b) (node3 c d e `consTree` m) sf
consTree a (Deep s (Three b c d) m sf) =
    Deep (size a + s) (Four a b c d) m sf
consTree a (Deep s (Two b c) m sf) =
    Deep (size a + s) (Three a b c) m sf
consTree a (Deep s (One b) m sf) =
    Deep (size a + s) (Two a b) m sf

-- | /O(1)/. Add an element to the right end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(|>)            :: Seq a -> a -> Seq a
Seq xs |> x     =  Seq (xs `snocTree` Elem x)

{-# SPECIALIZE snocTree :: FingerTree (Elem a) -> Elem a -> FingerTree (Elem a) #-}
{-# SPECIALIZE snocTree :: FingerTree (Node a) -> Node a -> FingerTree (Node a) #-}
snocTree        :: Sized a => FingerTree a -> a -> FingerTree a
snocTree EmptyT a       =  Single a
snocTree (Single a) b   =  deep (One a) EmptyT (One b)
-- See note on `seq` in `consTree`.
snocTree (Deep s pr m (Four a b c d)) e = m `seq`
    Deep (s + size e) pr (m `snocTree` node3 a b c) (Two d e)
snocTree (Deep s pr m (Three a b c)) d =
    Deep (s + size d) pr m (Four a b c d)
snocTree (Deep s pr m (Two a b)) c =
    Deep (s + size c) pr m (Three a b c)
snocTree (Deep s pr m (One a)) b =
    Deep (s + size b) pr m (Two a b)

-- | /O(log(min(n1,n2)))/. Concatenate two sequences.
(><)            :: Seq a -> Seq a -> Seq a
Seq xs >< Seq ys = Seq (appendTree0 xs ys)

-- The appendTree/addDigits gunk below is machine generated

appendTree0 :: FingerTree (Elem a) -> FingerTree (Elem a) -> FingerTree (Elem a)
appendTree0 EmptyT xs =
    xs
appendTree0 xs EmptyT =
    xs
appendTree0 (Single x) xs =
    x `consTree` xs
appendTree0 xs (Single x) =
    xs `snocTree` x
appendTree0 (Deep s1 pr1 m1 sf1) (Deep s2 pr2 m2 sf2) =
    Deep (s1 + s2) pr1 (addDigits0 m1 sf1 pr2 m2) sf2

{-# SPECIALIZE addDigits0 :: FingerTree (Node (Elem a)) -> Digit (Elem a) -> Digit (Elem a) -> FingerTree (Node (Elem a)) -> FingerTree (Node (Elem a)) #-}
{-# SPECIALIZE addDigits0 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a)) #-}
addDigits0 :: Sized a => FingerTree (Node a) -> Digit a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
addDigits0 m1 (One a) (One b) m2 =
    appendTree1 m1 (node2 a b) m2
addDigits0 m1 (One a) (Two b c) m2 =
    appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (One a) (Three b c d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (One a) (Four b c d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Two a b) (One c) m2 =
    appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (Two a b) (Two c d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (Two a b) (Three c d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Two a b) (Four c d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c) (One d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (Three a b c) (Two d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Three a b c) (Three d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c) (Four d e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits0 m1 (Four a b c d) (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Four a b c d) (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Four a b c d) (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits0 m1 (Four a b c d) (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2

appendTree1 :: FingerTree (Node a) -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree1 EmptyT a xs =
    a `consTree` xs
appendTree1 xs a EmptyT =
    xs `snocTree` a
appendTree1 (Single x) a xs =
    x `consTree` a `consTree` xs
appendTree1 xs a (Single x) =
    xs `snocTree` a `snocTree` x
appendTree1 (Deep s1 pr1 m1 sf1) a (Deep s2 pr2 m2 sf2) =
    Deep (s1 + size a + s2) pr1 (addDigits1 m1 sf1 a pr2 m2) sf2

addDigits1 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits1 m1 (One a) b (One c) m2 =
    appendTree1 m1 (node3 a b c) m2
addDigits1 m1 (One a) b (Two c d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits1 m1 (One a) b (Three c d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (One a) b (Four c d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b) c (One d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits1 m1 (Two a b) c (Two d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (Two a b) c (Three d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b) c (Four d e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Three a b c) d (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (Three a b c) d (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Three a b c) d (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Three a b c) d (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits1 m1 (Four a b c d) e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Four a b c d) e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Four a b c d) e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits1 m1 (Four a b c d) e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2

appendTree2 :: FingerTree (Node a) -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree2 EmptyT a b xs =
    a `consTree` b `consTree` xs
appendTree2 xs a b EmptyT =
    xs `snocTree` a `snocTree` b
appendTree2 (Single x) a b xs =
    x `consTree` a `consTree` b `consTree` xs
appendTree2 xs a b (Single x) =
    xs `snocTree` a `snocTree` b `snocTree` x
appendTree2 (Deep s1 pr1 m1 sf1) a b (Deep s2 pr2 m2 sf2) =
    Deep (s1 + size a + size b + s2) pr1 (addDigits2 m1 sf1 a b pr2 m2) sf2

addDigits2 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits2 m1 (One a) b c (One d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits2 m1 (One a) b c (Two d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits2 m1 (One a) b c (Three d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (One a) b c (Four d e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Two a b) c d (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits2 m1 (Two a b) c d (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Two a b) c d (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Two a b) c d (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Three a b c) d e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Three a b c) d e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Three a b c) d e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Three a b c) d e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (One g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Four a b c d) e f (Two g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Four a b c d) e f (Three g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (Four g h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2

appendTree3 :: FingerTree (Node a) -> Node a -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree3 EmptyT a b c xs =
    a `consTree` b `consTree` c `consTree` xs
appendTree3 xs a b c EmptyT =
    xs `snocTree` a `snocTree` b `snocTree` c
appendTree3 (Single x) a b c xs =
    x `consTree` a `consTree` b `consTree` c `consTree` xs
appendTree3 xs a b c (Single x) =
    xs `snocTree` a `snocTree` b `snocTree` c `snocTree` x
appendTree3 (Deep s1 pr1 m1 sf1) a b c (Deep s2 pr2 m2 sf2) =
    Deep (s1 + size a + size b + size c + s2) pr1 (addDigits3 m1 sf1 a b c pr2 m2) sf2

addDigits3 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits3 m1 (One a) b c d (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits3 m1 (One a) b c d (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (One a) b c d (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (One a) b c d (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (Two a b) c d e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Two a b) c d e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (One g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Three a b c) d e f (Two g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Three a b c) d e f (Three g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (Four g h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (One h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Four a b c d) e f g (Two h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Four a b c d) e f g (Three h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (Four h i j k) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2

appendTree4 :: FingerTree (Node a) -> Node a -> Node a -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree4 EmptyT a b c d xs =
    a `consTree` b `consTree` c `consTree` d `consTree` xs
appendTree4 xs a b c d EmptyT =
    xs `snocTree` a `snocTree` b `snocTree` c `snocTree` d
appendTree4 (Single x) a b c d xs =
    x `consTree` a `consTree` b `consTree` c `consTree` d `consTree` xs
appendTree4 xs a b c d (Single x) =
    xs `snocTree` a `snocTree` b `snocTree` c `snocTree` d `snocTree` x
appendTree4 (Deep s1 pr1 m1 sf1) a b c d (Deep s2 pr2 m2 sf2) =
    Deep (s1 + size a + size b + size c + size d + s2) pr1 (addDigits4 m1 sf1 a b c d pr2 m2) sf2

addDigits4 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits4 m1 (One a) b c d e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits4 m1 (One a) b c d e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (One a) b c d e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (One a) b c d e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (One g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (Two a b) c d e f (Two g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Two a b) c d e f (Three g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (Four g h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (One h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Three a b c) d e f g (Two h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Three a b c) d e f g (Three h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (Four h i j k) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (One i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Four a b c d) e f g h (Two i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Four a b c d) e f g h (Three i j k) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (Four i j k l) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node3 j k l) m2

-- | Builds a sequence from a seed value.  Takes time linear in the
-- number of generated elements.  /WARNING:/ If the number of generated
-- elements is infinite, this method will not terminate.
unfoldr :: (b -> Maybe (a, b)) -> b -> Seq a
unfoldr f = unfoldr' empty
  -- uses tail recursion rather than, for instance, the List implementation.
  where unfoldr' as b = maybe as (\ (a, b') -> unfoldr' (as |> a) b') (f b)

-- | @'unfoldl' f x@ is equivalent to @'reverse' ('unfoldr' ('fmap' swap . f) x)@.
unfoldl :: (b -> Maybe (b, a)) -> b -> Seq a
unfoldl f = unfoldl' empty
  where unfoldl' as b = maybe as (\ (b', a) -> unfoldl' (a <| as) b') (f b)

-- | /O(n)/.  Constructs a sequence by repeated application of a function
-- to a seed value.
--
-- > iterateN n f x = fromList (Prelude.take n (Prelude.iterate f x))
iterateN :: Int -> (a -> a) -> a -> Seq a
iterateN n f x
  | n >= 0      = replicateA n (State (\ y -> (f y, y))) `execState` x
  | otherwise   = error "iterateN takes a nonnegative integer argument"

------------------------------------------------------------------------
-- Deconstruction
------------------------------------------------------------------------

-- | /O(1)/. Is this the empty sequence?
null            :: Seq a -> Bool
null (Seq EmptyT) = True
null _            =  False

-- | /O(1)/. The number of elements in the sequence.
length          :: Seq a -> Int
length (Seq xs) =  size xs

-- Views

data Maybe2 a b = Nothing2 | Just2 a b

-- | View of the left end of a sequence.
data ViewL a
    = EmptyL        -- ^ empty sequence
    | a :< Seq a    -- ^ leftmost element and the rest of the sequence
#if __GLASGOW_HASKELL__
    deriving (Eq, Ord, Show, Read, Data)
#else
    deriving (Eq, Ord, Show, Read)
#endif

INSTANCE_TYPEABLE1(ViewL,viewLTc,"ViewL")

instance Functor ViewL where
    {-# INLINE fmap #-}
    fmap _ EmptyL       = EmptyL
    fmap f (x :< xs)    = f x :< fmap f xs

instance Foldable ViewL where
    foldr _ z EmptyL = z
    foldr f z (x :< xs) = f x (foldr f z xs)

    foldl _ z EmptyL = z
    foldl f z (x :< xs) = foldl f (f z x) xs

    foldl1 _ EmptyL = error "foldl1: empty view"
    foldl1 f (x :< xs) = foldl f x xs

instance Traversable ViewL where
    traverse _ EmptyL       = pure EmptyL
    traverse f (x :< xs)    = (:<) <$> f x <*> traverse f xs

-- | /O(1)/. Analyse the left end of a sequence.
viewl           ::  Seq a -> ViewL a
viewl (Seq xs)  =  case viewLTree xs of
    Nothing2 -> EmptyL
    Just2 (Elem x) xs' -> x :< Seq xs'

{-# SPECIALIZE viewLTree :: FingerTree (Elem a) -> Maybe2 (Elem a) (FingerTree (Elem a)) #-}
{-# SPECIALIZE viewLTree :: FingerTree (Node a) -> Maybe2 (Node a) (FingerTree (Node a)) #-}
viewLTree       :: Sized a => FingerTree a -> Maybe2 a (FingerTree a)
viewLTree EmptyT                = Nothing2
viewLTree (Single a)            = Just2 a EmptyT
viewLTree (Deep s (One a) m sf) = Just2 a (pullL (s - size a) m sf)
viewLTree (Deep s (Two a b) m sf) =
    Just2 a (Deep (s - size a) (One b) m sf)
viewLTree (Deep s (Three a b c) m sf) =
    Just2 a (Deep (s - size a) (Two b c) m sf)
viewLTree (Deep s (Four a b c d) m sf) =
    Just2 a (Deep (s - size a) (Three b c d) m sf)

-- | View of the right end of a sequence.
data ViewR a
    = EmptyR        -- ^ empty sequence
    | Seq a :> a    -- ^ the sequence minus the rightmost element,
            -- and the rightmost element
#if __GLASGOW_HASKELL__
    deriving (Eq, Ord, Show, Read, Data)
#else
    deriving (Eq, Ord, Show, Read)
#endif

INSTANCE_TYPEABLE1(ViewR,viewRTc,"ViewR")

instance Functor ViewR where
    {-# INLINE fmap #-}
    fmap _ EmptyR       = EmptyR
    fmap f (xs :> x)    = fmap f xs :> f x

instance Foldable ViewR where
    foldMap _ EmptyR = mempty
    foldMap f (xs :> x) = foldMap f xs `mappend` f x

    foldr _ z EmptyR = z
    foldr f z (xs :> x) = foldr f (f x z) xs

    foldl _ z EmptyR = z
    foldl f z (xs :> x) = foldl f z xs `f` x

    foldr1 _ EmptyR = error "foldr1: empty view"
    foldr1 f (xs :> x) = foldr f x xs
#if MIN_VERSION_base(4,8,0)
    -- The default definitions are sensible for ViewL, but not so much for
    -- ViewR.
    null EmptyR = True
    null (_ :> _) = False

    length = foldr' (\_ k -> k+1) 0
#endif

instance Traversable ViewR where
    traverse _ EmptyR       = pure EmptyR
    traverse f (xs :> x)    = (:>) <$> traverse f xs <*> f x

-- | /O(1)/. Analyse the right end of a sequence.
viewr           ::  Seq a -> ViewR a
viewr (Seq xs)  =  case viewRTree xs of
    Nothing2 -> EmptyR
    Just2 xs' (Elem x) -> Seq xs' :> x

{-# SPECIALIZE viewRTree :: FingerTree (Elem a) -> Maybe2 (FingerTree (Elem a)) (Elem a) #-}
{-# SPECIALIZE viewRTree :: FingerTree (Node a) -> Maybe2 (FingerTree (Node a)) (Node a) #-}
viewRTree       :: Sized a => FingerTree a -> Maybe2 (FingerTree a) a
viewRTree EmptyT                = Nothing2
viewRTree (Single z)            = Just2 EmptyT z
viewRTree (Deep s pr m (One z)) = Just2 (pullR (s - size z) pr m) z
viewRTree (Deep s pr m (Two y z)) =
    Just2 (Deep (s - size z) pr m (One y)) z
viewRTree (Deep s pr m (Three x y z)) =
    Just2 (Deep (s - size z) pr m (Two x y)) z
viewRTree (Deep s pr m (Four w x y z)) =
    Just2 (Deep (s - size z) pr m (Three w x y)) z

------------------------------------------------------------------------
-- Scans
--
-- These are not particularly complex applications of the Traversable
-- functor, though making the correspondence with Data.List exact
-- requires the use of (<|) and (|>).
--
-- Note that save for the single (<|) or (|>), we maintain the original
-- structure of the Seq, not having to do any restructuring of our own.
--
-- wasserman.louis@gmail.com, 5/23/09
------------------------------------------------------------------------

-- | 'scanl' is similar to 'foldl', but returns a sequence of reduced
-- values from the left:
--
-- > scanl f z (fromList [x1, x2, ...]) = fromList [z, z `f` x1, (z `f` x1) `f` x2, ...]
scanl :: (a -> b -> a) -> a -> Seq b -> Seq a
scanl f z0 xs = z0 <| snd (mapAccumL (\ x z -> let x' = f x z in (x', x')) z0 xs)

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument:
--
-- > scanl1 f (fromList [x1, x2, ...]) = fromList [x1, x1 `f` x2, ...]
scanl1 :: (a -> a -> a) -> Seq a -> Seq a
scanl1 f xs = case viewl xs of
    EmptyL          -> error "scanl1 takes a nonempty sequence as an argument"
    x :< xs'        -> scanl f x xs'

-- | 'scanr' is the right-to-left dual of 'scanl'.
scanr :: (a -> b -> b) -> b -> Seq a -> Seq b
scanr f z0 xs = snd (mapAccumR (\ z x -> let z' = f x z in (z', z')) z0 xs) |> z0

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: (a -> a -> a) -> Seq a -> Seq a
scanr1 f xs = case viewr xs of
    EmptyR          -> error "scanr1 takes a nonempty sequence as an argument"
    xs' :> x        -> scanr f x xs'

-- Indexing

-- | /O(log(min(i,n-i)))/. The element at the specified position,
-- counting from 0.  The argument should thus be a non-negative
-- integer less than the size of the sequence.
-- If the position is out of range, 'index' fails with an error.
index           :: Seq a -> Int -> a
index (Seq xs) i
  | 0 <= i && i < size xs = case lookupTree i xs of
                Place _ (Elem x) -> x
  | otherwise   = error "index out of bounds"

data Place a = Place {-# UNPACK #-} !Int a
#if TESTING
    deriving Show
#endif

{-# SPECIALIZE lookupTree :: Int -> FingerTree (Elem a) -> Place (Elem a) #-}
{-# SPECIALIZE lookupTree :: Int -> FingerTree (Node a) -> Place (Node a) #-}
lookupTree :: Sized a => Int -> FingerTree a -> Place a
lookupTree _ EmptyT = error "lookupTree of empty tree"
lookupTree i (Single x) = Place i x
lookupTree i (Deep totalSize pr m sf)
  | i < spr     =  lookupDigit i pr
  | i < spm     =  case lookupTree (i - spr) m of
                   Place i' xs -> lookupNode i' xs
  | otherwise   =  lookupDigit (i - spm) sf
  where
    spr     = size pr
    spm     = totalSize - size sf

{-# SPECIALIZE lookupNode :: Int -> Node (Elem a) -> Place (Elem a) #-}
{-# SPECIALIZE lookupNode :: Int -> Node (Node a) -> Place (Node a) #-}
lookupNode :: Sized a => Int -> Node a -> Place a
lookupNode i (Node2 _ a b)
  | i < sa      = Place i a
  | otherwise   = Place (i - sa) b
  where
    sa      = size a
lookupNode i (Node3 _ a b c)
  | i < sa      = Place i a
  | i < sab     = Place (i - sa) b
  | otherwise   = Place (i - sab) c
  where
    sa      = size a
    sab     = sa + size b

{-# SPECIALIZE lookupDigit :: Int -> Digit (Elem a) -> Place (Elem a) #-}
{-# SPECIALIZE lookupDigit :: Int -> Digit (Node a) -> Place (Node a) #-}
lookupDigit :: Sized a => Int -> Digit a -> Place a
lookupDigit i (One a) = Place i a
lookupDigit i (Two a b)
  | i < sa      = Place i a
  | otherwise   = Place (i - sa) b
  where
    sa      = size a
lookupDigit i (Three a b c)
  | i < sa      = Place i a
  | i < sab     = Place (i - sa) b
  | otherwise   = Place (i - sab) c
  where
    sa      = size a
    sab     = sa + size b
lookupDigit i (Four a b c d)
  | i < sa      = Place i a
  | i < sab     = Place (i - sa) b
  | i < sabc    = Place (i - sab) c
  | otherwise   = Place (i - sabc) d
  where
    sa      = size a
    sab     = sa + size b
    sabc    = sab + size c

-- | /O(log(min(i,n-i)))/. Replace the element at the specified position.
-- If the position is out of range, the original sequence is returned.
update          :: Int -> a -> Seq a -> Seq a
update i x      = adjust (const x) i

-- | /O(log(min(i,n-i)))/. Update the element at the specified position.
-- If the position is out of range, the original sequence is returned.
adjust          :: (a -> a) -> Int -> Seq a -> Seq a
adjust f i (Seq xs)
  | 0 <= i && i < size xs = Seq (adjustTree (const (fmap f)) i xs)
  | otherwise   = Seq xs

{-# SPECIALIZE adjustTree :: (Int -> Elem a -> Elem a) -> Int -> FingerTree (Elem a) -> FingerTree (Elem a) #-}
{-# SPECIALIZE adjustTree :: (Int -> Node a -> Node a) -> Int -> FingerTree (Node a) -> FingerTree (Node a) #-}
adjustTree      :: Sized a => (Int -> a -> a) ->
            Int -> FingerTree a -> FingerTree a
adjustTree _ _ EmptyT = error "adjustTree of empty tree"
adjustTree f i (Single x) = Single (f i x)
adjustTree f i (Deep s pr m sf)
  | i < spr     = Deep s (adjustDigit f i pr) m sf
  | i < spm     = Deep s pr (adjustTree (adjustNode f) (i - spr) m) sf
  | otherwise   = Deep s pr m (adjustDigit f (i - spm) sf)
  where
    spr     = size pr
    spm     = spr + size m

{-# SPECIALIZE adjustNode :: (Int -> Elem a -> Elem a) -> Int -> Node (Elem a) -> Node (Elem a) #-}
{-# SPECIALIZE adjustNode :: (Int -> Node a -> Node a) -> Int -> Node (Node a) -> Node (Node a) #-}
adjustNode      :: Sized a => (Int -> a -> a) -> Int -> Node a -> Node a
adjustNode f i (Node2 s a b)
  | i < sa      = Node2 s (f i a) b
  | otherwise   = Node2 s a (f (i - sa) b)
  where
    sa      = size a
adjustNode f i (Node3 s a b c)
  | i < sa      = Node3 s (f i a) b c
  | i < sab     = Node3 s a (f (i - sa) b) c
  | otherwise   = Node3 s a b (f (i - sab) c)
  where
    sa      = size a
    sab     = sa + size b

{-# SPECIALIZE adjustDigit :: (Int -> Elem a -> Elem a) -> Int -> Digit (Elem a) -> Digit (Elem a) #-}
{-# SPECIALIZE adjustDigit :: (Int -> Node a -> Node a) -> Int -> Digit (Node a) -> Digit (Node a) #-}
adjustDigit     :: Sized a => (Int -> a -> a) -> Int -> Digit a -> Digit a
adjustDigit f i (One a) = One (f i a)
adjustDigit f i (Two a b)
  | i < sa      = Two (f i a) b
  | otherwise   = Two a (f (i - sa) b)
  where
    sa      = size a
adjustDigit f i (Three a b c)
  | i < sa      = Three (f i a) b c
  | i < sab     = Three a (f (i - sa) b) c
  | otherwise   = Three a b (f (i - sab) c)
  where
    sa      = size a
    sab     = sa + size b
adjustDigit f i (Four a b c d)
  | i < sa      = Four (f i a) b c d
  | i < sab     = Four a (f (i - sa) b) c d
  | i < sabc    = Four a b (f (i - sab) c) d
  | otherwise   = Four a b c (f (i- sabc) d)
  where
    sa      = size a
    sab     = sa + size b
    sabc    = sab + size c

-- | /O(n)/. A generalization of 'fmap', 'mapWithIndex' takes a mapping
-- function that also depends on the element's index, and applies it to every
-- element in the sequence.
mapWithIndex :: (Int -> a -> b) -> Seq a -> Seq b
mapWithIndex f' (Seq xs') = Seq $ mapWithIndexTree (\s (Elem a) -> Elem (f' s a)) 0 xs'
 where
  {-# SPECIALIZE mapWithIndexTree :: (Int -> Elem y -> b) -> Int -> FingerTree (Elem y) -> FingerTree b #-}
  {-# SPECIALIZE mapWithIndexTree :: (Int -> Node y -> b) -> Int -> FingerTree (Node y) -> FingerTree b #-}
  mapWithIndexTree :: Sized a => (Int -> a -> b) -> Int -> FingerTree a -> FingerTree b
  mapWithIndexTree _ !_s EmptyT = EmptyT
  mapWithIndexTree f s (Single xs) = Single $ f s xs
  mapWithIndexTree f s (Deep n pr m sf) =
          Deep n
               (mapWithIndexDigit f s pr)
               (mapWithIndexTree (mapWithIndexNode f) sPspr m)
               (mapWithIndexDigit f sPsprm sf)
    where
      !sPspr = s + size pr
      !sPsprm = s + n - size sf

  {-# SPECIALIZE mapWithIndexDigit :: (Int -> Elem y -> b) -> Int -> Digit (Elem y) -> Digit b #-}
  {-# SPECIALIZE mapWithIndexDigit :: (Int -> Node y -> b) -> Int -> Digit (Node y) -> Digit b #-}
  mapWithIndexDigit :: Sized a => (Int -> a -> b) -> Int -> Digit a -> Digit b
  mapWithIndexDigit f !s (One a) = One (f s a)
  mapWithIndexDigit f s (Two a b) = Two (f s a) (f sPsa b)
    where
      !sPsa = s + size a
  mapWithIndexDigit f s (Three a b c) =
                                      Three (f s a) (f sPsa b) (f sPsab c)
    where
      !sPsa = s + size a
      !sPsab = sPsa + size b
  mapWithIndexDigit f s (Four a b c d) =
                          Four (f s a) (f sPsa b) (f sPsab c) (f sPsabc d)
    where
      !sPsa = s + size a
      !sPsab = sPsa + size b
      !sPsabc = sPsab + size c

  {-# SPECIALIZE mapWithIndexNode :: (Int -> Elem y -> b) -> Int -> Node (Elem y) -> Node b #-}
  {-# SPECIALIZE mapWithIndexNode :: (Int -> Node y -> b) -> Int -> Node (Node y) -> Node b #-}
  mapWithIndexNode :: Sized a => (Int -> a -> b) -> Int -> Node a -> Node b
  mapWithIndexNode f s (Node2 ns a b) = Node2 ns (f s a) (f sPsa b)
    where
      !sPsa = s + size a
  mapWithIndexNode f s (Node3 ns a b c) =
                                     Node3 ns (f s a) (f sPsa b) (f sPsab c)
    where
      !sPsa = s + size a
      !sPsab = sPsa + size b

#ifdef __GLASGOW_HASKELL__
{-# NOINLINE [1] mapWithIndex #-}
{-# RULES
"mapWithIndex/mapWithIndex" forall f g xs . mapWithIndex f (mapWithIndex g xs) =
  mapWithIndex (\k a -> f k (g k a)) xs
"mapWithIndex/fmapSeq" forall f g xs . mapWithIndex f (fmapSeq g xs) =
  mapWithIndex (\k a -> f k (g a)) xs
"fmapSeq/mapWithIndex" forall f g xs . fmapSeq f (mapWithIndex g xs) =
  mapWithIndex (\k a -> f (g k a)) xs
 #-}
#endif

-- | 'traverseWithIndex' is a version of 'traverse' that also offers
-- access to the index of each element.
--
-- @since 0.5.8
traverseWithIndex :: Applicative f => (Int -> a -> f b) -> Seq a -> f (Seq b)
traverseWithIndex f' (Seq xs') = Seq <$> traverseWithIndexTreeE (\s (Elem a) -> Elem <$> f' s a) 0 xs'
 where
-- We have to specialize these functions by hand, unfortunately, because
-- GHC does not specialize until *all* instances are determined.
-- Although the Sized instance is known at compile time, the Applicative
-- instance generally is not.
  traverseWithIndexTreeE :: Applicative f => (Int -> Elem a -> f b) -> Int -> FingerTree (Elem a) -> f (FingerTree b)
  traverseWithIndexTreeE _ !_s EmptyT = pure EmptyT
  traverseWithIndexTreeE f s (Single xs) = Single <$> f s xs
  traverseWithIndexTreeE f s (Deep n pr m sf) =
          deep' n <$>
               traverseWithIndexDigitE f s pr <*>
               traverseWithIndexTreeN (traverseWithIndexNodeE f) sPspr m <*>
               traverseWithIndexDigitE f sPsprm sf
    where
      !sPspr = s + size pr
      !sPsprm = s + n - size sf

  traverseWithIndexTreeN :: Applicative f => (Int -> Node a -> f b) -> Int -> FingerTree (Node a) -> f (FingerTree b)
  traverseWithIndexTreeN _ !_s EmptyT = pure EmptyT
  traverseWithIndexTreeN f s (Single xs) = Single <$> f s xs
  traverseWithIndexTreeN f s (Deep n pr m sf) =
          deep' n <$>
               traverseWithIndexDigitN f s pr <*>
               traverseWithIndexTreeN (traverseWithIndexNodeN f) sPspr m <*>
               traverseWithIndexDigitN f sPsprm sf
    where
      !sPspr = s + size pr
      !sPsprm = s + n - size sf

  traverseWithIndexDigitE :: Applicative f => (Int -> Elem a -> f b) -> Int -> Digit (Elem a) -> f (Digit b)
  traverseWithIndexDigitE f i t = traverseWithIndexDigit f i t

  traverseWithIndexDigitN :: Applicative f => (Int -> Node a -> f b) -> Int -> Digit (Node a) -> f (Digit b)
  traverseWithIndexDigitN f i t = traverseWithIndexDigit f i t

  {-# INLINE traverseWithIndexDigit #-}
  traverseWithIndexDigit :: (Applicative f, Sized a) => (Int -> a -> f b) -> Int -> Digit a -> f (Digit b)
  traverseWithIndexDigit f !s (One a) = One <$> f s a
  traverseWithIndexDigit f s (Two a b) = Two <$> f s a <*> f sPsa b
    where
      !sPsa = s + size a
  traverseWithIndexDigit f s (Three a b c) =
                                      Three <$> f s a <*> f sPsa b <*> f sPsab c
    where
      !sPsa = s + size a
      !sPsab = sPsa + size b
  traverseWithIndexDigit f s (Four a b c d) =
                          Four <$> f s a <*> f sPsa b <*> f sPsab c <*> f sPsabc d
    where
      !sPsa = s + size a
      !sPsab = sPsa + size b
      !sPsabc = sPsab + size c

  traverseWithIndexNodeE :: Applicative f => (Int -> Elem a -> f b) -> Int -> Node (Elem a) -> f (Node b)
  traverseWithIndexNodeE f i t = traverseWithIndexNode f i t

  traverseWithIndexNodeN :: Applicative f => (Int -> Node a -> f b) -> Int -> Node (Node a) -> f (Node b)
  traverseWithIndexNodeN f i t = traverseWithIndexNode f i t

  {-# INLINE traverseWithIndexNode #-}
  traverseWithIndexNode :: (Applicative f, Sized a) => (Int -> a -> f b) -> Int -> Node a -> f (Node b)
  traverseWithIndexNode f !s (Node2 ns a b) = node2' ns <$> f s a <*> f sPsa b
    where
      !sPsa = s + size a
  traverseWithIndexNode f s (Node3 ns a b c) =
                                     node3' ns <$> f s a <*> f sPsa b <*> f sPsab c
    where
      !sPsa = s + size a
      !sPsab = sPsa + size b


{-# NOINLINE [1] traverseWithIndex #-}
#ifdef __GLASGOW_HASKELL__
{-# RULES
"travWithIndex/mapWithIndex" forall f g xs . traverseWithIndex f (mapWithIndex g xs) =
  traverseWithIndex (\k a -> f k (g k a)) xs
"travWithIndex/fmapSeq" forall f g xs . traverseWithIndex f (fmapSeq g xs) =
  traverseWithIndex (\k a -> f k (g a)) xs
 #-}
#endif
{-
It might be nice to be able to rewrite

traverseWithIndex f (fromFunction i g)
to
replicateAWithIndex i (\k -> f k (g k))
and
traverse f (fromFunction i g)
to
replicateAWithIndex i (f . g)

but we don't have replicateAWithIndex as yet.

We might wish for a rule like
"fmapSeq/travWithIndex" forall f g xs . fmapSeq f <$> traverseWithIndex g xs =
  traverseWithIndex (\k a -> f <$> g k a) xs
Unfortunately, this rule could screw up the inliner's treatment of
fmap in general, and it also relies on the arbitrary Functor being
valid.
-}


-- | /O(n)/. Convert a given sequence length and a function representing that
-- sequence into a sequence.
fromFunction :: Int -> (Int -> a) -> Seq a
fromFunction len f | len < 0 = error "Data.Sequence.fromFunction called with negative len"
                   | len == 0 = empty
                   | otherwise = Seq $ create (lift_elem f) 1 0 len
  where
    create :: (Int -> a) -> Int -> Int -> Int -> FingerTree a
    create b{-tree_builder-} !s{-tree_size-} !i{-start_index-} trees = case trees of
       1 -> Single $ b i
       2 -> Deep (2*s) (One (b i)) EmptyT (One (b (i+s)))
       3 -> Deep (3*s) (createTwo i) EmptyT (One (b (i+2*s)))
       4 -> Deep (4*s) (createTwo i) EmptyT (createTwo (i+2*s))
       5 -> Deep (5*s) (createThree i) EmptyT (createTwo (i+3*s))
       6 -> Deep (6*s) (createThree i) EmptyT (createThree (i+3*s))
       _ -> case trees `quotRem` 3 of
           (trees', 1) -> Deep (trees*s) (createTwo i)
                              (create mb (3*s) (i+2*s) (trees'-1))
                              (createTwo (i+(2+3*(trees'-1))*s))
           (trees', 2) -> Deep (trees*s) (createThree i)
                              (create mb (3*s) (i+3*s) (trees'-1))
                              (createTwo (i+(3+3*(trees'-1))*s))
           (trees', _) -> Deep (trees*s) (createThree i)
                              (create mb (3*s) (i+3*s) (trees'-2))
                              (createThree (i+(3+3*(trees'-2))*s))
      where
        createTwo j = Two (b j) (b (j + s))
        {-# INLINE createTwo #-}
        createThree j = Three (b j) (b (j + s)) (b (j + 2*s))
        {-# INLINE createThree #-}
        mb j = Node3 (3*s) (b j) (b (j + s)) (b (j + 2*s))
        {-# INLINE mb #-}

    lift_elem :: (Int -> a) -> (Int -> Elem a)
#if __GLASGOW_HASKELL__ >= 708
    lift_elem g = coerce g
#else
    lift_elem g = Elem . g
#endif
    {-# INLINE lift_elem #-}

-- | /O(n)/. Create a sequence consisting of the elements of an 'Array'.
-- Note that the resulting sequence elements may be evaluated lazily (as on GHC),
-- so you must force the entire structure to be sure that the original array
-- can be garbage-collected.
fromArray :: Ix i => Array i a -> Seq a
#ifdef __GLASGOW_HASKELL__
fromArray a = fromFunction (GHC.Arr.numElements a) (GHC.Arr.unsafeAt a)
 where
  -- The following definition uses (Ix i) constraing, which is needed for the
  -- other fromArray definition.
  _ = Data.Array.rangeSize (Data.Array.bounds a)
#else
fromArray a = fromList2 (Data.Array.rangeSize (Data.Array.bounds a)) (Data.Array.elems a)
#endif

-- Splitting

-- | /O(log(min(i,n-i)))/. The first @i@ elements of a sequence.
-- If @i@ is negative, @'take' i s@ yields the empty sequence.
-- If the sequence contains fewer than @i@ elements, the whole sequence
-- is returned.
take            :: Int -> Seq a -> Seq a
take i          =  fst . splitAt' i

-- | /O(log(min(i,n-i)))/. Elements of a sequence after the first @i@.
-- If @i@ is negative, @'drop' i s@ yields the whole sequence.
-- If the sequence contains fewer than @i@ elements, the empty sequence
-- is returned.
drop            :: Int -> Seq a -> Seq a
drop i          =  snd . splitAt' i

-- | /O(log(min(i,n-i)))/. Split a sequence at a given position.
-- @'splitAt' i s = ('take' i s, 'drop' i s)@.
splitAt                 :: Int -> Seq a -> (Seq a, Seq a)
splitAt i (Seq xs)      =  (Seq l, Seq r)
  where (l, r)          =  split i xs

-- | /O(log(min(i,n-i))) A strict version of 'splitAt'.
splitAt'                 :: Int -> Seq a -> (Seq a, Seq a)
splitAt' i (Seq xs)      = case split i xs of
                             (l, r) -> (Seq l, Seq r)

split :: Int -> FingerTree (Elem a) ->
    (FingerTree (Elem a), FingerTree (Elem a))
split !_i EmptyT  = (EmptyT, EmptyT)
split i xs
  | size xs > i = case splitTree i xs of
                    Split l x r -> (l, consTree x r)
  | otherwise   = (xs, EmptyT)

data Split t a = Split t a t
#if TESTING
    deriving Show
#endif

{-# SPECIALIZE splitTree :: Int -> FingerTree (Elem a) -> Split (FingerTree (Elem a)) (Elem a) #-}
{-# SPECIALIZE splitTree :: Int -> FingerTree (Node a) -> Split (FingerTree (Node a)) (Node a) #-}
splitTree :: Sized a => Int -> FingerTree a -> Split (FingerTree a) a
splitTree _ EmptyT = error "splitTree of empty tree"
splitTree !_i (Single x) = Split EmptyT x EmptyT
splitTree i (Deep _ pr m sf)
  | i < spr     = case splitDigit i pr of
            Split l x r -> Split (maybe EmptyT digitToTree l) x (deepL r m sf)
  | i < spm     = case splitTree im m of
            Split ml xs mr -> case splitNode (im - size ml) xs of
                Split l x r -> Split (deepR pr ml l) x (deepL r mr sf)
  | otherwise   = case splitDigit (i - spm) sf of
            Split l x r -> Split (deepR pr m l) x (maybe EmptyT digitToTree r)
  where
    spr     = size pr
    spm     = spr + size m
    im      = i - spr

{-# SPECIALIZE splitNode :: Int -> Node (Elem a) -> Split (Maybe (Digit (Elem a))) (Elem a) #-}
{-# SPECIALIZE splitNode :: Int -> Node (Node a) -> Split (Maybe (Digit (Node a))) (Node a) #-}
splitNode :: Sized a => Int -> Node a -> Split (Maybe (Digit a)) a
splitNode i (Node2 _ a b)
  | i < sa      = Split Nothing a (Just (One b))
  | otherwise   = Split (Just (One a)) b Nothing
  where
    sa      = size a
splitNode i (Node3 _ a b c)
  | i < sa      = Split Nothing a (Just (Two b c))
  | i < sab     = Split (Just (One a)) b (Just (One c))
  | otherwise   = Split (Just (Two a b)) c Nothing
  where
    sa      = size a
    sab     = sa + size b

{-# SPECIALIZE splitDigit :: Int -> Digit (Elem a) -> Split (Maybe (Digit (Elem a))) (Elem a) #-}
{-# SPECIALIZE splitDigit :: Int -> Digit (Node a) -> Split (Maybe (Digit (Node a))) (Node a) #-}
splitDigit :: Sized a => Int -> Digit a -> Split (Maybe (Digit a)) a
splitDigit !_i (One a) = Split Nothing a Nothing
splitDigit i (Two a b)
  | i < sa      = Split Nothing a (Just (One b))
  | otherwise   = Split (Just (One a)) b Nothing
  where
    sa      = size a
splitDigit i (Three a b c)
  | i < sa      = Split Nothing a (Just (Two b c))
  | i < sab     = Split (Just (One a)) b (Just (One c))
  | otherwise   = Split (Just (Two a b)) c Nothing
  where
    sa      = size a
    sab     = sa + size b
splitDigit i (Four a b c d)
  | i < sa      = Split Nothing a (Just (Three b c d))
  | i < sab     = Split (Just (One a)) b (Just (Two c d))
  | i < sabc    = Split (Just (Two a b)) c (Just (One d))
  | otherwise   = Split (Just (Three a b c)) d Nothing
  where
    sa      = size a
    sab     = sa + size b
    sabc    = sab + size c

-- | /O(n)/.  Returns a sequence of all suffixes of this sequence,
-- longest first.  For example,
--
-- > tails (fromList "abc") = fromList [fromList "abc", fromList "bc", fromList "c", fromList ""]
--
-- Evaluating the /i/th suffix takes /O(log(min(i, n-i)))/, but evaluating
-- every suffix in the sequence takes /O(n)/ due to sharing.
tails                   :: Seq a -> Seq (Seq a)
tails (Seq xs)          = Seq (tailsTree (Elem . Seq) xs) |> empty

-- | /O(n)/.  Returns a sequence of all prefixes of this sequence,
-- shortest first.  For example,
--
-- > inits (fromList "abc") = fromList [fromList "", fromList "a", fromList "ab", fromList "abc"]
--
-- Evaluating the /i/th prefix takes /O(log(min(i, n-i)))/, but evaluating
-- every prefix in the sequence takes /O(n)/ due to sharing.
inits                   :: Seq a -> Seq (Seq a)
inits (Seq xs)          = empty <| Seq (initsTree (Elem . Seq) xs)

-- This implementation of tails (and, analogously, inits) has the
-- following algorithmic advantages:
--      Evaluating each tail in the sequence takes linear total time,
--      which is better than we could say for
--              @fromList [drop n xs | n <- [0..length xs]]@.
--      Evaluating any individual tail takes logarithmic time, which is
--      better than we can say for either
--              @scanr (<|) empty xs@ or @iterateN (length xs + 1) (\ xs -> let _ :< xs' = viewl xs in xs') xs@.
--
-- Moreover, if we actually look at every tail in the sequence, the
-- following benchmarks demonstrate that this implementation is modestly
-- faster than any of the above:
--
-- Times (ms)
--               min      mean    +/-sd    median    max
-- Seq.tails:   21.986   24.961   10.169   22.417   86.485
-- scanr:       85.392   87.942    2.488   87.425  100.217
-- iterateN:       29.952   31.245    1.574   30.412   37.268
--
-- The algorithm for tails (and, analogously, inits) is as follows:
--
-- A Node in the FingerTree of tails is constructed by evaluating the
-- corresponding tail of the FingerTree of Nodes, considering the first
-- Node in this tail, and constructing a Node in which each tail of this
-- Node is made to be the prefix of the remaining tree.  This ends up
-- working quite elegantly, as the remainder of the tail of the FingerTree
-- of Nodes becomes the middle of a new tail, the suffix of the Node is
-- the prefix, and the suffix of the original tree is retained.
--
-- In particular, evaluating the /i/th tail involves making as
-- many partial evaluations as the Node depth of the /i/th element.
-- In addition, when we evaluate the /i/th tail, and we also evaluate
-- the /j/th tail, and /m/ Nodes are on the path to both /i/ and /j/,
-- each of those /m/ evaluations are shared between the computation of
-- the /i/th and /j/th tails.
--
-- wasserman.louis@gmail.com, 7/16/09

tailsDigit :: Digit a -> Digit (Digit a)
tailsDigit (One a) = One (One a)
tailsDigit (Two a b) = Two (Two a b) (One b)
tailsDigit (Three a b c) = Three (Three a b c) (Two b c) (One c)
tailsDigit (Four a b c d) = Four (Four a b c d) (Three b c d) (Two c d) (One d)

initsDigit :: Digit a -> Digit (Digit a)
initsDigit (One a) = One (One a)
initsDigit (Two a b) = Two (One a) (Two a b)
initsDigit (Three a b c) = Three (One a) (Two a b) (Three a b c)
initsDigit (Four a b c d) = Four (One a) (Two a b) (Three a b c) (Four a b c d)

tailsNode :: Node a -> Node (Digit a)
tailsNode (Node2 s a b) = Node2 s (Two a b) (One b)
tailsNode (Node3 s a b c) = Node3 s (Three a b c) (Two b c) (One c)

initsNode :: Node a -> Node (Digit a)
initsNode (Node2 s a b) = Node2 s (One a) (Two a b)
initsNode (Node3 s a b c) = Node3 s (One a) (Two a b) (Three a b c)

{-# SPECIALIZE tailsTree :: (FingerTree (Elem a) -> Elem b) -> FingerTree (Elem a) -> FingerTree (Elem b) #-}
{-# SPECIALIZE tailsTree :: (FingerTree (Node a) -> Node b) -> FingerTree (Node a) -> FingerTree (Node b) #-}
-- | Given a function to apply to tails of a tree, applies that function
-- to every tail of the specified tree.
tailsTree :: Sized a => (FingerTree a -> b) -> FingerTree a -> FingerTree b
tailsTree _ EmptyT = EmptyT
tailsTree f (Single x) = Single (f (Single x))
tailsTree f (Deep n pr m sf) =
    Deep n (fmap (\ pr' -> f (deep pr' m sf)) (tailsDigit pr))
        (tailsTree f' m)
        (fmap (f . digitToTree) (tailsDigit sf))
  where
    f' ms = let Just2 node m' = viewLTree ms in
        fmap (\ pr' -> f (deep pr' m' sf)) (tailsNode node)

{-# SPECIALIZE initsTree :: (FingerTree (Elem a) -> Elem b) -> FingerTree (Elem a) -> FingerTree (Elem b) #-}
{-# SPECIALIZE initsTree :: (FingerTree (Node a) -> Node b) -> FingerTree (Node a) -> FingerTree (Node b) #-}
-- | Given a function to apply to inits of a tree, applies that function
-- to every init of the specified tree.
initsTree :: Sized a => (FingerTree a -> b) -> FingerTree a -> FingerTree b
initsTree _ EmptyT = EmptyT
initsTree f (Single x) = Single (f (Single x))
initsTree f (Deep n pr m sf) =
    Deep n (fmap (f . digitToTree) (initsDigit pr))
        (initsTree f' m)
        (fmap (f . deep pr m) (initsDigit sf))
  where
    f' ms =  let Just2 m' node = viewRTree ms in
             fmap (\ sf' -> f (deep pr m' sf')) (initsNode node)

{-# INLINE foldlWithIndex #-}
-- | 'foldlWithIndex' is a version of 'foldl' that also provides access
-- to the index of each element.
foldlWithIndex :: (b -> Int -> a -> b) -> b -> Seq a -> b
foldlWithIndex f z xs = foldl (\ g x !i -> f (g (i - 1)) i x) (const z) xs (length xs - 1)

{-# INLINE foldrWithIndex #-}
-- | 'foldrWithIndex' is a version of 'foldr' that also provides access
-- to the index of each element.
foldrWithIndex :: (Int -> a -> b -> b) -> b -> Seq a -> b
foldrWithIndex f z xs = foldr (\ x g !i -> f i x (g (i+1))) (const z) xs 0

{-# INLINE listToMaybe' #-}
-- 'listToMaybe\'' is a good consumer version of 'listToMaybe'.
listToMaybe' :: [a] -> Maybe a
listToMaybe' = foldr (\ x _ -> Just x) Nothing

-- | /O(i)/ where /i/ is the prefix length.  'takeWhileL', applied
-- to a predicate @p@ and a sequence @xs@, returns the longest prefix
-- (possibly empty) of @xs@ of elements that satisfy @p@.
takeWhileL :: (a -> Bool) -> Seq a -> Seq a
takeWhileL p = fst . spanl p

-- | /O(i)/ where /i/ is the suffix length.  'takeWhileR', applied
-- to a predicate @p@ and a sequence @xs@, returns the longest suffix
-- (possibly empty) of @xs@ of elements that satisfy @p@.
--
-- @'takeWhileR' p xs@ is equivalent to @'reverse' ('takeWhileL' p ('reverse' xs))@.
takeWhileR :: (a -> Bool) -> Seq a -> Seq a
takeWhileR p = fst . spanr p

-- | /O(i)/ where /i/ is the prefix length.  @'dropWhileL' p xs@ returns
-- the suffix remaining after @'takeWhileL' p xs@.
dropWhileL :: (a -> Bool) -> Seq a -> Seq a
dropWhileL p = snd . spanl p

-- | /O(i)/ where /i/ is the suffix length.  @'dropWhileR' p xs@ returns
-- the prefix remaining after @'takeWhileR' p xs@.
--
-- @'dropWhileR' p xs@ is equivalent to @'reverse' ('dropWhileL' p ('reverse' xs))@.
dropWhileR :: (a -> Bool) -> Seq a -> Seq a
dropWhileR p = snd . spanr p

-- | /O(i)/ where /i/ is the prefix length.  'spanl', applied to
-- a predicate @p@ and a sequence @xs@, returns a pair whose first
-- element is the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@ and the second element is the remainder of the sequence.
spanl :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
spanl p = breakl (not . p)

-- | /O(i)/ where /i/ is the suffix length.  'spanr', applied to a
-- predicate @p@ and a sequence @xs@, returns a pair whose /first/ element
-- is the longest /suffix/ (possibly empty) of @xs@ of elements that
-- satisfy @p@ and the second element is the remainder of the sequence.
spanr :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
spanr p = breakr (not . p)

{-# INLINE breakl #-}
-- | /O(i)/ where /i/ is the breakpoint index.  'breakl', applied to a
-- predicate @p@ and a sequence @xs@, returns a pair whose first element
-- is the longest prefix (possibly empty) of @xs@ of elements that
-- /do not satisfy/ @p@ and the second element is the remainder of
-- the sequence.
--
-- @'breakl' p@ is equivalent to @'spanl' (not . p)@.
breakl :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
breakl p xs = foldr (\ i _ -> splitAt i xs) (xs, empty) (findIndicesL p xs)

{-# INLINE breakr #-}
-- | @'breakr' p@ is equivalent to @'spanr' (not . p)@.
breakr :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
breakr p xs = foldr (\ i _ -> flipPair (splitAt (i + 1) xs)) (xs, empty) (findIndicesR p xs)
  where flipPair (x, y) = (y, x)

-- | /O(n)/.  The 'partition' function takes a predicate @p@ and a
-- sequence @xs@ and returns sequences of those elements which do and
-- do not satisfy the predicate.
partition :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
partition p = foldl part (empty, empty)
  where
    part (xs, ys) x
      | p x         = (xs |> x, ys)
      | otherwise   = (xs, ys |> x)

-- | /O(n)/.  The 'filter' function takes a predicate @p@ and a sequence
-- @xs@ and returns a sequence of those elements which satisfy the
-- predicate.
filter :: (a -> Bool) -> Seq a -> Seq a
filter p = foldl' (\ xs x -> if p x then xs |> x else xs) empty

-- Indexing sequences

-- | 'elemIndexL' finds the leftmost index of the specified element,
-- if it is present, and otherwise 'Nothing'.
elemIndexL :: Eq a => a -> Seq a -> Maybe Int
elemIndexL x = findIndexL (x ==)

-- | 'elemIndexR' finds the rightmost index of the specified element,
-- if it is present, and otherwise 'Nothing'.
elemIndexR :: Eq a => a -> Seq a -> Maybe Int
elemIndexR x = findIndexR (x ==)

-- | 'elemIndicesL' finds the indices of the specified element, from
-- left to right (i.e. in ascending order).
elemIndicesL :: Eq a => a -> Seq a -> [Int]
elemIndicesL x = findIndicesL (x ==)

-- | 'elemIndicesR' finds the indices of the specified element, from
-- right to left (i.e. in descending order).
elemIndicesR :: Eq a => a -> Seq a -> [Int]
elemIndicesR x = findIndicesR (x ==)

-- | @'findIndexL' p xs@ finds the index of the leftmost element that
-- satisfies @p@, if any exist.
findIndexL :: (a -> Bool) -> Seq a -> Maybe Int
findIndexL p = listToMaybe' . findIndicesL p

-- | @'findIndexR' p xs@ finds the index of the rightmost element that
-- satisfies @p@, if any exist.
findIndexR :: (a -> Bool) -> Seq a -> Maybe Int
findIndexR p = listToMaybe' . findIndicesR p

{-# INLINE findIndicesL #-}
-- | @'findIndicesL' p@ finds all indices of elements that satisfy @p@,
-- in ascending order.
findIndicesL :: (a -> Bool) -> Seq a -> [Int]
#if __GLASGOW_HASKELL__
findIndicesL p xs = build (\ c n -> let g i x z = if p x then c i z else z in
                foldrWithIndex g n xs)
#else
findIndicesL p xs = foldrWithIndex g [] xs
  where g i x is = if p x then i:is else is
#endif

{-# INLINE findIndicesR #-}
-- | @'findIndicesR' p@ finds all indices of elements that satisfy @p@,
-- in descending order.
findIndicesR :: (a -> Bool) -> Seq a -> [Int]
#if __GLASGOW_HASKELL__
findIndicesR p xs = build (\ c n ->
    let g z i x = if p x then c i z else z in foldlWithIndex g n xs)
#else
findIndicesR p xs = foldlWithIndex g [] xs
  where g is i x = if p x then i:is else is
#endif

------------------------------------------------------------------------
-- Lists
------------------------------------------------------------------------

-- The implementation below, by Ross Paterson, avoids the rebuilding
-- the previous (|>)-based implementation suffered from.

-- | /O(n)/. Create a sequence from a finite list of elements.
-- There is a function 'toList' in the opposite direction for all
-- instances of the 'Foldable' class, including 'Seq'.
fromList        :: [a] -> Seq a
fromList = Seq . mkTree 1 . map_elem
  where
    {-# SPECIALIZE mkTree :: Int -> [Elem a] -> FingerTree (Elem a) #-}
    {-# SPECIALIZE mkTree :: Int -> [Node a] -> FingerTree (Node a) #-}
    mkTree :: (Sized a) => Int -> [a] -> FingerTree a
    mkTree !_ [] = EmptyT
    mkTree _ [x1] = Single x1
    mkTree s [x1, x2] = Deep (2*s) (One x1) EmptyT (One x2)
    mkTree s [x1, x2, x3] = Deep (3*s) (One x1) EmptyT (Two x2 x3)
    mkTree s (x1:x2:x3:x4:xs) = case getNodes (3*s) x4 xs of
      (ns, sf) -> case mkTree (3*s) ns of
        !m -> Deep (3*size x1 + size m + size sf) (Three x1 x2 x3) m sf

    getNodes :: Int -> a -> [a] -> ([Node a], Digit a)
    getNodes !_ x1 [] = ([], One x1)
    getNodes _ x1 [x2] = ([], Two x1 x2)
    getNodes _ x1 [x2, x3] = ([], Three x1 x2 x3)
    getNodes s x1 (x2:x3:x4:xs) = (Node3 s x1 x2 x3:ns, d)
       where (ns, d) = getNodes s x4 xs

    map_elem :: [a] -> [Elem a]
#if __GLASGOW_HASKELL__ >= 708
    map_elem xs = coerce xs
#else
    map_elem xs = Data.List.map Elem xs
#endif
    {-# INLINE map_elem #-}

#if __GLASGOW_HASKELL__ >= 708
instance GHC.Exts.IsList (Seq a) where
    type Item (Seq a) = a
    fromList = fromList
    fromListN = fromList2
    toList = toList
#endif

#ifdef __GLASGOW_HASKELL__
instance IsString (Seq Char) where
    fromString = fromList
#endif

------------------------------------------------------------------------
-- Reverse
------------------------------------------------------------------------

-- | /O(n)/. The reverse of a sequence.
reverse :: Seq a -> Seq a
reverse (Seq xs) = Seq (reverseTree id xs)

reverseTree :: (a -> b) -> FingerTree a -> FingerTree b
reverseTree _ EmptyT = EmptyT
reverseTree f (Single x) = Single (f x)
reverseTree f (Deep s pr m sf) =
    Deep s (reverseDigit f sf)
        (reverseTree (reverseNode f) m)
        (reverseDigit f pr)

{-# INLINE reverseDigit #-}
reverseDigit :: (a -> b) -> Digit a -> Digit b
reverseDigit f (One a) = One (f a)
reverseDigit f (Two a b) = Two (f b) (f a)
reverseDigit f (Three a b c) = Three (f c) (f b) (f a)
reverseDigit f (Four a b c d) = Four (f d) (f c) (f b) (f a)

reverseNode :: (a -> b) -> Node a -> Node b
reverseNode f (Node2 s a b) = Node2 s (f b) (f a)
reverseNode f (Node3 s a b c) = Node3 s (f c) (f b) (f a)

------------------------------------------------------------------------
-- Mapping with a splittable value
------------------------------------------------------------------------

-- For zipping, it is useful to build a result by
-- traversing a sequence while splitting up something else.  For zipping, we
-- traverse the first sequence while splitting up the second.
--
-- What makes all this crazy code a good idea:
--
-- Suppose we zip together two sequences of the same length:
--
-- zs = zip xs ys
--
-- We want to get reasonably fast indexing into zs immediately, rather than
-- needing to construct the entire thing first, as the previous implementation
-- required. The first aspect is that we build the result "outside-in" or
-- "top-down", rather than left to right. That gives us access to both ends
-- quickly. But that's not enough, by itself, to give immediate access to the
-- center of zs. For that, we need to be able to skip over larger segments of
-- zs, delaying their construction until we actually need them. The way we do
-- this is to traverse xs, while splitting up ys according to the structure of
-- xs. If we have a Deep _ pr m sf, we split ys into three pieces, and hand off
-- one piece to the prefix, one to the middle, and one to the suffix of the
-- result. The key point is that we don't need to actually do anything further
-- with those pieces until we actually need them; the computations to split
-- them up further and zip them with their matching pieces can be delayed until
-- they're actually needed. We do the same thing for Digits (splitting into
-- between one and four pieces) and Nodes (splitting into two or three). The
-- ultimate result is that we can index into, or split at, any location in zs
-- in polylogarithmic time *immediately*, while still being able to force all
-- the thunks in O(n) time.
--
-- Benchmark info, and alternatives:
--
-- The old zipping code used mapAccumL to traverse the first sequence while
-- cutting down the second sequence one piece at a time.
--
-- An alternative way to express that basic idea is to convert both sequences
-- to lists, zip the lists, and then convert the result back to a sequence.
-- I'll call this the "listy" implementation.
--
-- I benchmarked two operations: Each started by zipping two sequences
-- constructed with replicate and/or fromList. The first would then immediately
-- index into the result. The second would apply deepseq to force the entire
-- result.  The new implementation worked much better than either of the others
-- on the immediate indexing test, as expected. It also worked better than the
-- old implementation for all the deepseq tests. For short sequences, the listy
-- implementation outperformed all the others on the deepseq test. However, the
-- splitting implementation caught up and surpassed it once the sequences grew
-- long enough. It seems likely that by avoiding rebuilding, it interacts
-- better with the cache hierarchy.
--
-- David Feuer, with excellent guidance from Carter Schonwald, December 2014

-- | /O(n)/. Constructs a new sequence with the same structure as an existing
-- sequence using a user-supplied mapping function along with a splittable
-- value and a way to split it. The value is split up lazily according to the
-- structure of the sequence, so one piece of the value is distributed to each
-- element of the sequence. The caller should provide a splitter function that
-- takes a number, @n@, and a splittable value, breaks off a chunk of size @n@
-- from the value, and returns that chunk and the remainder as a pair. The
-- following examples will hopefully make the usage clear:
--
-- > zipWith :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
-- > zipWith f s1 s2 = splitMap splitAt (\b a -> f a (b `index` 0)) s2' s1'
-- >   where
-- >     minLen = min (length s1) (length s2)
-- >     s1' = take minLen s1
-- >     s2' = take minLen s2
--
-- > mapWithIndex :: (Int -> a -> b) -> Seq a -> Seq b
-- > mapWithIndex f = splitMap (\n i -> (i, n+i)) f 0
splitMap :: (Int -> s -> (s,s)) -> (s -> a -> b) -> s -> Seq a -> Seq b
splitMap splt' = go
 where
  go f s (Seq xs) = Seq $ splitMapTree splt' (\s' (Elem a) -> Elem (f s' a)) s xs

  {-# SPECIALIZE splitMapTree :: (Int -> s -> (s,s)) -> (s -> Elem y -> b) -> s -> FingerTree (Elem y) -> FingerTree b #-}
  {-# SPECIALIZE splitMapTree :: (Int -> s -> (s,s)) -> (s -> Node y -> b) -> s -> FingerTree (Node y) -> FingerTree b #-}
  splitMapTree :: Sized a => (Int -> s -> (s,s)) -> (s -> a -> b) -> s -> FingerTree a -> FingerTree b
  splitMapTree _    _ _ EmptyT = EmptyT
  splitMapTree _    f s (Single xs) = Single $ f s xs
  splitMapTree splt f s (Deep n pr m sf) = Deep n (splitMapDigit splt f prs pr) (splitMapTree splt (splitMapNode splt f) ms m) (splitMapDigit splt f sfs sf)
    where
      (prs, r) = splt (size pr) s
      (ms, sfs) = splt (n - size pr - size sf) r

  {-# SPECIALIZE splitMapDigit :: (Int -> s -> (s,s)) -> (s -> Elem y -> b) -> s -> Digit (Elem y) -> Digit b #-}
  {-# SPECIALIZE splitMapDigit :: (Int -> s -> (s,s)) -> (s -> Node y -> b) -> s -> Digit (Node y) -> Digit b #-}
  splitMapDigit :: Sized a => (Int -> s -> (s,s)) -> (s -> a -> b) -> s -> Digit a -> Digit b
  splitMapDigit _    f s (One a) = One (f s a)
  splitMapDigit splt f s (Two a b) = Two (f first a) (f second b)
    where
      (first, second) = splt (size a) s
  splitMapDigit splt f s (Three a b c) = Three (f first a) (f second b) (f third c)
    where
      (first, r) = splt (size a) s
      (second, third) = splt (size b) r
  splitMapDigit splt f s (Four a b c d) = Four (f first a) (f second b) (f third c) (f fourth d)
    where
      (first, s') = splt (size a) s
      (middle, fourth) = splt (size b + size c) s'
      (second, third) = splt (size b) middle

  {-# SPECIALIZE splitMapNode :: (Int -> s -> (s,s)) -> (s -> Elem y -> b) -> s -> Node (Elem y) -> Node b #-}
  {-# SPECIALIZE splitMapNode :: (Int -> s -> (s,s)) -> (s -> Node y -> b) -> s -> Node (Node y) -> Node b #-}
  splitMapNode :: Sized a => (Int -> s -> (s,s)) -> (s -> a -> b) -> s -> Node a -> Node b
  splitMapNode splt f s (Node2 ns a b) = Node2 ns (f first a) (f second b)
    where
      (first, second) = splt (size a) s
  splitMapNode splt f s (Node3 ns a b c) = Node3 ns (f first a) (f second b) (f third c)
    where
      (first, r) = splt (size a) s
      (second, third) = splt (size b) r

{-# INLINE splitMap #-}

getSingleton :: Seq a -> a
getSingleton (Seq (Single (Elem a))) = a
getSingleton (Seq EmptyT) = error "getSingleton: EmptyT"
getSingleton _ = error "getSingleton: Not a singleton."

------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------

-- | /O(min(n1,n2))/.  'zip' takes two sequences and returns a sequence
-- of corresponding pairs.  If one input is short, excess elements are
-- discarded from the right end of the longer sequence.
zip :: Seq a -> Seq b -> Seq (a, b)
zip = zipWith (,)

-- | /O(min(n1,n2))/.  'zipWith' generalizes 'zip' by zipping with the
-- function given as the first argument, instead of a tupling function.
-- For example, @zipWith (+)@ is applied to two sequences to take the
-- sequence of corresponding sums.
zipWith :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
zipWith f s1 s2 = zipWith' f s1' s2'
  where
    minLen = min (length s1) (length s2)
    s1' = take minLen s1
    s2' = take minLen s2

-- | A version of zipWith that assumes the sequences have the same length.
zipWith' :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
zipWith' f s1 s2 = splitMap splitAt' (\s a -> f a (getSingleton s)) s2 s1

-- | /O(min(n1,n2,n3))/.  'zip3' takes three sequences and returns a
-- sequence of triples, analogous to 'zip'.
zip3 :: Seq a -> Seq b -> Seq c -> Seq (a,b,c)
zip3 = zipWith3 (,,)

-- | /O(min(n1,n2,n3))/.  'zipWith3' takes a function which combines
-- three elements, as well as three sequences and returns a sequence of
-- their point-wise combinations, analogous to 'zipWith'.
zipWith3 :: (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d
zipWith3 f s1 s2 s3 = zipWith' ($) (zipWith' f s1' s2') s3'
  where
    minLen = minimum [length s1, length s2, length s3]
    s1' = take minLen s1
    s2' = take minLen s2
    s3' = take minLen s3

zipWith3' :: (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d
zipWith3' f s1 s2 s3 = zipWith' ($) (zipWith' f s1 s2) s3

-- | /O(min(n1,n2,n3,n4))/.  'zip4' takes four sequences and returns a
-- sequence of quadruples, analogous to 'zip'.
zip4 :: Seq a -> Seq b -> Seq c -> Seq d -> Seq (a,b,c,d)
zip4 = zipWith4 (,,,)

-- | /O(min(n1,n2,n3,n4))/.  'zipWith4' takes a function which combines
-- four elements, as well as four sequences and returns a sequence of
-- their point-wise combinations, analogous to 'zipWith'.
zipWith4 :: (a -> b -> c -> d -> e) -> Seq a -> Seq b -> Seq c -> Seq d -> Seq e
zipWith4 f s1 s2 s3 s4 = zipWith' ($) (zipWith3' f s1' s2' s3') s4'
  where
    minLen = minimum [length s1, length s2, length s3, length s4]
    s1' = take minLen s1
    s2' = take minLen s2
    s3' = take minLen s3
    s4' = take minLen s4

------------------------------------------------------------------------
-- Sorting
--
-- sort and sortBy are implemented by simple deforestations of
--      \ xs -> fromList2 (length xs) . Data.List.sortBy cmp . toList
-- which does not get deforested automatically, it would appear.
--
-- Unstable sorting is performed by a heap sort implementation based on
-- pairing heaps.  Because the internal structure of sequences is quite
-- varied, it is difficult to get blocks of elements of roughly the same
-- length, which would improve merge sort performance.  Pairing heaps,
-- on the other hand, are relatively resistant to the effects of merging
-- heaps of wildly different sizes, as guaranteed by its amortized
-- constant-time merge operation.  Moreover, extensive use of SpecConstr
-- transformations can be done on pairing heaps, especially when we're
-- only constructing them to immediately be unrolled.
--
-- On purely random sequences of length 50000, with no RTS options,
-- I get the following statistics, in which heapsort is about 42.5%
-- faster:  (all comparisons done with -O2)
--
-- Times (ms)            min      mean    +/-sd    median    max
-- to/from list:       103.802  108.572    7.487  106.436  143.339
-- unstable heapsort:   60.686   62.968    4.275   61.187   79.151
--
-- Heapsort, it would seem, is less of a memory hog than Data.List.sortBy.
-- The gap is narrowed when more memory is available, but heapsort still
-- wins, 15% faster, with +RTS -H128m:
--
-- Times (ms)            min    mean    +/-sd  median    max
-- to/from list:       42.692  45.074   2.596  44.600  56.601
-- unstable heapsort:  37.100  38.344   3.043  37.715  55.526
--
-- In addition, on strictly increasing sequences the gap is even wider
-- than normal; heapsort is 68.5% faster with no RTS options:
-- Times (ms)            min    mean    +/-sd  median    max
-- to/from list:       52.236  53.574   1.987  53.034  62.098
-- unstable heapsort:  16.433  16.919   0.931  16.681  21.622
--
-- This may be attributed to the elegant nature of the pairing heap.
--
-- wasserman.louis@gmail.com, 7/20/09
------------------------------------------------------------------------

-- | /O(n log n)/.  'sort' sorts the specified 'Seq' by the natural
-- ordering of its elements.  The sort is stable.
-- If stability is not required, 'unstableSort' can be considerably
-- faster, and in particular uses less memory.
sort :: Ord a => Seq a -> Seq a
sort = sortBy compare

-- | /O(n log n)/.  'sortBy' sorts the specified 'Seq' according to the
-- specified comparator.  The sort is stable.
-- If stability is not required, 'unstableSortBy' can be considerably
-- faster, and in particular uses less memory.
sortBy :: (a -> a -> Ordering) -> Seq a -> Seq a
sortBy cmp xs = fromList2 (length xs) (Data.List.sortBy cmp (toList xs))

-- | /O(n log n)/.  'unstableSort' sorts the specified 'Seq' by
-- the natural ordering of its elements, but the sort is not stable.
-- This algorithm is frequently faster and uses less memory than 'sort',
-- and performs extremely well -- frequently twice as fast as 'sort' --
-- when the sequence is already nearly sorted.
unstableSort :: Ord a => Seq a -> Seq a
unstableSort = unstableSortBy compare

-- | /O(n log n)/.  A generalization of 'unstableSort', 'unstableSortBy'
-- takes an arbitrary comparator and sorts the specified sequence.
-- The sort is not stable.  This algorithm is frequently faster and
-- uses less memory than 'sortBy', and performs extremely well --
-- frequently twice as fast as 'sortBy' -- when the sequence is already
-- nearly sorted.
unstableSortBy :: (a -> a -> Ordering) -> Seq a -> Seq a
unstableSortBy cmp (Seq xs) =
    fromList2 (size xs) $ maybe [] (unrollPQ cmp) $
        toPQ cmp (\ (Elem x) -> PQueue x Nil) xs

-- | fromList2, given a list and its length, constructs a completely
-- balanced Seq whose elements are that list using the replicateA
-- generalization.
fromList2 :: Int -> [a] -> Seq a
fromList2 n = execState (replicateA n (State ht))
  where
    ht (x:xs) = (xs, x)
    ht []     = error "fromList2: short list"

-- | A 'PQueue' is a simple pairing heap.
data PQueue e = PQueue e (PQL e)
data PQL e = Nil | {-# UNPACK #-} !(PQueue e) :& PQL e

infixr 8 :&

#if TESTING

instance Functor PQueue where
    fmap f (PQueue x ts) = PQueue (f x) (fmap f ts)

instance Functor PQL where
    fmap f (q :& qs) = fmap f q :& fmap f qs
    fmap _ Nil = Nil

instance Show e => Show (PQueue e) where
    show = unlines . draw . fmap show

-- borrowed wholesale from Data.Tree, as Data.Tree actually depends
-- on Data.Sequence
draw :: PQueue String -> [String]
draw (PQueue x ts0) = x : drawSubTrees ts0
  where
    drawSubTrees Nil = []
    drawSubTrees (t :& Nil) =
        "|" : shift "`- " "   " (draw t)
    drawSubTrees (t :& ts) =
        "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

    shift first other = Data.List.zipWith (++) (first : repeat other)
#endif

-- | 'unrollPQ', given a comparator function, unrolls a 'PQueue' into
-- a sorted list.
unrollPQ :: (e -> e -> Ordering) -> PQueue e -> [e]
unrollPQ cmp = unrollPQ'
  where
    {-# INLINE unrollPQ' #-}
    unrollPQ' (PQueue x ts) = x:mergePQs0 ts
    (<+>) = mergePQ cmp
    mergePQs0 Nil = []
    mergePQs0 (t :& Nil) = unrollPQ' t
    mergePQs0 (t1 :& t2 :& ts) = mergePQs (t1 <+> t2) ts
    mergePQs !t ts = case ts of
        Nil             -> unrollPQ' t
        t1 :& Nil       -> unrollPQ' (t <+> t1)
        t1 :& t2 :& ts' -> mergePQs (t <+> (t1 <+> t2)) ts'

-- | 'toPQ', given an ordering function and a mechanism for queueifying
-- elements, converts a 'FingerTree' to a 'PQueue'.
toPQ :: (e -> e -> Ordering) -> (a -> PQueue e) -> FingerTree a -> Maybe (PQueue e)
toPQ _ _ EmptyT = Nothing
toPQ _ f (Single x) = Just (f x)
toPQ cmp f (Deep _ pr m sf) = Just (maybe (pr' <+> sf') ((pr' <+> sf') <+>) (toPQ cmp fNode m))
  where
    fDigit digit = case fmap f digit of
        One a           -> a
        Two a b         -> a <+> b
        Three a b c     -> a <+> b <+> c
        Four a b c d    -> (a <+> b) <+> (c <+> d)
    (<+>) = mergePQ cmp
    fNode = fDigit . nodeToDigit
    pr' = fDigit pr
    sf' = fDigit sf

-- | 'mergePQ' merges two 'PQueue's.
mergePQ :: (a -> a -> Ordering) -> PQueue a -> PQueue a -> PQueue a
mergePQ cmp q1@(PQueue x1 ts1) q2@(PQueue x2 ts2)
  | cmp x1 x2 == GT     = PQueue x2 (q1 :& ts2)
  | otherwise           = PQueue x1 (q2 :& ts1)
