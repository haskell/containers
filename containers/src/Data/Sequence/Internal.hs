{-# LANGUAGE CPP #-}
#include "containers.h"
{-# LANGUAGE BangPatterns #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
#endif
#ifdef DEFINE_PATTERN_SYNONYMS
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#endif
{-# LANGUAGE PatternGuards #-}

{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence.Internal
-- Copyright   :  (c) Ross Paterson 2005
--                (c) Louis Wasserman 2009
--                (c) Bertram Felgenhauer, David Feuer, Ross Paterson, and
--                    Milan Straka 2014
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
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
-- = Finite sequences (internals)
--
-- The @'Seq' a@ type represents a finite sequence of values of type @a@.
--
--
-- == Implementation
--
-- The implementation uses 2-3 finger trees annotated with sizes,
-- as described in section 4.2 of
--
--    * Ralf Hinze and Ross Paterson,
--      \"/Finger trees: a simple general-purpose data structure/\",
--      Journal of Functional Programming 16:2 (2006) pp 197-217.
--      <http://staff.city.ac.uk/~ross/papers/FingerTree.html>.
--
-- @since 0.5.9
-----------------------------------------------------------------------------

module Data.Sequence.Internal (
    Elem(..), FingerTree(..), Node(..), Digit(..), Sized(..), MaybeForce,
#if defined(DEFINE_PATTERN_SYNONYMS)
    Seq (.., Empty, (:<|), (:|>)),
#else
    Seq (..),
#endif
    foldDigit,
    foldNode,
    foldWithIndexDigit,
    foldWithIndexNode,

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
    replicateM,     -- :: Applicative m => Int -> m a -> m (Seq a)
    cycleTaking,    -- :: Int -> Seq a -> Seq a
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
    chunksOf,       -- :: Int -> Seq a -> Seq (Seq a)
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
    -- * Indexing
    lookup,         -- :: Int -> Seq a -> Maybe a
    (!?),           -- :: Seq a -> Int -> Maybe a
    index,          -- :: Seq a -> Int -> a
    adjust,         -- :: (a -> a) -> Int -> Seq a -> Seq a
    adjust',        -- :: (a -> a) -> Int -> Seq a -> Seq a
    update,         -- :: Int -> a -> Seq a -> Seq a
    take,           -- :: Int -> Seq a -> Seq a
    drop,           -- :: Int -> Seq a -> Seq a
    insertAt,       -- :: Int -> a -> Seq a -> Seq a
    deleteAt,       -- :: Int -> Seq a -> Seq a
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
    foldMapWithIndex, -- :: Monoid m => (Int -> a -> m) -> Seq a -> m
    foldlWithIndex, -- :: (b -> Int -> a -> b) -> b -> Seq a -> b
    foldrWithIndex, -- :: (Int -> a -> b -> b) -> b -> Seq a -> b
    -- * Transformations
    mapWithIndex,   -- :: (Int -> a -> b) -> Seq a -> Seq b
    traverseWithIndex, -- :: Applicative f => (Int -> a -> f b) -> Seq a -> f (Seq b)
    reverse,        -- :: Seq a -> Seq a
    intersperse,    -- :: a -> Seq a -> Seq a
    liftA2Seq,      -- :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
    -- ** Zips and unzips
    zip,            -- :: Seq a -> Seq b -> Seq (a, b)
    zipWith,        -- :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
    zip3,           -- :: Seq a -> Seq b -> Seq c -> Seq (a, b, c)
    zipWith3,       -- :: (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d
    zip4,           -- :: Seq a -> Seq b -> Seq c -> Seq d -> Seq (a, b, c, d)
    zipWith4,       -- :: (a -> b -> c -> d -> e) -> Seq a -> Seq b -> Seq c -> Seq d -> Seq e
    unzip,          -- :: Seq (a, b) -> (Seq a, Seq b)
    unzipWith,      -- :: (a -> (b, c)) -> Seq a -> (Seq b, Seq c)
#ifdef TESTING
    deep,
    node2,
    node3,
#endif
    ) where

import Utils.Containers.Internal.Prelude hiding (
    Functor(..),
#if MIN_VERSION_base(4,11,0)
    (<>),
#endif
    (<$>), Monoid,
    null, length, lookup, take, drop, splitAt,
    scanl, scanl1, scanr, scanr1, replicate, zip, zipWith, zip3, zipWith3,
    unzip, takeWhile, dropWhile, iterate, reverse, filter, mapM, sum, all)
import Prelude ()
import Control.Applicative ((<$>), (<**>),  Alternative,
                            liftA3)
import qualified Control.Applicative as Applicative
import Control.DeepSeq (NFData(rnf),NFData1(liftRnf))
import Control.Monad (MonadPlus(..))
import Data.Monoid (Monoid(..))
import Data.Functor (Functor(..))
import Utils.Containers.Internal.State (State(..), execState)
import Data.Foldable (foldr', toList)
import qualified Data.Foldable as F

import qualified Data.Semigroup as Semigroup
import Data.Functor.Classes
import Data.Traversable

-- GHC specific stuff
#if defined(__GLASGOW_HASKELL__) || defined(__MHS__)
import Text.Read (Lexeme(Ident), lexP, parens, prec,
    readPrec, readListPrec, readListPrecDefault)
#endif
#ifdef __GLASGOW_HASKELL__
import GHC.Exts (build)
import Data.Data
import Data.String (IsString(..))
import qualified Language.Haskell.TH.Syntax as TH
-- See Note [ Template Haskell Dependencies ]
import Language.Haskell.TH ()
import GHC.Generics (Generic, Generic1)

-- Array stuff, with GHC.Arr on GHC
import qualified GHC.Arr
import Data.Coerce
import qualified GHC.Exts
#else
import qualified Data.List
#endif

import Data.Array (Ix, Array)
import qualified Data.Array

import Data.Functor.Identity (Identity(..))

import Utils.Containers.Internal.StrictPair (StrictPair (..), toPair)
import Control.Monad.Zip (MonadZip (..))
import Control.Monad.Fix (MonadFix (..), fix)

default ()

-- We define our own copy here, for Monoid only, even though this
-- is now a Semigroup operator in base. The essential reason is that
-- we have absolutely no use for semigroups in this module. Everything
-- that needs to sum things up requires a Monoid constraint to deal
-- with empty sequences. I'm not sure if there's a risk of walking
-- through dictionaries to reach <> from Monoid, but I see no reason
-- to risk it.
infixr 6 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}

infixr 5 `consTree`
infixl 5 `snocTree`
infixr 5 `appendTree0`

infixr 5 ><
infixr 5 <|, :<
infixl 5 |>, :>

#ifdef DEFINE_PATTERN_SYNONYMS
infixr 5 :<|
infixl 5 :|>

{-# COMPLETE (:<|), Empty #-}
{-# COMPLETE (:|>), Empty #-}

-- | A bidirectional pattern synonym matching an empty sequence.
--
-- @since 0.5.8
pattern Empty :: Seq a
pattern Empty = Seq EmptyT

-- | A bidirectional pattern synonym viewing the front of a non-empty
-- sequence.
--
-- @since 0.5.8
pattern (:<|) :: a -> Seq a -> Seq a
pattern x :<| xs <- (viewl -> x :< xs)
  where
    x :<| xs = x <| xs

-- | A bidirectional pattern synonym viewing the rear of a non-empty
-- sequence.
--
-- @since 0.5.8
pattern (:|>) :: Seq a -> a -> Seq a
pattern xs :|> x <- (viewr -> xs :> x)
  where
    xs :|> x = xs |> x
#endif

class Sized a where
    size :: a -> Int

-- In much the same way that Sized lets us handle the
-- sizes of elements and nodes uniformly, MaybeForce lets
-- us handle their strictness (or lack thereof) uniformly.
-- We can `mseq` something and not have to worry about
-- whether it's an element or a node.
class MaybeForce a where
  maybeRwhnf :: a -> ()

mseq :: MaybeForce a => a -> b -> b
mseq a b = case maybeRwhnf a of () -> b
{-# INLINE mseq #-}

infixr 0 $!?
($!?) :: MaybeForce a => (a -> b) -> a -> b
f $!? a = case maybeRwhnf a of () -> f a
{-# INLINE ($!?) #-}

instance MaybeForce (Elem a) where
  maybeRwhnf _ = ()
  {-# INLINE maybeRwhnf #-}

instance MaybeForce (Node a) where
  maybeRwhnf !_ = ()
  {-# INLINE maybeRwhnf #-}

-- A wrapper making mseq = seq
newtype ForceBox a = ForceBox a
instance MaybeForce (ForceBox a) where
  maybeRwhnf !_ = ()
instance Sized (ForceBox a) where
  size _ = 1

-- | General-purpose finite sequences.
newtype Seq a = Seq (FingerTree (Elem a))

#ifdef __GLASGOW_HASKELL__
-- | @since 0.6.6
instance TH.Lift a => TH.Lift (Seq a) where
#  if MIN_VERSION_template_haskell(2,16,0)
  liftTyped t = [|| coerceFT z ||]
#  else
  lift t = [| coerceFT z |]
#  endif
    where
      -- We rebalance the sequence to use only 3-nodes before lifting its
      -- underlying finger tree. This should minimize the size and depth of the
      -- tree generated at run-time. It also reduces the size of the splice,
      -- but I don't know how that affects the size of the resulting Core once
      -- all the types are added.
      Seq ft = zipWith (flip const) (replicate (length t) ()) t

      -- We remove the 'Elem' constructors to reduce the size of the splice
      -- and the number of types and coercions in the generated Core. Instead
      -- of, say,
      --
      --   Seq (Deep 3 (Two (Elem 1) (Elem 2)) EmptyT (One (Elem 3)))
      --
      -- we generate
      --
      --   coerceFT (Deep 3 (Two 1 2)) EmptyT (One 3)
      z :: FingerTree a
      z = coerce ft

-- | We use this to help the types work out for splices in the
-- Lift instance. Things get a bit yucky otherwise.
coerceFT :: FingerTree a -> Seq a
coerceFT = coerce

#endif

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
"fmapSeq/coerce" fmapSeq coerce = coerce
 #-}
#endif

instance Foldable Seq where
#ifdef __GLASGOW_HASKELL__
    foldMap :: forall m a. Monoid m => (a -> m) -> Seq a -> m
    foldMap = coerce (foldMap :: (Elem a -> m) -> FingerTree (Elem a) -> m)

    foldr :: forall a b. (a -> b -> b) -> b -> Seq a -> b
    foldr = coerce (foldr :: (Elem a -> b -> b) -> b -> FingerTree (Elem a) -> b)

    foldl :: forall b a. (b -> a -> b) -> b -> Seq a -> b
    foldl = coerce (foldl :: (b -> Elem a -> b) -> b -> FingerTree (Elem a) -> b)

    foldr' :: forall a b. (a -> b -> b) -> b -> Seq a -> b
    foldr' = coerce (foldr' :: (Elem a -> b -> b) -> b -> FingerTree (Elem a) -> b)

    foldl' :: forall b a. (b -> a -> b) -> b -> Seq a -> b
    foldl' = coerce (foldl' :: (b -> Elem a -> b) -> b -> FingerTree (Elem a) -> b)

    foldr1 :: forall a. (a -> a -> a) -> Seq a -> a
    foldr1 = coerce (foldr1 :: (Elem a -> Elem a -> Elem a) -> FingerTree (Elem a) -> Elem a)

    foldl1 :: forall a. (a -> a -> a) -> Seq a -> a
    foldl1 = coerce (foldl1 :: (Elem a -> Elem a -> Elem a) -> FingerTree (Elem a) -> Elem a)
#else
    foldMap f (Seq xs) = foldMap (f . getElem) xs

    foldr f z (Seq xs) = foldr (f . getElem) z xs

    foldl f z (Seq xs) = foldl (\z' x -> f z' (getElem x)) z xs

    foldr' f z (Seq xs) = foldr' (f . getElem) z xs

    foldl' f z (Seq xs) = foldl' (\z' x -> f z' (getElem x)) z xs

    foldr1 f (Seq xs) = getElem (foldr1 f' xs)
      where f' (Elem x) (Elem y) = Elem (f x y)

    foldl1 f (Seq xs) = getElem (foldl1 f' xs)
      where f' (Elem x) (Elem y) = Elem (f x y)
#endif

    length = length
    {-# INLINE length #-}
    null   = null
    {-# INLINE null #-}

instance Traversable Seq where
#if __GLASGOW_HASKELL__
    {-# INLINABLE traverse #-}
#endif
    traverse _ (Seq EmptyT) = pure (Seq EmptyT)
    traverse f' (Seq (Single (Elem x'))) =
        (\x'' -> Seq (Single (Elem x''))) <$> f' x'
    traverse f' (Seq (Deep s' pr' m' sf')) =
        liftA3
            (\pr'' m'' sf'' -> Seq (Deep s' pr'' m'' sf''))
            (traverseDigitE f' pr')
            (traverseTree (traverseNodeE f') m')
            (traverseDigitE f' sf')
      where
        traverseTree
            :: Applicative f
            => (Node a -> f (Node b))
            -> FingerTree (Node a)
            -> f (FingerTree (Node b))
        traverseTree _ EmptyT = pure EmptyT
        traverseTree f (Single x) = Single <$> f x
        traverseTree f (Deep s pr m sf) =
            liftA3
                (Deep s)
                (traverseDigitN f pr)
                (traverseTree (traverseNodeN f) m)
                (traverseDigitN f sf)
        traverseDigitE
            :: Applicative f
            => (a -> f b) -> Digit (Elem a) -> f (Digit (Elem b))
        traverseDigitE f (One (Elem a)) =
            (\a' -> One (Elem a')) <$>
            f a
        traverseDigitE f (Two (Elem a) (Elem b)) =
            liftA2
                (\a' b' -> Two (Elem a') (Elem b'))
                (f a)
                (f b)
        traverseDigitE f (Three (Elem a) (Elem b) (Elem c)) =
            liftA3
                (\a' b' c' ->
                      Three (Elem a') (Elem b') (Elem c'))
                (f a)
                (f b)
                (f c)
        traverseDigitE f (Four (Elem a) (Elem b) (Elem c) (Elem d)) =
            liftA3
                (\a' b' c' d' -> Four (Elem a') (Elem b') (Elem c') (Elem d'))
                (f a)
                (f b)
                (f c) <*>
                (f d)
        traverseDigitN
            :: Applicative f
            => (Node a -> f (Node b)) -> Digit (Node a) -> f (Digit (Node b))
        traverseDigitN f t = traverse f t
        traverseNodeE
            :: Applicative f
            => (a -> f b) -> Node (Elem a) -> f (Node (Elem b))
        traverseNodeE f (Node2 s (Elem a) (Elem b)) =
            liftA2
                (\a' b' -> Node2 s (Elem a') (Elem b'))
                (f a)
                (f b)
        traverseNodeE f (Node3 s (Elem a) (Elem b) (Elem c)) =
            liftA3
                (\a' b' c' ->
                      Node3 s (Elem a') (Elem b') (Elem c'))
                (f a)
                (f b)
                (f c)
        traverseNodeN
            :: Applicative f
            => (Node a -> f (Node b)) -> Node (Node a) -> f (Node (Node b))
        traverseNodeN f t = traverse f t

instance NFData a => NFData (Seq a) where
    rnf (Seq xs) = rnf xs

-- | @since 0.8
instance NFData1 Seq where
    liftRnf rnfx (Seq xs) = liftRnf (liftRnf rnfx) xs

instance Monad Seq where
    xs >>= f = foldl' add empty xs
      where add ys x = ys >< f x
    (>>) = (*>)

-- | @since 0.5.11
instance MonadFix Seq where
    mfix = mfixSeq

-- This is just like the instance for lists, but we can take advantage of
-- constant-time length and logarithmic-time indexing to speed things up.
-- Using fromFunction, we make this about as lazy as we can.
mfixSeq :: (a -> Seq a) -> Seq a
mfixSeq f = fromFunction (length (f err)) (\k -> fix (\xk -> f xk `index` k))
  where
    err = error "mfix for Data.Sequence.Seq applied to strict function"

-- | @since 0.5.4
instance Applicative Seq where
    pure = singleton
    xs *> ys = cycleNTimes (length xs) ys
    (<*>) = apSeq
    liftA2 = liftA2Seq
    xs <* ys = beforeSeq xs ys

apSeq :: Seq (a -> b) -> Seq a -> Seq b
apSeq fs xs@(Seq xsFT) = case viewl fs of
  EmptyL -> empty
  firstf :< fs' -> case viewr fs' of
    EmptyR -> fmap firstf xs
    Seq fs''FT :> lastf -> case rigidify xsFT of
         RigidEmpty -> empty
         RigidOne (Elem x) -> fmap ($ x) fs
         RigidTwo (Elem x1) (Elem x2) ->
            Seq $ ap2FT firstf fs''FT lastf (x1, x2)
         RigidThree (Elem x1) (Elem x2) (Elem x3) ->
            Seq $ ap3FT firstf fs''FT lastf (x1, x2, x3)
         RigidFull r@(Rigid s pr _m sf) -> Seq $
               Deep (s * length fs)
                    (fmap (fmap firstf) (nodeToDigit pr))
                    (liftA2Middle (fmap firstf) (fmap lastf) fmap fs''FT r)
                    (fmap (fmap lastf) (nodeToDigit sf))
{-# NOINLINE [1] apSeq #-}

{-# RULES
"ap/fmap1" forall f xs ys . apSeq (fmapSeq f xs) ys = liftA2Seq f xs ys
"ap/fmap2" forall f gs xs . apSeq gs (fmapSeq f xs) =
                              liftA2Seq (\g x -> g (f x)) gs xs
"fmap/ap" forall f gs xs . fmapSeq f (gs `apSeq` xs) =
                             liftA2Seq (\g x -> f (g x)) gs xs
"fmap/liftA2" forall f g m n . fmapSeq f (liftA2Seq g m n) =
                       liftA2Seq (\x y -> f (g x y)) m n
"liftA2/fmap1" forall f g m n . liftA2Seq f (fmapSeq g m) n =
                       liftA2Seq (\x y -> f (g x) y) m n
"liftA2/fmap2" forall f g m n . liftA2Seq f m (fmapSeq g n) =
                       liftA2Seq (\x y -> f x (g y)) m n
 #-}

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

lift2FT :: (a -> b -> c) -> a -> FingerTree (Elem a) -> a -> (b,b) -> FingerTree (Elem c)
lift2FT f firstx xs lastx (y1,y2) =
                 Deep (size xs * 2 + 4)
                      (Two (Elem $ f firstx y1) (Elem $ f firstx y2))
                      (mapMulFT 2 (\(Elem x) -> Node2 2 (Elem (f x y1)) (Elem (f x y2))) xs)
                      (Two (Elem $ f lastx y1) (Elem $ f lastx y2))

lift3FT :: (a -> b -> c) -> a -> FingerTree (Elem a) -> a -> (b,b,b) -> FingerTree (Elem c)
lift3FT f firstx xs lastx (y1,y2,y3) =
                 Deep (size xs * 3 + 6)
                      (Three (Elem $ f firstx y1) (Elem $ f firstx y2) (Elem $ f firstx y3))
                      (mapMulFT 3 (\(Elem x) -> Node3 3 (Elem (f x y1)) (Elem (f x y2)) (Elem (f x y3))) xs)
                      (Three (Elem $ f lastx y1) (Elem $ f lastx y2) (Elem $ f lastx y3))

liftA2Seq :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
liftA2Seq f xs ys@(Seq ysFT) = case viewl xs of
  EmptyL -> empty
  firstx :< xs' -> case viewr xs' of
    EmptyR -> f firstx <$> ys
    Seq xs''FT :> lastx -> case rigidify ysFT of
      RigidEmpty -> empty
      RigidOne (Elem y) -> fmap (\x -> f x y) xs
      RigidTwo (Elem y1) (Elem y2) ->
        Seq $ lift2FT f firstx xs''FT lastx (y1, y2)
      RigidThree (Elem y1) (Elem y2) (Elem y3) ->
        Seq $ lift3FT f firstx xs''FT lastx (y1, y2, y3)
      RigidFull r@(Rigid s pr _m sf) -> Seq $
        Deep (s * length xs)
             (fmap (fmap (f firstx)) (nodeToDigit pr))
             (liftA2Middle (fmap (f firstx)) (fmap (f lastx)) (lift_elem f) xs''FT r)
             (fmap (fmap (f lastx)) (nodeToDigit sf))
  where
    lift_elem :: (a -> b -> c) -> a -> Elem b -> Elem c
#ifdef __GLASGOW_HASKELL__
    lift_elem = coerce
#else
    lift_elem f x (Elem y) = Elem (f x y)
#endif
{-# NOINLINE [1] liftA2Seq #-}


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

-- | 'liftA2Middle' does most of the hard work of computing @liftA2 f xs ys@.  It
-- produces the center part of a finger tree, with a prefix corresponding to
-- the first element of @xs@ and a suffix corresponding to its last element omitted;
-- the missing suffix and prefix are added by the caller.  For the recursive
-- call, it squashes the prefix and the suffix into the center tree. Once it
-- gets to the bottom, it turns the tree into a 2-3 tree, applies 'mapMulFT' to
-- produce the main body, and glues all the pieces together.
--
-- @f@ itself is a bit horrifying because of the nested types involved. Its
-- job is to map over the *elements* of a 2-3 tree, rather than the subtrees.
-- If we used a higher-order nested type with MPTC, we could probably use a
-- class, but as it is we have to build up @f@ explicitly through the
-- recursion.
--
-- === Description of parameters
--
-- ==== Types
--
-- @a@ remains constant through recursive calls (in the @DeepTh@ case),
-- while @b@ and @c@ do not: 'liftAMiddle' calls itself at types @Node b@ and
-- @Node c@.
--
-- ==== Values
--
-- 'liftA2Middle' is used when the original @xs :: Sequence a@ has at
-- least two elements, so it can be decomposed by taking off the first and last
-- elements:
--
-- > xs = firstx <: midxs :> lastx
--
-- - the first two arguments @ffirstx, flastx :: b -> c@ are equal to
--   @f firstx@ and @f lastx@, where @f :: a -> b -> c@ is the third argument.
--   This ensures sharing when @f@ computes some data upon being partially
--   applied to its first argument. The way @f@ gets accumulated also ensures
--   sharing for the middle section.
--
-- - the fourth argument is the middle part @midxs@, always constant.
--
-- - the last argument, a tuple of type @Rigid b@, holds all the elements of
--   @ys@, in three parts: a middle part around which the recursion is
--   structured, surrounded by a prefix and a suffix that accumulate
--   elements on the side as we walk down the middle.
--
-- === Invariants
--
-- > 1. Viewing the various trees as the lists they represent
-- >    (the types of the toList functions are given a few paragraphs below):
-- >
-- >    toListFTN result
-- >      =  (ffirstx                    <$> (toListThinN m ++ toListD sf))
-- >      ++ (f      <$> toListFTE midxs <*> (toListD pr ++ toListThinN m ++ toListD sf))
-- >      ++ (flastx                     <$> (toListD pr ++ toListThinN m))
-- >
-- > 2. s = size m + size pr + size sf
-- >
-- > 3. size (ffirstx y) = size (flastx y) = size (f x y) = size y
-- >      for any (x :: a) (y :: b)
--
-- Projecting invariant 1 on sizes, using 2 and 3 to simplify, we have the
-- following corollary.
-- It is weaker than invariant 1, but it may be easier to keep track of.
--
-- > 1a. size result = s * (size midxs + 1) + size m
--
-- In invariant 1, the types of the auxiliary functions are as follows
-- for reference:
--
-- > toListFTE   :: FingerTree (Elem a) -> [a]
-- > toListFTN   :: FingerTree (Node c) -> [c]
-- > toListThinN :: Thin (Node b) -> [b]
-- > toListD     :: Digit12 b -> [b]
liftA2Middle
  :: (b -> c)              -- ^ @ffirstx@
  -> (b -> c)              -- ^ @flastx@
  -> (a -> b -> c)         -- ^ @f@
  -> FingerTree (Elem a)   -- ^ @midxs@
  -> Rigid b               -- ^ @Rigid s pr m sf@ (@pr@: prefix, @sf@: suffix)
  -> FingerTree (Node c)

-- Not at the bottom yet

liftA2Middle
    ffirstx
    flastx
    f
    midxs
    (Rigid s pr (DeepTh sm prm mm sfm) sf)
    -- note: size (DeepTh sm pr mm sfm) = sm = size pr + size mm + size sfm
    = Deep (sm + s * (size midxs + 1)) -- note: sm = s - size pr - size sf
           (fmap (fmap ffirstx) (digit12ToDigit prm))
           (liftA2Middle
               (fmap ffirstx)
               (fmap flastx)
               (fmap . f)
               midxs
               (Rigid s (squashL pr prm) mm (squashR sfm sf)))
           (fmap (fmap flastx) (digit12ToDigit sfm))

-- At the bottom

liftA2Middle
    ffirstx
    flastx
    f
    midxs
    (Rigid s pr EmptyTh sf)
    = deep
           (One (fmap ffirstx sf))
           (mapMulFT s (\(Elem x) -> fmap (fmap (f x)) converted) midxs)
           (One (fmap flastx pr))
   where converted = node2 pr sf

liftA2Middle
    ffirstx
    flastx
    f
    midxs
    (Rigid s pr (SingleTh q) sf)
    = deep
           (Two (fmap ffirstx q) (fmap ffirstx sf))
           (mapMulFT s (\(Elem x) -> fmap (fmap (f x)) converted) midxs)
           (Two (fmap flastx pr) (fmap flastx q))
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


-- | \(O(mn)\) (incremental) Takes an \(O(m)\) function and a finger tree of size
-- \(n\) and maps the function over the tree leaves. Unlike the usual 'fmap', the
-- function is applied to the "leaves" of the 'FingerTree' (i.e., given a
-- @FingerTree (Elem a)@, it applies the function to elements of type @Elem
-- a@), replacing the leaves with subtrees of at least the same height, e.g.,
-- @Node(Node(Elem y))@. The multiplier argument serves to make the annotations
-- match up properly.
mapMulFT :: Int -> (a -> b) -> FingerTree a -> FingerTree b
mapMulFT !_ _ EmptyT = EmptyT
mapMulFT _mul f (Single a) = Single (f a)
mapMulFT mul f (Deep s pr m sf) = Deep (mul * s) (fmap f pr) (mapMulFT mul (mapMulNode mul f) m) (fmap f sf)

mapMulNode :: Int -> (a -> b) -> Node a -> Node b
mapMulNode mul f (Node2 s a b)   = Node2 (mul * s) (f a) (f b)
mapMulNode mul f (Node3 s a b c) = Node3 (mul * s) (f a) (f b) (f c)

-- | \(O(\log n)\) (incremental) Takes the extra flexibility out of a 'FingerTree'
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
   ConsLTree (Node2 _ b c) m' -> rigidifyRight s (node3 a b c) m' sf
   ConsLTree (Node3 _ b c d) m' -> rigidifyRight s (node2 a b) (node2 c d `consTree` m') sf
   EmptyLTree -> case sf of
     One b -> RigidTwo a b
     Two b c -> RigidThree a b c
     Three b c d -> RigidFull $ Rigid s (node2 a b) EmptyTh (node2 c d)
     Four b c d e -> RigidFull $ Rigid s (node3 a b c) EmptyTh (node2 d e)

-- | \(O(\log n)\) (incremental) Takes a tree whose left side has been rigidified
-- and finishes the job.
rigidifyRight :: Int -> Digit23 (Elem a) -> FingerTree (Node (Elem a)) -> Digit (Elem a) -> Rigidified (Elem a)

-- The right digit is Two, Three, or Four
rigidifyRight s pr m (Two a b) = RigidFull $ Rigid s pr (thin m) (node2 a b)
rigidifyRight s pr m (Three a b c) = RigidFull $ Rigid s pr (thin m) (node3 a b c)
rigidifyRight s pr m (Four a b c d) = RigidFull $ Rigid s pr (thin $ m `snocTree` node2 a b) (node2 c d)

-- The right digit is One
rigidifyRight s pr m (One e) = case viewRTree m of
    SnocRTree m' (Node2 _ a b) -> RigidFull $ Rigid s pr (thin m') (node3 a b e)
    SnocRTree m' (Node3 _ a b c) -> RigidFull $ Rigid s pr (thin $ m' `snocTree` node2 a b) (node2 c e)
    EmptyRTree -> case pr of
      Node2 _ a b -> RigidThree a b e
      Node3 _ a b c -> RigidFull $ Rigid s (node2 a b) EmptyTh (node2 c e)

-- | \(O(\log n)\) (incremental) Rejigger a finger tree so the digits are all ones
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

-- | \( O(n) \). Intersperse an element between the elements of a sequence.
--
-- @
-- intersperse a empty = empty
-- intersperse a (singleton x) = singleton x
-- intersperse a (fromList [x,y]) = fromList [x,a,y]
-- intersperse a (fromList [x,y,z]) = fromList [x,a,y,a,z]
-- @
--
-- @since 0.5.8
intersperse :: a -> Seq a -> Seq a
intersperse y xs = case viewl xs of
  EmptyL -> empty
  p :< ps -> p <| (ps <**> (const y <| singleton id))
-- We used to use
--
-- intersperse y xs = drop 1 $ xs <**> (const y <| singleton id)
--
-- but if length xs = ((maxBound :: Int) `quot` 2) + 1 then
--
-- length (xs <**> (const y <| singleton id)) will wrap around to negative
-- and the drop won't work. The new implementation can produce a result
-- right up to maxBound :: Int

instance MonadPlus Seq where
    mzero = empty
    mplus = (><)

-- | @since 0.5.4
instance Alternative Seq where
    empty = empty
    (<|>) = (><)

instance Eq a => Eq (Seq a) where
  xs == ys = liftEq (==) xs ys
  {-# INLINABLE (==) #-}

instance Ord a => Ord (Seq a) where
  compare xs ys = liftCompare compare xs ys
  {-# INLINABLE compare #-}

#ifdef TESTING
instance Show a => Show (Seq a) where
    showsPrec p (Seq x) = showsPrec p x
#else
instance Show a => Show (Seq a) where
    showsPrec p xs = showParen (p > 10) $
        showString "fromList " . shows (toList xs)
#endif

-- | @since 0.5.9
instance Show1 Seq where
  liftShowsPrec _shwsPrc shwList p xs = showParen (p > 10) $
        showString "fromList " . shwList (toList xs)

-- | @since 0.5.9
instance Eq1 Seq where
  liftEq eq xs ys =
    sameSize xs ys && sameSizeLiftEqLists eq (toList xs) (toList ys)
  {-# INLINE liftEq #-}

-- | @since 0.5.9
instance Ord1 Seq where
  liftCompare f xs ys = liftCmpLists f (toList xs) (toList ys)
  {-# INLINE liftCompare #-}

-- Note [Eq and Ord]
-- ~~~~~~~~~~~~~~~~~
-- Eq and Ord for Seq are implemented by converting to lists, which turns out
-- to be quite efficient.
-- However, we define our own functions to work with lists because the relevant
-- list functions in base have performance issues (liftEq and liftCompare are
-- recursive and cannot inline, (==) and compare are not INLINABLE and cannot
-- specialize).

-- Same as `length xs == length ys` but uses the structure invariants to skip
-- unnecessary cases.
sameSize :: Seq a -> Seq b -> Bool
sameSize (Seq t1) (Seq t2) = case (t1, t2) of
  (EmptyT, EmptyT) -> True
  (Single _, Single _) -> True
  (Deep v1 _ _ _, Deep v2 _ _ _) -> v1 == v2
  _ -> False

-- Assumes the lists are of equal size to skip some cases.
sameSizeLiftEqLists :: (a -> b -> Bool) -> [a] -> [b] -> Bool
sameSizeLiftEqLists eq = go
  where
    go (x:xs) (y:ys) = eq x y && go xs ys
    go _ _ = True
{-# INLINE sameSizeLiftEqLists #-}

liftCmpLists :: (a -> b -> Ordering) -> [a] -> [b] -> Ordering
liftCmpLists cmp = go
  where
    go [] [] = EQ
    go [] (_:_) = LT
    go (_:_) [] = GT
    go (x:xs) (y:ys) = cmp x y <> go xs ys
{-# INLINE liftCmpLists #-}

instance Read a => Read (Seq a) where
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
instance Read1 Seq where
  liftReadsPrec _rp readLst p = readParen (p > 10) $ \r -> do
    ("fromList",s) <- lex r
    (xs,t) <- readLst s
    pure (fromList xs, t)

-- | @mempty@ = 'empty'
instance Monoid (Seq a) where
    mempty = empty
#if !MIN_VERSION_base(4,11,0)
    mappend = (Semigroup.<>)
#endif

-- | @(<>)@ = '(><)'
--
-- @since 0.5.7
instance Semigroup.Semigroup (Seq a) where
    (<>)    = (><)
    stimes = cycleNTimes . fromIntegral

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
#ifdef TESTING
    deriving Show
#endif

#ifdef __GLASGOW_HASKELL__
-- | @since 0.6.1
deriving instance Generic1 FingerTree

-- | @since 0.6.1
deriving instance Generic (FingerTree a)

-- | @since 0.6.6
deriving instance TH.Lift a => TH.Lift (FingerTree a)
#endif

instance Sized a => Sized (FingerTree a) where
    {-# SPECIALIZE instance Sized (FingerTree (Elem a)) #-}
    {-# SPECIALIZE instance Sized (FingerTree (Node a)) #-}
    size EmptyT             = 0
    size (Single x)         = size x
    size (Deep v _ _ _)     = v

instance Foldable FingerTree where
    foldMap _ EmptyT = mempty
    foldMap f' (Single x') = f' x'
    foldMap f' (Deep _ pr' m' sf') =
        foldMapDigit f' pr' <>
        foldMapTree (foldMapNode f') m' <>
        foldMapDigit f' sf'
      where
        foldMapTree :: Monoid m => (Node a -> m) -> FingerTree (Node a) -> m
        foldMapTree _ EmptyT = mempty
        foldMapTree f (Single x) = f x
        foldMapTree f (Deep _ pr m sf) =
            foldMapDigitN f pr <>
            foldMapTree (foldMapNodeN f) m <>
            foldMapDigitN f sf

        foldMapDigit :: Monoid m => (a -> m) -> Digit a -> m
        foldMapDigit f t = foldDigit (<>) f t

        foldMapDigitN :: Monoid m => (Node a -> m) -> Digit (Node a) -> m
        foldMapDigitN f t = foldDigit (<>) f t

        foldMapNode :: Monoid m => (a -> m) -> Node a -> m
        foldMapNode f t = foldNode (<>) f t

        foldMapNodeN :: Monoid m => (Node a -> m) -> Node (Node a) -> m
        foldMapNodeN f t = foldNode (<>) f t
#if __GLASGOW_HASKELL__
    {-# INLINABLE foldMap #-}
#endif

    foldr _ z' EmptyT = z'
    foldr f' z' (Single x') = x' `f'` z'
    foldr f' z' (Deep _ pr' m' sf') =
        foldrDigit f' (foldrTree (foldrNode f') (foldrDigit f' z' sf') m') pr'
      where
        foldrTree :: (Node a -> b -> b) -> b -> FingerTree (Node a) -> b
        foldrTree _ z EmptyT = z
        foldrTree f z (Single x) = x `f` z
        foldrTree f z (Deep _ pr m sf) =
            foldrDigitN f (foldrTree (foldrNodeN f) (foldrDigitN f z sf) m) pr

        foldrDigit :: (a -> b -> b) -> b -> Digit a -> b
        foldrDigit f z t = foldr f z t

        foldrDigitN :: (Node a -> b -> b) -> b -> Digit (Node a) -> b
        foldrDigitN f z t = foldr f z t

        foldrNode :: (a -> b -> b) -> Node a -> b -> b
        foldrNode f t z = foldr f z t

        foldrNodeN :: (Node a -> b -> b) -> Node (Node a) -> b -> b
        foldrNodeN f t z = foldr f z t
    {-# INLINE foldr #-}


    foldl _ z' EmptyT = z'
    foldl f' z' (Single x') = z' `f'` x'
    foldl f' z' (Deep _ pr' m' sf') =
        foldlDigit f' (foldlTree (foldlNode f') (foldlDigit f' z' pr') m') sf'
      where
        foldlTree :: (b -> Node a -> b) -> b -> FingerTree (Node a) -> b
        foldlTree _ z EmptyT = z
        foldlTree f z (Single x) = z `f` x
        foldlTree f z (Deep _ pr m sf) =
            foldlDigitN f (foldlTree (foldlNodeN f) (foldlDigitN f z pr) m) sf

        foldlDigit :: (b -> a -> b) -> b -> Digit a -> b
        foldlDigit f z t = foldl f z t

        foldlDigitN :: (b -> Node a -> b) -> b -> Digit (Node a) -> b
        foldlDigitN f z t = foldl f z t

        foldlNode :: (b -> a -> b) -> b -> Node a -> b
        foldlNode f z t = foldl f z t

        foldlNodeN :: (b -> Node a -> b) -> b -> Node (Node a) -> b
        foldlNodeN f z t = foldl f z t
    {-# INLINE foldl #-}

    foldr' _  !z' EmptyT = z'
    foldr' f' !z' (Single x') = f' x' z'
    foldr' f' !z' (Deep _ pr' m' sf') =
        (foldrDigit' f' $! (foldrTree' (foldrNode' f') $! (foldrDigit' f' z') sf') m') pr'
      where
        foldrTree' :: (Node a -> b -> b) -> b -> FingerTree (Node a) -> b
        foldrTree' _ !z EmptyT = z
        foldrTree' f !z (Single x) = f x z
        foldrTree' f !z (Deep _ pr m sf) =
            (foldr' f $! (foldrTree' (foldrNodeN' f) $! foldr' f z sf) m) pr

        foldrDigit' :: (a -> b -> b) -> b -> Digit a -> b
        foldrDigit' f z t = foldr' f z t

        foldrNode' :: (a -> b -> b) -> Node a -> b -> b
        foldrNode' f t z = foldr' f z t

        foldrNodeN' :: (Node a -> b -> b) -> Node (Node a) -> b -> b
        foldrNodeN' f t z = foldr' f z t
    {-# INLINE foldr' #-}

    foldl' _  !z' EmptyT = z'
    foldl' f' !z' (Single x') = f' z' x'
    foldl' f' !z' (Deep _ pr' m' sf') =
        (foldlDigit' f' $!
         (foldlTree' (foldlNode' f') $! (foldlDigit' f' z') pr') m')
            sf'
      where
        foldlTree' :: (b -> Node a -> b) -> b -> FingerTree (Node a) -> b
        foldlTree' _ !z EmptyT = z
        foldlTree' f !z (Single xs) = f z xs
        foldlTree' f !z (Deep _ pr m sf) =
            (foldl' f $! (foldlTree' (foldl' f) $! foldl' f z pr) m) sf

        foldlDigit' :: (b -> a -> b) -> b -> Digit a -> b
        foldlDigit' f z t = foldl' f z t

        foldlNode' :: (b -> a -> b) -> b -> Node a -> b
        foldlNode' f z t = foldl' f z t
    {-# INLINE foldl' #-}

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
        liftA3 (Deep v) (traverse f pr) (traverse (traverse f) m)
            (traverse f sf)

instance NFData a => NFData (FingerTree a) where
    rnf EmptyT = ()
    rnf (Single x) = rnf x
    rnf (Deep _ pr m sf) = rnf pr `seq` rnf sf `seq` rnf m

-- | @since 0.8
instance NFData1 FingerTree where
    liftRnf _ EmptyT = ()
    liftRnf rnfx (Single x) = rnfx x
    liftRnf rnfx (Deep _ pr m sf) = liftRnf rnfx pr `seq` liftRnf (liftRnf rnfx) m `seq` liftRnf rnfx sf

{-# INLINE deep #-}
deep            :: Sized a => Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
deep pr m sf    =  Deep (size pr + size m + size sf) pr m sf

{-# INLINE pullL #-}
pullL :: Int -> FingerTree (Node a) -> Digit a -> FingerTree a
pullL s m sf = case viewLTree m of
    EmptyLTree          -> digitToTree' s sf
    ConsLTree pr m'     -> Deep s (nodeToDigit pr) m' sf

{-# INLINE pullR #-}
pullR :: Int -> Digit a -> FingerTree (Node a) -> FingerTree a
pullR s pr m = case viewRTree m of
    EmptyRTree          -> digitToTree' s pr
    SnocRTree m' sf     -> Deep s pr m' (nodeToDigit sf)

-- Digits

data Digit a
    = One a
    | Two a a
    | Three a a a
    | Four a a a a
#ifdef TESTING
    deriving Show
#endif

#ifdef __GLASGOW_HASKELL__
-- | @since 0.6.1
deriving instance Generic1 Digit

-- | @since 0.6.1
deriving instance Generic (Digit a)

-- | @since 0.6.6
deriving instance TH.Lift a => TH.Lift (Digit a)
#endif

foldDigit :: (b -> b -> b) -> (a -> b) -> Digit a -> b
foldDigit _     f (One a) = f a
foldDigit (<+>) f (Two a b) = f a <+> f b
foldDigit (<+>) f (Three a b c) = f a <+> f b <+> f c
foldDigit (<+>) f (Four a b c d) = f a <+> f b <+> f c <+> f d
{-# INLINE foldDigit #-}

instance Foldable Digit where
    foldMap = foldDigit mappend

    foldr f z (One a) = a `f` z
    foldr f z (Two a b) = a `f` (b `f` z)
    foldr f z (Three a b c) = a `f` (b `f` (c `f` z))
    foldr f z (Four a b c d) = a `f` (b `f` (c `f` (d `f` z)))
    {-# INLINE foldr #-}

    foldl f z (One a) = z `f` a
    foldl f z (Two a b) = (z `f` a) `f` b
    foldl f z (Three a b c) = ((z `f` a) `f` b) `f` c
    foldl f z (Four a b c d) = (((z `f` a) `f` b) `f` c) `f` d
    {-# INLINE foldl #-}

    foldr' f !z (One a) = f a z
    foldr' f !z (Two a b) = f a $! f b z
    foldr' f !z (Three a b c) = f a $! f b $! f c z
    foldr' f !z (Four a b c d) = f a $! f b $! f c $! f d z
    {-# INLINE foldr' #-}

    foldl' f !z (One a) = f z a
    foldl' f !z (Two a b) = (f $! f z a) b
    foldl' f !z (Three a b c) = (f $! (f $! f z a) b) c
    foldl' f !z (Four a b c d) = (f $! (f $! (f $! f z a) b) c) d
    {-# INLINE foldl' #-}

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
    traverse f (Two a b) = liftA2 Two (f a) (f b)
    traverse f (Three a b c) = liftA3 Three (f a) (f b) (f c)
    traverse f (Four a b c d) = liftA3 Four (f a) (f b) (f c) <*> f d

instance NFData a => NFData (Digit a) where
    rnf (One a) = rnf a
    rnf (Two a b) = rnf a `seq` rnf b
    rnf (Three a b c) = rnf a `seq` rnf b `seq` rnf c
    rnf (Four a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

-- | @since 0.8
instance NFData1 Digit where
    liftRnf rnfx (One a) = rnfx a
    liftRnf rnfx (Two a b) = rnfx a `seq` rnfx b
    liftRnf rnfx (Three a b c) = rnfx a `seq` rnfx b `seq` rnfx c
    liftRnf rnfx (Four a b c d) = rnfx a `seq` rnfx b `seq` rnfx c `seq` rnfx d

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
#ifdef TESTING
    deriving Show
#endif

#ifdef __GLASGOW_HASKELL__
-- | @since 0.6.1
deriving instance Generic1 Node

-- | @since 0.6.1
deriving instance Generic (Node a)

-- | @since 0.6.6
deriving instance TH.Lift a => TH.Lift (Node a)
#endif

foldNode :: (b -> b -> b) -> (a -> b) -> Node a -> b
foldNode (<+>) f (Node2 _ a b) = f a <+> f b
foldNode (<+>) f (Node3 _ a b c) = f a <+> f b <+> f c
{-# INLINE foldNode #-}

instance Foldable Node where
    foldMap = foldNode mappend

    foldr f z (Node2 _ a b) = a `f` (b `f` z)
    foldr f z (Node3 _ a b c) = a `f` (b `f` (c `f` z))
    {-# INLINE foldr #-}

    foldl f z (Node2 _ a b) = (z `f` a) `f` b
    foldl f z (Node3 _ a b c) = ((z `f` a) `f` b) `f` c
    {-# INLINE foldl #-}

    foldr' f !z (Node2 _ a b) = f a $! f b z
    foldr' f !z (Node3 _ a b c) = f a $! f b $! f c z
    {-# INLINE foldr' #-}

    foldl' f !z (Node2 _ a b) = (f $! f z a) b
    foldl' f !z (Node3 _ a b c) = (f $! (f $! f z a) b) c
    {-# INLINE foldl' #-}

instance Functor Node where
    {-# INLINE fmap #-}
    fmap f (Node2 v a b) = Node2 v (f a) (f b)
    fmap f (Node3 v a b c) = Node3 v (f a) (f b) (f c)

instance Traversable Node where
    {-# INLINE traverse #-}
    traverse f (Node2 v a b) = liftA2 (Node2 v) (f a) (f b)
    traverse f (Node3 v a b c) = liftA3 (Node3 v) (f a) (f b) (f c)

instance NFData a => NFData (Node a) where
    rnf (Node2 _ a b) = rnf a `seq` rnf b
    rnf (Node3 _ a b c) = rnf a `seq` rnf b `seq` rnf c

-- | @since 0.8
instance NFData1 Node where
    liftRnf rnfx (Node2 _ a b) = rnfx a `seq` rnfx b
    liftRnf rnfx (Node3 _ a b c) = rnfx a `seq` rnfx b `seq` rnfx c

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
#ifdef TESTING
    deriving Show
#endif

#ifdef __GLASGOW_HASKELL__
-- | @since 0.6.1
deriving instance Generic1 Elem

-- | @since 0.6.1
deriving instance Generic (Elem a)
#endif

instance Sized (Elem a) where
    size _ = 1

instance Functor Elem where
#ifdef __GLASGOW_HASKELL__
-- This cuts the time for <*> by around a fifth.
    fmap = coerce
#else
    fmap f (Elem x) = Elem (f x)
#endif

instance Foldable Elem where
    foldr f z (Elem x) = f x z
#ifdef __GLASGOW_HASKELL__
    foldMap = coerce
    foldl = coerce
    foldl' = coerce
#else
    foldMap f (Elem x) = f x
    foldl f z (Elem x) = f z x
    foldl' f z (Elem x) = f z x
#endif

instance Traversable Elem where
    traverse f (Elem x) = Elem <$> f x

instance NFData a => NFData (Elem a) where
    rnf (Elem x) = rnf x

-- | @since 0.8
instance NFData1 Elem where
    liftRnf rnfx (Elem x) = rnfx x

-------------------------------------------------------
-- Applicative construction
-------------------------------------------------------

-- | 'applicativeTree' takes an Applicative-wrapped construction of a
-- piece of a FingerTree, assumed to always have the same size (which
-- is put in the second argument), and replicates it as many times as
-- specified.  This is a generalization of 'replicateA', which itself
-- is a generalization of many Data.Sequence methods.
{-# SPECIALIZE applicativeTree :: Int -> Int -> State s a -> State s (FingerTree a) #-}
{-# SPECIALIZE applicativeTree :: Int -> Int -> Identity a -> Identity (FingerTree a) #-}
-- Special note: the Identity specialization automatically does node sharing,
-- reducing memory usage of the resulting tree to \(O(\log n)\).
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
            n3 = liftA3 (Node3 mSize') m m m
  where
    one = fmap One m
    two = liftA2 Two m m
    three = liftA3 Three m m m
    deepA = liftA3 (Deep (n * mSize))
    emptyTree = pure EmptyT

data RCountMid a = RCountMid
  !(Node a)  -- End of the first
  !Int -- Number of units in the middle
  !(Node a)  -- Beginning of the last

{-
We could generalize beforeSeq quite easily to

  beforeSeq :: (a -> c) -> Seq a -> Seq b -> Seq c

This would let us add a rewrite rule

  fmap f xs <* ys  ==>  beforeSeq f xs ys

We don't currently bother because I don't yet know of a practical use for (<*)
for sequences; a rewrite rule to optimize it seems like extreme overkill.
-}

beforeSeq :: Seq a -> Seq b -> Seq a
beforeSeq xs ys = replicateEach (length ys) xs

-- | Replicate each element of a sequence the given number of times.
--
-- @replicateEach 3 [1,2] = [1,1,1,2,2,2]@
-- @replicateEach n xs = xs >>= replicate n@
replicateEach :: Int -> Seq a -> Seq a
-- The main idea is that we construct a function that takes an element and
-- produces a 2-3 tree representing that element replicated lenys times. We map
-- that function over the sequence to (mostly) produce the desired fingertree. But
-- if we *just* did that, we'd end up with a fingertree of 2-3 trees of the given
-- size, not of elements. So we need to work our way down to the appropriate
-- level by building the left side of the fingertree corresponding to the first
-- 2-3 tree and the right side corresponding to the last one, along with the
-- 2-3 trees corresponding to the right side of the first and the left side of
-- the last.
replicateEach lenys xs = case viewl xs of
  EmptyL -> empty
  firstx :< xs' -> case viewr xs' of
    EmptyR -> replicate lenys firstx
    Seq midxs :> lastx -> case lenys of
      0 -> empty
      1 -> xs
      2 ->
        Seq $ rep2EachFT fxE midxs lxE
      3 ->
        Seq $ rep3EachFT fxE midxs lxE
      _ -> Seq $ case lenys `quotRem` 3 of  -- lenys > 3
             (q,0) -> Deep (lenys * length xs) fd3
               (repEachMiddle_ lift_elem (RCountMid fn3 (q - 2) ln3))
               ld3
                   where
                    lift_elem a = let n3a = n3 a in (n3a, n3a, n3a)
             (q,1) -> Deep (lenys * length xs) fd2
               (repEachMiddle_ lift_elem (RCountMid fn2 (q - 1) ln2))
               ld2
                   where
                    lift_elem a = let n2a = n2 a in (n2a, n3 a, n2a)
             (q,_) -> Deep (lenys * length xs) fd3
               (repEachMiddle_ lift_elem (RCountMid fn2 (q - 1) ln3))
               ld2
                   where
                    lift_elem a = let n3a = n3 a in (n3a, n3a, n2 a)
        where
          repEachMiddle_ = repEachMiddle midxs lenys 3 fn3 ln3
          fd2 = Two fxE fxE
          fd3 = Three fxE fxE fxE
          ld2 = Two lxE lxE
          ld3 = Three lxE lxE lxE
          fn2 = Node2 2 fxE fxE
          fn3 = Node3 3 fxE fxE fxE
          ln2 = Node2 2 lxE lxE
          ln3 = Node3 3 lxE lxE lxE
          n3 a = Node3 3 (Elem a) (Elem a) (Elem a)
          n2 a = Node2 2 (Elem a) (Elem a)
      where
          fxE = Elem firstx
          lxE = Elem lastx

rep2EachFT :: Elem a -> FingerTree (Elem a) -> Elem a -> FingerTree (Elem a)
rep2EachFT firstx xs lastx =
                 Deep (size xs * 2 + 4)
                      (Two firstx firstx)
                      (mapMulFT 2 (\ex -> Node2 2 ex ex) xs)
                      (Two lastx lastx)

rep3EachFT :: Elem a -> FingerTree (Elem a) -> Elem a -> FingerTree (Elem a)
rep3EachFT firstx xs lastx =
                 Deep (size xs * 3 + 6)
                      (Three firstx firstx firstx)
                      (mapMulFT 3 (\ex -> Node3 3 ex ex ex) xs)
                      (Three lastx lastx lastx)

-- Invariants for repEachMiddle:
--
-- 1. midxs is constant: the middle bit in the original sequence (xs = (first <: Seq midxs :> last))
-- 2. lenys is constant: the length of ys
-- 3. firstx and pr repeat the same element: the first one in the original sequence xs
-- 4. lastx  and sf repeat the same element: the last  one in the original sequence xs
-- 5. sizec = size firstx = size lastx
-- 6. lenys = deep_count * sizec + size pr + size pf
-- 7. let (lft, fill, rght) = fill23 x, for any x:
--      7a. All three sequences repeat the element x
--      7b. size fill = sizec
--      7c. size lft  = size sf
--      7d. size rght = size pr
-- 8. size result = deep_count * sizec + lenys * (size midxs + 1)
repEachMiddle
  :: FingerTree (Elem a)  -- midxs
  -> Int                  -- lenys
  -> Int                  -- sizec
  -> Node c               -- firstx
  -> Node c               -- lastx
  -> (a -> (Node c, Node c, Node c))  -- fill23
  -> RCountMid c          -- (RCountMid pr deep_count sf)
  -> FingerTree (Node c)  -- result

-- At the bottom

repEachMiddle midxs lenys
            !_sizec
            _firstx
            _lastx
            fill23
            (RCountMid pr 0 sf)
     = Deep (lenys * (size midxs + 1))
            (One pr)
            (mapMulFT lenys fill23_final midxs)
            (One sf)
   where
     -- fill23_final ::  Elem a -> Node (Node c)
     fill23_final (Elem a) = case fill23 a of
        -- See the note on lift_fill23 for an explanation of this
        -- lazy pattern.
        ~(lft, _fill, rght) -> Node2 (size pr + size sf) lft rght

repEachMiddle midxs lenys
            !sizec
            firstx
            lastx
            fill23
            (RCountMid pr 1 sf)
     = Deep (sizec + lenys * (size midxs + 1))
            (Two pr firstx)
            (mapMulFT lenys fill23_final midxs)
            (Two lastx sf)
   where
     -- fill23_final ::  Elem a -> Node (Node c)
     fill23_final (Elem a) = case fill23 a of
        -- See the note on lift_fill23 for an explanation of this
        -- lazy pattern.
        ~(lft, fill, rght) -> Node3 (size pr + size sf + sizec) lft fill rght

-- Not at the bottom yet

repEachMiddle midxs lenys
            !sizec
            firstx
            lastx
            fill23
            (RCountMid pr deep_count sf)  -- deep_count > 1
  = case deep_count `quotRem` 3 of
      (q,0)
       -> deep'
        (Two firstx firstx)
        (repEachMiddle_
           (lift_fill23 TOT3 TOT2 fill23)
           (RCountMid pr' (q - 1) sf'))
        (One lastx)
       where
        pr' = node2 firstx pr
        sf' = node3 lastx lastx sf
      (q,1)
       -> deep'
        (Two firstx firstx)
        (repEachMiddle_
           (lift_fill23 TOT3 TOT3 fill23)
           (RCountMid pr' (q - 1) sf'))
        (Two lastx lastx)
       where
        pr' = node3 firstx firstx pr
        sf' = node3 lastx lastx sf
      (q,_) -- the remainder is 2
       -> deep'
        (One firstx)
        (repEachMiddle_
           (lift_fill23 TOT2 TOT2 fill23)
           (RCountMid pr' q sf'))
        (One lastx)
       where
        pr' = node2 firstx pr
        sf' = node2 lastx sf

  where
    deep' = Deep (deep_count * sizec + lenys * (size midxs + 1))
    repEachMiddle_ = repEachMiddle midxs lenys sizec' fn3 ln3
    sizec' = 3 * sizec
    fn3 = Node3 sizec' firstx firstx firstx
    ln3 = Node3 sizec' lastx lastx lastx
    spr = size pr
    ssf = size sf
    lift_fill23
      :: TwoOrThree
      -> TwoOrThree
      -> (a -> (b, b, b))
      -> a -> (Node b, Node b, Node b)
    lift_fill23 !tl !tr f a = (lft', fill', rght')
      where
        -- We use a strict pattern match on the recursive call.  This means
        -- that we build the 2-3 trees from the *bottom up* instead of from the
        -- *top down*. We do it this way for two reasons:
        --
        -- 1. The trees are never very deep, so we don't get much locality
        -- benefit from building them lazily.
        --
        -- 2. Building the trees lazily would require us to build four thunks
        -- at each level of each tree, which seems just a bit pricy.
        --
        -- Does this break the incremental optimality? I don't believe it does.
        -- As far as I can tell, each sequence operation that inspects one of
        -- these trees either inspects only its root (to get its size for
        -- indexing purposes) or descends all the way to the bottom. So we're
        -- strict here, and lazy in the construction of
        -- the root in fill23_final.
        !(lft, fill, rght) = f a
        !fill' = Node3 (3 * sizec) fill fill fill
        !lft' = case tl of
          TOT2 -> Node2 (ssf + sizec) lft fill
          TOT3 -> Node3 (ssf + 2 * sizec) lft fill fill
        !rght' = case tr of
          TOT2 -> Node2 (spr + sizec) rght fill
          TOT3 -> Node3 (spr + 2 * sizec) rght fill fill

data TwoOrThree = TOT2 | TOT3

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

-- | \( O(1) \). The empty sequence.
empty           :: Seq a
empty           =  Seq EmptyT

-- | \( O(1) \). A singleton sequence.
singleton       :: a -> Seq a
singleton x     =  Seq (Single (Elem x))

-- | \( O(\log n) \). @replicate n x@ is a sequence consisting of @n@ copies of @x@.
--
-- Calls 'error' if @n < 0@.
--
-- __Note__: This function is partial.
replicate       :: Int -> a -> Seq a
replicate n x
  | n >= 0      = runIdentity (replicateA n (Identity x))
  | otherwise   = error "replicate takes a nonnegative integer argument"

-- | 'replicateA' is an 'Applicative' version of 'replicate', and makes
-- \( O(\log n) \) calls to 'liftA2' and 'pure'.
--
-- Calls 'error' if @n < 0@.
--
-- __Note__: This function is partial.
--
-- > replicateA n x = sequenceA (replicate n x)
replicateA :: Applicative f => Int -> f a -> f (Seq a)
replicateA n x
  | n >= 0      = Seq <$> applicativeTree n 1 (Elem <$> x)
  | otherwise   = error "replicateA takes a nonnegative integer argument"
{-# SPECIALIZE replicateA :: Int -> State a b -> State a (Seq b) #-}

-- | Synonym for 'replicateA'.
--
-- This definition exists for backwards compatibility.
replicateM :: Applicative m => Int -> m a -> m (Seq a)
replicateM = replicateA

-- | \(O(\log k)\). @'cycleTaking' k xs@ forms a sequence of length @k@ by
-- repeatedly concatenating @xs@ with itself. @xs@ may only be empty if
-- @k@ is 0.
--
-- prop> cycleTaking k = fromList . take k . cycle . toList
--
-- If you wish to concatenate a possibly empty sequence @xs@ with
-- itself precisely @k@ times, use @'Data.Semigroup.stimes' k xs@ instead of
-- this function.
--
-- Calls 'error' if @k > 0@ and @null xs@.
--
-- __Note__: This function is partial.
--
-- @since 0.5.8
cycleTaking :: Int -> Seq a -> Seq a
cycleTaking n !_xs | n <= 0 = empty
cycleTaking _n xs  | null xs = error "cycleTaking cannot take a positive number of elements from an empty cycle."
cycleTaking n xs = cycleNTimes reps xs >< take final xs
  where
    (reps, final) = n `quotRem` length xs

-- \( O(\log(kn)) \). @'cycleNTimes' k xs@ concatenates @k@ copies of @xs@. This
-- operation uses time and additional space logarithmic in the size of its
-- result.
cycleNTimes :: Int -> Seq a -> Seq a
cycleNTimes n !xs
  | n <= 0    = empty
  | n == 1    = xs
cycleNTimes n (Seq xsFT) = case rigidify xsFT of
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


-- | \( O(1) \). Add an element to the left end of a sequence.
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

cons' :: a -> Seq a -> Seq a
cons' x (Seq xs) = Seq (Elem x `consTree'` xs)

snoc' :: Seq a -> a -> Seq a
snoc' (Seq xs) x = Seq (xs `snocTree'` Elem x)

{-# SPECIALIZE consTree' :: Elem a -> FingerTree (Elem a) -> FingerTree (Elem a) #-}
{-# SPECIALIZE consTree' :: Node a -> FingerTree (Node a) -> FingerTree (Node a) #-}
consTree'        :: Sized a => a -> FingerTree a -> FingerTree a
consTree' a EmptyT       = Single a
consTree' a (Single b)   = deep (One a) EmptyT (One b)
-- As described in the paper, we force the middle of a tree
-- *before* consing onto it; this preserves the amortized
-- bounds but prevents repeated consing from building up
-- gigantic suspensions.
consTree' a (Deep s (Four b c d e) m sf) =
    Deep (size a + s) (Two a b) m' sf
  where !m' = abc `consTree'` m
        !abc = node3 c d e
consTree' a (Deep s (Three b c d) m sf) =
    Deep (size a + s) (Four a b c d) m sf
consTree' a (Deep s (Two b c) m sf) =
    Deep (size a + s) (Three a b c) m sf
consTree' a (Deep s (One b) m sf) =
    Deep (size a + s) (Two a b) m sf

-- | \( O(1) \). Add an element to the right end of a sequence.
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

{-# SPECIALIZE snocTree' :: FingerTree (Elem a) -> Elem a -> FingerTree (Elem a) #-}
{-# SPECIALIZE snocTree' :: FingerTree (Node a) -> Node a -> FingerTree (Node a) #-}
snocTree'        :: Sized a => FingerTree a -> a -> FingerTree a
snocTree' EmptyT a       =  Single a
snocTree' (Single a) b   =  deep (One a) EmptyT (One b)
-- See note on `seq` in `consTree`.
snocTree' (Deep s pr m (Four a b c d)) e =
    Deep (s + size e) pr m' (Two d e)
  where !m' = m `snocTree'` abc
        !abc = node3 a b c
snocTree' (Deep s pr m (Three a b c)) d =
    Deep (s + size d) pr m (Four a b c d)
snocTree' (Deep s pr m (Two a b)) c =
    Deep (s + size c) pr m (Three a b c)
snocTree' (Deep s pr m (One a)) b =
    Deep (s + size b) pr m (Two a b)

-- | \( O(\log(\min(n_1,n_2))) \). Concatenate two sequences.
(><)            :: Seq a -> Seq a -> Seq a
Seq xs >< Seq ys = Seq (appendTree0 xs ys)

-- The appendTree/addDigits gunk below was originally machine generated via mkappend.hs,
-- but has since been manually edited to include strictness annotations.

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
    Deep (s1 + s2) pr1 m sf2
  where !m = addDigits0 m1 sf1 pr2 m2

addDigits0 :: FingerTree (Node (Elem a)) -> Digit (Elem a) -> Digit (Elem a) -> FingerTree (Node (Elem a)) -> FingerTree (Node (Elem a))
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
appendTree1 EmptyT !a xs =
    a `consTree` xs
appendTree1 xs !a EmptyT =
    xs `snocTree` a
appendTree1 (Single x) !a xs =
    x `consTree` a `consTree` xs
appendTree1 xs !a (Single x) =
    xs `snocTree` a `snocTree` x
appendTree1 (Deep s1 pr1 m1 sf1) a (Deep s2 pr2 m2 sf2) =
    Deep (s1 + size a + s2) pr1 m sf2
  where !m = addDigits1 m1 sf1 a pr2 m2

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
appendTree2 EmptyT !a !b xs =
    a `consTree` b `consTree` xs
appendTree2 xs !a !b EmptyT =
    xs `snocTree` a `snocTree` b
appendTree2 (Single x) a b xs =
    x `consTree` a `consTree` b `consTree` xs
appendTree2 xs a b (Single x) =
    xs `snocTree` a `snocTree` b `snocTree` x
appendTree2 (Deep s1 pr1 m1 sf1) a b (Deep s2 pr2 m2 sf2) =
    Deep (s1 + size a + size b + s2) pr1 m sf2
  where !m = addDigits2 m1 sf1 a b pr2 m2

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
appendTree3 EmptyT !a !b !c xs =
    a `consTree` b `consTree` c `consTree` xs
appendTree3 xs !a !b !c EmptyT =
    xs `snocTree` a `snocTree` b `snocTree` c
appendTree3 (Single x) a b c xs =
    x `consTree` a `consTree` b `consTree` c `consTree` xs
appendTree3 xs a b c (Single x) =
    xs `snocTree` a `snocTree` b `snocTree` c `snocTree` x
appendTree3 (Deep s1 pr1 m1 sf1) a b c (Deep s2 pr2 m2 sf2) =
    Deep (s1 + size a + size b + size c + s2) pr1 m sf2
  where !m = addDigits3 m1 sf1 a b c pr2 m2

addDigits3 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits3 m1 (One a) !b !c !d (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits3 m1 (One a) b c d (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (One a) b c d (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (One a) b c d (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) !c !d !e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (Two a b) c d e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Two a b) c d e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) !d !e !f (One g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Three a b c) d e f (Two g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Three a b c) d e f (Three g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (Four g h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) !e !f !g (One h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Four a b c d) e f g (Two h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Four a b c d) e f g (Three h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (Four h i j k) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2

appendTree4 :: FingerTree (Node a) -> Node a -> Node a -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree4 EmptyT !a !b !c !d xs =
    a `consTree` b `consTree` c `consTree` d `consTree` xs
appendTree4 xs !a !b !c !d EmptyT =
    xs `snocTree` a `snocTree` b `snocTree` c `snocTree` d
appendTree4 (Single x) a b c d xs =
    x `consTree` a `consTree` b `consTree` c `consTree` d `consTree` xs
appendTree4 xs a b c d (Single x) =
    xs `snocTree` a `snocTree` b `snocTree` c `snocTree` d `snocTree` x
appendTree4 (Deep s1 pr1 m1 sf1) a b c d (Deep s2 pr2 m2 sf2) =
    Deep (s1 + size a + size b + size c + size d + s2) pr1 m sf2
  where !m = addDigits4 m1 sf1 a b c d pr2 m2

addDigits4 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits4 m1 (One a) !b !c !d !e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits4 m1 (One a) b c d e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (One a) b c d e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (One a) b c d e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) !c !d !e !f (One g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (Two a b) c d e f (Two g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Two a b) c d e f (Three g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (Four g h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) !d !e !f !g (One h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Three a b c) d e f g (Two h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Three a b c) d e f g (Three h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (Four h i j k) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) !e !f !g !h (One i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Four a b c d) !e !f !g !h (Two i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Four a b c d) !e !f !g !h (Three i j k) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) !e !f !g !h (Four i j k l) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node3 j k l) m2

-- | Builds a sequence from a seed value.  Takes time linear in the
-- number of generated elements.  /WARNING:/ If the number of generated
-- elements is infinite, this method will not terminate.
unfoldr :: (b -> Maybe (a, b)) -> b -> Seq a
unfoldr f = unfoldr' empty
  -- uses tail recursion rather than, for instance, the List implementation.
  where unfoldr' !as b = maybe as (\ (a, b') -> unfoldr' (as `snoc'` a) b') (f b)

-- | @'unfoldl' f x@ is equivalent to @'reverse' ('unfoldr' ('fmap' swap . f) x)@.
unfoldl :: (b -> Maybe (b, a)) -> b -> Seq a
unfoldl f = unfoldl' empty
  where unfoldl' !as b = maybe as (\ (b', a) -> unfoldl' (a `cons'` as) b') (f b)

-- | \( O(n) \).  Constructs a sequence by repeated application of a function
-- to a seed value.
--
-- Calls 'error' if @n < 0@.
--
-- __Note__: This function is partial.
iterateN :: Int -> (a -> a) -> a -> Seq a
iterateN n f x
  | n >= 0      = replicateA n (State (\ y -> (f y, y))) `execState` x
  | otherwise   = error "iterateN takes a nonnegative integer argument"

------------------------------------------------------------------------
-- Deconstruction
------------------------------------------------------------------------

-- | \( O(1) \). Is this the empty sequence?
null            :: Seq a -> Bool
null (Seq EmptyT) = True
null _            =  False

-- | \( O(1) \). The number of elements in the sequence.
length          :: Seq a -> Int
length (Seq xs) =  size xs

-- Views

data ViewLTree a = ConsLTree a (FingerTree a) | EmptyLTree
data ViewRTree a = SnocRTree (FingerTree a) a | EmptyRTree

-- | View of the left end of a sequence.
data ViewL a
    = EmptyL        -- ^ empty sequence
    | a :< Seq a    -- ^ leftmost element and the rest of the sequence
    deriving (Eq, Ord, Show, Read)

#ifdef __GLASGOW_HASKELL__
deriving instance Data a => Data (ViewL a)

-- | @since 0.5.8
deriving instance Generic1 ViewL

-- | @since 0.5.8
deriving instance Generic (ViewL a)

-- | @since 0.6.6
deriving instance TH.Lift a => TH.Lift (ViewL a)
#endif

instance Functor ViewL where
    {-# INLINE fmap #-}
    fmap _ EmptyL       = EmptyL
    fmap f (x :< xs)    = f x :< fmap f xs

instance Foldable ViewL where
    foldMap _ EmptyL = mempty
    foldMap f (x :< xs) = f x <> foldMap f xs

    foldr _ z EmptyL = z
    foldr f z (x :< xs) = f x (foldr f z xs)

    foldl _ z EmptyL = z
    foldl f z (x :< xs) = foldl f (f z x) xs

    foldl1 _ EmptyL = error "foldl1: empty view"
    foldl1 f (x :< xs) = foldl f x xs

    null EmptyL = True
    null (_ :< _) = False

    length EmptyL = 0
    length (_ :< xs) = 1 + length xs

instance Traversable ViewL where
    traverse _ EmptyL       = pure EmptyL
    traverse f (x :< xs)    = liftA2 (:<) (f x) (traverse f xs)

-- | \( O(1) \). Analyse the left end of a sequence.
viewl           ::  Seq a -> ViewL a
viewl (Seq xs)  =  case viewLTree xs of
    EmptyLTree -> EmptyL
    ConsLTree (Elem x) xs' -> x :< Seq xs'

{-# SPECIALIZE viewLTree :: FingerTree (Elem a) -> ViewLTree (Elem a) #-}
{-# SPECIALIZE viewLTree :: FingerTree (Node a) -> ViewLTree (Node a) #-}
viewLTree       :: Sized a => FingerTree a -> ViewLTree a
viewLTree EmptyT                = EmptyLTree
viewLTree (Single a)            = ConsLTree a EmptyT
viewLTree (Deep s (One a) m sf) = ConsLTree a (pullL (s - size a) m sf)
viewLTree (Deep s (Two a b) m sf) =
    ConsLTree a (Deep (s - size a) (One b) m sf)
viewLTree (Deep s (Three a b c) m sf) =
    ConsLTree a (Deep (s - size a) (Two b c) m sf)
viewLTree (Deep s (Four a b c d) m sf) =
    ConsLTree a (Deep (s - size a) (Three b c d) m sf)

-- | View of the right end of a sequence.
data ViewR a
    = EmptyR        -- ^ empty sequence
    | Seq a :> a    -- ^ the sequence minus the rightmost element,
            -- and the rightmost element
    deriving (Eq, Ord, Show, Read)

#ifdef __GLASGOW_HASKELL__
deriving instance Data a => Data (ViewR a)

-- | @since 0.5.8
deriving instance Generic1 ViewR

-- | @since 0.5.8
deriving instance Generic (ViewR a)

-- | @since 0.6.6
deriving instance TH.Lift a => TH.Lift (ViewR a)
#endif

instance Functor ViewR where
    {-# INLINE fmap #-}
    fmap _ EmptyR       = EmptyR
    fmap f (xs :> x)    = fmap f xs :> f x

instance Foldable ViewR where
    foldMap _ EmptyR = mempty
    foldMap f (xs :> x) = foldMap f xs <> f x

    foldr _ z EmptyR = z
    foldr f z (xs :> x) = foldr f (f x z) xs

    foldl _ z EmptyR = z
    foldl f z (xs :> x) = foldl f z xs `f` x

    foldr1 _ EmptyR = error "foldr1: empty view"
    foldr1 f (xs :> x) = foldr f x xs

    null EmptyR = True
    null (_ :> _) = False

    length EmptyR = 0
    length (xs :> _) = length xs + 1

instance Traversable ViewR where
    traverse _ EmptyR       = pure EmptyR
    traverse f (xs :> x)    = liftA2 (:>) (traverse f xs) (f x)

-- | \( O(1) \). Analyse the right end of a sequence.
viewr           ::  Seq a -> ViewR a
viewr (Seq xs)  =  case viewRTree xs of
    EmptyRTree -> EmptyR
    SnocRTree xs' (Elem x) -> Seq xs' :> x

{-# SPECIALIZE viewRTree :: FingerTree (Elem a) -> ViewRTree (Elem a) #-}
{-# SPECIALIZE viewRTree :: FingerTree (Node a) -> ViewRTree (Node a) #-}
viewRTree       :: Sized a => FingerTree a -> ViewRTree a
viewRTree EmptyT                = EmptyRTree
viewRTree (Single z)            = SnocRTree EmptyT z
viewRTree (Deep s pr m (One z)) = SnocRTree (pullR (s - size z) pr m) z
viewRTree (Deep s pr m (Two y z)) =
    SnocRTree (Deep (s - size z) pr m (One y)) z
viewRTree (Deep s pr m (Three x y z)) =
    SnocRTree (Deep (s - size z) pr m (Two x y)) z
viewRTree (Deep s pr m (Four w x y z)) =
    SnocRTree (Deep (s - size z) pr m (Three w x y)) z

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
-- Calls 'error' if the sequence is empty.
--
-- __Note__: This function is partial.
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
--
-- Calls 'error' if the sequence is empty.
--
-- __Note__: This function is partial.
scanr1 :: (a -> a -> a) -> Seq a -> Seq a
scanr1 f xs = case viewr xs of
    EmptyR          -> error "scanr1 takes a nonempty sequence as an argument"
    xs' :> x        -> scanr f x xs'

-- Indexing

-- | \( O(\log(\min(i,n-i))) \). The element at the specified position,
-- counting from 0.  The argument should thus be a non-negative
-- integer less than the size of the sequence.
-- If the position is out of range, 'index' fails with an error.
--
-- prop> xs `index` i = toList xs !! i
--
-- __Note__: This function is partial. Prefer 'lookup'.
--
-- __Note__: 'index' necessarily delays retrieving the requested
-- element until the result is forced. It can therefore lead to a space
-- leak if the result is stored, unforced, in another structure. To retrieve
-- an element immediately without forcing it, use 'lookup' or '(!?)'.
index           :: Seq a -> Int -> a
index (Seq xs) i
  -- See note on unsigned arithmetic in splitAt
  | fromIntegral i < (fromIntegral (size xs) :: Word) = case lookupTree i xs of
                Place _ (Elem x) -> x
  | otherwise   =
      error $ "index out of bounds in call to: Data.Sequence.index " ++ show i

-- | \( O(\log(\min(i,n-i))) \). The element at the specified position,
-- counting from 0. If the specified position is negative or at
-- least the length of the sequence, 'lookup' returns 'Nothing'.
--
-- prop> 0 <= i < length xs ==> lookup i xs == Just (toList xs !! i)
-- prop> i < 0 || i >= length xs ==> lookup i xs = Nothing
--
-- Unlike 'index', this can be used to retrieve an element without
-- forcing it. For example, to insert the fifth element of a sequence
-- @xs@ into a 'Data.Map.Lazy.Map' @m@ at key @k@, you could use
--
-- @
-- case lookup 5 xs of
--   Nothing -> m
--   Just x -> 'Data.Map.Lazy.insert' k x m
-- @
--
-- @since 0.5.8
lookup            :: Int -> Seq a -> Maybe a
lookup i (Seq xs)
  -- Note: we perform the lookup *before* applying the Just constructor
  -- to ensure that we don't hold a reference to the whole sequence in
  -- a thunk. If we applied the Just constructor around the case, the
  -- actual lookup wouldn't be performed unless and until the value was
  -- forced.
  | fromIntegral i < (fromIntegral (size xs) :: Word) = case lookupTree i xs of
                Place _ (Elem x) -> Just x
  | otherwise = Nothing

-- | \( O(\log(\min(i,n-i))) \). A flipped, infix version of `lookup`.
--
-- @since 0.5.8
(!?) ::           Seq a -> Int -> Maybe a
(!?) = flip lookup

data Place a = Place {-# UNPACK #-} !Int a
#ifdef TESTING
    deriving Show
#endif

{-# SPECIALIZE lookupTree :: Int -> FingerTree (Elem a) -> Place (Elem a) #-}
{-# SPECIALIZE lookupTree :: Int -> FingerTree (Node a) -> Place (Node a) #-}
lookupTree :: Sized a => Int -> FingerTree a -> Place a
lookupTree !_ EmptyT = error "lookupTree of empty tree"
lookupTree i (Single x) = Place i x
lookupTree i (Deep _ pr m sf)
  | i < spr     =  lookupDigit i pr
  | i < spm     =  case lookupTree (i - spr) m of
                   Place i' xs -> lookupNode i' xs
  | otherwise   =  lookupDigit (i - spm) sf
  where
    spr     = size pr
    spm     = spr + size m

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

-- | \( O(\log(\min(i,n-i))) \). Replace the element at the specified position.
-- If the position is out of range, the original sequence is returned.
update          :: Int -> a -> Seq a -> Seq a
update i x (Seq xs)
  -- See note on unsigned arithmetic in splitAt
  | fromIntegral i < (fromIntegral (size xs) :: Word) = Seq (updateTree (Elem x) i xs)
  | otherwise   = Seq xs

-- It seems a shame to copy the implementation of the top layer of
-- `adjust` instead of just using `update i x = adjust (const x) i`.
-- With the latter implementation, updating the same position many
-- times could lead to silly thunks building up around that position.
-- The thunks will each look like @const v a@, where @v@ is the new
-- value and @a@ the old.
updateTree      :: Elem a -> Int -> FingerTree (Elem a) -> FingerTree (Elem a)
updateTree _ !_ EmptyT = EmptyT -- Unreachable
updateTree v _i (Single _) = Single v
updateTree v i (Deep s pr m sf)
  | i < spr     = Deep s (updateDigit v i pr) m sf
  | i < spm     = let !m' = adjustTree (updateNode v) (i - spr) m
                  in Deep s pr m' sf
  | otherwise   = Deep s pr m (updateDigit v (i - spm) sf)
  where
    spr     = size pr
    spm     = spr + size m

updateNode      :: Elem a -> Int -> Node (Elem a) -> Node (Elem a)
updateNode v i (Node2 s a b)
  | i < sa      = Node2 s v b
  | otherwise   = Node2 s a v
  where
    sa      = size a
updateNode v i (Node3 s a b c)
  | i < sa      = Node3 s v b c
  | i < sab     = Node3 s a v c
  | otherwise   = Node3 s a b v
  where
    sa      = size a
    sab     = sa + size b

updateDigit     :: Elem a -> Int -> Digit (Elem a) -> Digit (Elem a)
updateDigit v !_i (One _) = One v
updateDigit v i (Two a b)
  | i < sa      = Two v b
  | otherwise   = Two a v
  where
    sa      = size a
updateDigit v i (Three a b c)
  | i < sa      = Three v b c
  | i < sab     = Three a v c
  | otherwise   = Three a b v
  where
    sa      = size a
    sab     = sa + size b
updateDigit v i (Four a b c d)
  | i < sa      = Four v b c d
  | i < sab     = Four a v c d
  | i < sabc    = Four a b v d
  | otherwise   = Four a b c v
  where
    sa      = size a
    sab     = sa + size b
    sabc    = sab + size c

-- | \( O(\log(\min(i,n-i))) \). Update the element at the specified position.  If
-- the position is out of range, the original sequence is returned.  'adjust'
-- can lead to poor performance and even memory leaks, because it does not
-- force the new value before installing it in the sequence. 'adjust'' should
-- usually be preferred.
--
-- @since 0.5.8
adjust          :: (a -> a) -> Int -> Seq a -> Seq a
adjust f i (Seq xs)
  -- See note on unsigned arithmetic in splitAt
  | fromIntegral i < (fromIntegral (size xs) :: Word) = Seq (adjustTree (`seq` fmap f) i xs)
  | otherwise   = Seq xs

-- | \( O(\log(\min(i,n-i))) \). Update the element at the specified position.
-- If the position is out of range, the original sequence is returned.
-- The new value is forced before it is installed in the sequence.
--
-- @
-- adjust' f i xs =
--  case xs !? i of
--    Nothing -> xs
--    Just x -> let !x' = f x
--              in update i x' xs
-- @
--
-- @since 0.5.8
adjust'          :: forall a . (a -> a) -> Int -> Seq a -> Seq a
#ifdef __GLASGOW_HASKELL__
adjust' f i xs
  -- See note on unsigned arithmetic in splitAt
  | fromIntegral i < (fromIntegral (length xs) :: Word) =
      coerce $ adjustTree (\ !_k (ForceBox a) -> ForceBox (f a)) i (coerce xs)
  | otherwise   = xs
#else
-- This is inefficient, but fixing it would take a lot of fuss and bother
-- for little immediate gain. We can deal with that when we have another
-- Haskell implementation to worry about.
adjust' f i xs =
  case xs !? i of
    Nothing -> xs
    Just x -> let !x' = f x
              in update i x' xs
#endif

{-# SPECIALIZE adjustTree :: (Int -> ForceBox a -> ForceBox a) -> Int -> FingerTree (ForceBox a) -> FingerTree (ForceBox a) #-}
{-# SPECIALIZE adjustTree :: (Int -> Elem a -> Elem a) -> Int -> FingerTree (Elem a) -> FingerTree (Elem a) #-}
{-# SPECIALIZE adjustTree :: (Int -> Node a -> Node a) -> Int -> FingerTree (Node a) -> FingerTree (Node a) #-}
adjustTree      :: (Sized a, MaybeForce a) => (Int -> a -> a) ->
             Int -> FingerTree a -> FingerTree a
adjustTree _ !_ EmptyT = EmptyT -- Unreachable
adjustTree f i (Single x) = Single $!? f i x
adjustTree f i (Deep s pr m sf)
  | i < spr     = Deep s (adjustDigit f i pr) m sf
  | i < spm     = let !m' = adjustTree (adjustNode f) (i - spr) m
                  in Deep s pr m' sf
  | otherwise   = Deep s pr m (adjustDigit f (i - spm) sf)
  where
    spr     = size pr
    spm     = spr + size m

{-# SPECIALIZE adjustNode :: (Int -> Elem a -> Elem a) -> Int -> Node (Elem a) -> Node (Elem a) #-}
{-# SPECIALIZE adjustNode :: (Int -> Node a -> Node a) -> Int -> Node (Node a) -> Node (Node a) #-}
adjustNode      :: (Sized a, MaybeForce a) => (Int -> a -> a) -> Int -> Node a -> Node a
adjustNode f i (Node2 s a b)
  | i < sa      = let fia = f i a in fia `mseq` Node2 s fia b
  | otherwise   = let fisab = f (i - sa) b in fisab `mseq` Node2 s a fisab
  where
    sa      = size a
adjustNode f i (Node3 s a b c)
  | i < sa      = let fia = f i a in fia `mseq` Node3 s fia b c
  | i < sab     = let fisab = f (i - sa) b in fisab `mseq` Node3 s a fisab c
  | otherwise   = let fisabc = f (i - sab) c in fisabc `mseq` Node3 s a b fisabc
  where
    sa      = size a
    sab     = sa + size b

{-# SPECIALIZE adjustDigit :: (Int -> Elem a -> Elem a) -> Int -> Digit (Elem a) -> Digit (Elem a) #-}
{-# SPECIALIZE adjustDigit :: (Int -> Node a -> Node a) -> Int -> Digit (Node a) -> Digit (Node a) #-}
adjustDigit     :: (Sized a, MaybeForce a) => (Int -> a -> a) -> Int -> Digit a -> Digit a
adjustDigit f !i (One a) = One $!? f i a
adjustDigit f i (Two a b)
  | i < sa      = let fia = f i a in fia `mseq` Two fia b
  | otherwise   = let fisab = f (i - sa) b in fisab `mseq` Two a fisab
  where
    sa      = size a
adjustDigit f i (Three a b c)
  | i < sa      = let fia = f i a in fia `mseq` Three fia b c
  | i < sab     = let fisab = f (i - sa) b in fisab `mseq` Three a fisab c
  | otherwise   = let fisabc = f (i - sab) c in fisabc `mseq` Three a b fisabc
  where
    sa      = size a
    sab     = sa + size b
adjustDigit f i (Four a b c d)
  | i < sa      = let fia = f i a in fia `mseq` Four fia b c d
  | i < sab     = let fisab = f (i - sa) b in fisab `mseq` Four a fisab c d
  | i < sabc    = let fisabc = f (i - sab) c in fisabc `mseq` Four a b fisabc d
  | otherwise   = let fisabcd = f (i - sabc) d in fisabcd `mseq` Four a b c fisabcd
  where
    sa      = size a
    sab     = sa + size b
    sabc    = sab + size c

-- | \( O(\log(\min(i,n-i))) \). @'insertAt' i x xs@ inserts @x@ into @xs@
-- at the index @i@, shifting the rest of the sequence over.
--
-- @
-- insertAt 2 x (fromList [a,b,c,d]) = fromList [a,b,x,c,d]
-- insertAt 4 x (fromList [a,b,c,d]) = insertAt 10 x (fromList [a,b,c,d])
--                                   = fromList [a,b,c,d,x]
-- @
--
-- prop> insertAt i x xs = take i xs >< singleton x >< drop i xs
--
-- @since 0.5.8
insertAt :: Int -> a -> Seq a -> Seq a
insertAt i a s@(Seq xs)
  | fromIntegral i < (fromIntegral (size xs) :: Word)
      = Seq (insTree (`seq` InsTwo (Elem a)) i xs)
  | i <= 0 = a <| s
  | otherwise = s |> a

data Ins a = InsOne a | InsTwo a a

{-# SPECIALIZE insTree :: (Int -> Elem a -> Ins (Elem a)) -> Int -> FingerTree (Elem a) -> FingerTree (Elem a) #-}
{-# SPECIALIZE insTree :: (Int -> Node a -> Ins (Node a)) -> Int -> FingerTree (Node a) -> FingerTree (Node a) #-}
insTree      :: Sized a => (Int -> a -> Ins a) ->
             Int -> FingerTree a -> FingerTree a
insTree _ !_ EmptyT = EmptyT -- Unreachable
insTree f i (Single x) = case f i x of
  InsOne x' -> Single x'
  InsTwo m n -> deep (One m) EmptyT (One n)
insTree f i (Deep s pr m sf)
  | i < spr     = case insLeftDigit f i pr of
     InsLeftDig pr' -> Deep (s + 1) pr' m sf
     InsDigNode pr' n -> m `seq` Deep (s + 1) pr' (n `consTree` m) sf
  | i < spm     = let !m' = insTree (insNode f) (i - spr) m
                  in Deep (s + 1) pr m' sf
  | otherwise   = case insRightDigit f (i - spm) sf of
     InsRightDig sf' -> Deep (s + 1) pr m sf'
     InsNodeDig n sf' -> m `seq` Deep (s + 1) pr (m `snocTree` n) sf'
  where
    spr     = size pr
    spm     = spr + size m

{-# SPECIALIZE insNode :: (Int -> Elem a -> Ins (Elem a)) -> Int -> Node (Elem a) -> Ins (Node (Elem a)) #-}
{-# SPECIALIZE insNode :: (Int -> Node a -> Ins (Node a)) -> Int -> Node (Node a) -> Ins (Node (Node a)) #-}
insNode :: Sized a => (Int -> a -> Ins a) -> Int -> Node a -> Ins (Node a)
insNode f i (Node2 s a b)
  | i < sa = case f i a of
      InsOne n -> InsOne $ Node2 (s + 1) n b
      InsTwo m n -> InsOne $ Node3 (s + 1) m n b
  | otherwise = case f (i - sa) b of
      InsOne n -> InsOne $ Node2 (s + 1) a n
      InsTwo m n -> InsOne $ Node3 (s + 1) a m n
  where sa = size a
insNode f i (Node3 s a b c)
  | i < sa = case f i a of
      InsOne n -> InsOne $ Node3 (s + 1) n b c
      InsTwo m n -> InsTwo (Node2 (sa + 1) m n) (Node2 (s - sa) b c)
  | i < sab = case f (i - sa) b of
      InsOne n -> InsOne $ Node3 (s + 1) a n c
      InsTwo m n -> InsTwo am nc
        where !am = node2 a m
              !nc = node2 n c
  | otherwise = case f (i - sab) c of
      InsOne n -> InsOne $ Node3 (s + 1) a b n
      InsTwo m n -> InsTwo (Node2 sab a b) (Node2 (s - sab + 1) m n)
  where sa = size a
        sab = sa + size b

data InsDigNode a = InsLeftDig !(Digit a) | InsDigNode !(Digit a) !(Node a)
{-# SPECIALIZE insLeftDigit :: (Int -> Elem a -> Ins (Elem a)) -> Int -> Digit (Elem a) -> InsDigNode (Elem a) #-}
{-# SPECIALIZE insLeftDigit :: (Int -> Node a -> Ins (Node a)) -> Int -> Digit (Node a) -> InsDigNode (Node a) #-}
insLeftDigit :: Sized a => (Int -> a -> Ins a) -> Int -> Digit a -> InsDigNode a
insLeftDigit f !i (One a) = case f i a of
  InsOne a' -> InsLeftDig $ One a'
  InsTwo a1 a2 -> InsLeftDig $ Two a1 a2
insLeftDigit f i (Two a b)
  | i < sa = case f i a of
     InsOne a' -> InsLeftDig $ Two a' b
     InsTwo a1 a2 -> InsLeftDig $ Three a1 a2 b
  | otherwise = case f (i - sa) b of
     InsOne b' -> InsLeftDig $ Two a b'
     InsTwo b1 b2 -> InsLeftDig $ Three a b1 b2
  where sa = size a
insLeftDigit f i (Three a b c)
  | i < sa = case f i a of
     InsOne a' -> InsLeftDig $ Three a' b c
     InsTwo a1 a2 -> InsLeftDig $ Four a1 a2 b c
  | i < sab = case f (i - sa) b of
     InsOne b' -> InsLeftDig $ Three a b' c
     InsTwo b1 b2 -> InsLeftDig $ Four a b1 b2 c
  | otherwise = case f (i - sab) c of
     InsOne c' -> InsLeftDig $ Three a b c'
     InsTwo c1 c2 -> InsLeftDig $ Four a b c1 c2
  where sa = size a
        sab = sa + size b
insLeftDigit f i (Four a b c d)
  | i < sa = case f i a of
     InsOne a' -> InsLeftDig $ Four a' b c d
     InsTwo a1 a2 -> InsDigNode (Two a1 a2) (node3 b c d)
  | i < sab = case f (i - sa) b of
     InsOne b' -> InsLeftDig $ Four a b' c d
     InsTwo b1 b2 -> InsDigNode (Two a b1) (node3 b2 c d)
  | i < sabc = case f (i - sab) c of
     InsOne c' -> InsLeftDig $ Four a b c' d
     InsTwo c1 c2 -> InsDigNode (Two a b) (node3 c1 c2 d)
  | otherwise = case f (i - sabc) d of
     InsOne d' -> InsLeftDig $ Four a b c d'
     InsTwo d1 d2 -> InsDigNode (Two a b) (node3 c d1 d2)
  where sa = size a
        sab = sa + size b
        sabc = sab + size c

data InsNodeDig a = InsRightDig !(Digit a) | InsNodeDig !(Node a) !(Digit a)
{-# SPECIALIZE insRightDigit :: (Int -> Elem a -> Ins (Elem a)) -> Int -> Digit (Elem a) -> InsNodeDig (Elem a) #-}
{-# SPECIALIZE insRightDigit :: (Int -> Node a -> Ins (Node a)) -> Int -> Digit (Node a) -> InsNodeDig (Node a) #-}
insRightDigit :: Sized a => (Int -> a -> Ins a) -> Int -> Digit a -> InsNodeDig a
insRightDigit f !i (One a) = case f i a of
  InsOne a' -> InsRightDig $ One a'
  InsTwo a1 a2 -> InsRightDig $ Two a1 a2
insRightDigit f i (Two a b)
  | i < sa = case f i a of
     InsOne a' -> InsRightDig $ Two a' b
     InsTwo a1 a2 -> InsRightDig $ Three a1 a2 b
  | otherwise = case f (i - sa) b of
     InsOne b' -> InsRightDig $ Two a b'
     InsTwo b1 b2 -> InsRightDig $ Three a b1 b2
  where sa = size a
insRightDigit f i (Three a b c)
  | i < sa = case f i a of
     InsOne a' -> InsRightDig $ Three a' b c
     InsTwo a1 a2 -> InsRightDig $ Four a1 a2 b c
  | i < sab = case f (i - sa) b of
     InsOne b' -> InsRightDig $ Three a b' c
     InsTwo b1 b2 -> InsRightDig $ Four a b1 b2 c
  | otherwise = case f (i - sab) c of
     InsOne c' -> InsRightDig $ Three a b c'
     InsTwo c1 c2 -> InsRightDig $ Four a b c1 c2
  where sa = size a
        sab = sa + size b
insRightDigit f i (Four a b c d)
  | i < sa = case f i a of
     InsOne a' -> InsRightDig $ Four a' b c d
     InsTwo a1 a2 -> InsNodeDig (node3 a1 a2 b) (Two c d)
  | i < sab = case f (i - sa) b of
     InsOne b' -> InsRightDig $ Four a b' c d
     InsTwo b1 b2 -> InsNodeDig (node3 a b1 b2) (Two c d)
  | i < sabc = case f (i - sab) c of
     InsOne c' -> InsRightDig $ Four a b c' d
     InsTwo c1 c2 -> InsNodeDig (node3 a b c1) (Two c2 d)
  | otherwise = case f (i - sabc) d of
     InsOne d' -> InsRightDig $ Four a b c d'
     InsTwo d1 d2 -> InsNodeDig (node3 a b c) (Two d1 d2)
  where sa = size a
        sab = sa + size b
        sabc = sab + size c

-- | \( O(\log(\min(i,n-i))) \). Delete the element of a sequence at a given
-- index. Return the original sequence if the index is out of range.
--
-- @
-- deleteAt 2 [a,b,c,d] = [a,b,d]
-- deleteAt 4 [a,b,c,d] = deleteAt (-1) [a,b,c,d] = [a,b,c,d]
-- @
--
-- @since 0.5.8
deleteAt :: Int -> Seq a -> Seq a
deleteAt i (Seq xs)
  | fromIntegral i < (fromIntegral (size xs) :: Word) = Seq $ delTreeE i xs
  | otherwise = Seq xs

delTreeE :: Int -> FingerTree (Elem a) -> FingerTree (Elem a)
delTreeE !_i EmptyT = EmptyT -- Unreachable
delTreeE _i Single{} = EmptyT
delTreeE i (Deep s pr m sf)
  | i < spr = delLeftDigitE i s pr m sf
  | i < spm = case delTree delNodeE (i - spr) m of
     FullTree m' -> Deep (s - 1) pr m' sf
     DefectTree e -> delRebuildMiddle (s - 1) pr e sf
  | otherwise = delRightDigitE (i - spm) s pr m sf
  where spr = size pr
        spm = spr + size m

delNodeE :: Int -> Node (Elem a) -> Del (Elem a)
delNodeE i (Node3 _ a b c) = case i of
  0 -> Full $ Node2 2 b c
  1 -> Full $ Node2 2 a c
  _ -> Full $ Node2 2 a b
delNodeE i (Node2 _ a b) = case i of
  0 -> Defect b
  _ -> Defect a


delLeftDigitE :: Int -> Int -> Digit (Elem a) -> FingerTree (Node (Elem a)) -> Digit (Elem a) -> FingerTree (Elem a)
delLeftDigitE !_i s One{} m sf = pullL (s - 1) m sf
delLeftDigitE i s (Two a b) m sf
  | i == 0 = Deep (s - 1) (One b) m sf
  | otherwise = Deep (s - 1) (One a) m sf
delLeftDigitE i s (Three a b c) m sf
  | i == 0 = Deep (s - 1) (Two b c) m sf
  | i == 1 = Deep (s - 1) (Two a c) m sf
  | otherwise = Deep (s - 1) (Two a b) m sf
delLeftDigitE i s (Four a b c d) m sf
  | i == 0 = Deep (s - 1) (Three b c d) m sf
  | i == 1 = Deep (s - 1) (Three a c d) m sf
  | i == 2 = Deep (s - 1) (Three a b d) m sf
  | otherwise = Deep (s - 1) (Three a b c) m sf

delRightDigitE :: Int -> Int -> Digit (Elem a) -> FingerTree (Node (Elem a)) -> Digit (Elem a) -> FingerTree (Elem a)
delRightDigitE !_i s pr m One{} = pullR (s - 1) pr m
delRightDigitE i s pr m (Two a b)
  | i == 0 = Deep (s - 1) pr m (One b)
  | otherwise = Deep (s - 1) pr m (One a)
delRightDigitE i s pr m (Three a b c)
  | i == 0 = Deep (s - 1) pr m (Two b c)
  | i == 1 = Deep (s - 1) pr m (Two a c)
  | otherwise = deep pr m (Two a b)
delRightDigitE i s pr m (Four a b c d)
  | i == 0 = Deep (s - 1) pr m (Three b c d)
  | i == 1 = Deep (s - 1) pr m (Three a c d)
  | i == 2 = Deep (s - 1) pr m (Three a b d)
  | otherwise = Deep (s - 1) pr m (Three a b c)

data DelTree a = FullTree !(FingerTree (Node a)) | DefectTree a

{-# SPECIALIZE delTree :: (Int -> Node (Elem a) -> Del (Elem a)) -> Int -> FingerTree (Node (Elem a)) -> DelTree (Elem a) #-}
{-# SPECIALIZE delTree :: (Int -> Node (Node a) -> Del (Node a)) -> Int -> FingerTree (Node (Node a)) -> DelTree (Node a) #-}
delTree :: Sized a => (Int -> Node a -> Del a) -> Int -> FingerTree (Node a) -> DelTree a
delTree _f !_i EmptyT = FullTree EmptyT -- Unreachable
delTree f i (Single a) = case f i a of
  Full a' -> FullTree (Single a')
  Defect e -> DefectTree e
delTree f i (Deep s pr m sf)
  | i < spr = case delDigit f i pr of
     FullDig pr' -> FullTree $ Deep (s - 1) pr' m sf
     DefectDig e -> case viewLTree m of
                      EmptyLTree -> FullTree $ delRebuildRightDigit (s - 1) e sf
                      ConsLTree n m' -> FullTree $ delRebuildLeftSide (s - 1) e n m' sf
  | i < spm = case delTree (delNode f) (i - spr) m of
     FullTree m' -> FullTree (Deep (s - 1) pr m' sf)
     DefectTree e -> FullTree $ delRebuildMiddle (s - 1) pr e sf
  | otherwise = case delDigit f (i - spm) sf of
     FullDig sf' -> FullTree $ Deep (s - 1) pr m sf'
     DefectDig e -> case viewRTree m of
                      EmptyRTree -> FullTree $ delRebuildLeftDigit (s - 1) pr e
                      SnocRTree m' n -> FullTree $ delRebuildRightSide (s - 1) pr m' n e
  where spr = size pr
        spm = spr + size m

data Del a = Full !(Node a) | Defect a

{-# SPECIALIZE delNode :: (Int -> Node (Elem a) -> Del (Elem a)) -> Int -> Node (Node (Elem a)) -> Del (Node (Elem a)) #-}
{-# SPECIALIZE delNode :: (Int -> Node (Node a) -> Del (Node a)) -> Int -> Node (Node (Node a)) -> Del (Node (Node a)) #-}
delNode :: Sized a => (Int -> Node a -> Del a) -> Int -> Node (Node a) -> Del (Node a)
delNode f i (Node3 s a b c)
  | i < sa = case f i a of
     Full a' -> Full $ Node3 (s - 1) a' b c
     Defect e -> let !se = size e in case b of
       Node3 sxyz x y z -> Full $ Node3 (s - 1) (Node2 (se + sx) e x) (Node2 (sxyz - sx) y z) c
         where !sx = size x
       Node2 sxy x y -> Full $ Node2 (s - 1) (Node3 (sxy + se) e x y) c
  | i < sab = case f (i - sa) b of
     Full b' -> Full $ Node3 (s - 1) a b' c
     Defect e -> let !se = size e in case a of
       Node3 sxyz x y z -> Full $ Node3 (s - 1) (Node2 (sxyz - sz) x y) (Node2 (sz + se) z e) c
         where !sz = size z
       Node2 sxy x y -> Full $ Node2 (s - 1) (Node3 (sxy + se) x y e) c
  | otherwise = case f (i - sab) c of
     Full c' -> Full $ Node3 (s - 1) a b c'
     Defect e -> let !se = size e in case b of
       Node3 sxyz x y z -> Full $ Node3 (s - 1) a (Node2 (sxyz - sz) x y) (Node2 (sz + se) z e)
         where !sz = size z
       Node2 sxy x y -> Full $ Node2 (s - 1) a (Node3 (sxy + se) x y e)
  where sa = size a
        sab = sa + size b
delNode f i (Node2 s a b)
  | i < sa = case f i a of
     Full a' -> Full $ Node2 (s - 1) a' b
     Defect e -> let !se = size e in case b of
       Node3 sxyz x y z -> Full $ Node2 (s - 1) (Node2 (se + sx) e x) (Node2 (sxyz - sx) y z)
        where !sx = size x
       Node2 _ x y -> Defect $ Node3 (s - 1) e x y
  | otherwise = case f (i - sa) b of
     Full b' -> Full $ Node2 (s - 1) a b'
     Defect e -> let !se = size e in case a of
       Node3 sxyz x y z -> Full $ Node2 (s - 1) (Node2 (sxyz - sz) x y) (Node2 (sz + se) z e)
         where !sz = size z
       Node2 _ x y -> Defect $ Node3 (s - 1) x y e
  where sa = size a

{-# SPECIALIZE delRebuildRightDigit :: Int -> Elem a -> Digit (Node (Elem a)) -> FingerTree (Node (Elem a)) #-}
{-# SPECIALIZE delRebuildRightDigit :: Int -> Node a -> Digit (Node (Node a)) -> FingerTree (Node (Node a)) #-}
delRebuildRightDigit :: Sized a => Int -> a -> Digit (Node a) -> FingerTree (Node a)
delRebuildRightDigit s p (One a) = let !sp = size p in case a of
  Node3 sxyz x y z -> Deep s (One (Node2 (sp + sx) p x)) EmptyT (One (Node2 (sxyz - sx) y z))
    where !sx = size x
  Node2 sxy x y -> Single (Node3 (sp + sxy) p x y)
delRebuildRightDigit s p (Two a b) = let !sp = size p in case a of
  Node3 sxyz x y z -> Deep s (Two (Node2 (sp + sx) p x) (Node2 (sxyz - sx) y z)) EmptyT (One b)
    where !sx = size x
  Node2 sxy x y -> Deep s (One (Node3 (sp + sxy) p x y)) EmptyT (One b)
delRebuildRightDigit s p (Three a b c) = let !sp = size p in case a of
  Node3 sxyz x y z -> Deep s (Two (Node2 (sp + sx) p x) (Node2 (sxyz - sx) y z)) EmptyT (Two b c)
    where !sx = size x
  Node2 sxy x y -> Deep s (Two (Node3 (sp + sxy) p x y) b) EmptyT (One c)
delRebuildRightDigit s p (Four a b c d) = let !sp = size p in case a of
  Node3 sxyz x y z -> Deep s (Three (Node2 (sp + sx) p x) (Node2 (sxyz - sx) y z) b) EmptyT (Two c d)
    where !sx = size x
  Node2 sxy x y -> Deep s (Two (Node3 (sp + sxy) p x y) b) EmptyT (Two c d)

{-# SPECIALIZE delRebuildLeftDigit :: Int -> Digit (Node (Elem a)) -> Elem a -> FingerTree (Node (Elem a)) #-}
{-# SPECIALIZE delRebuildLeftDigit :: Int -> Digit (Node (Node a)) -> Node a -> FingerTree (Node (Node a)) #-}
delRebuildLeftDigit :: Sized a => Int -> Digit (Node a) -> a -> FingerTree (Node a)
delRebuildLeftDigit s (One a) p = let !sp = size p in case a of
  Node3 sxyz x y z -> Deep s (One (Node2 (sxyz - sz) x y)) EmptyT (One (Node2 (sz + sp) z p))
    where !sz = size z
  Node2 sxy x y -> Single (Node3 (sxy + sp) x y p)
delRebuildLeftDigit s (Two a b) p = let !sp = size p in case b of
  Node3 sxyz x y z -> Deep s (Two a (Node2 (sxyz - sz) x y)) EmptyT (One (Node2 (sz + sp) z p))
    where !sz = size z
  Node2 sxy x y -> Deep s (One a) EmptyT (One (Node3 (sxy + sp) x y p))
delRebuildLeftDigit s (Three a b c) p = let !sp = size p in case c of
  Node3 sxyz x y z -> Deep s (Two a b) EmptyT (Two (Node2 (sxyz - sz) x y) (Node2 (sz + sp) z p))
    where !sz = size z
  Node2 sxy x y -> Deep s (Two a b) EmptyT (One (Node3 (sxy + sp) x y p))
delRebuildLeftDigit s (Four a b c d) p = let !sp = size p in case d of
  Node3 sxyz x y z -> Deep s (Three a b c) EmptyT (Two (Node2 (sxyz - sz) x y) (Node2 (sz + sp) z p))
    where !sz = size z
  Node2 sxy x y -> Deep s (Two a b) EmptyT (Two c (Node3 (sxy + sp) x y p))

delRebuildLeftSide :: Sized a
                   => Int -> a -> Node (Node a) -> FingerTree (Node (Node a)) -> Digit (Node a)
                   -> FingerTree (Node a)
delRebuildLeftSide s p (Node2 _ a b) m sf = let !sp = size p in case a of
  Node2 sxy x y -> Deep s (Two (Node3 (sp + sxy) p x y) b) m sf
  Node3 sxyz x y z -> Deep s (Three (Node2 (sp + sx) p x) (Node2 (sxyz - sx) y z) b) m sf
    where !sx = size x
delRebuildLeftSide s p (Node3 _ a b c) m sf = let !sp = size p in case a of
  Node2 sxy x y -> Deep s (Three (Node3 (sp + sxy) p x y) b c) m sf
  Node3 sxyz x y z -> Deep s (Four (Node2 (sp + sx) p x) (Node2 (sxyz - sx) y z) b c) m sf
    where !sx = size x

delRebuildRightSide :: Sized a
                    => Int -> Digit (Node a) -> FingerTree (Node (Node a)) -> Node (Node a) -> a
                    -> FingerTree (Node a)
delRebuildRightSide s pr m (Node2 _ a b) p = let !sp = size p in case b of
  Node2 sxy x y -> Deep s pr m (Two a (Node3 (sxy + sp) x y p))
  Node3 sxyz x y z -> Deep s pr m (Three a (Node2 (sxyz - sz) x y) (Node2 (sz + sp) z p))
    where !sz = size z
delRebuildRightSide s pr m (Node3 _ a b c) p = let !sp = size p in case c of
  Node2 sxy x y -> Deep s pr m (Three a b (Node3 (sxy + sp) x y p))
  Node3 sxyz x y z -> Deep s pr m (Four a b (Node2 (sxyz - sz) x y) (Node2 (sz + sp) z p))
    where !sz = size z

delRebuildMiddle :: Sized a
                 => Int -> Digit a -> a -> Digit a
                 -> FingerTree a
delRebuildMiddle s (One a) e sf = Deep s (Two a e) EmptyT sf
delRebuildMiddle s (Two a b) e sf = Deep s (Three a b e) EmptyT sf
delRebuildMiddle s (Three a b c) e sf = Deep s (Four a b c e) EmptyT sf
delRebuildMiddle s (Four a b c d) e sf = Deep s (Two a b) (Single (node3 c d e)) sf

data DelDig a = FullDig !(Digit (Node a)) | DefectDig a

{-# SPECIALIZE delDigit :: (Int -> Node (Elem a) -> Del (Elem a)) -> Int -> Digit (Node (Elem a)) -> DelDig (Elem a) #-}
{-# SPECIALIZE delDigit :: (Int -> Node (Node a) -> Del (Node a)) -> Int -> Digit (Node (Node a)) -> DelDig (Node a) #-}
delDigit :: Sized a => (Int -> Node a -> Del a) -> Int -> Digit (Node a) -> DelDig a
delDigit f !i (One a) = case f i a of
  Full a' -> FullDig $ One a'
  Defect e -> DefectDig e
delDigit f i (Two a b)
  | i < sa = case f i a of
     Full a' -> FullDig $ Two a' b
     Defect e -> let !se = size e in case b of
       Node3 sxyz x y z -> FullDig $ Two (Node2 (se + sx) e x) (Node2 (sxyz - sx) y z)
         where !sx = size x
       Node2 sxy x y -> FullDig $ One (Node3 (se + sxy) e x y)
  | otherwise = case f (i - sa) b of
     Full b' -> FullDig $ Two a b'
     Defect e -> let !se = size e in case a of
       Node3 sxyz x y z -> FullDig $ Two (Node2 (sxyz - sz) x y) (Node2 (sz + se) z e)
         where !sz = size z
       Node2 sxy x y -> FullDig $ One (Node3 (sxy + se) x y e)
  where sa = size a
delDigit f i (Three a b c)
  | i < sa = case f i a of
     Full a' -> FullDig $ Three a' b c
     Defect e -> let !se = size e in case b of
       Node3 sxyz x y z -> FullDig $ Three (Node2 (se + sx) e x) (Node2 (sxyz - sx) y z) c
         where !sx = size x
       Node2 sxy x y -> FullDig $ Two (Node3 (se + sxy) e x y) c
  | i < sab = case f (i - sa) b of
     Full b' -> FullDig $ Three a b' c
     Defect e -> let !se = size e in case a of
       Node3 sxyz x y z -> FullDig $ Three (Node2 (sxyz - sz) x y) (Node2 (sz + se) z e) c
         where !sz = size z
       Node2 sxy x y -> FullDig $ Two (Node3 (sxy + se) x y e) c
  | otherwise = case f (i - sab) c of
     Full c' -> FullDig $ Three a b c'
     Defect e -> let !se = size e in case b of
       Node3 sxyz x y z -> FullDig $ Three a (Node2 (sxyz - sz) x y) (Node2 (sz + se) z e)
         where !sz = size z
       Node2 sxy x y -> FullDig $ Two a (Node3 (sxy + se) x y e)
  where sa = size a
        sab = sa + size b
delDigit f i (Four a b c d)
  | i < sa = case f i a of
     Full a' -> FullDig $ Four a' b c d
     Defect e -> let !se = size e in case b of
       Node3 sxyz x y z -> FullDig $ Four (Node2 (se + sx) e x) (Node2 (sxyz - sx) y z) c d
         where !sx = size x
       Node2 sxy x y -> FullDig $ Three (Node3 (se + sxy) e x y) c d
  | i < sab = case f (i - sa) b of
     Full b' -> FullDig $ Four a b' c d
     Defect e -> let !se = size e in case a of
       Node3 sxyz x y z -> FullDig $ Four (Node2 (sxyz - sz) x y) (Node2 (sz + se) z e) c d
         where !sz = size z
       Node2 sxy x y -> FullDig $ Three (Node3 (sxy + se) x y e) c d
  | i < sabc = case f (i - sab) c of
     Full c' -> FullDig $ Four a b c' d
     Defect e -> let !se = size e in case b of
       Node3 sxyz x y z -> FullDig $ Four a (Node2 (sxyz - sz) x y) (Node2 (sz + se) z e) d
         where !sz = size z
       Node2 sxy x y -> FullDig $ Three a (Node3 (sxy + se) x y e) d
  | otherwise = case f (i - sabc) d of
     Full d' -> FullDig $ Four a b c d'
     Defect e -> let !se = size e in case c of
       Node3 sxyz x y z -> FullDig $ Four a b (Node2 (sxyz - sz) x y) (Node2 (sz + se) z e)
         where !sz = size z
       Node2 sxy x y -> FullDig $ Three a b (Node3 (sxy + se) x y e)
  where sa = size a
        sab = sa + size b
        sabc = sab + size c


-- | A generalization of 'fmap', 'mapWithIndex' takes a mapping
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
      !sPsprm = sPspr + size m

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

{-# INLINE foldWithIndexDigit #-}
foldWithIndexDigit :: Sized a => (b -> b -> b) -> (Int -> a -> b) -> Int -> Digit a -> b
foldWithIndexDigit _ f !s (One a) = f s a
foldWithIndexDigit (<+>) f s (Two a b) = f s a <+> f sPsa b
  where
    !sPsa = s + size a
foldWithIndexDigit (<+>) f s (Three a b c) = f s a <+> f sPsa b <+> f sPsab c
  where
    !sPsa = s + size a
    !sPsab = sPsa + size b
foldWithIndexDigit (<+>) f s (Four a b c d) =
    f s a <+> f sPsa b <+> f sPsab c <+> f sPsabc d
  where
    !sPsa = s + size a
    !sPsab = sPsa + size b
    !sPsabc = sPsab + size c

{-# INLINE foldWithIndexNode #-}
foldWithIndexNode :: Sized a => (m -> m -> m) -> (Int -> a -> m) -> Int -> Node a -> m
foldWithIndexNode (<+>) f !s (Node2 _ a b) = f s a <+> f sPsa b
  where
    !sPsa = s + size a
foldWithIndexNode (<+>) f s (Node3 _ a b c) = f s a <+> f sPsa b <+> f sPsab c
  where
    !sPsa = s + size a
    !sPsab = sPsa + size b

-- A generalization of 'foldMap', 'foldMapWithIndex' takes a folding
-- function that also depends on the element's index, and applies it to every
-- element in the sequence.
--
-- @since 0.5.8
foldMapWithIndex :: Monoid m => (Int -> a -> m) -> Seq a -> m
foldMapWithIndex f' (Seq xs') = foldMapWithIndexTreeE (lift_elem f') 0 xs'
 where
  lift_elem :: (Int -> a -> m) -> (Int -> Elem a -> m)
#ifdef __GLASGOW_HASKELL__
  lift_elem g = coerce g
#else
  lift_elem g = \s (Elem a) -> g s a
#endif
  {-# INLINE lift_elem #-}
-- We have to specialize these functions by hand, unfortunately, because
-- GHC does not specialize until *all* instances are determined.
-- Although the Sized instance is known at compile time, the Monoid
-- instance generally is not.
  foldMapWithIndexTreeE :: Monoid m => (Int -> Elem a -> m) -> Int -> FingerTree (Elem a) -> m
  foldMapWithIndexTreeE _ !_s EmptyT = mempty
  foldMapWithIndexTreeE f s (Single xs) = f s xs
  foldMapWithIndexTreeE f s (Deep _ pr m sf) =
               foldMapWithIndexDigitE f s pr <>
               foldMapWithIndexTreeN (foldMapWithIndexNodeE f) sPspr m <>
               foldMapWithIndexDigitE f sPsprm sf
    where
      !sPspr = s + size pr
      !sPsprm = sPspr + size m

  foldMapWithIndexTreeN :: Monoid m => (Int -> Node a -> m) -> Int -> FingerTree (Node a) -> m
  foldMapWithIndexTreeN _ !_s EmptyT = mempty
  foldMapWithIndexTreeN f s (Single xs) = f s xs
  foldMapWithIndexTreeN f s (Deep _ pr m sf) =
               foldMapWithIndexDigitN f s pr <>
               foldMapWithIndexTreeN (foldMapWithIndexNodeN f) sPspr m <>
               foldMapWithIndexDigitN f sPsprm sf
    where
      !sPspr = s + size pr
      !sPsprm = sPspr + size m

  foldMapWithIndexDigitE :: Monoid m => (Int -> Elem a -> m) -> Int -> Digit (Elem a) -> m
  foldMapWithIndexDigitE f i t = foldWithIndexDigit (<>) f i t

  foldMapWithIndexDigitN :: Monoid m => (Int -> Node a -> m) -> Int -> Digit (Node a) -> m
  foldMapWithIndexDigitN f i t = foldWithIndexDigit (<>) f i t

  foldMapWithIndexNodeE :: Monoid m => (Int -> Elem a -> m) -> Int -> Node (Elem a) -> m
  foldMapWithIndexNodeE f i t = foldWithIndexNode (<>) f i t

  foldMapWithIndexNodeN :: Monoid m => (Int -> Node a -> m) -> Int -> Node (Node a) -> m
  foldMapWithIndexNodeN f i t = foldWithIndexNode (<>) f i t

#if __GLASGOW_HASKELL__
{-# INLINABLE foldMapWithIndex #-}
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
          liftA3 (Deep n)
               (traverseWithIndexDigitE f s pr)
               (traverseWithIndexTreeN (traverseWithIndexNodeE f) sPspr m)
               (traverseWithIndexDigitE f sPsprm sf)
    where
      !sPspr = s + size pr
      !sPsprm = sPspr + size m

  traverseWithIndexTreeN :: Applicative f => (Int -> Node a -> f b) -> Int -> FingerTree (Node a) -> f (FingerTree b)
  traverseWithIndexTreeN _ !_s EmptyT = pure EmptyT
  traverseWithIndexTreeN f s (Single xs) = Single <$> f s xs
  traverseWithIndexTreeN f s (Deep n pr m sf) =
          liftA3 (Deep n)
               (traverseWithIndexDigitN f s pr)
               (traverseWithIndexTreeN (traverseWithIndexNodeN f) sPspr m)
               (traverseWithIndexDigitN f sPsprm sf)
    where
      !sPspr = s + size pr
      !sPsprm = sPspr + size m

  traverseWithIndexDigitE :: Applicative f => (Int -> Elem a -> f b) -> Int -> Digit (Elem a) -> f (Digit b)
  traverseWithIndexDigitE f i t = traverseWithIndexDigit f i t

  traverseWithIndexDigitN :: Applicative f => (Int -> Node a -> f b) -> Int -> Digit (Node a) -> f (Digit b)
  traverseWithIndexDigitN f i t = traverseWithIndexDigit f i t

  {-# INLINE traverseWithIndexDigit #-}
  traverseWithIndexDigit :: (Applicative f, Sized a) => (Int -> a -> f b) -> Int -> Digit a -> f (Digit b)
  traverseWithIndexDigit f !s (One a) = One <$> f s a
  traverseWithIndexDigit f s (Two a b) = liftA2 Two (f s a) (f sPsa b)
    where
      !sPsa = s + size a
  traverseWithIndexDigit f s (Three a b c) =
                                      liftA3 Three (f s a) (f sPsa b) (f sPsab c)
    where
      !sPsa = s + size a
      !sPsab = sPsa + size b
  traverseWithIndexDigit f s (Four a b c d) =
                          liftA3 Four (f s a) (f sPsa b) (f sPsab c) <*> f sPsabc d
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
  traverseWithIndexNode f !s (Node2 ns a b) = liftA2 (Node2 ns) (f s a) (f sPsa b)
    where
      !sPsa = s + size a
  traverseWithIndexNode f s (Node3 ns a b c) =
                           liftA3 (Node3 ns) (f s a) (f sPsa b) (f sPsab c)
    where
      !sPsa = s + size a
      !sPsab = sPsa + size b


#ifdef __GLASGOW_HASKELL__
{-# INLINABLE [1] traverseWithIndex #-}
#else
{-# INLINE [1] traverseWithIndex #-}
#endif

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


-- | \( O(n) \). Convert a given sequence length and a function representing that
-- sequence into a sequence.
--
-- Calls 'error' if @n < 0@.
--
-- __Note__: This function is partial.
--
-- @since 0.5.6.2
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
#ifdef __GLASGOW_HASKELL__
    lift_elem g = coerce g
#else
    lift_elem g = Elem . g
#endif
    {-# INLINE lift_elem #-}

-- | \( O(n) \). Create a sequence consisting of the elements of an 'Array'.
-- Note that the resulting sequence elements may be evaluated lazily (as on GHC),
-- so you must force the entire structure to be sure that the original array
-- can be garbage-collected.
--
-- @since 0.5.6.2
fromArray :: Ix i => Array i a -> Seq a
#ifdef __GLASGOW_HASKELL__
fromArray a = fromFunction (GHC.Arr.numElements a) (GHC.Arr.unsafeAt a)
 where
  -- The following definition uses an (Ix i) constraint, which is needed for
  -- the other fromArray definition.
  _ = Data.Array.rangeSize (Data.Array.bounds a)
#else
fromArray a = fromList2 (Data.Array.rangeSize (Data.Array.bounds a)) (Data.Array.elems a)
#endif

-- Splitting

-- | \( O(\log(\min(i,n-i))) \). The first @i@ elements of a sequence.
-- If @i@ is negative, @'take' i s@ yields the empty sequence.
-- If the sequence contains fewer than @i@ elements, the whole sequence
-- is returned.
take :: Int -> Seq a -> Seq a
take i xs@(Seq t)
    -- See note on unsigned arithmetic in splitAt
  | fromIntegral i - 1 < (fromIntegral (length xs) - 1 :: Word) =
      Seq (takeTreeE i t)
  | i <= 0 = empty
  | otherwise = xs

takeTreeE :: Int -> FingerTree (Elem a) -> FingerTree (Elem a)
takeTreeE !_i EmptyT = EmptyT
takeTreeE i t@(Single _)
   | i <= 0 = EmptyT
   | otherwise = t
takeTreeE i (Deep s pr m sf)
  | i < spr     = takePrefixE i pr
  | i < spm     = case takeTreeN im m of
            ml :*: xs -> takeMiddleE (im - size ml) spr pr ml xs
  | otherwise   = takeSuffixE (i - spm) s pr m sf
  where
    spr     = size pr
    spm     = spr + size m
    im      = i - spr

takeTreeN :: Int -> FingerTree (Node a) -> StrictPair (FingerTree (Node a)) (Node a)
takeTreeN !_i EmptyT = error "takeTreeN of empty tree"
takeTreeN _i (Single x) = EmptyT :*: x
takeTreeN i (Deep s pr m sf)
  | i < spr     = takePrefixN i pr
  | i < spm     = case takeTreeN im m of
            ml :*: xs -> takeMiddleN (im - size ml) spr pr ml xs
  | otherwise   = takeSuffixN (i - spm) s pr m sf  where
    spr     = size pr
    spm     = spr + size m
    im      = i - spr

takeMiddleN :: Int -> Int
             -> Digit (Node a) -> FingerTree (Node (Node a)) -> Node (Node a)
             -> StrictPair (FingerTree (Node a)) (Node a)
takeMiddleN i spr pr ml (Node2 _ a b)
  | i < sa      = pullR sprml pr ml :*: a
  | otherwise   = Deep sprmla pr ml (One a) :*: b
  where
    sa      = size a
    sprml   = spr + size ml
    sprmla  = sa + sprml
takeMiddleN i spr pr ml (Node3 _ a b c)
  | i < sa      = pullR sprml pr ml :*: a
  | i < sab     = Deep sprmla pr ml (One a) :*: b
  | otherwise   = Deep sprmlab pr ml (Two a b) :*: c
  where
    sa      = size a
    sab     = sa + size b
    sprml   = spr + size ml
    sprmla  = sa + sprml
    sprmlab = sprmla + size b

takeMiddleE :: Int -> Int
             -> Digit (Elem a) -> FingerTree (Node (Elem a)) -> Node (Elem a)
             -> FingerTree (Elem a)
takeMiddleE i spr pr ml (Node2 _ a _)
  | i < 1       = pullR sprml pr ml
  | otherwise   = Deep sprmla pr ml (One a)
  where
    sprml   = spr + size ml
    sprmla  = 1 + sprml
takeMiddleE i spr pr ml (Node3 _ a b _)
  | i < 1       = pullR sprml pr ml
  | i < 2       = Deep sprmla pr ml (One a)
  | otherwise   = Deep sprmlab pr ml (Two a b)
  where
    sprml   = spr + size ml
    sprmla  = 1 + sprml
    sprmlab = sprmla + 1

takePrefixE :: Int -> Digit (Elem a) -> FingerTree (Elem a)
takePrefixE !_i (One _) = EmptyT
takePrefixE i (Two a _)
  | i < 1       = EmptyT
  | otherwise   = Single a
takePrefixE i (Three a b _)
  | i < 1       = EmptyT
  | i < 2       = Single a
  | otherwise   = Deep 2 (One a) EmptyT (One b)
takePrefixE i (Four a b c _)
  | i < 1       = EmptyT
  | i < 2       = Single a
  | i < 3       = Deep 2 (One a) EmptyT (One b)
  | otherwise   = Deep 3 (Two a b) EmptyT (One c)

takePrefixN :: Int -> Digit (Node a)
                    -> StrictPair (FingerTree (Node a)) (Node a)
takePrefixN !_i (One a) = EmptyT :*: a
takePrefixN i (Two a b)
  | i < sa      = EmptyT :*: a
  | otherwise   = Single a :*: b
  where
    sa      = size a
takePrefixN i (Three a b c)
  | i < sa      = EmptyT :*: a
  | i < sab     = Single a :*: b
  | otherwise   = Deep sab (One a) EmptyT (One b) :*: c
  where
    sa      = size a
    sab     = sa + size b
takePrefixN i (Four a b c d)
  | i < sa      = EmptyT :*: a
  | i < sab     = Single a :*: b
  | i < sabc    = Deep sab (One a) EmptyT (One b) :*: c
  | otherwise   = Deep sabc (Two a b) EmptyT (One c) :*: d
  where
    sa      = size a
    sab     = sa + size b
    sabc    = sab + size c

takeSuffixE :: Int -> Int -> Digit (Elem a) -> FingerTree (Node (Elem a)) -> Digit (Elem a) ->
   FingerTree (Elem a)
takeSuffixE !_i !s pr m (One _) = pullR (s - 1) pr m
takeSuffixE i s pr m (Two a _)
  | i < 1      = pullR (s - 2) pr m
  | otherwise  = Deep (s - 1) pr m (One a)
takeSuffixE i s pr m (Three a b _)
  | i < 1      = pullR (s - 3) pr m
  | i < 2      = Deep (s - 2) pr m (One a)
  | otherwise  = Deep (s - 1) pr m (Two a b)
takeSuffixE i s pr m (Four a b c _)
  | i < 1      = pullR (s - 4) pr m
  | i < 2      = Deep (s - 3) pr m (One a)
  | i < 3      = Deep (s - 2) pr m (Two a b)
  | otherwise  = Deep (s - 1) pr m (Three a b c)

takeSuffixN :: Int -> Int -> Digit (Node a) -> FingerTree (Node (Node a)) -> Digit (Node a) ->
   StrictPair (FingerTree (Node a)) (Node a)
takeSuffixN !_i !s pr m (One a) = pullR (s - size a) pr m :*: a
takeSuffixN i s pr m (Two a b)
  | i < sa      = pullR (s - sa - size b) pr m :*: a
  | otherwise   = Deep (s - size b) pr m (One a) :*: b
  where
    sa      = size a
takeSuffixN i s pr m (Three a b c)
  | i < sa      = pullR (s - sab - size c) pr m :*: a
  | i < sab     = Deep (s - size b - size c) pr m (One a) :*: b
  | otherwise   = Deep (s - size c) pr m (Two a b) :*: c
  where
    sa      = size a
    sab     = sa + size b
takeSuffixN i s pr m (Four a b c d)
  | i < sa      = pullR (s - sa - sbcd) pr m :*: a
  | i < sab     = Deep (s - sbcd) pr m (One a) :*: b
  | i < sabc    = Deep (s - scd) pr m (Two a b) :*: c
  | otherwise   = Deep (s - sd) pr m (Three a b c) :*: d
  where
    sa      = size a
    sab     = sa + size b
    sabc    = sab + size c
    sd      = size d
    scd     = size c + sd
    sbcd    = size b + scd

-- | \( O(\log(\min(i,n-i))) \). Elements of a sequence after the first @i@.
-- If @i@ is negative, @'drop' i s@ yields the whole sequence.
-- If the sequence contains fewer than @i@ elements, the empty sequence
-- is returned.
drop            :: Int -> Seq a -> Seq a
drop i xs@(Seq t)
    -- See note on unsigned arithmetic in splitAt
  | fromIntegral i - 1 < (fromIntegral (length xs) - 1 :: Word) =
      Seq (takeTreeER (length xs - i) t)
  | i <= 0 = xs
  | otherwise = empty

-- We implement `drop` using a "take from the rear" strategy.  There's no
-- particular technical reason for this; it just lets us reuse the arithmetic
-- from `take` (which itself reuses the arithmetic from `splitAt`) instead of
-- figuring it out from scratch and ending up with lots of off-by-one errors.
takeTreeER :: Int -> FingerTree (Elem a) -> FingerTree (Elem a)
takeTreeER !_i EmptyT = EmptyT
takeTreeER i t@(Single _)
   | i <= 0 = EmptyT
   | otherwise = t
takeTreeER i (Deep s pr m sf)
  | i < ssf     = takeSuffixER i sf
  | i < ssm     = case takeTreeNR im m of
            xs :*: mr -> takeMiddleER (im - size mr) ssf xs mr sf
  | otherwise   = takePrefixER (i - ssm) s pr m sf
  where
    ssf     = size sf
    ssm     = ssf + size m
    im      = i - ssf

takeTreeNR :: Int -> FingerTree (Node a) -> StrictPair (Node a) (FingerTree (Node a))
takeTreeNR !_i EmptyT = error "takeTreeNR of empty tree"
takeTreeNR _i (Single x) = x :*: EmptyT
takeTreeNR i (Deep s pr m sf)
  | i < ssf     = takeSuffixNR i sf
  | i < ssm     = case takeTreeNR im m of
            xs :*: mr -> takeMiddleNR (im - size mr) ssf xs mr sf
  | otherwise   = takePrefixNR (i - ssm) s pr m sf  where
    ssf     = size sf
    ssm     = ssf + size m
    im      = i - ssf

takeMiddleNR :: Int -> Int
             -> Node (Node a) -> FingerTree (Node (Node a)) -> Digit (Node a)
             -> StrictPair (Node a) (FingerTree (Node a))
takeMiddleNR i ssf (Node2 _ a b) mr sf
  | i < sb      = b :*: pullL ssfmr mr sf
  | otherwise   = a :*: Deep ssfmrb (One b) mr sf
  where
    sb      = size b
    ssfmr   = ssf + size mr
    ssfmrb  = sb + ssfmr
takeMiddleNR i ssf (Node3 _ a b c) mr sf
  | i < sc      = c :*: pullL ssfmr mr sf
  | i < sbc     = b :*: Deep ssfmrc (One c) mr sf
  | otherwise   = a :*: Deep ssfmrbc (Two b c) mr sf
  where
    sc      = size c
    sbc     = sc + size b
    ssfmr   = ssf + size mr
    ssfmrc  = sc + ssfmr
    ssfmrbc = ssfmrc + size b

takeMiddleER :: Int -> Int
             -> Node (Elem a) -> FingerTree (Node (Elem a)) -> Digit (Elem a)
             -> FingerTree (Elem a)
takeMiddleER i ssf (Node2 _ _ b) mr sf
  | i < 1       = pullL ssfmr mr sf
  | otherwise   = Deep ssfmrb (One b) mr sf
  where
    ssfmr   = ssf + size mr
    ssfmrb  = 1 + ssfmr
takeMiddleER i ssf (Node3 _ _ b c) mr sf
  | i < 1       = pullL ssfmr mr sf
  | i < 2       = Deep ssfmrc (One c) mr sf
  | otherwise   = Deep ssfmrbc (Two b c) mr sf
  where
    ssfmr   = ssf + size mr
    ssfmrc  = 1 + ssfmr
    ssfmrbc = ssfmr + 2

takeSuffixER :: Int -> Digit (Elem a) -> FingerTree (Elem a)
takeSuffixER !_i (One _) = EmptyT
takeSuffixER i (Two _ b)
  | i < 1       = EmptyT
  | otherwise   = Single b
takeSuffixER i (Three _ b c)
  | i < 1       = EmptyT
  | i < 2       = Single c
  | otherwise   = Deep 2 (One b) EmptyT (One c)
takeSuffixER i (Four _ b c d)
  | i < 1       = EmptyT
  | i < 2       = Single d
  | i < 3       = Deep 2 (One c) EmptyT (One d)
  | otherwise   = Deep 3 (Two b c) EmptyT (One d)

takeSuffixNR :: Int -> Digit (Node a)
                    -> StrictPair (Node a) (FingerTree (Node a))
takeSuffixNR !_i (One a) = a :*: EmptyT
takeSuffixNR i (Two a b)
  | i < sb      = b :*: EmptyT
  | otherwise   = a :*: Single b
  where
    sb      = size b
takeSuffixNR i (Three a b c)
  | i < sc      = c :*: EmptyT
  | i < sbc     = b :*: Single c
  | otherwise   = a :*: Deep sbc (One b) EmptyT (One c)
  where
    sc      = size c
    sbc     = sc + size b
takeSuffixNR i (Four a b c d)
  | i < sd      = d :*: EmptyT
  | i < scd     = c :*: Single d
  | i < sbcd    = b :*: Deep scd (One c) EmptyT (One d)
  | otherwise   = a :*: Deep sbcd (Two b c) EmptyT (One d)
  where
    sd      = size d
    scd     = sd + size c
    sbcd    = scd + size b

takePrefixER :: Int -> Int -> Digit (Elem a) -> FingerTree (Node (Elem a)) -> Digit (Elem a) ->
   FingerTree (Elem a)
takePrefixER !_i !s (One _) m sf = pullL (s - 1) m sf
takePrefixER i s (Two _ b) m sf
  | i < 1      = pullL (s - 2) m sf
  | otherwise  = Deep (s - 1) (One b) m sf
takePrefixER i s (Three _ b c) m sf
  | i < 1      = pullL (s - 3) m sf
  | i < 2      = Deep (s - 2) (One c) m sf
  | otherwise  = Deep (s - 1) (Two b c) m sf
takePrefixER i s (Four _ b c d) m sf
  | i < 1      = pullL (s - 4) m sf
  | i < 2      = Deep (s - 3) (One d) m sf
  | i < 3      = Deep (s - 2) (Two c d) m sf
  | otherwise  = Deep (s - 1) (Three b c d) m sf

takePrefixNR :: Int -> Int -> Digit (Node a) -> FingerTree (Node (Node a)) -> Digit (Node a) ->
   StrictPair (Node a) (FingerTree (Node a))
takePrefixNR !_i !s (One a) m sf = a :*: pullL (s - size a) m sf
takePrefixNR i s (Two a b) m sf
  | i < sb      = b :*: pullL (s - sb - size a) m sf
  | otherwise   = a :*: Deep (s - size a) (One b) m sf
  where
    sb      = size b
takePrefixNR i s (Three a b c) m sf
  | i < sc      = c :*: pullL (s - sbc - size a) m sf
  | i < sbc     = b :*: Deep (s - size b - size a) (One c) m sf
  | otherwise   = a :*: Deep (s - size a) (Two b c) m sf
  where
    sc      = size c
    sbc     = sc + size b
takePrefixNR i s (Four a b c d) m sf
  | i < sd      = d :*: pullL (s - sd - sabc) m sf
  | i < scd     = c :*: Deep (s - sabc) (One d) m sf
  | i < sbcd    = b :*: Deep (s - sab) (Two c d) m sf
  | otherwise   = a :*: Deep (s - sa) (Three b c d) m sf
  where
    sa      = size a
    sab     = sa + size b
    sabc    = sab + size c
    sd      = size d
    scd     = size c + sd
    sbcd    = size b + scd

-- | \( O(\log(\min(i,n-i))) \). Split a sequence at a given position.
-- @'splitAt' i s = ('take' i s, 'drop' i s)@.
splitAt                  :: Int -> Seq a -> (Seq a, Seq a)
splitAt i xs@(Seq t)
  -- We use an unsigned comparison to make the common case
  -- faster. This only works because our representation of
  -- sizes as (signed) Ints gives us a free high bit to play
  -- with. Note also that there's no sharing to lose in the
  -- case that the length is 0.
  | fromIntegral i - 1 < (fromIntegral (length xs) - 1 :: Word) =
      case splitTreeE i t of
        l :*: r -> (Seq l, Seq r)
  | i <= 0 = (empty, xs)
  | otherwise = (xs, empty)

-- | \( O(\log(\min(i,n-i))) \) A version of 'splitAt' that does not attempt to
-- enhance sharing when the split point is less than or equal to 0, and that
-- gives completely wrong answers when the split point is at least the length
-- of the sequence, unless the sequence is a singleton. This is used to
-- implement zipWith and chunksOf, which are extremely sensitive to the cost of
-- splitting very short sequences. There is just enough of a speed increase to
-- make this worth the trouble.
uncheckedSplitAt :: Int -> Seq a -> (Seq a, Seq a)
uncheckedSplitAt i (Seq xs) = case splitTreeE i xs of
  l :*: r -> (Seq l, Seq r)

data Split a = Split !(FingerTree (Node a)) !(Node a) !(FingerTree (Node a))
#ifdef TESTING
    deriving Show
#endif

splitTreeE :: Int -> FingerTree (Elem a) -> StrictPair (FingerTree (Elem a)) (FingerTree (Elem a))
splitTreeE !_i EmptyT = EmptyT :*: EmptyT
splitTreeE i t@(Single _)
   | i <= 0 = EmptyT :*: t
   | otherwise = t :*: EmptyT
splitTreeE i (Deep s pr m sf)
  | i < spr     = splitPrefixE i s pr m sf
  | i < spm     = case splitTreeN im m of
            Split ml xs mr -> splitMiddleE (im - size ml) s spr pr ml xs mr sf
  | otherwise   = splitSuffixE (i - spm) s pr m sf
  where
    spr     = size pr
    spm     = spr + size m
    im      = i - spr

splitTreeN :: Int -> FingerTree (Node a) -> Split a
splitTreeN !_i EmptyT = error "splitTreeN of empty tree"
splitTreeN _i (Single x) = Split EmptyT x EmptyT
splitTreeN i (Deep s pr m sf)
  | i < spr     = splitPrefixN i s pr m sf
  | i < spm     = case splitTreeN im m of
            Split ml xs mr -> splitMiddleN (im - size ml) s spr pr ml xs mr sf
  | otherwise   = splitSuffixN (i - spm) s pr m sf  where
    spr     = size pr
    spm     = spr + size m
    im      = i - spr

splitMiddleN :: Int -> Int -> Int
             -> Digit (Node a) -> FingerTree (Node (Node a)) -> Node (Node a) -> FingerTree (Node (Node a)) -> Digit (Node a)
             -> Split a
splitMiddleN i s spr pr ml (Node2 _ a b) mr sf
  | i < sa      = Split (pullR sprml pr ml) a (Deep (s - sprmla) (One b) mr sf)
  | otherwise   = Split (Deep sprmla pr ml (One a)) b (pullL (s - sprmla - size b) mr sf)
  where
    sa      = size a
    sprml   = spr + size ml
    sprmla  = sa + sprml
splitMiddleN i s spr pr ml (Node3 _ a b c) mr sf
  | i < sa      = Split (pullR sprml pr ml) a (Deep (s - sprmla) (Two b c) mr sf)
  | i < sab     = Split (Deep sprmla pr ml (One a)) b (Deep (s - sprmlab) (One c) mr sf)
  | otherwise   = Split (Deep sprmlab pr ml (Two a b)) c (pullL (s - sprmlab - size c) mr sf)
  where
    sa      = size a
    sab     = sa + size b
    sprml   = spr + size ml
    sprmla  = sa + sprml
    sprmlab = sprmla + size b

splitMiddleE :: Int -> Int -> Int
             -> Digit (Elem a) -> FingerTree (Node (Elem a)) -> Node (Elem a) -> FingerTree (Node (Elem a)) -> Digit (Elem a)
             -> StrictPair (FingerTree (Elem a)) (FingerTree (Elem a))
splitMiddleE i s spr pr ml (Node2 _ a b) mr sf
  | i < 1       = pullR sprml pr ml :*: Deep (s - sprml) (Two a b) mr sf
  | otherwise   = Deep sprmla pr ml (One a) :*: Deep (s - sprmla) (One b) mr sf
  where
    sprml   = spr + size ml
    sprmla  = 1 + sprml
splitMiddleE i s spr pr ml (Node3 _ a b c) mr sf = case i of
  0 -> pullR sprml pr ml :*: Deep (s - sprml) (Three a b c) mr sf
  1 -> Deep sprmla pr ml (One a) :*: Deep (s - sprmla) (Two b c) mr sf
  _ -> Deep sprmlab pr ml (Two a b) :*: Deep (s - sprmlab) (One c) mr sf
  where
    sprml   = spr + size ml
    sprmla  = 1 + sprml
    sprmlab = sprmla + 1

splitPrefixE :: Int -> Int -> Digit (Elem a) -> FingerTree (Node (Elem a)) -> Digit (Elem a) ->
                    StrictPair (FingerTree (Elem a)) (FingerTree (Elem a))
splitPrefixE !_i !s (One a) m sf = EmptyT :*: Deep s (One a) m sf
splitPrefixE i s (Two a b) m sf = case i of
  0 -> EmptyT :*: Deep s (Two a b) m sf
  _ -> Single a :*: Deep (s - 1) (One b) m sf
splitPrefixE i s (Three a b c) m sf = case i of
  0 -> EmptyT :*: Deep s (Three a b c) m sf
  1 -> Single a :*: Deep (s - 1) (Two b c) m sf
  _ -> Deep 2 (One a) EmptyT (One b) :*: Deep (s - 2) (One c) m sf
splitPrefixE i s (Four a b c d) m sf = case i of
  0 -> EmptyT :*: Deep s (Four a b c d) m sf
  1 -> Single a :*: Deep (s - 1) (Three b c d) m sf
  2 -> Deep 2 (One a) EmptyT (One b) :*: Deep (s - 2) (Two c d) m sf
  _ -> Deep 3 (Two a b) EmptyT (One c) :*: Deep (s - 3) (One d) m sf

splitPrefixN :: Int -> Int -> Digit (Node a) -> FingerTree (Node (Node a)) -> Digit (Node a) ->
                    Split a
splitPrefixN !_i !s (One a) m sf = Split EmptyT a (pullL (s - size a) m sf)
splitPrefixN i s (Two a b) m sf
  | i < sa      = Split EmptyT a (Deep (s - sa) (One b) m sf)
  | otherwise   = Split (Single a) b (pullL (s - sa - size b) m sf)
  where
    sa      = size a
splitPrefixN i s (Three a b c) m sf
  | i < sa      = Split EmptyT a (Deep (s - sa) (Two b c) m sf)
  | i < sab     = Split (Single a) b (Deep (s - sab) (One c) m sf)
  | otherwise   = Split (Deep sab (One a) EmptyT (One b)) c (pullL (s - sab - size c) m sf)
  where
    sa      = size a
    sab     = sa + size b
splitPrefixN i s (Four a b c d) m sf
  | i < sa      = Split EmptyT a $ Deep (s - sa) (Three b c d) m sf
  | i < sab     = Split (Single a) b $ Deep (s - sab) (Two c d) m sf
  | i < sabc    = Split (Deep sab (One a) EmptyT (One b)) c $ Deep (s - sabc) (One d) m sf
  | otherwise   = Split (Deep sabc (Two a b) EmptyT (One c)) d $ pullL (s - sabc - size d) m sf
  where
    sa      = size a
    sab     = sa + size b
    sabc    = sab + size c

splitSuffixE :: Int -> Int -> Digit (Elem a) -> FingerTree (Node (Elem a)) -> Digit (Elem a) ->
   StrictPair (FingerTree (Elem a)) (FingerTree (Elem a))
splitSuffixE !_i !s pr m (One a) = pullR (s - 1) pr m :*: Single a
splitSuffixE i s pr m (Two a b) = case i of
  0 -> pullR (s - 2) pr m :*: Deep 2 (One a) EmptyT (One b)
  _ -> Deep (s - 1) pr m (One a) :*: Single b
splitSuffixE i s pr m (Three a b c) = case i of
  0 -> pullR (s - 3) pr m :*: Deep 3 (Two a b) EmptyT (One c)
  1 -> Deep (s - 2) pr m (One a) :*: Deep 2 (One b) EmptyT (One c)
  _ -> Deep (s - 1) pr m (Two a b) :*: Single c
splitSuffixE i s pr m (Four a b c d) = case i of
  0 -> pullR (s - 4) pr m :*: Deep 4 (Two a b) EmptyT (Two c d)
  1 -> Deep (s - 3) pr m (One a) :*: Deep 3 (Two b c) EmptyT (One d)
  2 -> Deep (s - 2) pr m (Two a b) :*: Deep 2 (One c) EmptyT (One d)
  _ -> Deep (s - 1) pr m (Three a b c) :*: Single d

splitSuffixN :: Int -> Int -> Digit (Node a) -> FingerTree (Node (Node a)) -> Digit (Node a) ->
   Split a
splitSuffixN !_i !s pr m (One a) = Split (pullR (s - size a) pr m) a EmptyT
splitSuffixN i s pr m (Two a b)
  | i < sa      = Split (pullR (s - sa - size b) pr m) a (Single b)
  | otherwise   = Split (Deep (s - size b) pr m (One a)) b EmptyT
  where
    sa      = size a
splitSuffixN i s pr m (Three a b c)
  | i < sa      = Split (pullR (s - sab - size c) pr m) a (deep (One b) EmptyT (One c))
  | i < sab     = Split (Deep (s - size b - size c) pr m (One a)) b (Single c)
  | otherwise   = Split (Deep (s - size c) pr m (Two a b)) c EmptyT
  where
    sa      = size a
    sab     = sa + size b
splitSuffixN i s pr m (Four a b c d)
  | i < sa      = Split (pullR (s - sa - sbcd) pr m) a (Deep sbcd (Two b c) EmptyT (One d))
  | i < sab     = Split (Deep (s - sbcd) pr m (One a)) b (Deep scd (One c) EmptyT (One d))
  | i < sabc    = Split (Deep (s - scd) pr m (Two a b)) c (Single d)
  | otherwise   = Split (Deep (s - sd) pr m (Three a b c)) d EmptyT
  where
    sa      = size a
    sab     = sa + size b
    sabc    = sab + size c
    sd      = size d
    scd     = size c + sd
    sbcd    = size b + scd

-- | \(O \Bigl(\bigl(\frac{n}{c}\bigr) \log c\Bigr)\). @chunksOf c xs@ splits @xs@ into chunks of size @c>0@.
-- If @c@ does not divide the length of @xs@ evenly, then the last element
-- of the result will be short.
--
-- Side note: the given performance bound is missing some messy terms that only
-- really affect edge cases. Performance degrades smoothly from \( O(1) \) (for
-- \( c = n \)) to \( O(n) \) (for \( c = 1 \)). The true bound is more like
-- \( O \Bigl( \bigl(\frac{n}{c} - 1\bigr) (\log (c + 1)) + 1 \Bigr) \)
--
-- Calls 'error' if @n <= 0@ and @not (null xs)@.
--
-- __Note__: This function is partial.
--
-- @since 0.5.8
chunksOf :: Int -> Seq a -> Seq (Seq a)
chunksOf n xs | n <= 0 =
  if null xs
    then empty
    else error "chunksOf: A non-empty sequence can only be broken up into positively-sized chunks."
chunksOf 1 s = fmap singleton s
chunksOf n s = splitMap (uncheckedSplitAt . (*n)) const most (replicate numReps ())
                 >< if null end then empty else singleton end
  where
    (numReps, endLength) = length s `quotRem` n
    (most, end) = splitAt (length s - endLength) s

-- | \( O(n) \).  Returns a sequence of all suffixes of this sequence,
-- longest first.  For example,
--
-- > tails (fromList "abc") = fromList [fromList "abc", fromList "bc", fromList "c", fromList ""]
--
-- Evaluating the \( i \)th suffix takes \( O(\log(\min(i, n-i))) \), but evaluating
-- every suffix in the sequence takes \( O(n) \) due to sharing.
tails                   :: Seq a -> Seq (Seq a)
tails (Seq xs)          = Seq (tailsTree (Elem . Seq) xs) |> empty

-- | \( O(n) \).  Returns a sequence of all prefixes of this sequence,
-- shortest first.  For example,
--
-- > inits (fromList "abc") = fromList [fromList "", fromList "a", fromList "ab", fromList "abc"]
--
-- Evaluating the \( i \)th prefix takes \( O(\log(\min(i, n-i))) \), but evaluating
-- every prefix in the sequence takes \( O(n) \) due to sharing.
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
    f' ms = let ConsLTree node m' = viewLTree ms in
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
    f' ms =  let SnocRTree m' node = viewRTree ms in
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

-- | \( O(i) \) where \( i \) is the prefix length. 'takeWhileL', applied
-- to a predicate @p@ and a sequence @xs@, returns the longest prefix
-- (possibly empty) of @xs@ of elements that satisfy @p@.
takeWhileL :: (a -> Bool) -> Seq a -> Seq a
takeWhileL p = fst . spanl p

-- | \( O(i) \) where \( i \) is the suffix length.  'takeWhileR', applied
-- to a predicate @p@ and a sequence @xs@, returns the longest suffix
-- (possibly empty) of @xs@ of elements that satisfy @p@.
--
-- @'takeWhileR' p xs@ is equivalent to @'reverse' ('takeWhileL' p ('reverse' xs))@.
takeWhileR :: (a -> Bool) -> Seq a -> Seq a
takeWhileR p = fst . spanr p

-- | \( O(i) \) where \( i \) is the prefix length.  @'dropWhileL' p xs@ returns
-- the suffix remaining after @'takeWhileL' p xs@.
dropWhileL :: (a -> Bool) -> Seq a -> Seq a
dropWhileL p = snd . spanl p

-- | \( O(i) \) where \( i \) is the suffix length.  @'dropWhileR' p xs@ returns
-- the prefix remaining after @'takeWhileR' p xs@.
--
-- @'dropWhileR' p xs@ is equivalent to @'reverse' ('dropWhileL' p ('reverse' xs))@.
dropWhileR :: (a -> Bool) -> Seq a -> Seq a
dropWhileR p = snd . spanr p

-- | \( O(i) \) where \( i \) is the prefix length.  'spanl', applied to
-- a predicate @p@ and a sequence @xs@, returns a pair whose first
-- element is the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@ and the second element is the remainder of the sequence.
spanl :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
spanl p = breakl (not . p)

-- | \( O(i) \) where \( i \) is the suffix length.  'spanr', applied to a
-- predicate @p@ and a sequence @xs@, returns a pair whose /first/ element
-- is the longest /suffix/ (possibly empty) of @xs@ of elements that
-- satisfy @p@ and the second element is the remainder of the sequence.
spanr :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
spanr p = breakr (not . p)

{-# INLINE breakl #-}
-- | \( O(i) \) where \( i \) is the breakpoint index.  'breakl', applied to a
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

-- | \( O(n) \).  The 'partition' function takes a predicate @p@ and a
-- sequence @xs@ and returns sequences of those elements which do and
-- do not satisfy the predicate.
partition :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
partition p = toPair . foldl' part (empty :*: empty)
  where
    part (xs :*: ys) x
      | p x         = (xs `snoc'` x) :*: ys
      | otherwise   = xs :*: (ys `snoc'` x)

-- | \( O(n) \).  The 'filter' function takes a predicate @p@ and a sequence
-- @xs@ and returns a sequence of those elements which satisfy the
-- predicate.
filter :: (a -> Bool) -> Seq a -> Seq a
filter p = foldl' (\ xs x -> if p x then xs `snoc'` x else xs) empty

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

-- The implementation below is based on an idea by Ross Paterson and
-- implemented by Lennart Spitzner. It avoids the rebuilding the original
-- (|>)-based implementation suffered from. It also avoids the excessive pair
-- allocations Paterson's implementation suffered from.
--
-- David Feuer suggested building in nine-element chunks, which reduces
-- intermediate conses from around (1/2)*n to around (1/8)*n with a concomitant
-- improvement in benchmark constant factors. In fact, it should be even
-- better to work in chunks of 27 `Elem`s and chunks of three `Node`s, rather
-- than nine of each, but it seems hard to avoid a code explosion with
-- such large chunks.
--
-- Paterson's code can be seen, for example, in
-- https://github.com/haskell/containers/blob/74034b3244fa4817c7bef1202e639b887a975d9e/Data/Sequence.hs#L3532
--
-- Given a list
--
-- [1..302]
--
-- the original code forms Three 1 2 3 | [node3 4 5 6, node3 7 8 9, node3 10 11
-- 12, ...] | Two 301 302
--
-- Then it recurses on the middle list. The middle lists become successively
-- shorter as their elements become successively deeper nodes.
--
-- The original implementation of the list shortener, getNodes, included the
-- recursive step

--     getNodes s x1 (x2:x3:x4:xs) = (Node3 s x1 x2 x3:ns, d)
--            where (ns, d) = getNodes s x4 xs

-- This allocates a cons and a lazy pair at each 3-element step. It relies on
-- the Haskell implementation using Wadler's technique, described in "Fixing
-- some space leaks with a garbage collector"
-- http://homepages.inf.ed.ac.uk/wadler/papers/leak/leak.ps.gz, to repeatedly
-- simplify the `d` thunk. Although GHC uses this GC trick, heap profiling at
-- least appears to indicate that the pair constructors and conses build up
-- with this implementation.
--
-- Spitzner's implementation uses a similar approach, but replaces the middle
-- list, in each level, with a customized stream type that finishes off with
-- the final digit in that level and (since it works in nines) in the one
-- above. To work around the nested tree structure, the overall computation is
-- structured using continuation-passing style, with a function that, at the
-- bottom of the tree, deals with a stream that terminates in a nested-pair
-- representation of the entire right side of the tree. Perhaps someone will
-- eventually find a less mind-bending way to accomplish this.

-- | \( O(n) \). Create a sequence from a finite list of elements.
-- There is a function 'toList' in the opposite direction for all
-- instances of the 'Foldable' class, including 'Seq'.
fromList        :: [a] -> Seq a
-- Note: we can avoid map_elem if we wish by scattering
-- Elem applications throughout mkTreeE and getNodesE, but
-- it gets a bit hard to read.
fromList = Seq . mkTree . map_elem
  where
#if defined(__GLASGOW_HASKELL__) || defined(__MHS__)
    mkTree :: forall a' . [Elem a'] -> FingerTree (Elem a')
#else
    mkTree :: [Elem a] -> FingerTree (Elem a)
#endif
    mkTree [] = EmptyT
    mkTree [x1] = Single x1
    mkTree [x1, x2] = Deep 2 (One x1) EmptyT (One x2)
    mkTree [x1, x2, x3] = Deep 3 (Two x1 x2) EmptyT (One x3)
    mkTree [x1, x2, x3, x4] = Deep 4 (Two x1 x2) EmptyT (Two x3 x4)
    mkTree [x1, x2, x3, x4, x5] = Deep 5 (Three x1 x2 x3) EmptyT (Two x4 x5)
    mkTree [x1, x2, x3, x4, x5, x6] =
      Deep 6 (Three x1 x2 x3) EmptyT (Three x4 x5 x6)
    mkTree [x1, x2, x3, x4, x5, x6, x7] =
      Deep 7 (Two x1 x2) (Single (Node3 3 x3 x4 x5)) (Two x6 x7)
    mkTree [x1, x2, x3, x4, x5, x6, x7, x8] =
      Deep 8 (Three x1 x2 x3) (Single (Node3 3 x4 x5 x6)) (Two x7 x8)
    mkTree [x1, x2, x3, x4, x5, x6, x7, x8, x9] =
      Deep 9 (Three x1 x2 x3) (Single (Node3 3 x4 x5 x6)) (Three x7 x8 x9)
    mkTree [x1, x2, x3, x4, x5, x6, x7, x8, y0, y1] =
      Deep 10 (Two x1 x2)
              (Deep 6 (One (Node3 3 x3 x4 x5)) EmptyT (One (Node3 3 x6 x7 x8)))
              (Two y0 y1)
    mkTree [x1, x2, x3, x4, x5, x6, x7, x8, x9, y0, y1] =
      Deep 11 (Three x1 x2 x3)
              (Deep 6 (One (Node3 3 x4 x5 x6)) EmptyT (One (Node3 3 x7 x8 x9)))
              (Two y0 y1)
    mkTree [x1, x2, x3, x4, x5, x6, x7, x8, x9, y0, y1, y2] =
      Deep 12 (Three x1 x2 x3)
              (Deep 6 (One (Node3 3 x4 x5 x6)) EmptyT (One (Node3 3 x7 x8 x9)))
              (Three y0 y1 y2)
    mkTree [x1, x2, x3, x4, x5, x6, x7, x8, y0, y1, y2, y3, y4] =
      Deep 13 (Two x1 x2)
              (Deep 9 (Two (Node3 3 x3 x4 x5) (Node3 3 x6 x7 x8)) EmptyT (One (Node3 3 y0 y1 y2)))
              (Two y3 y4)
    mkTree [x1, x2, x3, x4, x5, x6, x7, x8, x9, y0, y1, y2, y3, y4] =
      Deep 14 (Three x1 x2 x3)
              (Deep 9 (Two (Node3 3 x4 x5 x6) (Node3 3 x7 x8 x9)) EmptyT (One (Node3 3 y0 y1 y2)))
              (Two y3 y4)
    mkTree [x1, x2, x3, x4, x5, x6, x7, x8, x9, y0, y1, y2, y3, y4, y5] =
      Deep 15 (Three x1 x2 x3)
              (Deep 9 (Two (Node3 3 x4 x5 x6) (Node3 3 x7 x8 x9)) EmptyT (One (Node3 3 y0 y1 y2)))
              (Three y3 y4 y5)
    mkTree (x1:x2:x3:x4:x5:x6:x7:x8:x9:y0:y1:y2:y3:y4:y5:y6:xs) =
        mkTreeC cont 9 (getNodes 3 (Node3 3 y3 y4 y5) y6 xs)
      where
        d2 = Three x1 x2 x3
        d1 = Three (Node3 3 x4 x5 x6) (Node3 3 x7 x8 x9) (Node3 3 y0 y1 y2)
#if defined(__GLASGOW_HASKELL__) || defined(__MHS__)
        cont :: (Digit (Node (Elem a')), Digit (Elem a')) -> FingerTree (Node (Node (Elem a'))) -> FingerTree (Elem a')
#endif
        cont (!r1, !r2) !sub =
          let !sub1 = Deep (9 + size r1 + size sub) d1 sub r1
          in Deep (3 + size r2 + size sub1) d2 sub1 r2

    getNodes :: forall a . Int
             -> Node a
             -> a
             -> [a]
             -> ListFinal (Node (Node a)) (Digit (Node a), Digit a)
    getNodes !_ n1 x1 [] = LFinal (One n1, One x1)
    getNodes _ n1 x1 [x2] = LFinal (One n1, Two x1 x2)
    getNodes _ n1 x1 [x2, x3] = LFinal (One n1, Three x1 x2 x3)
    getNodes s n1 x1 [x2, x3, x4] = LFinal (Two n1 (Node3 s x1 x2 x3), One x4)
    getNodes s n1 x1 [x2, x3, x4, x5] = LFinal (Two n1 (Node3 s x1 x2 x3), Two x4 x5)
    getNodes s n1 x1 [x2, x3, x4, x5, x6] = LFinal (Two n1 (Node3 s x1 x2 x3), Three x4 x5 x6)
    getNodes s n1 x1 [x2, x3, x4, x5, x6, x7] = LFinal (Three n1 (Node3 s x1 x2 x3) (Node3 s x4 x5 x6), One x7)
    getNodes s n1 x1 [x2, x3, x4, x5, x6, x7, x8] = LFinal (Three n1 (Node3 s x1 x2 x3) (Node3 s x4 x5 x6), Two x7 x8)
    getNodes s n1 x1 [x2, x3, x4, x5, x6, x7, x8, x9] = LFinal (Three n1 (Node3 s x1 x2 x3) (Node3 s x4 x5 x6), Three x7 x8 x9)
    getNodes s n1 x1 (x2:x3:x4:x5:x6:x7:x8:x9:x10:xs) = LCons n10 (getNodes s (Node3 s x7 x8 x9) x10 xs)
      where !n2 = Node3 s x1 x2 x3
            !n3 = Node3 s x4 x5 x6
            !n10 = Node3 (3*s) n1 n2 n3

    mkTreeC ::
#if defined(__GLASGOW_HASKELL__) || defined(__MHS__)
               forall a b c .
#endif
               (b -> FingerTree (Node a) -> c)
            -> Int
            -> ListFinal (Node a) b
            -> c
    mkTreeC cont !_ (LFinal b) =
      cont b EmptyT
    mkTreeC cont _ (LCons x1 (LFinal b)) =
      cont b (Single x1)
    mkTreeC cont s (LCons x1 (LCons x2 (LFinal b))) =
      cont b (Deep (2*s) (One x1) EmptyT (One x2))
    mkTreeC cont s (LCons x1 (LCons x2 (LCons x3 (LFinal b)))) =
      cont b (Deep (3*s) (Two x1 x2) EmptyT (One x3))
    mkTreeC cont s (LCons x1 (LCons x2 (LCons x3 (LCons x4 (LFinal b))))) =
      cont b (Deep (4*s) (Two x1 x2) EmptyT (Two x3 x4))
    mkTreeC cont s (LCons x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LFinal b)))))) =
      cont b (Deep (5*s) (Three x1 x2 x3) EmptyT (Two x4 x5))
    mkTreeC cont s (LCons x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LCons x6 (LFinal b))))))) =
      cont b (Deep (6*s) (Three x1 x2 x3) EmptyT (Three x4 x5 x6))
    mkTreeC cont s (LCons x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LCons x6 (LCons x7 (LFinal b)))))))) =
      cont b (Deep (7*s) (Two x1 x2) (Single (Node3 (3*s) x3 x4 x5)) (Two x6 x7))
    mkTreeC cont s (LCons x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LCons x6 (LCons x7 (LCons x8 (LFinal b))))))))) =
      cont b (Deep (8*s) (Three x1 x2 x3) (Single (Node3 (3*s) x4 x5 x6)) (Two x7 x8))
    mkTreeC cont s (LCons x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LCons x6 (LCons x7 (LCons x8 (LCons x9 (LFinal b)))))))))) =
      cont b (Deep (9*s) (Three x1 x2 x3) (Single (Node3 (3*s) x4 x5 x6)) (Three x7 x8 x9))
    mkTreeC cont s (LCons x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LCons x6 (LCons x7 (LCons x8 (LCons y0 (LCons y1 (LFinal b))))))))))) =
      cont b (Deep (10*s) (Two x1 x2) (Deep (6*s) (One (Node3 (3*s) x3 x4 x5)) EmptyT (One (Node3 (3*s) x6 x7 x8))) (Two y0 y1))
    mkTreeC cont s (LCons x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LCons x6 (LCons x7 (LCons x8 (LCons x9 (LCons y0 (LCons y1 (LFinal b)))))))))))) =
      cont b (Deep (11*s) (Three x1 x2 x3) (Deep (6*s) (One (Node3 (3*s) x4 x5 x6)) EmptyT (One (Node3 (3*s) x7 x8 x9))) (Two y0 y1))
    mkTreeC cont s (LCons x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LCons x6 (LCons x7 (LCons x8 (LCons x9 (LCons y0 (LCons y1 (LCons y2 (LFinal b))))))))))))) =
      cont b (Deep (12*s) (Three x1 x2 x3) (Deep (6*s) (One (Node3 (3*s) x4 x5 x6)) EmptyT (One (Node3 (3*s) x7 x8 x9))) (Three y0 y1 y2))
    mkTreeC cont s (LCons x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LCons x6 (LCons x7 (LCons x8 (LCons y0 (LCons y1 (LCons y2 (LCons y3 (LCons y4 (LFinal b)))))))))))))) =
      cont b (Deep (13*s) (Two x1 x2) (Deep (9*s) (Two (Node3 (3*s) x3 x4 x5) (Node3 (3*s) x6 x7 x8)) EmptyT (One (Node3 (3*s) y0 y1 y2))) (Two y3 y4))
    mkTreeC cont s (LCons x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LCons x6 (LCons x7 (LCons x8 (LCons x9 (LCons y0 (LCons y1 (LCons y2 (LCons y3 (LCons y4 (LFinal b))))))))))))))) =
      cont b (Deep (14*s) (Three x1 x2 x3) (Deep (9*s) (Two (Node3 (3*s) x4 x5 x6) (Node3 (3*s) x7 x8 x9)) EmptyT (One (Node3 (3*s) y0 y1 y2))) (Two y3 y4))
    mkTreeC cont s (LCons x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LCons x6 (LCons x7 (LCons x8 (LCons x9 (LCons y0 (LCons y1 (LCons y2 (LCons y3 (LCons y4 (LCons y5 (LFinal b)))))))))))))))) =
      cont b (Deep (15*s) (Three x1 x2 x3) (Deep (9*s) (Two (Node3 (3*s) x4 x5 x6) (Node3 (3*s) x7 x8 x9)) EmptyT (One (Node3 (3*s) y0 y1 y2))) (Three y3 y4 y5))
    mkTreeC cont s (LCons x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LCons x6 (LCons x7 (LCons x8 (LCons x9 (LCons y0 (LCons y1 (LCons y2 (LCons y3 (LCons y4 (LCons y5 (LCons y6 xs)))))))))))))))) =
      mkTreeC cont2 (9*s) (getNodesC (3*s) (Node3 (3*s) y3 y4 y5) y6 xs)
      where
#if defined(__GLASGOW_HASKELL__) || defined(__MHS__)
        cont2 :: (b, Digit (Node (Node a)), Digit (Node a)) -> FingerTree (Node (Node (Node a))) -> c
#endif
        cont2 (b, r1, r2) !sub =
          let d2 = Three x1 x2 x3
              d1 = Three (Node3 (3*s) x4 x5 x6) (Node3 (3*s) x7 x8 x9) (Node3 (3*s) y0 y1 y2)
              !sub1 = Deep (9*s + size r1 + size sub) d1 sub r1
          in cont b $! Deep (3*s + size r2 + size sub1) d2 sub1 r2

    getNodesC :: Int
              -> Node a
              -> a
              -> ListFinal a b
              -> ListFinal (Node (Node a)) (b, Digit (Node a), Digit a)
    getNodesC !_ n1 x1 (LFinal b) = LFinal $ (b, One n1, One x1)
    getNodesC _  n1  x1 (LCons x2 (LFinal b)) = LFinal $ (b, One n1, Two x1 x2)
    getNodesC _  n1  x1 (LCons x2 (LCons x3 (LFinal b))) = LFinal $ (b, One n1, Three x1 x2 x3)
    getNodesC s  n1  x1 (LCons x2 (LCons x3 (LCons x4 (LFinal b)))) =
      let !n2 = Node3 s x1 x2 x3
      in LFinal $ (b, Two n1 n2, One x4)
    getNodesC s  n1  x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LFinal b))))) =
      let !n2 = Node3 s x1 x2 x3
      in LFinal $ (b, Two n1 n2, Two x4 x5)
    getNodesC s  n1  x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LCons x6 (LFinal b)))))) =
      let !n2 = Node3 s x1 x2 x3
      in LFinal $ (b, Two n1 n2, Three x4 x5 x6)
    getNodesC s  n1  x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LCons x6 (LCons x7 (LFinal b))))))) =
      let !n2 = Node3 s x1 x2 x3
          !n3 = Node3 s x4 x5 x6
      in LFinal $ (b, Three n1 n2 n3, One x7)
    getNodesC s  n1  x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LCons x6 (LCons x7 (LCons x8 (LFinal b)))))))) =
      let !n2 = Node3 s x1 x2 x3
          !n3 = Node3 s x4 x5 x6
      in LFinal $ (b, Three n1 n2 n3, Two x7 x8)
    getNodesC s  n1  x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LCons x6 (LCons x7 (LCons x8 (LCons x9 (LFinal b))))))))) =
      let !n2 = Node3 s x1 x2 x3
          !n3 = Node3 s x4 x5 x6
      in LFinal $ (b, Three n1 n2 n3, Three x7 x8 x9)
    getNodesC s  n1  x1 (LCons x2 (LCons x3 (LCons x4 (LCons x5 (LCons x6 (LCons x7 (LCons x8 (LCons x9 (LCons x10 xs))))))))) =
        LCons n10 $ getNodesC s (Node3 s x7 x8 x9) x10 xs
      where !n2 = Node3 s x1 x2 x3
            !n3 = Node3 s x4 x5 x6
            !n10 = Node3 (3*s) n1 n2 n3

    map_elem :: [a] -> [Elem a]
#ifdef __GLASGOW_HASKELL__
    map_elem xs = coerce xs
#else
    map_elem xs = Data.List.map Elem xs
#endif
    {-# INLINE map_elem #-}

-- essentially: Free ((,) a) b.
data ListFinal a cont = LFinal !cont | LCons !a (ListFinal a cont)

#ifdef __GLASGOW_HASKELL__
instance GHC.Exts.IsList (Seq a) where
    type Item (Seq a) = a
    fromList = fromList
    fromListN = fromList2
    toList = toList
#endif

#ifdef __GLASGOW_HASKELL__
-- | @since 0.5.7
instance a ~ Char => IsString (Seq a) where
    fromString = fromList
#endif

------------------------------------------------------------------------
-- Reverse
------------------------------------------------------------------------

-- | \( O(n) \). The reverse of a sequence.
reverse :: Seq a -> Seq a
reverse (Seq xs) = Seq (fmapReverseTree id xs)

#ifdef __GLASGOW_HASKELL__
{-# NOINLINE [1] reverse #-}

-- | \( O(n) \). Reverse a sequence while mapping over it. This is not
-- currently exported, but is used in rewrite rules.
fmapReverse :: (a -> b) -> Seq a -> Seq b
fmapReverse f (Seq xs) = Seq (fmapReverseTree (lift_elem f) xs)
  where
    lift_elem :: (a -> b) -> (Elem a -> Elem b)
#ifdef __GLASGOW_HASKELL__
    lift_elem = coerce
#else
    lift_elem g (Elem a) = Elem (g a)
#endif

-- If we're mapping over a sequence, we can reverse it at the same time
-- at no extra charge.
{-# RULES
"fmapSeq/reverse" forall f xs . fmapSeq f (reverse xs) = fmapReverse f xs
"reverse/fmapSeq" forall f xs . reverse (fmapSeq f xs) = fmapReverse f xs
 #-}
#endif

fmapReverseTree :: (a -> b) -> FingerTree a -> FingerTree b
fmapReverseTree _ EmptyT = EmptyT
fmapReverseTree f (Single x) = Single (f x)
fmapReverseTree f (Deep s pr m sf) =
    Deep s (reverseDigit f sf)
        (fmapReverseTree (reverseNode f) m)
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
-- David Feuer, with some guidance from Carter Schonwald, December 2014

-- | \( O(n) \). Constructs a new sequence with the same structure as an existing
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
#ifdef __GLASGOW_HASKELL__
-- We use ScopedTypeVariables to improve performance and make
-- performance less sensitive to minor changes.

-- We INLINE this so GHC can see that the function passed in is
-- strict in its Int argument.
{-# INLINE splitMap #-}
splitMap :: forall s a' b' . (Int -> s -> (s,s)) -> (s -> a' -> b') -> s -> Seq a' -> Seq b'
splitMap splt f0 s0 (Seq xs0) = Seq $ splitMapTreeE (\s' (Elem a) -> Elem (f0 s' a)) s0 xs0
  where
    {-# INLINE splitMapTreeE #-}
    splitMapTreeE :: (s -> Elem y -> b) -> s -> FingerTree (Elem y) -> FingerTree b
    splitMapTreeE  _ _ EmptyT = EmptyT
    splitMapTreeE  f s (Single xs) = Single $ f s xs
    splitMapTreeE  f s (Deep n pr m sf) = Deep n (splitMapDigit f prs pr) (splitMapTreeN (\eta1 eta2 -> splitMapNode f eta1 eta2) ms m) (splitMapDigit f sfs sf)
          where
            !spr = size pr
            !sm = n - spr - size sf
            (prs, r) = splt spr s
            (ms, sfs) = splt sm r

    splitMapTreeN :: (s -> Node a -> b) -> s -> FingerTree (Node a) -> FingerTree b
    splitMapTreeN _ _ EmptyT = EmptyT
    splitMapTreeN f s (Single xs) = Single $ f s xs
    splitMapTreeN f s (Deep n pr m sf) = Deep n (splitMapDigit f prs pr) (splitMapTreeN (\eta1 eta2 -> splitMapNode f eta1 eta2) ms m) (splitMapDigit f sfs sf)
          where
            (prs, r) = splt (size pr) s
            (ms, sfs) = splt (size m) r

    {-# INLINE splitMapDigit #-}
    splitMapDigit :: Sized a => (s -> a -> b) -> s -> Digit a -> Digit b
    splitMapDigit f s (One a) = One (f s a)
    splitMapDigit f s (Two a b) = Two (f first a) (f second b)
      where
        (first, second) = splt (size a) s
    splitMapDigit f s (Three a b c) = Three (f first a) (f second b) (f third c)
      where
        (first, r) = splt (size a) s
        (second, third) = splt (size b) r
    splitMapDigit f s (Four a b c d) = Four (f first a) (f second b) (f third c) (f fourth d)
      where
        (first, s') = splt (size a) s
        (middle, fourth) = splt (size b + size c) s'
        (second, third) = splt (size b) middle

    {-# INLINE splitMapNode #-}
    splitMapNode :: Sized a => (s -> a -> b) -> s -> Node a -> Node b
    splitMapNode f s (Node2 ns a b) = Node2 ns (f first a) (f second b)
      where
        (first, second) = splt (size a) s
    splitMapNode f s (Node3 ns a b c) = Node3 ns (f first a) (f second b) (f third c)
      where
        (first, r) = splt (size a) s
        (second, third) = splt (size b) r

#else
-- Implementation without ScopedTypeVariables--somewhat slower,
-- and much more sensitive to minor changes in various places.

{-# INLINE splitMap #-}
splitMap :: (Int -> s -> (s,s)) -> (s -> a -> b) -> s -> Seq a -> Seq b
splitMap splt' f0 s0 (Seq xs0) = Seq $ splitMapTreeE splt' (\s' (Elem a) -> Elem (f0 s' a)) s0 xs0

{-# INLINE splitMapTreeE #-}
splitMapTreeE :: (Int -> s -> (s,s)) -> (s -> Elem y -> b) -> s -> FingerTree (Elem y) -> FingerTree b
splitMapTreeE _    _ _ EmptyT = EmptyT
splitMapTreeE _    f s (Single xs) = Single $ f s xs
splitMapTreeE splt f s (Deep n pr m sf) = Deep n (splitMapDigit splt f prs pr) (splitMapTreeN splt (\eta1 eta2 -> splitMapNode splt f eta1 eta2) ms m) (splitMapDigit splt f sfs sf)
      where
        !spr = size pr
        sm = n - spr - size sf
        (prs, r) = splt spr s
        (ms, sfs) = splt sm r

splitMapTreeN :: (Int -> s -> (s,s)) -> (s -> Node a -> b) -> s -> FingerTree (Node a) -> FingerTree b
splitMapTreeN _    _ _ EmptyT = EmptyT
splitMapTreeN _    f s (Single xs) = Single $ f s xs
splitMapTreeN splt f s (Deep n pr m sf) = Deep n (splitMapDigit splt f prs pr) (splitMapTreeN splt (\eta1 eta2 -> splitMapNode splt f eta1 eta2) ms m) (splitMapDigit splt f sfs sf)
      where
        (prs, r) = splt (size pr) s
        (ms, sfs) = splt (size m) r

{-# INLINE splitMapDigit #-}
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

{-# INLINE splitMapNode #-}
splitMapNode :: Sized a => (Int -> s -> (s,s)) -> (s -> a -> b) -> s -> Node a -> Node b
splitMapNode splt f s (Node2 ns a b) = Node2 ns (f first a) (f second b)
  where
    (first, second) = splt (size a) s
splitMapNode splt f s (Node3 ns a b c) = Node3 ns (f first a) (f second b) (f third c)
  where
    (first, r) = splt (size a) s
    (second, third) = splt (size b) r
#endif

------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------

-- We use a custom definition of munzip to avoid retaining
-- memory longer than necessary. Using the default definition, if
-- we write
--
-- let (xs,ys) = munzip zs
-- in xs `deepseq` (... ys ...)
--
-- then ys will retain the entire zs sequence until ys itself is fully forced.
-- This implementation uses the selector thunk optimization to prevent that.
-- Unfortunately, that optimization is fragile, so we can't actually guarantee
-- anything.

-- | @ 'mzipWith' = 'zipWith' @
--
-- @ 'munzip' = 'unzip' @
--
-- @since 0.5.10.1
instance MonadZip Seq where
  mzipWith = zipWith
  munzip = unzip

-- | Unzip a sequence of pairs.
--
-- @
-- unzip ps = ps ``seq`` ('fmap' 'fst' ps) ('fmap' 'snd' ps)
-- @
--
-- Example:
--
-- @
-- unzip $ fromList [(1,"a"), (2,"b"), (3,"c")] =
--   (fromList [1,2,3], fromList ["a", "b", "c"])
-- @
--
-- See the note about efficiency at 'unzipWith'.
--
-- @since 0.5.11
unzip :: Seq (a, b) -> (Seq a, Seq b)
unzip xs = unzipWith id xs

-- | \( O(n) \). Unzip a sequence using a function to divide elements.
--
-- @ unzipWith f xs == 'unzip' ('fmap' f xs) @
--
-- Efficiency note:
--
-- @unzipWith@ produces its two results in lockstep. If you calculate
-- @ unzipWith f xs @ and fully force /either/ of the results, then the
-- entire structure of the /other/ one will be built as well. This
-- behavior allows the garbage collector to collect each calculated
-- pair component as soon as it dies, without having to wait for its mate
-- to die. If you do not need this behavior, you may be better off simply
-- calculating the sequence of pairs and using 'fmap' to extract each
-- component sequence.
--
-- @since 0.5.11
unzipWith :: (a -> (b, c)) -> Seq a -> (Seq b, Seq c)
unzipWith f = unzipWith' (\x ->
  let
    {-# NOINLINE fx #-}
    fx = f x
    (y,z) = fx
  in (y,z))
-- Why do we lazify `f`? Because we don't want the strictness to depend
-- on exactly how the sequence is balanced. For example, what do we want
-- from
--
-- unzip [(1,2), undefined, (5,6)]?
--
-- The argument could be represented as
--
-- Seq $ Deep 3 (One (Elem (1,2))) EmptyT (Two undefined (Elem (5,6)))
--
-- or as
--
-- Seq $ Deep 3 (Two (Elem (1,2)) undefined) EmptyT (One (Elem (5,6)))
--
-- We don't want the tree balance to determine whether we get
--
-- ([1, undefined, undefined], [2, undefined, undefined])
--
-- or
--
-- ([undefined, undefined, 5], [undefined, undefined, 6])
--
-- so we pretty much have to be completely lazy in the elements.

#ifdef __GLASGOW_HASKELL__
{-# NOINLINE [1] unzipWith #-}

-- We don't need a special rule for unzip:
--
-- unzip (fmap f xs) = unzipWith id f xs,
--
-- which rewrites to unzipWith (id . f) xs
--
-- It's true that if GHC doesn't know the arity of `f` then
-- it won't reduce further, but that doesn't seem like too
-- big a deal here.
{-# RULES
"unzipWith/fmapSeq" forall f g xs. unzipWith f (fmapSeq g xs) =
                                     unzipWith (f . g) xs
 #-}
#endif

class UnzipWith f where
  unzipWith' :: (x -> (a, b)) -> f x -> (f a, f b)

-- This instance is only used at the very top of the tree;
-- the rest of the elements are handled by unzipWithNodeElem
instance UnzipWith Elem where
#ifdef __GLASGOW_HASKELL__
  unzipWith' = coerce
#else
  unzipWith' f (Elem a) = case f a of (x, y) -> (Elem x, Elem y)
#endif

-- We're very lazy here for the sake of efficiency. We want to be able to
-- reach any element of either result in logarithmic time. If we pattern
-- match strictly, we'll end up building entire 2-3 trees at once, which
-- would take linear time.
--
-- However, we're not *entirely* lazy! We are careful to build pieces
-- of each sequence as the corresponding pieces of the *other* sequence
-- are demanded. This allows the garbage collector to get rid of each
-- *component* of each result pair as soon as it is dead.
--
-- Note that this instance is used only for *internal* nodes. Nodes
-- containing elements are handled by 'unzipWithNodeElem'
instance UnzipWith Node where
  unzipWith' f (Node2 s x y) =
    ( Node2 s x1 y1
    , Node2 s x2 y2)
    where
      {-# NOINLINE fx #-}
      {-# NOINLINE fy #-}
      fx = strictifyPair (f x)
      fy = strictifyPair (f y)
      (x1, x2) = fx
      (y1, y2) = fy
  unzipWith' f (Node3 s x y z) =
    ( Node3 s x1 y1 z1
    , Node3 s x2 y2 z2)
    where
      {-# NOINLINE fx #-}
      {-# NOINLINE fy #-}
      {-# NOINLINE fz #-}
      fx = strictifyPair (f x)
      fy = strictifyPair (f y)
      fz = strictifyPair (f z)
      (x1, x2) = fx
      (y1, y2) = fy
      (z1, z2) = fz

-- Force both elements of a pair
strictifyPair :: (a, b) -> (a, b)
strictifyPair (!x, !y) = (x, y)

-- We're strict here for the sake of efficiency. The Node instance
-- is lazy, so we don't particularly need to add an extra thunk on top
-- of each node.
instance UnzipWith Digit where
  unzipWith' f (One x)
    | (x1, x2) <- f x
    = (One x1, One x2)
  unzipWith' f (Two x y)
    | (x1, x2) <- f x
    , (y1, y2) <- f y
    = ( Two x1 y1
      , Two x2 y2)
  unzipWith' f (Three x y z)
    | (x1, x2) <- f x
    , (y1, y2) <- f y
    , (z1, z2) <- f z
    = ( Three x1 y1 z1
      , Three x2 y2 z2)
  unzipWith' f (Four x y z w)
    | (x1, x2) <- f x
    , (y1, y2) <- f y
    , (z1, z2) <- f z
    , (w1, w2) <- f w
    = ( Four x1 y1 z1 w1
      , Four x2 y2 z2 w2)

instance UnzipWith FingerTree where
  unzipWith' _ EmptyT = (EmptyT, EmptyT)
  unzipWith' f (Single x)
    | (x1, x2) <- f x
    = (Single x1, Single x2)
  unzipWith' f (Deep s pr m sf)
    | (!pr1, !pr2) <- unzipWith' f pr
    , (!sf1, !sf2) <- unzipWith' f sf
    = (Deep s pr1 m1 sf1, Deep s pr2 m2 sf2)
    where
      {-# NOINLINE m1m2 #-}
      m1m2 = strictifyPair $ unzipWith' (unzipWith' f) m
      (m1, m2) = m1m2

instance UnzipWith Seq where
  unzipWith' _ (Seq EmptyT) = (empty, empty)
  unzipWith' f (Seq (Single (Elem x)))
    | (x1, x2) <- f x
    = (singleton x1, singleton x2)
  unzipWith' f (Seq (Deep s pr m sf))
    | (!pr1, !pr2) <- unzipWith' (unzipWith' f) pr
    , (!sf1, !sf2) <- unzipWith' (unzipWith' f) sf
    = (Seq (Deep s pr1 m1 sf1), Seq (Deep s pr2 m2 sf2))
    where
      {-# NOINLINE m1m2 #-}
      m1m2 = strictifyPair $ unzipWith' (unzipWithNodeElem f) m
      (m1, m2) = m1m2

-- Here we need to be lazy in the children (because they're
-- Elems), but we can afford to be strict in the results
-- of `f` because it's sure to return a pair immediately
-- (unzipWith lazifies the function it's passed).
unzipWithNodeElem :: (x -> (a, b))
       -> Node (Elem x) -> (Node (Elem a), Node (Elem b))
unzipWithNodeElem f (Node2 s (Elem x) (Elem y))
  | (x1, x2) <- f x
  , (y1, y2) <- f y
  = ( Node2 s (Elem x1) (Elem y1)
    , Node2 s (Elem x2) (Elem y2))
unzipWithNodeElem f (Node3 s (Elem x) (Elem y) (Elem z))
  | (x1, x2) <- f x
  , (y1, y2) <- f y
  , (z1, z2) <- f z
  = ( Node3 s (Elem x1) (Elem y1) (Elem z1)
    , Node3 s (Elem x2) (Elem y2) (Elem z2))

-- | \( O(\min(n_1,n_2)) \).  'zip' takes two sequences and returns a sequence
-- of corresponding pairs.  If one input is short, excess elements are
-- discarded from the right end of the longer sequence.
zip :: Seq a -> Seq b -> Seq (a, b)
zip = zipWith (,)

-- | \( O(\min(n_1,n_2)) \).  'zipWith' generalizes 'zip' by zipping with the
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
zipWith' f s1 s2 = splitMap uncheckedSplitAt goLeaf s2 s1
  where
    goLeaf (Seq (Single (Elem b))) a = f a b
    goLeaf _ _ = error "Data.Sequence.zipWith'.goLeaf internal error: not a singleton"

-- | \( O(\min(n_1,n_2,n_3)) \).  'zip3' takes three sequences and returns a
-- sequence of triples, analogous to 'zip'.
zip3 :: Seq a -> Seq b -> Seq c -> Seq (a,b,c)
zip3 = zipWith3 (,,)

-- | \( O(\min(n_1,n_2,n_3)) \).  'zipWith3' takes a function which combines
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

-- | \( O(\min(n_1,n_2,n_3,n_4)) \).  'zip4' takes four sequences and returns a
-- sequence of quadruples, analogous to 'zip'.
zip4 :: Seq a -> Seq b -> Seq c -> Seq d -> Seq (a,b,c,d)
zip4 = zipWith4 (,,,)

-- | \( O(\min(n_1,n_2,n_3,n_4)) \).  'zipWith4' takes a function which combines
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

-- | fromList2, given a list and its length, constructs a completely
-- balanced Seq whose elements are that list using the replicateA
-- generalization.
fromList2 :: Int -> [a] -> Seq a
fromList2 n = execState (replicateA n (State ht))
  where
    ht (x:xs) = (xs, x)
    ht []     = error "fromList2: short list"
