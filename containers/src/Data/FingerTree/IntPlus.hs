{-# LANGUAGE CPP #-}
#include "containers.h"
{-# LANGUAGE BangPatterns #-}

#ifdef DEFINE_PATTERN_SYNONYMS
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#endif

-- | This module exports a type of finger trees with measurements ("sizes") in
-- the @(Int, +)@ monoid. This type is used to implement sequences in
-- "Data.Sequence". It may occasionally be useful for other purposes.
--
-- Caution: splitting and lookup functions assume that the size of the tree is
-- at most @'maxBound' :: Int@. If this is not the case, then they may produce
-- errors and/or utter nonsense.

module Data.FingerTree.IntPlus
  ( 
#ifdef DEFINE_PATTERN_SYNONYMS
    FingerTree (Empty, (:<|), (:|>), Singleton)
#else
    FingerTree
#endif
  , Elem (..)
  , Sized (..)
  , Split (..)
  , UncheckedSplit (..)
  , ViewL (..)
  , ViewR (..)
  , (<|)
  , (|>)
  , (><)
  , fromList
  , viewl
  , viewr
  , split
  , uncheckedSplit
  ) where

import Data.Sequence.Internal
  ( FingerTree (..), Sized (..), Elem (..) )
import qualified Data.Sequence.Internal as S
#if !MIN_VERSION_base(4,8,0)
import Data.Word (Word)
#endif

infixr 5 ><
infixr 5 <|, :<
infixl 5 |>, :>

(<|) :: Sized a => a -> FingerTree a -> FingerTree a
(<|) = S.consTree

(|>) :: Sized a => FingerTree a -> a -> FingerTree a
(|>) = S.snocTree

(><) :: Sized a => FingerTree a -> FingerTree a -> FingerTree a
(><) = S.appendTree

fromList :: Sized a => [a] -> FingerTree a
fromList = S.fromListFT

data ViewL a = a :< FingerTree a | EmptyL
data ViewR a = FingerTree a :> a | EmptyR

{-# INLINE viewl #-}
viewl :: Sized a => FingerTree a -> ViewL a
viewl t = case S.viewLTree t of
  S.ConsLTree a as -> a :< as
  S.EmptyLTree -> EmptyL

{-# INLINE viewr #-}
viewr :: Sized a => FingerTree a -> ViewR a
viewr t = case S.viewRTree t of
  S.SnocRTree as a -> as :> a
  S.EmptyRTree -> EmptyR

#ifdef DEFINE_PATTERN_SYNONYMS
infixr 5 :<|
infixl 5 :|>

#if __GLASGOW_HASKELL__ >= 801
{-# COMPLETE (:<|), Empty #-}
{-# COMPLETE (:|>), Empty #-}
#endif

-- | A bidirectional pattern synonym matching an empty finger tree.
pattern Empty :: S.FingerTree a
pattern Empty = S.EmptyT

-- | A bidirectional pattern synonym viewing the front of a non-empty
-- finger tree.
pattern (:<|) :: Sized a => a -> FingerTree a -> FingerTree a
pattern x :<| xs <- (viewl -> x :< xs)
  where
    x :<| xs = x <| xs

-- | A bidirectional pattern synonym viewing the rear of a non-empty
-- finger tree.
pattern (:|>) :: Sized a => FingerTree a -> a -> FingerTree a
pattern xs :|> x <- (viewr -> xs :> x)
  where
    xs :|> x = xs |> x

-- | A bidirectional pattern synonym for a singleton
-- sequence. @Singleton xs@ is equivalent to @xs :< Empty@.
pattern Singleton :: a -> FingerTree a
pattern Singleton x <- S.Single x
  where
    Singleton = S.Single
#endif

data Split a
  = Split !(FingerTree a) a !(FingerTree a)
  | EmptySplit

data UncheckedSplit a
  = UncheckedSplit !(FingerTree a) a !(FingerTree a)

-- | Split a finger tree around a measurement.
--
-- @split i xs = EmptySplit@ if and only if @xs = Empty@. Given that
--
-- @
-- split i xs = 'Split' l x r
-- @
--
-- it's guaranteed that
--
-- 1. @ xs = l <> (x <| r) @
-- 2. @i >= size l@ or @l = Empty@
-- 3. @i < size l + size x@ or @r = Empty@

split :: Sized a => Int -> FingerTree a -> Split a
split !_i S.EmptyT = EmptySplit
split i ft
  | S.Split l m r <- S.splitTree i ft
  = Split l m r

-- | Split a nonempty finger tree around a measurement. Given that
--
-- @
-- uncheckedSplit i xs = 'UncheckedSplit' l x r
-- @
--
-- it's guaranteed that
--
-- 1. @ xs = l <> (x <| r) @
-- 2. @i >= size l@ or @l = Empty@
-- 3. @i < size l + size x@ or @r = Empty@
uncheckedSplit :: Sized a => Int -> FingerTree a -> UncheckedSplit a
uncheckedSplit i ft
  | S.Split l m r <- S.splitTree i ft
  = UncheckedSplit l m r
