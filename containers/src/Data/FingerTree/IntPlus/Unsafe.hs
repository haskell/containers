{-# LANGUAGE CPP #-}
#include "containers.h"

-- | This module exports functions that can easily
-- produce finger trees violating the annotation invariants.
-- Trees violating these invariants will produce garbage
-- when split.
module Data.FingerTree.IntPlus.Unsafe
  ( unsafeMap
  , unsafeTraverse
  ) where

import Data.Sequence.Internal
  ( FingerTree (..), Node (..) )
import qualified Data.Sequence.Internal as S
import Control.Applicative
#if !MIN_VERSION_base(4,8,0)
import Data.Traversable (traverse)
#endif

-- | Map over a 'FingerTree'. The following precondition
-- is assumed but not checked:
--
-- For each @a@ in the @FingerTree@, @size (f a) = size a@.
unsafeMap :: (a -> b) -> FingerTree a -> FingerTree b
unsafeMap = S.unsafeMapFT

-- | Traverse a 'FingerTree'. The following precondition is required
-- but not checked:
--
-- For each element @a@ in the 'FingerTree',
-- @size <$> f a = size a <$ f a@
unsafeTraverse :: Applicative f => (a -> f b) -> FingerTree a -> f (FingerTree b)
unsafeTraverse _ EmptyT = pure EmptyT
unsafeTraverse f (Single x) = Single <$> f x
unsafeTraverse f (Deep v pr m sf) =
        liftA3 (Deep v) (traverse f pr) (unsafeTraverse (unsafeTraverseNode f) m) (traverse f sf)

-- | Traverse a 'Node'. The following precondition is required
-- but not checked:
--
-- For each element @a@ in the 'Node',
-- @size <$> f a = size a <$ f a@
unsafeTraverseNode :: Applicative f => (a -> f b) -> Node a -> f (Node b)
unsafeTraverseNode f (Node2 v a b) = liftA2 (Node2 v) (f a) (f b)
unsafeTraverseNode f (Node3 v a b c) = liftA3 (Node3 v) (f a) (f b) (f c)
