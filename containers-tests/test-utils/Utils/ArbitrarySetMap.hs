module Utils.ArbitrarySetMap
  (
    -- MonadGen
    MonadGen(..)

    -- Set
  , mkArbSet
  , setFromList

    -- Map
  , mkArbMap
  , mapFromKeysList
  ) where

import Control.Monad (liftM, liftM3, liftM4)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Test.QuickCheck

import Data.Set (Set)
import qualified Data.Set.Internal as S
import Data.Map (Map)
import qualified Data.Map.Internal as M

{--------------------------------------------------------------------
  MonadGen
--------------------------------------------------------------------}

class Monad m => MonadGen m where
  liftGen :: Gen a -> m a
instance MonadGen Gen where
  liftGen = id
instance MonadGen m => MonadGen (StateT s m) where
  liftGen = lift . liftGen

{--------------------------------------------------------------------
  Set
--------------------------------------------------------------------}

-- | Given an action that produces successively larger elements and
-- a size, produce a set of arbitrary shape with exactly that size.
mkArbSet :: MonadGen m => m a -> Int -> m (Set a)
mkArbSet step n
  | n <= 0 = return S.Tip
  | n == 1 = S.singleton `liftM` step
  | n == 2 = do
     dir <- liftGen arbitrary
     p <- step
     q <- step
     if dir
       then return (S.Bin 2 q (S.singleton p) S.Tip)
       else return (S.Bin 2 p S.Tip (S.singleton q))
  | otherwise = do
      -- This assumes a balance factor of delta = 3
      let upper = (3*(n - 1)) `quot` 4
      let lower = (n + 2) `quot` 4
      ln <- liftGen $ choose (lower, upper)
      let rn = n - ln - 1
      liftM3
        (\lt x rt -> S.Bin n x lt rt)
        (mkArbSet step ln)
        step
        (mkArbSet step rn)
{-# INLINABLE mkArbSet #-}

-- | Given a strictly increasing list of elements, produce an arbitrarily
-- shaped set with exactly those elements.
setFromList :: [a] -> Gen (Set a)
setFromList xs = flip evalStateT xs $ mkArbSet step (length xs)
  where
    step = state $ fromMaybe (error "setFromList") . List.uncons

{--------------------------------------------------------------------
  Map
--------------------------------------------------------------------}

-- | Given an action that produces successively larger keys and
-- a size, produce a map of arbitrary shape with exactly that size.
mkArbMap :: (MonadGen m, Arbitrary v) => m k -> Int -> m (Map k v)
mkArbMap step n
  | n <= 0 = return M.Tip
  | n == 1 = do
     k <- step
     v <- liftGen arbitrary
     return (M.singleton k v)
  | n == 2 = do
     dir <- liftGen arbitrary
     p <- step
     q <- step
     vOuter <- liftGen arbitrary
     vInner <- liftGen arbitrary
     if dir
       then return (M.Bin 2 q vOuter (M.singleton p vInner) M.Tip)
       else return (M.Bin 2 p vOuter M.Tip (M.singleton q vInner))
  | otherwise = do
      -- This assumes a balance factor of delta = 3
      let upper = (3*(n - 1)) `quot` 4
      let lower = (n + 2) `quot` 4
      ln <- liftGen $ choose (lower, upper)
      let rn = n - ln - 1
      liftM4
        (\lt x v rt -> M.Bin n x v lt rt)
        (mkArbMap step ln)
        step
        (liftGen arbitrary)
        (mkArbMap step rn)
{-# INLINABLE mkArbMap #-}

-- | Given a strictly increasing list of keys, produce an arbitrarily
-- shaped map with exactly those keys.
mapFromKeysList :: Arbitrary a => [k] -> Gen (Map k a)
mapFromKeysList xs = flip evalStateT xs $ mkArbMap step (length xs)
  where
    step = state $ fromMaybe (error "mapFromKeysList") . List.uncons
{-# INLINABLE mapFromKeysList #-}
