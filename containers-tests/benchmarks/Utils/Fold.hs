{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Fold
  ( foldBenchmarks
  , foldWithKeyBenchmarks
  ) where

import Control.Monad.Trans.State.Strict
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.Monoid (Any(..))
import Prelude hiding (Foldable(..))
import Test.Tasty.Bench (Benchmark, bench, defaultMain, whnf, nf)
import qualified GHC.Exts

-- | Benchmarks for folds on a structure of @Int@s.

-- See Note [Choice of benchmarks]
foldBenchmarks
  :: forall f.
     (forall b. (Int -> b -> b) -> b -> f -> b)
  -> (forall b. (b -> Int -> b) -> b -> f -> b)
  -> (forall b. (Int -> b -> b) -> b -> f -> b)
  -> (forall b. (b -> Int -> b) -> b -> f -> b)
  -> (forall m. Monoid m => (Int -> m) -> f -> m)
  -> f
  -> [Benchmark]
foldBenchmarks foldr foldl foldr' foldl' foldMap xs =
  [-- foldr
    bench "foldr_elem" $ whnf foldr_elem xs
  , bench "foldr_cpsSum" $ whnf foldr_cpsSum xs
  , bench "foldr_cpsOneShotSum" $ whnf foldr_cpsOneShotSum xs
  , bench "foldr_traverseSum" $ whnf foldr_traverseSum xs

    -- foldl
  , bench "foldl_elem" $ whnf foldl_elem xs
  , bench "foldl_cpsSum" $ whnf foldl_cpsSum xs
  , bench "foldl_cpsOneShotSum" $ whnf foldl_cpsOneShotSum xs
  , bench "foldl_traverseSum" $ whnf foldl_traverseSum xs

    -- foldr'
  , bench "foldr'_sum" $ whnf (foldr' (+) 0) xs
  , bench "foldr'_maximum" $ whnf foldr'_maximum xs

    -- foldl'
  , bench "foldl'_sum" $ whnf (foldl' (+) 0) xs
  , bench "foldl'_maximum" $ whnf foldl'_maximum xs

    -- foldMap
  , bench "foldMap_elem" $ whnf foldMap_elem xs
  , bench "foldMap_traverseSum" $ whnf foldMap_traverseSum xs
  ]
  where
    foldr_elem :: f -> Bool
    foldr_elem = foldr (\x z -> x == minBound || z) False

    foldr_cpsSum :: f -> Int
    foldr_cpsSum xs = foldr (\x k !acc -> k (x + acc)) id xs 0

    foldr_cpsOneShotSum :: f -> Int
    foldr_cpsOneShotSum xs =
      foldr (\x k -> GHC.Exts.oneShot (\ !acc -> k (x + acc))) id xs 0

    foldr_traverseSum :: f -> Int
    foldr_traverseSum xs =
      execState (foldr (\x z -> modify' (+x) *> z) (pure ()) xs) 0

    foldl_elem :: f -> Bool
    foldl_elem = foldl (\z x -> x == minBound || z) False

    foldl_cpsSum :: f -> Int
    foldl_cpsSum xs = foldl (\k x !acc -> k (x + acc)) id xs 0

    foldl_cpsOneShotSum :: f -> Int
    foldl_cpsOneShotSum xs =
      foldl (\k x -> GHC.Exts.oneShot (\ !acc -> k (x + acc))) id xs 0

    foldl_traverseSum :: f -> Int
    foldl_traverseSum xs =
      execState (foldl (\z x -> modify' (+x) *> z) (pure ()) xs) 0

    foldr'_maximum :: f -> Maybe Int
    foldr'_maximum = foldr' (\x z -> Just $! maybe x (max x) z) Nothing

    foldl'_maximum :: f -> Maybe Int
    foldl'_maximum = foldl' (\z x -> Just $! maybe x (max x) z) Nothing

    foldMap_elem :: f -> Any
    foldMap_elem = foldMap (\x -> Any (x == minBound))

    foldMap_traverseSum :: f -> Int
    foldMap_traverseSum xs =
      execState (runEffect (foldMap (\x -> Effect (modify' (+x))) xs)) 0
{-# INLINE foldBenchmarks #-}

-- | Benchmarks for folds on a structure of @Int@ keys and @Int@ values.
foldWithKeyBenchmarks
  :: (forall b. (Int -> Int -> b -> b) -> b -> f -> b)
  -> (forall b. (b -> Int -> Int -> b) -> b -> f -> b)
  -> (forall b. (Int -> Int -> b -> b) -> b -> f -> b)
  -> (forall b. (b -> Int -> Int -> b) -> b -> f -> b)
  -> (forall m. Monoid m => (Int -> Int -> m) -> f -> m)
  -> f
  -> [Benchmark]
foldWithKeyBenchmarks
  foldrWithKey foldlWithKey foldrWithKey' foldlWithKey' foldMapWithKey =
  foldBenchmarks
    (\f -> foldrWithKey (\k x z -> f (k + x) z))
    (\f -> foldlWithKey (\z k x -> f z (k + x)))
    (\f -> foldrWithKey' (\k x z -> f (k + x) z))
    (\f -> foldlWithKey' (\z k x -> f z (k + x)))
    (\f -> foldMapWithKey (\k x -> f (k + x)))
{-# INLINE foldWithKeyBenchmarks #-}

newtype Effect f = Effect { runEffect :: f () }

instance Applicative f => Semigroup (Effect f) where
  Effect f1 <> Effect f2 = Effect (f1 *> f2)

instance Applicative f => Monoid (Effect f) where
  mempty = Effect (pure ())
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif


-- Note [Choice of benchmarks]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- foldr_elem, foldl_elem
--   Simple lazy fold that visits every element. In practice:
--   * Worst case for short-circuiting folds
--   * Data.Foldable.toList
--
-- foldr_cpsSum, foldr_cpsOneShotSum, foldl_cpsSum, foldl_cpsOneShotSum
--   The well-known foldl'-via-foldr pattern. GHC.Exts.oneShot is used to help
--   GHC with optimizations. In practice:
--   * Used for early-return with an accumulator
--   * Used by the foldl library
--
-- foldr_traverseSum, foldr_traverseSum
--   Folding with an effect. In practice:
--   * Folds defined using foldr, such as Data.Foldable.traverse_ and friends
--
-- foldl'_sum, foldr'_sum
--   Strict folds.
--
-- foldl'_maximum, foldr'_maximum
--  Strict folds with a `Maybe` as accumulator which could be optimized away.
--
-- foldMap_elem
--   Simple lazy fold that visits every element. In practice:
--   * Worst case for lazy folds defined using foldMap, such as
--     Data.Foldable.any, Data.Foldable.find, etc.
--
-- foldMap_traverseSum
--   Folding with an effect. In practice:
--   * With the lens library, using traverseOf_ on a foldMap based fold.
