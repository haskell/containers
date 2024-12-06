module Utils.Random
  ( shuffle
  ) where

import Data.List (unfoldr)
import System.Random (RandomGen, randomR)
import qualified Data.Sequence as Seq

-- | O(n log n) Fisher-Yates shuffle.
shuffle :: RandomGen g => g -> [a] -> [a]
shuffle g0 xs0 = unfoldr f (g0, Seq.fromList xs0)
  where
    f (g, xs)
      | Seq.null xs = Nothing
      | otherwise = Just (x, (g', xs'))
      where
        (i, g') = randomR (0, Seq.length xs - 1) g
        x = Seq.index xs i
        xs' = Seq.deleteAt i xs
{-# INLINABLE shuffle #-}
