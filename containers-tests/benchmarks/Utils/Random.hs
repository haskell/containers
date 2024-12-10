module Utils.Random
  ( shuffle
  ) where

import Data.List (unfoldr)
import qualified Data.Sequence as Seq
import System.Random (StdGen, mkStdGen, randomR)

-- O(n log n). Deterministic shuffle. Implements Fisher-Yates.
shuffle :: [a] -> [a]
shuffle xs0 = unfoldr f (gen, Seq.fromList xs0)
  where
    f (g, xs)
      | Seq.null xs = Nothing
      | otherwise = Just (x, (g', xs'))
      where
        (i, g') = randomR (0, Seq.length xs - 1) g
        x = Seq.index xs i
        xs' = Seq.deleteAt i xs

gen :: StdGen
gen = mkStdGen 42
