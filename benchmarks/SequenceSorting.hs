module Main (main) where

import           Criterion.Main (Benchmark, bench, defaultMain, env, nf, bgroup)
import           System.Random  (randomIO)

import qualified Data.Sequence  as S

randInt :: IO Int
randInt = randomIO

benchAtSize :: Int -> Benchmark
benchAtSize n =
    env (S.replicateA n randInt) $
    \xs ->
         bgroup
             (show n)
             [ bench "old" $ nf S.unstableSort xs
             , bench "new" $ nf S.unstableSort' xs]

main :: IO ()
main = defaultMain (map benchAtSize [500, 50000, 500000, 1000000])
