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
             [ bench "old" $ nf S.unstableSort xs ]

incBenchAtSize :: Int -> Benchmark
incBenchAtSize n =
    env (S.unstableSort <$> S.replicateA n randInt) $
    \xs ->
         bgroup
             (show n)
             [ bench "old" $ nf S.unstableSort xs ]

main :: IO ()
main =
    defaultMain
        [ bgroup "unordered" $ map benchAtSize [500, 5000, 50000]
        , bgroup "increasing" $ map incBenchAtSize [500, 5000, 50000]
        ]
