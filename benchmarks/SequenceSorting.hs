module Main (main) where

import           Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, nf)
import           System.Random  (randomIO)

import qualified Data.Sequence  as S

randInt :: IO Int
randInt = randomIO

benchSorts :: S.Seq Int -> [Benchmark]
benchSorts xs =
             [bench "to/from list" $ nf S.sort xs
             ,bench "unstable heapsort" $ nf S.unstableSort xs]

main :: IO ()
main =
    defaultMain
        [ env (S.replicateA 50000 randInt) (bgroup "unordered" . benchSorts)
        , env
              (S.unstableSort <$> S.replicateA 50000 randInt)
              (bgroup "increasing" . benchSorts)]
