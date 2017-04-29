module Main where

import Control.Applicative
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad.Trans.State.Strict
import Criterion.Main (bench, bgroup, defaultMain, nf, Benchmark, env)
import Data.Foldable (foldl', foldr')
import qualified Data.Sequence as S
import qualified Data.Foldable
import Data.Traversable (traverse)
import System.Random (mkStdGen, randoms, randomIO)

randInt :: IO Int
randInt = randomIO

benchAtSize :: Int -> Benchmark
benchAtSize n =
    env (S.replicateA n randInt) $
    \xs ->
         bgroup (show n)
           [ bench "unrolled" $ nf S.unstableSort xs
           , bench "state" $ nf S.unstableSort' xs]

main :: IO ()
main = defaultMain (map benchAtSize [50000,500000,1000000])
