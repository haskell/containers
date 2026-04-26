{-# LANGUAGE BangPatterns #-}

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import qualified Data.Containers.ListUtils as LU

import Test.Tasty.Bench (bench, bgroup, defaultMain, whnf)

main :: IO ()
main = do
  evaluate $ rnf [xs_distinct, xs_repeat]
  evaluate $ rnf [xss_distinct, xss_repeat]
  defaultMain
    [ bgroup "nubOrd"
      [ bench "no_fusion_distinct" $
          whnf (consumeNoFusion . LU.nubOrd) xs_distinct
      , bench "no_fusion_repeat" $
          whnf (consumeNoFusion . LU.nubOrd) xs_repeat
      , bench "issue1202_distinct" $
          whnf (consumeNoFusion . collectFrameworksDirs) xss_distinct
      , bench "issue1202_repeat" $
          whnf (consumeNoFusion . collectFrameworksDirs) xss_repeat
      ]
    , bgroup "nubInt"
      [ bench "no_fusion_distinct" $
          whnf (consumeNoFusion . LU.nubInt) xs_distinct
      , bench "no_fusion_repeat" $
          whnf (consumeNoFusion . LU.nubInt) xs_repeat
      , bench "issue1202_distinct" $
          whnf (consumeNoFusion . collectFrameworksDirs_nubInt) xss_distinct
      , bench "issue1202_repeat" $
          whnf (consumeNoFusion . collectFrameworksDirs_nubInt) xss_repeat
      ]
    ]
  where
    bound = 1000 :: Int
    xs_distinct = [1..bound]
    xs_repeat = replicate bound 1 :: [Int]
    xss_distinct = [[i] | i <- [1..bound]]
    xss_repeat = replicate bound [1] :: [[Int]]

-- Simple version of the case reported in
-- https://github.com/haskell/containers/issues/1202
collectFrameworksDirs :: [[Int]] -> [Int]
collectFrameworksDirs =
  map (*2) . LU.nubOrd . filter (/=0) . concatMap id
{-# NOINLINE collectFrameworksDirs #-}

collectFrameworksDirs_nubInt :: [[Int]] -> [Int]
collectFrameworksDirs_nubInt =
  map (*2) . LU.nubInt . filter (/=0) . concatMap id
{-# NOINLINE collectFrameworksDirs_nubInt #-}

consumeNoFusion :: [a] -> ()
consumeNoFusion = foldr (\_ z -> z) ()
{-# NOINLINE consumeNoFusion #-}
