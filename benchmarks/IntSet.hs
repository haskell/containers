{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Main
import Data.List (foldl')
import qualified Data.IntSet as S

instance NFData S.IntSet where
    rnf S.Nil = ()
    rnf (S.Tip a) = rnf a
    rnf (S.Bin p m l r) = rnf p `seq` rnf m `seq` rnf l `seq` rnf r

main = do
    let s = S.fromAscList elems :: S.IntSet
        s_even = S.fromAscList elems_even :: S.IntSet
        s_odd = S.fromAscList elems_odd :: S.IntSet
    defaultMainWith
        defaultConfig
        (liftIO . evaluate $ rnf [s, s_even, s_odd])
        [ bench "member" $ nf (member elems) s
        , bench "insert" $ nf (ins elems) S.empty
        , bench "map" $ nf (S.map (+ 1)) s
        , bench "filter" $ nf (S.filter ((== 0) . (`mod` 2))) s
        , bench "partition" $ nf (S.partition ((== 0) . (`mod` 2))) s
        , bench "fold" $ nf (S.fold (:) []) s
        , bench "delete" $ nf (del elems) s
        , bench "findMin" $ nf S.findMin s
        , bench "findMax" $ nf S.findMax s
        , bench "deleteMin" $ nf S.deleteMin s
        , bench "deleteMax" $ nf S.deleteMax s
        , bench "unions" $ nf S.unions [s_even, s_odd]
        , bench "union" $ nf (S.union s_even) s_odd
        , bench "difference" $ nf (S.difference s) s_even
        , bench "intersection" $ nf (S.intersection s) s_even
        ]
  where
    elems = [1..2^10]
    elems_even = [2,4..2^10]
    elems_odd = [1,3..2^10]

member :: [Int] -> S.IntSet -> Int
member xs s = foldl' (\n x -> if S.member x s then n + 1 else n) 0 xs

ins :: [Int] -> S.IntSet -> S.IntSet
ins xs s0 = foldl' (\s a -> S.insert a s) s0 xs

del :: [Int] -> S.IntSet -> S.IntSet
del xs s0 = foldl' (\s k -> S.delete k s) s0 xs
