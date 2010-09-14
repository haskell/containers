{-# LANGUAGE BangPatterns #-}

-- > ghc -DTESTING --make -O2 -fforce-recomp -i.. Set.hs
module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Main
import Data.List (foldl')
import qualified Data.Set as S

instance NFData a => NFData (S.Set a) where
    rnf S.Tip = ()
    rnf (S.Bin _ a l r) = rnf a `seq` rnf l `seq` rnf r

main = do
    let s = S.fromAscList elems :: S.Set Int
        s_even = S.fromAscList elems_even :: S.Set Int
        s_odd = S.fromAscList elems_odd :: S.Set Int
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

member :: [Int] -> S.Set Int -> Int
member xs s = foldl' (\n x -> if S.member x s then n + 1 else n) 0 xs

ins :: [Int] -> S.Set Int -> S.Set Int
ins xs s0 = foldl' (\s a -> S.insert a s) s0 xs

del :: [Int] -> S.Set Int -> S.Set Int
del xs s0 = foldl' (\s k -> S.delete k s) s0 xs
