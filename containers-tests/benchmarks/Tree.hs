{-# LANGUAGE BangPatterns #-}
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)

import Data.Foldable
import Data.Monoid
import Test.Tasty.Bench

import Data.Tree

main :: IO ()
main = do
    let lim = 1000 * 1000
        t :: Tree Int
        t = unfoldTree (\x -> (x, filter (<=lim) [2*x, 2*x+1])) 1

    evaluate $ rnf [t]
    defaultMain
        [ bench "foldMap'Tree Sum" $ whnf (foldMap'Tree Sum) t
        , bench "sum"              $ whnf sum t
        ]

foldMap'Tree :: Monoid m => (a -> m) -> Tree a -> m
foldMap'Tree f = go mempty
  where go !z (Node x ts) = foldl' go (z <> f x) ts
