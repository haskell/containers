module Main where

import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Data.Coerce (coerce)
import Data.Foldable (fold, foldl', toList)
import Data.Monoid (All(..))
import Test.Tasty.Bench (Benchmark, Benchmarkable, bench, bgroup, defaultMain, whnf, nf)
import qualified Data.Tree as T

main :: IO ()
main = do
  evaluate $ rnf ts `seq` rnf tsBool
  defaultMain
    [ bgroup "fold" $ forTs tsBool $ whnf fold . (coerce :: T.Tree Bool -> T.Tree All)
    , bgroup "foldMap" $ forTs tsBool $ whnf (foldMap All)
    , bgroup "foldr_1" $ forTs tsBool $ whnf (foldr (&&) True)
    , bgroup "foldr_2" $ forTs ts $ whnf (length . foldr (:) [])
    , bgroup "foldr_3" $ forTs ts $ whnf (\t -> foldr (\x k acc -> if acc < 0 then acc else k $! acc + x) id t 0)
    , bgroup "foldl'" $ forTs ts $ whnf (foldl' (+) 0)
    , bgroup "foldr1" $ forTs tsBool $ whnf (foldr1 (&&))
    , bgroup "foldl1" $ forTs ts $ whnf (foldl1 (+))
    , bgroup "toList" $ forTs ts $ nf toList
    , bgroup "elem" $ forTs ts $ whnf (elem 0)
    , bgroup "maximum" $ forTs ts $ whnf maximum
    , bgroup "sum" $ forTs ts $ whnf sum
    ]
  where
    ts = [binaryTree, lineTree] <*> [1000, 1000000]
    tsBool = [t { getT = True <$ getT t } | t <- ts]

forTs :: [Tree a] -> (T.Tree a -> Benchmarkable) -> [Benchmark]
forTs ts f = [bench label (f t) | Tree label t <- ts]

data Tree a = Tree
  { getLabel :: String
  , getT :: T.Tree a
  }

instance NFData a => NFData (Tree a) where
  rnf (Tree label t) = rnf label `seq` rnf t

binaryTree :: Int -> Tree Int
binaryTree n = Tree label t
  where
    label = "bin,n=" ++ show n
    t = T.unfoldTree (\x -> (x, takeWhile (<=n) [2*x, 2*x+1])) 1

lineTree :: Int -> Tree Int
lineTree n = Tree label t
  where
    label = "line,n=" ++ show n
    t = T.unfoldTree (\x -> (x, [x+1 | x+1 <= n])) 1
