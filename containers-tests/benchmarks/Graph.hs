module Main where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Data.Array (assocs, bounds)
import System.Random (mkStdGen, randomRs)
import Test.Tasty.Bench (bench, defaultMain, nf)
import qualified Data.Graph as G

main :: IO ()
main = do
  evaluate $ rnf randomG `seq` rnf randomGEdges `seq` rnf randomGAdjList
  defaultMain
    [ bench "buildG" $ nf (G.buildG (bounds randomG)) randomGEdges
    , bench "graphFromEdges" $ nf ((\(g, _, _) -> g) . G.graphFromEdges) randomGAdjList
    , bench "dfs" $ nf (flip G.dfs [1]) randomG
    , bench "dff" $ nf G.dff randomG
    , bench "topSort" $ nf G.topSort randomG
    , bench "scc" $ nf G.topSort randomG
    , bench "bcc_small" $ nf G.bcc smallRandomG
    , bench "stronglyConnCompR" $ nf G.stronglyConnCompR randomGAdjList
    ]
  where
    randomG = buildRandomGraph 10000 100000
    randomGEdges = G.edges randomG
    randomGAdjList = [(u, u, vs) | (u, vs) <- assocs randomG]
    smallRandomG = buildRandomGraph 1000 10000

-- Note: In practice it does not make sense to run topSort or bcc on a random
-- graph. For topSort the graph should be acyclic and for bcc the graph should
-- be undirected. But these functions don't check or depend on these properties,
-- so we can keep things simple and run them on random graphs in benchmarks.

-- A graph with vertices [1..n] and m random edges.
buildRandomGraph :: Int -> Int -> G.Graph
buildRandomGraph n m = G.buildG (1, n) (zip us vs)
  where
    xs = randomRs (1, n) (mkStdGen 1)
    (us, xs') = splitAt m xs
    vs = take m xs'
