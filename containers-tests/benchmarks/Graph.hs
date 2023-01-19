module Main where

import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Data.Array (assocs, bounds)
import System.Random (mkStdGen, randomRs)
import Test.Tasty.Bench (Benchmark, Benchmarkable, bench, bgroup, defaultMain, nf)
import qualified Data.Graph as G

main :: IO ()
main = do
  evaluate $ rnf randGs
  defaultMain
    [ bgroup "buildG" $ forGs randGs $ \g -> nf (G.buildG (bounds (getG g))) (getEdges g)
    , bgroup "graphFromEdges" $ forGs randGs $ nf ((\(g, _, _) -> g) . G.graphFromEdges) . getAdjList
    , bgroup "transposeG" $ forGs randGs $ nf G.transposeG . getG
    , bgroup "dfs" $ forGs randGs $ nf (flip G.dfs [1]) . getG
    , bgroup "dff" $ forGs randGs $ nf G.dff . getG
    , bgroup "topSort" $ forGs randGs $ nf G.topSort . getG
    , bgroup "scc" $ forGs randGs $ nf G.scc . getG
    , bgroup "bcc" $ forGs randGs $ nf G.bcc . getG
    , bgroup "stronglyConnCompR" $ forGs randGs $ nf G.stronglyConnCompR . getAdjList
    ]
  where
    randG1 = buildRandG 100 1000 
    randG2 = buildRandG 100 10000
    randG3 = buildRandG 10000 100000
    randG4 = buildRandG 100000 1000000
    randGs = [randG1, randG2, randG3, randG4]

-- Note: In practice it does not make sense to run topSort or bcc on a random
-- graph. For topSort the graph should be acyclic and for bcc the graph should
-- be undirected. But these functions don't check or depend on these properties,
-- so we can keep things simple and run them on random graphs in benchmarks.

forGs :: [Graph] -> (Graph -> Benchmarkable) -> [Benchmark]
forGs gs f = [bench (getLabel g) (f g) | g <- gs]

data Graph = Graph
  { getLabel :: String
  , getG :: G.Graph
  , getEdges :: [(G.Vertex, G.Vertex)]
  , getAdjList :: [(Int, G.Vertex, [G.Vertex])]
  }

instance NFData Graph where
  rnf (Graph label g edges adj) = rnf label `seq` rnf g `seq` rnf edges `seq` rnf adj

-- A graph with vertices [1..n] and m random edges.
buildRandG :: Int -> Int -> Graph
buildRandG n m = Graph label g (G.edges g) [(u, u, vs') | (u, vs') <- assocs g]
  where
    label = "n=" ++ show n ++ ",m=" ++ show m
    xs = randomRs (1, n) (mkStdGen 1)
    (us, xs') = splitAt m xs
    vs = take m xs'
    g = G.buildG (1, n) (zip us vs)
