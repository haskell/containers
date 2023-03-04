module Main where

import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Data.Array (assocs, bounds)
import System.Random (mkStdGen, randomRs)
import Test.Tasty.Bench (Benchmark, Benchmarkable, bench, bgroup, defaultMain, nf)
import qualified Data.Graph as G

main :: IO ()
main = do
  evaluate $ rnf allGs
  defaultMain
    [ bgroup "buildG" $ forGs allGs $ \g -> nf (G.buildG (bounds (getG g))) (getEdges g)
    , bgroup "graphFromEdges" $ forGs allGs $ nf ((\(g, _, _) -> g) . G.graphFromEdges) . getAdjList
    , bgroup "transposeG" $ forGs allGs $ nf G.transposeG . getG
    , bgroup "dfs" $ forGs allGs $ nf (flip G.dfs [1]) . getG
    , bgroup "dff" $ forGs allGs $ nf G.dff . getG
    , bgroup "topSort" $ forGs allGs $ nf G.topSort . getG
    , bgroup "scc" $ forGs allGs $ nf G.scc . getG
    , bgroup "bcc" $ forGs allGs $ nf G.bcc . getG
    , bgroup "stronglyConnCompR" $ forGs allGs $ nf G.stronglyConnCompR . getAdjList
    ]
  where
    allGs = randGs ++ starGs ++ lineGs ++ maxDAGs
    randGs = map (uncurry buildRandG) [(100, 1000), (100, 10000), (10000, 100000), (100000, 1000000)]
    starGs = map buildStarG [100, 1000000]
    lineGs = map buildLineG [100, 1000000]
    maxDAGs = map buildMaxDAG [15, 1500]

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

-- Makes a Graph for benchmarks, from a label, vertex bounds, and the edge list.
makeG :: String -> G.Bounds -> [G.Edge] -> Graph
makeG label bnds edges =
  let g = G.buildG bnds edges
  in Graph label g edges [(u, u, vs) | (u, vs) <- assocs g]

-- A graph with vertices [1..n] and m random edges.
buildRandG :: Int -> Int -> Graph
buildRandG n m = makeG label (1, n) edges
  where
    label = "rand,n=" ++ show n ++ ",m=" ++ show m
    xs = randomRs (1, n) (mkStdGen 1)
    (us, xs') = splitAt m xs
    vs = take m xs'
    edges = zip us vs

-- A star graph, i.e. a graph with an edge from vertex 1 to every other vertex.
-- This serves as an extreme case of a "wide" graph.
buildStarG :: Int -> Graph
buildStarG n = makeG label (1, n) [(1, i) | i <- [2..n]]
  where
    label = "star,n=" ++ show n

-- A line graph, i.e. a graph with an edge from every vertex i to i+1. This
-- serves an as extreme case of a "deep" graph.
buildLineG :: Int -> Graph
buildLineG n = makeG label (1, n) (zip [1..n-1] [2..])
  where
    label = "line,n=" ++ show n

-- A maximal DAG. There is an edge from vertex i to j for every i, j when i < j.
-- The number of edges is n * (n - 1) / 2.
buildMaxDAG :: Int -> Graph
buildMaxDAG n = makeG label (1, n) [(i, j) | i <- [1 .. n-1], j <- [i+1 .. n]]
  where
    label = "maxDAG,n=" ++ show n
