module Main where

import Control.DeepSeq (force, NFData(..))
import Control.Exception (evaluate)
import Criterion.Main (bench, bgroup, defaultMain, nf)
import Data.Graph(graphFromEdges, graphFromEdgesWithConsecutiveKeys, graphFromEdgesWithConsecutiveAscKeys)
import Data.Maybe(mapMaybe)
import System.Random (mkStdGen)
import System.Random.Shuffle(shuffle')

-- | Governs the average node degree in the generated graph.
-- Note that we cannot have cycles in the graph, else criterion's normal form
-- evaluation of the graph will not terminate.
data DegreeDistribution =
    Zero -- every node has 0 neighbour.
  | Constant -- every node hase up to a constant number of neighbours
  | LinearInVertices -- every node has a count of neigbours that is (up to)
                     -- proportional to the number of nodes.
  deriving (Show)

-- | Governs the order in wich nodes are passed to the function creating the graph.
data ConsecutiveKeys =
    AscendingConsecutive -- The keys are consecutive and ascending (e.g. [4,5,6,7] or [12,13])
  | UnsortedConsecutive -- The keys are consecutive but unsorted (e.g [6,5,7,4] or [13,12])
  deriving (Show)

instance NFData DegreeDistribution where
  rnf _ = ()

-- |
mkTests :: DegreeDistribution
        -- ^ Specifies the average number of neighbours for every node.
        -> Int
        -- ^ Count of nodes in the graph
        -> ConsecutiveKeys
        -- ^ Specifies nodes ordering.
        -> (String, [(Int,Int,[Int])])
        -- ^ Returns the test name, and nodes.
mkTests degree n keysType = (show n ++ "_" ++ keysName, map mkNode keys)
 where
  (keysName,keys) = case keysType of
    AscendingConsecutive -> ("ascending", ascendingKeys)
    UnsortedConsecutive -> ("unsorted", unsortedKeys)
  gen = mkStdGen 1
  ascendingKeys = [0..n-1] :: [Int]
  unsortedKeys = shuffle' ascendingKeys n gen

  mkNode k = (k, k, mapMaybe (mkNeighbour k) neighbourDistances)

  neighbourDistances = case degree of
    Zero -> []
    Constant -> [1..2]
    LinearInVertices -> [1..quot n 5]

  mkNeighbour k distance
    | neighbour < n = Just neighbour
    | otherwise = Nothing
    where
      neighbour = k + distance

main :: IO ()
main = do
  let nodeCounts =
        [ 10
        , 100
        , 1000
        ]
      tests = map
        (\degree -> (degree, map (mkTests degree) nodeCounts))
        [ LinearInVertices
        , Constant
        , Zero
        ]

  _ <- evaluate $ force tests

  -- The tests verifies that to create a graph from a list of nodes with consecutive keys,
  -- graphFromEdgesWithConsecutiveKeys and graphFromEdgesWithConsecutiveAscKeys
  -- are faster than graphFromEdges.
  --
  -- Respective time complexities are:
  --
  -- graphFromEdges                       :   O( (E+V) * log V )
  -- graphFromEdgesWithConsecutiveKeys    :   O( E + (V*log V) )
  -- graphFromEdgesWithConsecutiveAscKeys :   O( E + V )
  --
  -- The test also shows that the bigger the degree of the graph (see 'DegreeDistribution'),
  -- the more pronounced the time difference is, which is to be expected given the complexities.
  defaultMain $ map
    (\(degree, testsForDegree) -> bgroup ("Degree_" ++ show degree) $ concatMap
      (\mkKeys ->
        let ascendingCompatible =
              (graphFromEdgesWithConsecutiveAscKeys, "graphFromEdgesWithConsecutiveAscKeys") :
              randomCompatible
            randomCompatible =
              [ (graphFromEdgesWithConsecutiveKeys, "graphFromEdgesWithConsecutiveKeys")
              , (graphFromEdges, "graphFromEdges")
              ]
            mkBench nameKeys keys createGraph label =
              bench (nameKeys ++ "_" ++ label) $ nf createGraph keys
        in map (uncurry $ uncurry mkBench $ mkKeys AscendingConsecutive) ascendingCompatible ++
           map (uncurry $ uncurry mkBench $ mkKeys UnsortedConsecutive) randomCompatible
        )
      testsForDegree)
    tests
