import Data.Array (bounds, listArray)
import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.Foldable as F
import qualified Data.Graph as G
import qualified Data.List as L
import qualified Data.Set as S

default (Int)

main :: IO ()
main = defaultMain $ testGroup "graph-properties"
  [ testCase "buildG" test_buildG
  , testCase "graphFromEdges" test_graphFromEdges
  , testCase "dfs" test_dfs
  , testCase "dff" test_dff

  , testProperty "prop_dfs" prop_dfs
  , testProperty "prop_dff" prop_dff
  , testProperty "prop_topSort" prop_topSort
  , testProperty "prop_scc" prop_scc
  , testProperty "prop_bcc" prop_bcc
  , testProperty "prop_stronglyConnCompR" prop_stronglyConnCompR
  ]

----------------------------------------------------------------
-- Arbitrary graphs
----------------------------------------------------------------

newtype Graph = Graph G.Graph deriving Show

instance Arbitrary Graph where
  arbitrary = sized $ \sz0 -> do
    sz <- choose (0, sz0)
    l <- arbitrary
    let u = l + sz - 1
    edges <- if sz == 0
             then pure []
             else listOf $ (,) <$> choose (l,u) <*> choose (l,u)
    pure $ Graph $ G.buildG (l,u) edges

-- Directed acyclic graph
newtype DAG = DAG G.Graph deriving Show

instance Arbitrary DAG where
  arbitrary = sized $ \sz0 -> do
    sz <- choose (0, sz0)
    l <- arbitrary
    let u = l + sz - 1
    vs <- shuffle [l..u]
    -- edges are directed in the order in which their vertices appear in vs
    edges <- if sz <= 1
             then pure []
             else listOf $ ((,) <$> choose (l,u) <*> choose (l,u)) `suchThat`
                           \(from, to) -> fromJust (L.elemIndex from vs) < fromJust (L.elemIndex to vs)
    pure $ DAG $ G.buildG (l,u) edges

-- A graph where for every edge (u,v), the reverse edge (v,u) exists
newtype UndirectedG = UndirectedG G.Graph deriving Show

instance Arbitrary UndirectedG where
  arbitrary = do
    Graph g <- arbitrary
    let edges = G.edges g
    pure $ UndirectedG $ G.buildG (bounds g) (edges ++ [(v,u) | (u,v) <- edges])

newtype AdjList node key = AdjList [(node, key, [key])] deriving Show

instance (Arbitrary node, Arbitrary key, Eq key) => Arbitrary (AdjList node key) where
  arbitrary = do
    keys <- L.nub <$> arbitrary
    keyss <- vectorOf (length keys) arbitrary
    nodes <- vectorOf (length keys) arbitrary
    pure $ AdjList $ zip3 nodes keys keyss

----------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------

test_buildG :: Assertion
test_buildG = do
  G.buildG (1,0) [] @?= listArray (1,0) []
  G.buildG (1,1) [(1,1), (1,1), (1,1)] @?= listArray (1,1) [[1, 1, 1]]
  G.buildG (1,3) [(1,2), (1,3), (2,3)] @?= listArray (1,3) [[3, 2], [3], []]
  G.buildG (1,3) [(1,2), (1,3), (2,1), (2,3), (3,1), (3,2)] @?= listArray (1, 3) [[3, 2], [3, 1], [2, 1]]

test_graphFromEdges :: Assertion
test_graphFromEdges = do
  let (graph1, _, _) = G.graphFromEdges ([] :: [(Int, Int, [Int])])
  graph1 @?= listArray (0,-1) []

  let (graph2, nodeFromVertex2, vertexFromKey2) = G.graphFromEdges [('a', 10, [10])]
  graph2 @?= listArray (0,0) [[0]]
  nodeFromVertex2 0 @?= ('a', 10, [10])
  vertexFromKey2 10 @?= Just 0

  let (graph3, nodeFromVertex3, vertexFromKey3) = G.graphFromEdges [('b', 20, [30, 40]), ('a', 10, [20, 30, 40]), ('d', 40, []), ('c', 30, [40])]
  graph3 @?= listArray (0,3) [[1, 2, 3], [2, 3], [3], []]
  map nodeFromVertex3 [0..3] @?= [('a', 10, [20, 30, 40]), ('b', 20, [30, 40]), ('c', 30, [40]), ('d', 40, [])]
  map vertexFromKey3 [10, 20, 30, 40] @?= map Just [0..3]

test_dfs :: Assertion
test_dfs = do
  G.dfs (G.buildG (1,0) []) [] @?= []
  G.dfs (G.buildG (1,1) [(1,1), (1,1), (1,1)]) [1] @?= [G.Node 1 []]
  G.dfs (G.buildG (1,3) [(1,2), (1,3), (2,3)]) [1] @?= [G.Node 1 [G.Node 3 [], G.Node 2 []]]
  G.dfs (G.buildG (1,3) [(1,2), (1,3), (2,3)]) [2] @?= [G.Node 2 [G.Node 3 []]]
  G.dfs (G.buildG (1,3) [(1,2), (1,3), (2,3)]) [3] @?= [G.Node 3 []]
  G.dfs (G.buildG (1,3) [(1,2), (1,3), (2,3)]) [3,2,1] @?= [G.Node 3 [], G.Node 2 [], G.Node 1 []]

test_dff :: Assertion
test_dff = do
  G.dff (G.buildG (1,0) []) @?= []
  G.dff (G.buildG (1,1) [(1,1), (1,1), (1,1)]) @?= [G.Node 1 []]
  G.dff (G.buildG (1,3) [(1,2), (1,3), (2,3)]) @?= [G.Node 1 [G.Node 3 [], G.Node 2 []]]
  G.dff (G.buildG (1,3) [(1,2), (1,3), (2,1), (2,3), (3,1), (3,2)]) @?= [G.Node 1 [G.Node 3 [G.Node 2 []]]]

----------------------------------------------------------------
-- QuickCheck
----------------------------------------------------------------

-- Note: This tests some simple properties but not complete correctness
prop_dfs :: Graph -> Property
prop_dfs (Graph g) =
  let vsgen = if null (G.vertices g) then pure [] else listOf $ choose (bounds g)
  in forAll vsgen $ \vs ->
    let ts = G.dfs g vs
    in S.fromList (concatMap F.toList ts) `S.isSubsetOf` S.fromList (G.vertices g) .&&.
       S.fromList (concatMap treeEdges ts) `S.isSubsetOf` S.fromList (G.edges g)

-- Note: This tests some simple properties but not complete correctness
prop_dff :: Graph -> Property
prop_dff (Graph g) =
  let ts = G.dff g
  in L.sort (concatMap F.toList ts) === G.vertices g .&&.
     S.fromList (concatMap treeEdges ts) `S.isSubsetOf` S.fromList (G.edges g)

prop_topSort :: DAG -> Property
prop_topSort (DAG g) =
  let vs = G.topSort g
  in L.sort vs === G.vertices g .&&.
     and [not (G.path g v u) | u:vs' <- L.tails vs, v <- vs']

prop_scc :: Graph -> Property
prop_scc (Graph g) =
  let ts = G.scc g
  in L.sort (concatMap F.toList ts) === G.vertices g .&&.
     S.fromList (concatMap treeEdges ts) `S.isSubsetOf` S.fromList (G.edges g) .&&.
     -- vertices in a component are mutually reachable
     and [G.path g u v | t <- ts, u <- F.toList t, v <- F.toList t] .&&.
     -- vertices in later components are not reachable from earlier components, due to reverse
     -- topological order
     and [not (G.path g u v) | t:ts' <- L.tails ts, u <- F.toList t, v <- concatMap F.toList ts']

prop_bcc :: UndirectedG -> Property
prop_bcc (UndirectedG g) =
  let ts = G.bcc g
      comps = concatMap F.toList ts :: [[G.Vertex]]
  in S.fromList (concat comps) `S.isSubsetOf` S.fromList (G.vertices g) .&&.
     all testBCC comps .&&.
     all (uncurry testBCCs) (concatMap treeEdges ts)
  where
    -- a biconnected component remains connected even if any single vertex is removed
    testBCC c = and [subsetComponents (L.delete x c) == 1 | x <- c]
    -- adjacent biconnected components are connected, but become disconnected if their common
    -- vertex is removed
    testBCCs c1 c2 = case c1 `L.intersect` c2 of
      [x] -> subsetComponents (c1 ++ c2) == 1 &&
             subsetComponents ((c1 ++ c2) L.\\ [x, x]) == 2
      _   -> False
    -- the number of components in the given subset of vertices
    subsetComponents xs =
      let g' = G.buildG (bounds g) [(u,v) | (u,v) <- G.edges g, u `elem` xs && v `elem` xs]
      in length (G.dfs g' xs)

prop_stronglyConnCompR :: AdjList Int Int -> Property
prop_stronglyConnCompR (AdjList adj) =
  let comps = G.stronglyConnCompR adj
  in L.sort (G.flattenSCCs comps) === L.sort adj .&&.
     all testSCC comps .&&.
     -- vertices in later components are not reachable from earlier components, due to reverse
     -- topological order
     and [ not (G.path g (getv k) (getv k'))
         | c:cs <- L.tails comps
         , (_,k,_) <- G.flattenSCC c
         , (_,k',_) <- G.flattenSCCs cs
         ]
  where
    (g, _, vertexFromKey) = G.graphFromEdges adj
    getv = fromJust . vertexFromKey
    -- vertices in a cyclic component are mutually reachable
    testSCC (G.AcyclicSCC (_, k, ks)) = k `notElem` ks
    testSCC (G.CyclicSCC [(_, k, ks)]) = k `elem` ks
    testSCC (G.CyclicSCC xs) = and [G.path g (getv k) (getv k') | (_,k,_) <- xs , (_,k',_) <- xs]

treeEdges :: G.Tree a -> [(a, a)]
treeEdges t = go t []
  where go (G.Node x ts) acc = [(x,y) | G.Node y _ <- ts] ++ foldr go acc ts
