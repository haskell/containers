{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- = Finite Graphs
--
-- The @'Graph'@ type is an adjacency list representation of a finite, directed
-- graph with vertices of type @Int@.
--
-- The @'SCC'@ type represents a
-- <https://en.wikipedia.org/wiki/Strongly_connected_component strongly-connected component>
-- of a graph.
--
-- == Implementation
--
-- The implementation is based on
--
--   * /Structuring Depth-First Search Algorithms in Haskell/,
--     by David King and John Launchbury, <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.52.6526>
--
-----------------------------------------------------------------------------

module Data.Graph (

    -- * Graphs
      Graph
    , Bounds
    , Edge
    , Vertex
    , Table

    -- ** Graph Construction
    , graphFromEdges
    , graphFromEdges'
    , buildG

    -- ** Graph Properties
    , vertices
    , edges
    , outdegree
    , indegree

    -- ** Graph Transformations
    , transposeG

    -- ** Graph Algorithms
    , dfs
    , dff
    , topSort
    , reverseTopSort
    , components
    , scc
    , bcc
    , reachable
    , path


    -- * Strongly Connected Components
    , SCC(..
#ifdef __GLASGOW_HASKELL__
      , CyclicSCC
#endif
      )

    -- ** Construction
    , stronglyConnComp
    , stronglyConnCompR

    -- ** Conversion
    , flattenSCC
    , flattenSCCs

    -- * Trees
    , module Data.Tree

    ) where

import Utils.Containers.Internal.Prelude
import Prelude ()
#if USE_ST_MONAD
import Control.Monad.ST
import Data.Array.ST.Safe (newArray, readArray, writeArray)
# if USE_UNBOXED_ARRAYS
import Data.Array.ST.Safe (STUArray)
# else
import Data.Array.ST.Safe (STArray)
# endif
#else
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
#endif
import Data.Tree (Tree(Node), Forest)

-- std interfaces
import Data.Foldable as F
#if MIN_VERSION_base(4,18,0)
import qualified Data.Foldable1 as F1
#endif
import Control.DeepSeq (NFData(rnf))
import Data.Maybe
import Data.Array
#if USE_UNBOXED_ARRAYS
import qualified Data.Array.Unboxed as UA
import Data.Array.Unboxed ( UArray )
#else
import qualified Data.Array as UA
#endif
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Functor.Classes
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
#ifdef __GLASGOW_HASKELL__
import GHC.Generics (Generic, Generic1)
import Data.Data (Data)
import Language.Haskell.TH.Syntax (Lift(..))
-- See Note [ Template Haskell Dependencies ]
import Language.Haskell.TH ()
#endif

-- Make sure we don't use Integer by mistake.
default ()

-------------------------------------------------------------------------
--                                                                      -
--      Strongly Connected Components
--                                                                      -
-------------------------------------------------------------------------

-- | Strongly connected component.
data SCC vertex
  = AcyclicSCC vertex
  -- ^ A single vertex that is not in any cycle.
  | NECyclicSCC {-# UNPACK #-} !(NonEmpty vertex)
  -- ^ A maximal set of mutually reachable vertices.
  --
  -- @since 0.7.0
  deriving ( Eq   -- ^ @since 0.5.9
           , Show -- ^ @since 0.5.9
           , Read -- ^ @since 0.5.9
           )

-- | Partial pattern synonym for backward compatibility with @containers < 0.7@.
pattern CyclicSCC :: [vertex] -> SCC vertex
pattern CyclicSCC xs <- NECyclicSCC (NE.toList -> xs) where
  CyclicSCC [] = error "CyclicSCC: an argument cannot be an empty list"
  CyclicSCC (x : xs) = NECyclicSCC (x :| xs)

{-# COMPLETE AcyclicSCC, CyclicSCC #-}

#ifdef __GLASGOW_HASKELL__
-- | @since 0.5.9
deriving instance Data vertex => Data (SCC vertex)

-- | @since 0.5.9
deriving instance Generic1 SCC

-- | @since 0.5.9
deriving instance Generic (SCC vertex)

-- There is no instance Lift (NonEmpty v) before template-haskell-2.15.
#if MIN_VERSION_template_haskell(2,15,0)
-- | @since 0.6.6
deriving instance Lift vertex => Lift (SCC vertex)
#else
instance Lift vertex => Lift (SCC vertex) where
  lift (AcyclicSCC v) = [| AcyclicSCC v |]
  lift (NECyclicSCC (v :| vs)) = [| NECyclicSCC (v :| vs) |]
#endif

#endif

-- | @since 0.5.9
instance Eq1 SCC where
  liftEq eq (AcyclicSCC v1) (AcyclicSCC v2) = eq v1 v2
  liftEq eq (NECyclicSCC vs1) (NECyclicSCC vs2) = liftEq eq vs1 vs2
  liftEq _ _ _ = False
-- | @since 0.5.9
instance Show1 SCC where
  liftShowsPrec sp _sl d (AcyclicSCC v) = showsUnaryWith sp "AcyclicSCC" d v
  liftShowsPrec sp sl d (NECyclicSCC vs) = showsUnaryWith (liftShowsPrec sp sl) "NECyclicSCC" d vs
-- | @since 0.5.9
instance Read1 SCC where
  liftReadsPrec rp rl = readsData $
    readsUnaryWith rp "AcyclicSCC" AcyclicSCC <>
    readsUnaryWith (liftReadsPrec rp rl) "NECyclicSCC" NECyclicSCC <>
    readsUnaryWith (const rl) "CyclicSCC" CyclicSCC

-- | @since 0.5.9
instance F.Foldable SCC where
  foldr c n (AcyclicSCC v) = c v n
  foldr c n (NECyclicSCC vs) = foldr c n vs

#if MIN_VERSION_base(4,18,0)
-- | @since 0.7.0
instance F1.Foldable1 SCC where
  foldMap1 f (AcyclicSCC v) = f v
  foldMap1 f (NECyclicSCC vs) = F1.foldMap1 f vs
  -- TODO define more methods
#endif

-- | @since 0.5.9
instance Traversable SCC where
  traverse f (AcyclicSCC vertex) = AcyclicSCC <$> f vertex
  -- Avoid traverse from instance Traversable NonEmpty,
  -- it is redundantly lazy.
  traverse f (NECyclicSCC (x :| xs)) =
    liftA2 (\x' xs' -> NECyclicSCC (x' :| xs')) (f x) (traverse f xs)

instance NFData a => NFData (SCC a) where
    rnf (AcyclicSCC v) = rnf v
    rnf (NECyclicSCC vs) = rnf vs

-- | @since 0.5.4
instance Functor SCC where
    fmap f (AcyclicSCC v) = AcyclicSCC (f v)
    -- Avoid fmap from instance Functor NonEmpty,
    -- it is redundantly lazy.
    fmap f (NECyclicSCC (x :| xs)) = NECyclicSCC (f x :| map f xs)

-- | The vertices of a list of strongly connected components.
flattenSCCs :: [SCC a] -> [a]
flattenSCCs = concatMap flattenSCC

-- | The vertices of a strongly connected component.
flattenSCC :: SCC vertex -> [vertex]
flattenSCC (AcyclicSCC v) = [v]
flattenSCC (NECyclicSCC vs) = NE.toList vs

-- | \(O((V+E) \log V)\). The strongly connected components of a directed graph,
-- reverse topologically sorted.
--
-- ==== __Examples__
--
-- > stronglyConnComp [("a",0,[1]),("b",1,[2,3]),("c",2,[1]),("d",3,[3])]
-- >   == [CyclicSCC ["d"],CyclicSCC ["b","c"],AcyclicSCC "a"]
stronglyConnComp
        :: Ord key
        => [(node, key, [key])]
                -- ^ The graph: a list of nodes uniquely identified by keys,
                -- with a list of keys of nodes this node has edges to.
                -- The out-list may contain keys that don't correspond to
                -- nodes of the graph; such edges are ignored.
        -> [SCC node]

stronglyConnComp edges0
  = map get_node (stronglyConnCompR edges0)
  where
    get_node (AcyclicSCC (n, _, _)) = AcyclicSCC n
    get_node (NECyclicSCC ((n0, _, _) :| triples)) =
      NECyclicSCC (n0 :| [n | (n, _, _) <- triples])
{-# INLINABLE stronglyConnComp #-}

-- | \(O((V+E) \log V)\). The strongly connected components of a directed graph,
-- reverse topologically sorted.  The function is the same as
-- 'stronglyConnComp', except that all the information about each node retained.
-- This interface is used when you expect to apply 'SCC' to
-- (some of) the result of 'SCC', so you don't want to lose the
-- dependency information.
--
-- ==== __Examples__
--
-- > stronglyConnCompR [("a",0,[1]),("b",1,[2,3]),("c",2,[1]),("d",3,[3])]
-- >  == [CyclicSCC [("d",3,[3])],CyclicSCC [("b",1,[2,3]),("c",2,[1])],AcyclicSCC ("a",0,[1])]
stronglyConnCompR
        :: Ord key
        => [(node, key, [key])]
                -- ^ The graph: a list of nodes uniquely identified by keys,
                -- with a list of keys of nodes this node has edges to.
                -- The out-list may contain keys that don't correspond to
                -- nodes of the graph; such edges are ignored.
        -> [SCC (node, key, [key])]     -- ^ Reverse topologically sorted

stronglyConnCompR [] = []  -- added to avoid creating empty array in graphFromEdges -- SOF
stronglyConnCompR edges0
  = map decode forest
  where
    (graph, vertex_fn,_) = graphFromEdges edges0
    forest             = scc graph

    decode (Node v []) | mentions_itself v = NECyclicSCC (vertex_fn v :| [])
                       | otherwise         = AcyclicSCC (vertex_fn v)
    decode (Node v ts) = NECyclicSCC (vertex_fn v :| foldr dec [] ts)

    dec (Node v ts) vs = vertex_fn v : foldr dec vs ts
    mentions_itself v = v `elem` (graph ! v)
{-# INLINABLE stronglyConnCompR #-}

-------------------------------------------------------------------------
--                                                                      -
--      Graphs
--                                                                      -
-------------------------------------------------------------------------

-- | Abstract representation of vertices.
type Vertex  = Int
-- | Table indexed by a contiguous set of vertices.
--
-- /Note: This is included for backwards compatibility./
type Table a = Array Vertex a
-- | Adjacency list representation of a graph, mapping each vertex to its
-- list of successors.
type Graph   = Array Vertex [Vertex]
-- | The bounds of an @Array@.
type Bounds  = (Vertex, Vertex)
-- | An edge from the first vertex to the second.
type Edge    = (Vertex, Vertex)

#if !USE_UNBOXED_ARRAYS
type UArray i a = Array i a
#endif

-- | \(O(V)\). Returns the list of vertices in the graph.
--
-- ==== __Examples__
--
-- > vertices (buildG (0,-1) []) == []
--
-- > vertices (buildG (0,2) [(0,1),(1,2)]) == [0,1,2]
vertices :: Graph -> [Vertex]
vertices  = indices
-- See Note [Inline for fusion]
{-# INLINE vertices #-}

-- | \(O(V+E)\). Returns the list of edges in the graph.
--
-- ==== __Examples__
--
-- > edges (buildG (0,-1) []) == []
--
-- > edges (buildG (0,2) [(0,1),(1,2)]) == [(0,1),(1,2)]
edges    :: Graph -> [Edge]
edges g   = [ (v, w) | v <- vertices g, w <- g!v ]
-- See Note [Inline for fusion]
{-# INLINE edges #-}

-- | \(O(V+E)\). Build a graph from a list of edges.
--
-- Warning: This function will cause a runtime exception if a vertex in the edge
-- list is not within the given @Bounds@.
--
-- ==== __Examples__
--
-- > buildG (0,-1) [] == array (0,-1) []
-- > buildG (0,2) [(0,1), (1,2)] == array (0,1) [(0,[1]),(1,[2])]
-- > buildG (0,2) [(0,1), (0,2), (1,2)] == array (0,2) [(0,[2,1]),(1,[2]),(2,[])]
buildG :: Bounds -> [Edge] -> Graph
buildG = accumArray (flip (:)) []
-- See Note [Inline for fusion]
{-# INLINE buildG #-}

-- | \(O(V+E)\). The graph obtained by reversing all edges.
--
-- ==== __Examples__
--
-- > transposeG (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,[]),(1,[0]),(2,[1])]
transposeG  :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

reverseE    :: Graph -> [Edge]
reverseE g   = [ (w, v) | (v, w) <- edges g ]
-- See Note [Inline for fusion]
{-# INLINE reverseE #-}

-- | \(O(V+E)\). A table of the count of edges from each node.
--
-- ==== __Examples__
--
-- > outdegree (buildG (0,-1) []) == array (0,-1) []
--
-- > outdegree (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,1),(1,1),(2,0)]
outdegree :: Graph -> Array Vertex Int
-- This is bizarrely lazy. We build an array filled with thunks, instead
-- of actually calculating anything. This is the historical behavior, and I
-- suppose someone *could* be relying on it, but it might be worth finding
-- out. Note that we *can't* be so lazy with indegree.
outdegree  = fmap length

-- | \(O(V+E)\). A table of the count of edges into each node.
--
-- ==== __Examples__
--
-- > indegree (buildG (0,-1) []) == array (0,-1) []
--
-- > indegree (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,0),(1,1),(2,1)]
indegree :: Graph -> Array Vertex Int
indegree g = accumArray (+) 0 (bounds g) [(v, 1) | (_, outs) <- assocs g, v <- outs]

-- | \(O((V+E) \log V)\). Identical to 'graphFromEdges', except that the return
-- value does not include the function which maps keys to vertices. This
-- version of 'graphFromEdges' is for backwards compatibility.
graphFromEdges'
        :: Ord key
        => [(node, key, [key])]
        -> (Graph, Vertex -> (node, key, [key]))
graphFromEdges' x = (a,b) where
    (a,b,_) = graphFromEdges x
{-# INLINABLE graphFromEdges' #-}

-- | \(O((V+E) \log V)\). Build a graph from a list of nodes uniquely identified
-- by keys, with a list of keys of nodes this node should have edges to.
--
-- This function takes an adjacency list representing a graph with vertices of
-- type @key@ labeled by values of type @node@ and produces a @Graph@-based
-- representation of that list. The @Graph@ result represents the /shape/ of the
-- graph, and the functions describe a) how to retrieve the label and adjacent
-- vertices of a given vertex, and b) how to retrieve a vertex given a key.
--
-- @(graph, nodeFromVertex, vertexFromKey) = graphFromEdges edgeList@
--
-- * @graph :: Graph@ is the raw, array based adjacency list for the graph.
-- * @nodeFromVertex :: Vertex -> (node, key, [key])@ returns the node
--   associated with the given 0-based @Int@ vertex; see /warning/ below. This
--   runs in \(O(1)\) time.
-- * @vertexFromKey :: key -> Maybe Vertex@ returns the @Int@ vertex for the
--   key if it exists in the graph, @Nothing@ otherwise. This runs in
--   \(O(\log V)\) time.
--
-- To safely use this API you must either extract the list of vertices directly
-- from the graph or first call @vertexFromKey k@ to check if a vertex
-- corresponds to the key @k@. Once it is known that a vertex exists you can use
-- @nodeFromVertex@ to access the labelled node and adjacent vertices. See below
-- for examples.
--
-- Note: The out-list may contain keys that don't correspond to nodes of the
-- graph; they are ignored.
--
-- Warning: The @nodeFromVertex@ function will cause a runtime exception if the
-- given @Vertex@ does not exist.
--
-- ==== __Examples__
--
-- An empty graph.
--
-- > (graph, nodeFromVertex, vertexFromKey) = graphFromEdges []
-- > graph = array (0,-1) []
--
-- A graph where the out-list references unspecified nodes (@\'c\'@), these are
-- ignored.
--
-- > (graph, _, _) = graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c'])]
-- > array (0,1) [(0,[1]),(1,[])]
--
--
-- A graph with 3 vertices: ("a") -> ("b") -> ("c")
--
-- > (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c']), ("c", 'c', [])]
-- > graph == array (0,2) [(0,[1]),(1,[2]),(2,[])]
-- > nodeFromVertex 0 == ("a",'a',"b")
-- > vertexFromKey 'a' == Just 0
--
-- Get the label for a given key.
--
-- > let getNodePart (n, _, _) = n
-- > (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c']), ("c", 'c', [])]
-- > getNodePart . nodeFromVertex <$> vertexFromKey 'a' == Just "A"
--
graphFromEdges
        :: Ord key
        => [(node, key, [key])]
        -> (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)
graphFromEdges edges0
  = (graph, \v -> vertex_map ! v, key_vertex)
  where
    max_v           = length edges0 - 1
    bounds0         = (0,max_v) :: (Vertex, Vertex)
    sorted_edges    = L.sortBy lt edges0
    edges1          = zipWith (,) [0..] sorted_edges

    graph           = array bounds0 [(,) v (mapMaybe key_vertex ks) | (,) v (_,    _, ks) <- edges1]
    key_map         = array bounds0 [(,) v k                       | (,) v (_,    k, _ ) <- edges1]
    vertex_map      = array bounds0 edges1

    (_,k1,_) `lt` (_,k2,_) = k1 `compare` k2

    -- key_vertex :: key -> Maybe Vertex
    --  returns Nothing for non-interesting vertices
    key_vertex k   = findVertex 0 max_v
                   where
                     findVertex a b | a > b
                              = Nothing
                     findVertex a b = case compare k (key_map ! mid) of
                                   LT -> findVertex a (mid-1)
                                   EQ -> Just mid
                                   GT -> findVertex (mid+1) b
                              where
                                mid = a + (b - a) `div` 2
{-# INLINABLE graphFromEdges #-}

-------------------------------------------------------------------------
--                                                                      -
--      Depth first search
--                                                                      -
-------------------------------------------------------------------------

-- | \(O(V+E)\). A spanning forest of the graph, obtained from a depth-first
-- search of the graph starting from each vertex in an unspecified order.
dff          :: Graph -> [Tree Vertex]
dff g         = dfs g (vertices g)

-- | \(O(V+E)\). A spanning forest of the part of the graph reachable from the
-- listed vertices, obtained from a depth-first search of the graph starting at
-- each of the listed vertices in order.

-- This function deviates from King and Launchbury's implementation by
-- bundling together the functions generate, prune, and chop for efficiency
-- reasons.
dfs :: Graph -> [Vertex] -> [Tree Vertex]
dfs g vs0 = run (bounds g) $ go vs0
  where
    go :: [Vertex] -> SetM s [Tree Vertex]
    go [] = pure []
    go (v:vs) = do
      visited <- contains v
      if visited
      then go vs
      else do
        include v
        as <- go (g!v)
        bs <- go vs
        pure $ Node v as : bs

-- A monad holding a set of vertices visited so far.
#if USE_ST_MONAD

-- Use the ST monad if available, for constant-time primitives.

#if USE_UNBOXED_ARRAYS
newtype SetM s a = SetM { runSetM :: STUArray s Vertex Bool -> ST s a }
#else
newtype SetM s a = SetM { runSetM :: STArray  s Vertex Bool -> ST s a }
#endif

instance Monad (SetM s) where
    return = pure
    {-# INLINE return #-}
    SetM v >>= f = SetM $ \s -> do { x <- v s; runSetM (f x) s }
    {-# INLINE (>>=) #-}

instance Functor (SetM s) where
    f `fmap` SetM v = SetM $ \s -> f `fmap` v s
    {-# INLINE fmap #-}

instance Applicative (SetM s) where
    pure x = SetM $ const (return x)
    {-# INLINE pure #-}
    SetM f <*> SetM v = SetM $ \s -> f s >>= (`fmap` v s)
    -- We could also use the following definition
    --   SetM f <*> SetM v = SetM $ \s -> f s <*> v s
    -- but Applicative (ST s) instance is present only in GHC 7.2+
    {-# INLINE (<*>) #-}

run          :: Bounds -> (forall s. SetM s a) -> a
run bnds act  = runST (newArray bnds False >>= runSetM act)

contains     :: Vertex -> SetM s Bool
contains v    = SetM $ \ m -> readArray m v

include      :: Vertex -> SetM s ()
include v     = SetM $ \ m -> writeArray m v True

#else /* !USE_ST_MONAD */

-- Portable implementation using IntSet.

newtype SetM s a = SetM { runSetM :: IntSet -> (a, IntSet) }

instance Monad (SetM s) where
    return x     = SetM $ \s -> (x, s)
    SetM v >>= f = SetM $ \s -> case v s of (x, s') -> runSetM (f x) s'

instance Functor (SetM s) where
    f `fmap` SetM v = SetM $ \s -> case v s of (x, s') -> (f x, s')
    {-# INLINE fmap #-}

instance Applicative (SetM s) where
    pure x = SetM $ \s -> (x, s)
    {-# INLINE pure #-}
    SetM f <*> SetM v = SetM $ \s -> case f s of (k, s') -> case v s' of (x, s'') -> (k x, s'')
    {-# INLINE (<*>) #-}

run          :: Bounds -> SetM s a -> a
run _ act     = fst (runSetM act Set.empty)

contains     :: Vertex -> SetM s Bool
contains v    = SetM $ \ m -> (Set.member v m, m)

include      :: Vertex -> SetM s ()
include v     = SetM $ \ m -> ((), Set.insert v m)

#endif /* !USE_ST_MONAD */

-------------------------------------------------------------------------
--                                                                      -
--      Algorithms
--                                                                      -
-------------------------------------------------------------------------

------------------------------------------------------------
-- Algorithm 1: depth first search numbering
------------------------------------------------------------

preorder' :: Tree a -> [a] -> [a]
preorder' (Node a ts) = (a :) . preorderF' ts

preorderF' :: [Tree a] -> [a] -> [a]
preorderF' ts = foldr (.) id $ map preorder' ts

preorderF :: [Tree a] -> [a]
preorderF ts = preorderF' ts []

tabulate        :: Bounds -> [Vertex] -> UArray Vertex Int
tabulate bnds vs = UA.array bnds (zipWith (flip (,)) [1..] vs)
-- Why zipWith (flip (,)) instead of just using zip with the
-- arguments in the other order? We want the [1..] to fuse
-- away, and these days that only happens when it's the first
-- list argument.

preArr          :: Bounds -> [Tree Vertex] -> UArray Vertex Int
preArr bnds      = tabulate bnds . preorderF

------------------------------------------------------------
-- Algorithm 2: topological sorting
------------------------------------------------------------

postorder :: Tree a -> [a] -> [a]
postorder (Node a ts) = postorderF ts . (a :)

postorderF   :: [Tree a] -> [a] -> [a]
postorderF ts = foldr (.) id $ map postorder ts

postOrd :: Graph -> [Vertex]
postOrd g = postorderF (dff g) []

-- | \(O(V+E)\). A topological sort of the graph.
-- The order is partially specified by the condition that a vertex /i/
-- precedes /j/ whenever /j/ is reachable from /i/ but not vice versa.
--
-- Note: A topological sort exists only when there are no cycles in the graph.
-- If the graph has cycles, the output of this function will not be a
-- topological sort. In such a case consider using 'scc'.
topSort      :: Graph -> [Vertex]
topSort       = reverse . postOrd

-- | \(O(V+E)\). Reverse ordering of `topSort`.
--
-- See note in 'topSort'.
--
-- @since 0.6.4
reverseTopSort :: Graph -> [Vertex]
reverseTopSort = postOrd

------------------------------------------------------------
-- Algorithm 3: connected components
------------------------------------------------------------

-- | \(O(V+E)\). The connected components of a graph.
-- Two vertices are connected if there is a path between them, traversing
-- edges in either direction.
components   :: Graph -> [Tree Vertex]
components    = dff . undirected

undirected   :: Graph -> Graph
undirected g  = buildG (bounds g) (edges g ++ reverseE g)

-- Algorithm 4: strongly connected components

-- | \(O(V+E)\). The strongly connected components of a graph, in reverse
-- topological order.
--
-- ==== __Examples__
--
-- > scc (buildG (0,3) [(3,1),(1,2),(2,0),(0,1)])
-- >   == [Node {rootLabel = 0, subForest = [Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []}]}]}
-- >      ,Node {rootLabel = 3, subForest = []}]

scc  :: Graph -> [Tree Vertex]
scc g = dfs g (reverse (postOrd (transposeG g)))

------------------------------------------------------------
-- Algorithm 5: Classifying edges
------------------------------------------------------------

{-
XXX unused code

tree              :: Bounds -> Forest Vertex -> Graph
tree bnds ts       = buildG bnds (concat (map flat ts))
 where flat (Node v ts') = [ (v, w) | Node w _us <- ts' ]
                        ++ concat (map flat ts')

back              :: Graph -> Table Int -> Graph
back g post        = mapT select g
 where select v ws = [ w | w <- ws, post!v < post!w ]

cross             :: Graph -> Table Int -> Table Int -> Graph
cross g pre post   = mapT select g
 where select v ws = [ w | w <- ws, post!v > post!w, pre!v > pre!w ]

forward           :: Graph -> Graph -> Table Int -> Graph
forward g tree' pre = mapT select g
 where select v ws = [ w | w <- ws, pre!v < pre!w ] \\ tree' ! v

mapT    :: (Vertex -> a -> b) -> Array Vertex a -> Array Vertex b
mapT f t = array (bounds t) [ (,) v (f v (t!v)) | v <- indices t ]
-}

------------------------------------------------------------
-- Algorithm 6: Finding reachable vertices
------------------------------------------------------------

-- | \(O(V+E)\). Returns the list of vertices reachable from a given vertex.
--
-- ==== __Examples__
--
-- > reachable (buildG (0,0) []) 0 == [0]
--
-- > reachable (buildG (0,2) [(0,1), (1,2)]) 0 == [0,1,2]
reachable :: Graph -> Vertex -> [Vertex]
reachable g v = preorderF (dfs g [v])

-- | \(O(V+E)\). Returns @True@ if the second vertex reachable from the first.
--
-- ==== __Examples__
--
-- > path (buildG (0,0) []) 0 0 == True
--
-- > path (buildG (0,2) [(0,1), (1,2)]) 0 2 == True
--
-- > path (buildG (0,2) [(0,1), (1,2)]) 2 0 == False
path :: Graph -> Vertex -> Vertex -> Bool
path g v w    = w `elem` (reachable g v)

------------------------------------------------------------
-- Algorithm 7: Biconnected components
------------------------------------------------------------

-- | \(O(V+E)\). The biconnected components of a graph.
-- An undirected graph is biconnected if the deletion of any vertex
-- leaves it connected.
--
-- The input graph is expected to be undirected, i.e. for every edge in the
-- graph the reverse edge is also in the graph. If the graph is not undirected
-- the output is arbitrary.
bcc :: Graph -> [Tree [Vertex]]
bcc g = concatMap bicomps forest
  where
    -- The algorithm here is the same as given by King and Launchbury, which is
    -- an adaptation of Hopcroft and Tarjan's. The implementation, however, has
    -- been modified from King and Launchbury to make it efficient.

    forest = dff g

    -- dnum!v is the index of vertex v in the dfs preorder of vertices
    dnum = preArr (bounds g) forest

    -- Wraps up the component of every child of the root
    bicomps :: Tree Vertex -> [Tree [Vertex]]
    bicomps (Node v tws) =
      [Node (v : curw []) (donew []) | (_, curw, donew) <- map collect tws]

    -- Returns a triple of
    -- * lowpoint of v
    -- * difference list of vertices in v's component
    -- * difference list of trees of components, whose root components are
    --   adjacent to v's component
    collect :: Tree Vertex
            -> (Int, [Vertex] -> [Vertex], [Tree [Vertex]] -> [Tree [Vertex]])
    collect (Node v tws) = (lowv, (v:) . curv, donev)
      where
        dv = dnum UA.! v
        accf (lowv', curv', donev') tw
          | loww < dv  -- w's component extends through v
            = (lowv'', curv' . curw, donev' . donew)
          | otherwise  -- w's component ends with v as an articulation point
            = (lowv'', curv', donev' . (Node (v : curw []) (donew []) :))
          where
            (loww, curw, donew) = collect tw
            !lowv'' = min lowv' loww
        !lowv0 = F.foldl' min dv [dnum UA.! w | w <- g!v]
        !(lowv, curv, donev) = F.foldl' accf (lowv0, id, id) tws

--------------------------------------------------------------------------------

-- Note [Inline for fusion]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We inline simple functions that produce or consume lists so that list fusion
-- can fire. transposeG is a function where this is particularly useful; it has
-- two intermediate lists in its definition which get fused away.
