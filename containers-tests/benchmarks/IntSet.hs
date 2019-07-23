{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}

module Main where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Gauge (bench, defaultMain, whnf)
import Data.List (foldl')
import Data.Monoid (Sum(..))
#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (foldMap)
#endif
import qualified Data.IntSet as IS
-- benchmarks for "instance Ord IntSet"
-- uses IntSet as keys of maps, and elements of sets
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M

main = do
    let s = IS.fromAscList elems :: IS.IntSet
        s_even = IS.fromAscList elems_even :: IS.IntSet
        s_odd = IS.fromAscList elems_odd :: IS.IntSet
    evaluate $ rnf [s, s_even, s_odd]
    defaultMain
        [ bench "member" $ whnf (member elems) s
        , bench "insert" $ whnf (ins elems) IS.empty
        , bench "map" $ whnf (IS.map (+ 1)) s
        , bench "filter" $ whnf (IS.filter ((== 0) . (`mod` 2))) s
        , bench "partition" $ whnf (IS.partition ((== 0) . (`mod` 2))) s
        , bench "fold" $ whnf (IS.fold (:) []) s
        , bench "delete" $ whnf (del elems) s
        , bench "findMin" $ whnf IS.findMin s
        , bench "findMax" $ whnf IS.findMax s
        , bench "deleteMin" $ whnf IS.deleteMin s
        , bench "deleteMax" $ whnf IS.deleteMax s
        , bench "unions" $ whnf IS.unions [s_even, s_odd]
        , bench "union" $ whnf (IS.union s_even) s_odd
        , bench "difference" $ whnf (IS.difference s) s_even
        , bench "intersection" $ whnf (IS.intersection s) s_even
        , bench "fromList" $ whnf IS.fromList elems
        , bench "fromAscList" $ whnf IS.fromAscList elems
        , bench "fromDistinctAscList" $ whnf IS.fromDistinctAscList elems
        , bench "disjoint:false" $ whnf (IS.disjoint s) s_even
        , bench "disjoint:true" $ whnf (IS.disjoint s_odd) s_even
        , bench "null.intersection:false" $ whnf (IS.null. IS.intersection s) s_even
        , bench "null.intersection:true" $ whnf (IS.null. IS.intersection s_odd) s_even
        , bench "instanceOrd:dense" -- the IntSet will just use one Tip
          $ whnf (num_transitions . det 2 0) $ hard_nfa    1 16
        , bench "instanceOrd:sparse" -- many Bin, each Tip is singleton
          $ whnf (num_transitions . det 2 0) $ hard_nfa 1111 16
        ]
  where
    elems = [1..2^12]
    elems_even = [2,4..2^12]
    elems_odd = [1,3..2^12]

member :: [Int] -> IS.IntSet -> Int
member xs s = foldl' (\n x -> if IS.member x s then n + 1 else n) 0 xs

ins :: [Int] -> IS.IntSet -> IS.IntSet
ins xs s0 = foldl' (\s a -> IS.insert a s) s0 xs

del :: [Int] -> IS.IntSet -> IS.IntSet
del xs s0 = foldl' (\s k -> IS.delete k s) s0 xs



-- | Automata contain just the transitions
type NFA = IM.IntMap (IM.IntMap IS.IntSet)
type DFA = IM.IntMap (M.Map IS.IntSet IS.IntSet)

newtype State = State Int deriving (Num, Enum)
instance Show State where show (State s) = show s
newtype Sigma = Sigma Int deriving (Num, Enum, Eq)

num_transitions :: DFA -> Int
num_transitions = getSum . foldMap (Sum . M.size)

det :: Sigma -> State -> NFA -> DFA
det sigma (State initial) aut =
  let get :: State -> Sigma -> IS.IntSet
      get (State p) (Sigma s) = IM.findWithDefault IS.empty p
              $ IM.findWithDefault IM.empty s aut
      go :: DFA -> S.Set IS.IntSet -> S.Set IS.IntSet -> DFA
      go !accu !done !todo = case S.minView todo of
        Nothing -> accu
        Just (t, odo) ->
          if S.member t done
          then go accu done odo
          else let ts = do
                     s <- [0 .. sigma-1]
                     let next :: IS.IntSet
                         next = foldMap (\p -> get (State p) s) $ IS.toList t
                     return (t, s, next)
               in  go (union_dfa (dfa ts) accu)
                      (S.insert t done)
                      (Data.List.foldl' (\ o (_,_,q) -> S.insert q o) odo ts)
  in go IM.empty S.empty $ S.singleton $ IS.singleton initial

nfa :: [(State,Sigma,State)] -> NFA 
nfa ts = IM.fromListWith ( IM.unionWith IS.union )
  $ Prelude.map (\(State p,Sigma s,State q) ->
           (s, IM.singleton p (IS.singleton q))) ts

dfa :: [(IS.IntSet, Sigma, IS.IntSet)] -> DFA
dfa ts = IM.fromListWith ( M.unionWith ( error "WAT") )
  $ Prelude.map (\( p, Sigma s, q) ->
           (s, M.singleton p q)) ts

union_dfa a b = IM.unionWith (M.unionWith (error "WAT")) a b

-- | for the language Sigma^* 1 Sigma^{n-2}  where Sigma={0,1}.
-- this NFA has  n  states. DFA has 2^(n-1) states
-- since it needs to remember the last n characters.
-- Extra parameter delta: the automaton will use states [0, delta .. ]
-- for IntSet, larger deltas should be harder,
-- since for delta=1, all the states do fit in one Tip
hard_nfa :: State -> Int -> NFA
hard_nfa delta n = nfa
  $ [ (0, 0, 0), (0,1,0), (0, 1, delta) ]
  ++ do k <- [1 .. State n - 2] ; c <- [0,1] ; return (delta * k,c,delta *(k+1))
