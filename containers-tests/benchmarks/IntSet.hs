{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}

module Main where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Test.Tasty.Bench (bench, bgroup, defaultMain, whnf)
import Data.List (foldl')
import Data.Monoid (Sum(..), All(..))
import qualified Data.IntSet as IS
-- benchmarks for "instance Ord IntSet"
-- uses IntSet as keys of maps, and elements of sets
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M
import Data.Word (Word8)
import System.Random (StdGen, mkStdGen, randoms, randomRs)

import Utils.Fold (foldBenchmarks)

main = do
    let s = IS.fromAscList elems :: IS.IntSet
        s_even = IS.fromAscList elems_even :: IS.IntSet
        s_odd = IS.fromAscList elems_odd :: IS.IntSet
        s_sparse = IS.fromAscList elems_sparse :: IS.IntSet
    evaluate $
      rnf
        [ elems_asc
        , elems_asc_sparse
        , elems_random
        , elems_randomDups
        , elems_fromListWorstCase
        ]
    evaluate $ rnf [s, s_even, s_odd, s_sparse]
    defaultMain
        [ bench "member" $ whnf (member elems) s
        , bench "insert" $ whnf (ins elems) IS.empty
        , bench "map" $ whnf (IS.map (+ 1)) s
        , bench "filter" $ whnf (IS.filter ((== 0) . (`mod` 2))) s
        , bench "partition" $ whnf (IS.partition ((== 0) . (`mod` 2))) s
        , bench "delete" $ whnf (del elems) s
        , bench "findMin" $ whnf IS.findMin s
        , bench "findMax" $ whnf IS.findMax s
        , bench "deleteMin" $ whnf IS.deleteMin s
        , bench "deleteMax" $ whnf IS.deleteMax s
        , bench "unions" $ whnf IS.unions [s_even, s_odd]
        , bench "union" $ whnf (IS.union s_even) s_odd
        , bench "difference" $ whnf (IS.difference s) s_even
        , bench "intersection" $ whnf (IS.intersection s) s_even
        , bench "fromList:asc" $ whnf fromListNoinline elems_asc
        , bench "fromList:asc:fusion" $ whnf (\n -> IS.fromList [1..n]) bound
        , bench "fromList:asc:sparse" $ whnf fromListNoinline elems_asc_sparse
        , bench "fromList:asc:sparse:fusion" $
            whnf (\n -> IS.fromList (map (*64) [1..n])) bound
        , bench "fromList:random" $ whnf fromListNoinline elems_random
        , bench "fromList:random:fusion" $
            whnf (\(n,g) -> IS.fromList (take n (randoms g))) (bound,gen)
        , bench "fromList:randomDups" $ whnf fromListNoinline elems_randomDups
        , bench "fromList:randomDups:fusion" $
            whnf (\(n,g) -> IS.fromList (take n (map word8ToInt (randoms g)))) (bound,gen)
        , bench "fromList:worstCase" $ whnf fromListNoinline elems_fromListWorstCase
        , bench "fromRange" $ whnf IS.fromRange (1,bound)
        , bench "fromRange:small" $ whnf IS.fromRange (-1,0)
        , bench "fromAscList" $ whnf fromAscListNoinline elems
        , bench "fromAscList:fusion" $ whnf (\n -> IS.fromAscList [1..n]) bound
        , bench "fromAscList:sparse" $ whnf fromAscListNoinline elems_sparse
        , bench "fromAscList:sparse:fusion" $
            whnf (\n -> IS.fromAscList (map (*64) [1..n])) bound
        , bench "disjoint:false" $ whnf (IS.disjoint s) s_even
        , bench "disjoint:true" $ whnf (IS.disjoint s_odd) s_even
        , bench "null.intersection:false" $ whnf (IS.null. IS.intersection s) s_even
        , bench "null.intersection:true" $ whnf (IS.null. IS.intersection s_odd) s_even
        , bench "instanceOrd:dense" -- the IntSet will just use one Tip
          $ whnf (num_transitions . det 2 0) $ hard_nfa    1 16
        , bench "instanceOrd:sparse" -- many Bin, each Tip is singleton
          $ whnf (num_transitions . det 2 0) $ hard_nfa 1111 16
        , bench "spanAntitone:dense" $ whnf (IS.spanAntitone (<elem_mid)) s
        , bench "spanAntitone:sparse" $ whnf (IS.spanAntitone (<elem_sparse_mid)) s_sparse
        , bench "split:dense" $ whnf (IS.split elem_mid) s
        , bench "split:sparse" $ whnf (IS.split elem_sparse_mid) s_sparse
        , bench "splitMember:dense" $ whnf (IS.splitMember elem_mid) s
        , bench "splitMember:sparse" $ whnf (IS.splitMember elem_sparse_mid) s_sparse
        , bench "eq" $ whnf (\s' -> s' == s') s -- worst case, compares everything
        , bench "compare:dense" $ whnf (\s' -> compare s' s') s -- worst case, compares everything
        , bench "compare:sparse" $ whnf (\s' -> compare s' s') s_sparse -- worst case, compares everything
        , bench "size" $ whnf IS.size s
        , bench "size:sparse" $ whnf IS.size s_sparse
        , bench "compareSize:2" $ whnf (flip IS.compareSize 2) s
        , bench "compareSize:sparse:2" $ whnf (flip IS.compareSize 2) s_sparse
        , bench "compareSize:n" $ whnf (flip IS.compareSize bound) s
        , bench "compareSize:sparse:n" $ whnf (flip IS.compareSize bound) s_sparse
        , bgroup "folds:dense" $ foldBenchmarks IS.foldr IS.foldl IS.foldr' IS.foldl' IS.foldMap s
        , bgroup "folds:sparse" $ foldBenchmarks IS.foldr IS.foldl IS.foldr' IS.foldl' IS.foldMap s_sparse
        ]
  where
    !bound = 2^12
    elems = [1..bound]
    elems_even = [2,4..bound]
    elems_odd = [1,3..bound]
    elem_mid = bound `div` 2 + 31 -- falls in the middle of a packed Tip bitmask (assuming 64-bit words)
    elems_sparse = map (*64) elems -- when built into a map, each Tip is a singleton
    elem_sparse_mid = bound `div` 2 * 64
    elems_asc = elems
    elems_asc_sparse = elems_sparse
    elems_random = take bound (randoms gen)
    -- Random elements in a small range to produce duplicates
    elems_randomDups = take bound (map word8ToInt (randoms gen))
    -- Worst case for the current fromList algorithm. Consider removing this
    -- test case if the algorithm changes.
    elems_fromListWorstCase =
      take bound $
      concat
        [ take 63 (iterate (*2) 1)
        , take 63 (map negate (iterate (*2) 1))
        , interleave [1..] (map negate [1..])
        ]

member :: [Int] -> IS.IntSet -> Int
member xs s = foldl' (\n x -> if IS.member x s then n + 1 else n) 0 xs

ins :: [Int] -> IS.IntSet -> IS.IntSet
ins xs s0 = foldl' (\s a -> IS.insert a s) s0 xs

del :: [Int] -> IS.IntSet -> IS.IntSet
del xs s0 = foldl' (\s k -> IS.delete k s) s0 xs

-- NOINLINE to work around an issue where the inlined function doesn't get
-- optimized (see GHC #25878).
fromAscListNoinline :: [Int] -> IS.IntSet
fromAscListNoinline = IS.fromAscList
{-# NOINLINE fromAscListNoinline #-}

fromListNoinline :: [Int] -> IS.IntSet
fromListNoinline = IS.fromList
{-# NOINLINE fromListNoinline #-}

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) (y:ys) = x : y : interleave xs ys

gen :: StdGen
gen = mkStdGen 42

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

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
