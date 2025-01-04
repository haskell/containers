{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Test.Tasty.Bench (bench, bgroup, defaultMain, whnf)
import Data.List (foldl')
import qualified Data.Set as S

import Utils.Fold (foldBenchmarks)
import Utils.Random (shuffle)

main = do
    let s = S.fromList elems :: S.Set Int
        s_even = S.fromList elems_even :: S.Set Int
        s_odd = S.fromList elems_odd :: S.Set Int
        strings_s = S.fromList strings
    evaluate $ rnf [s, s_even, s_odd]
    evaluate $ rnf
      [elems_distinct_asc, elems_distinct_desc, elems_asc, elems_desc]
    defaultMain
        [ bench "member" $ whnf (member elems) s
        , bench "insert" $ whnf (ins elems) S.empty
        , bench "map" $ whnf (S.map (+ 1)) s
        , bench "filter" $ whnf (S.filter ((== 0) . (`mod` 2))) s
        , bench "partition" $ whnf (S.partition ((== 0) . (`mod` 2))) s
        , bench "delete" $ whnf (del elems) s
        , bench "findMin" $ whnf S.findMin s
        , bench "findMax" $ whnf S.findMax s
        , bench "deleteMin" $ whnf S.deleteMin s
        , bench "deleteMax" $ whnf S.deleteMax s
        , bench "unions" $ whnf S.unions [s_even, s_odd]
        , bench "union" $ whnf (S.union s_even) s_odd
        , bench "difference" $ whnf (S.difference s) s_even
        , bench "intersection" $ whnf (S.intersection s) s_even
        , bench "fromList" $ whnf S.fromList elems
        , bench "fromList-desc" $ whnf S.fromList elems_desc
        , bench "fromAscList" $ whnf S.fromAscList elems_asc
        , bench "fromAscList:fusion" $
            whnf (\n -> S.fromAscList [i `div` 2 | i <- [1..n]]) bound
        , bench "fromDistinctAscList" $ whnf S.fromDistinctAscList elems_distinct_asc
        , bench "fromDistinctAscList:fusion" $ whnf (\n -> S.fromDistinctAscList [1..n]) bound
        , bench "fromDescList" $ whnf S.fromDescList elems_desc
        , bench "fromDescList:fusion" $
            whnf (\n -> S.fromDescList [i `div` 2 | i <- [n,n-1..1]]) bound
        , bench "fromDistinctDescList" $ whnf S.fromDistinctDescList elems_distinct_desc
        , bench "fromDistinctDescList:fusion" $ whnf (\n -> S.fromDistinctDescList [n,n-1..1]) bound
        , bench "disjoint:false" $ whnf (S.disjoint s) s_even
        , bench "disjoint:true" $ whnf (S.disjoint s_odd) s_even
        , bench "null.intersection:false" $ whnf (S.null. S.intersection s) s_even
        , bench "null.intersection:true" $ whnf (S.null. S.intersection s_odd) s_even
        , bench "alterF:member" $ whnf (alterF_member elems) s
        , bench "alterF:insert" $ whnf (alterF_ins elems) S.empty
        , bench "alterF:delete" $ whnf (alterF_del elems) s
        , bench "alterF:four" $ whnf (alterF_four elems) s
        , bench "alterF:four:strings" $ whnf (alterF_four strings) strings_s
        , bench "alterF_naive:four" $ whnf (alterF_naive_four elems) s
        , bench "alterF_naive:four:strings" $ whnf (alterF_naive_four strings) strings_s
        , bench "powerSet (15)" $ whnf S.powerSet (S.fromList[1..15])
        , bench "powerSet (16)" $ whnf S.powerSet (S.fromList[1..16])
        , bench "member.powerSet (14)" $ whnf (\ s -> all (flip S.member s) s) (S.powerSet (S.fromList [1..14]))
        , bench "member.powerSet (15)" $ whnf (\ s -> all (flip S.member s) s) (S.powerSet (S.fromList [1..15]))
        , bench "eq" $ whnf (\s' -> s' == s') s -- worst case, compares everything
        , bench "compare" $ whnf (\s' -> compare s' s') s -- worst case, compares everything
        , bgroup "folds" $ foldBenchmarks S.foldr S.foldl S.foldr' S.foldl' foldMap s
        ]
  where
    bound = 2^12
    elems_distinct_asc = [1..bound]
    elems_distinct_desc = reverse elems_distinct_asc
    elems = shuffle elems_distinct_asc
    elems_even = shuffle [2,4..bound]
    elems_odd = shuffle [1,3..bound]
    elems_asc = map (`div` 2) [1..bound] -- [0,1,1,2,2..]
    elems_desc = map (`div` 2) [bound,bound-1..1] -- [..2,2,1,1,0]
    strings = map show elems

member :: [Int] -> S.Set Int -> Int
member xs s = foldl' (\n x -> if S.member x s then n + 1 else n) 0 xs

ins :: [Int] -> S.Set Int -> S.Set Int
ins xs s0 = foldl' (\s a -> S.insert a s) s0 xs

del :: [Int] -> S.Set Int -> S.Set Int
del xs s0 = foldl' (\s k -> S.delete k s) s0 xs

alterF_member :: [Int] -> S.Set Int -> Int
alterF_member xs s = foldl' (\n x -> if member' x s then n + 1 else n) 0 xs
  where
    member' k s = getConsty (S.alterF (\b -> Consty b) k s)

alterF_ins :: [Int] -> S.Set Int -> S.Set Int
alterF_ins xs s0 = foldl' (\s a -> insert' a s) s0 xs
  where
    insert' k s = runIdent (S.alterF (const (Ident True)) k s)

alterF_del :: [Int] -> S.Set Int -> S.Set Int
alterF_del xs s0 = foldl' (\s k -> delete' k s) s0 xs
  where
    delete' k s = runIdent (S.alterF (const (Ident False)) k s)

alterF_four :: Ord a => [a] -> S.Set a -> S.Set a
alterF_four xs s0 = foldl' (\s k -> S.alterF four k s `seq` s) s0 xs

alterF_naive_four :: Ord a => [a] -> S.Set a -> S.Set a
alterF_naive_four xs s0 = foldl' (\s k -> alterF_naive four k s `seq` s) s0 xs

alterF_naive :: (Ord a, Functor f) => (Bool -> f Bool) -> a -> S.Set a -> f (S.Set a)
alterF_naive f k s = fmap g (f (k `S.member` s))
  where
    g True  = S.insert k s
    g False = S.delete k s

four :: Bool -> Four Bool
               -- insert  delete  reinsert  toggle
four True  = Four True    False   True      False
four False = Four True    False   False     True

newtype Consty a b = Consty { getConsty :: a}
instance Functor (Consty a) where
  fmap _ (Consty a) = Consty a

newtype Ident a = Ident { runIdent :: a }
instance Functor Ident where
  fmap f (Ident a) = Ident (f a)

data Four a = Four !a !a !a !a
instance Functor Four where
  fmap f (Four a b c d) = Four (f a) (f b) (f c) (f d)
