{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Gauge (bench, defaultMain, whnf)
import Data.List (foldl')
import qualified Data.Set as S

main = do
    let s = S.fromAscList elems :: S.Set Int
        s_even = S.fromAscList elems_even :: S.Set Int
        s_odd = S.fromAscList elems_odd :: S.Set Int
        strings_s = S.fromList strings
    evaluate $ rnf [s, s_even, s_odd]
    defaultMain
        [ bench "member" $ whnf (member elems) s
        , bench "insert" $ whnf (ins elems) S.empty
        , bench "map" $ whnf (S.map (+ 1)) s
        , bench "filter" $ whnf (S.filter ((== 0) . (`mod` 2))) s
        , bench "partition" $ whnf (S.partition ((== 0) . (`mod` 2))) s
        , bench "fold" $ whnf (S.fold (:) []) s
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
        , bench "fromList-desc" $ whnf S.fromList (reverse elems)
        , bench "fromAscList" $ whnf S.fromAscList elems
        , bench "fromDistinctAscList" $ whnf S.fromDistinctAscList elems
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
        ]
  where
    elems = [1..2^12]
    elems_even = [2,4..2^12]
    elems_odd = [1,3..2^12]
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
