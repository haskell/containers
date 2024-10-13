{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Applicative (Const(Const, getConst), pure)
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Test.Tasty.Bench (bench, defaultMain, whnf, nf)
import Data.Functor.Identity (Identity(..))
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import Data.Map (alterF)
import Data.Maybe (fromMaybe)
import Data.Functor ((<$))
import Data.Coerce
import Prelude hiding (lookup)

main = do
    let m = M.fromAscList elems :: M.Map Int Int
        m_even = M.fromAscList elems_even :: M.Map Int Int
        m_odd = M.fromAscList elems_odd :: M.Map Int Int
        m_sparse = M.filter (\v -> v `mod` 15 == 0) m_even
    evaluate $ rnf [m, m_even, m_odd, m_sparse]
    evaluate $ rnf [elems_rev, elems_alts]
    defaultMain
        [ bench "lookup absent" $ whnf (lookup evens) m_odd
        , bench "lookup present" $ whnf (lookup evens) m_even
        , bench "map" $ whnf (M.map (+ 1)) m
        , bench "map really" $ nf (M.map (+ 2)) m
        , bench "<$" $ whnf ((1 :: Int) <$) m
        , bench "<$ really" $ nf ((2 :: Int) <$) m
        , bench "alterF lookup absent" $ whnf (atLookup evens) m_odd
        , bench "alterF lookup present" $ whnf (atLookup evens) m_even
        , bench "alterF no rules lookup absent" $ whnf (atLookupNoRules evens) m_odd
        , bench "alterF no rules lookup present" $ whnf (atLookupNoRules evens) m_even
        , bench "insert absent" $ whnf (ins elems_even) m_odd
        , bench "insert present" $ whnf (ins elems_even) m_even
        , bench "insert alternate" $ whnf (ins elems_alts) m_even
        , bench "alterF insert absent" $ whnf (atIns elems_even) m_odd
        , bench "alterF insert present" $ whnf (atIns elems_even) m_even
        , bench "alterF no rules insert absent" $ whnf (atInsNoRules elems_even) m_odd
        , bench "alterF no rules insert present" $ whnf (atInsNoRules elems_even) m_even
        , bench "delete absent" $ whnf (del evens) m_odd
        , bench "delete present" $ whnf (del evens) m
        , bench "alterF delete absent" $ whnf (atDel evens) m_odd
        , bench "alterF delete present" $ whnf (atDel evens) m
        , bench "alterF no rules delete absent" $ whnf (atDelNoRules evens) m_odd
        , bench "alterF no rules delete present" $ whnf (atDelNoRules evens) m
        , bench "alter absent"  $ whnf (alt id evens) m_odd
        , bench "alter insert"  $ whnf (alt (const (Just 1)) evens) m_odd
        , bench "alter update"  $ whnf (alt id evens) m_even
        , bench "alter delete"  $ whnf (alt (const Nothing) evens) m
        , bench "alterF alter absent" $ whnf (atAlt id evens) m_odd
        , bench "alterF alter insert" $ whnf (atAlt (const (Just 1)) evens) m_odd
        , bench "alterF alter update" $ whnf (atAlt id evens) m_even
        , bench "alterF alter delete" $ whnf (atAlt (const Nothing) evens) m
        , bench "alterF no rules alter absent" $ whnf (atAltNoRules id evens) m_odd
        , bench "alterF no rules alter insert" $ whnf (atAltNoRules (const (Just 1)) evens) m_odd
        , bench "alterF no rules alter update" $ whnf (atAltNoRules id evens) m_even
        , bench "alterF no rules alter delete" $ whnf (atAltNoRules (const Nothing) evens) m
        , bench "insertWith absent" $ whnf (insWith elems_even) m_odd
        , bench "insertWith present" $ whnf (insWith elems_even) m_even
        , bench "insertWith' absent" $ whnf (insWith' elems_even) m_odd
        , bench "insertWith' present" $ whnf (insWith' elems_even) m_even
        , bench "insertWithKey absent" $ whnf (insWithKey elems_even) m_odd
        , bench "insertWithKey present" $ whnf (insWithKey elems_even) m_even
        , bench "insertWithKey' absent" $ whnf (insWithKey' elems_even) m_odd
        , bench "insertWithKey' present" $ whnf (insWithKey' elems_even) m_even
        , bench "insertLookupWithKey absent" $ whnf (insLookupWithKey elems_even) m_odd
        , bench "insertLookupWithKey present" $ whnf (insLookupWithKey elems_even) m_even
        , bench "insertLookupWithKey' absent" $ whnf (insLookupWithKey' elems_even) m_odd
        , bench "insertLookupWithKey' present" $ whnf (insLookupWithKey' elems_even) m_even
        , bench "mapWithKey" $ whnf (M.mapWithKey (+)) m
        , bench "foldlWithKey" $ whnf (ins elems) m
        , bench "foldlWithKey'" $ whnf (M.foldlWithKey' sum 0) m
        , bench "foldrWithKey" $ whnf (M.foldrWithKey consPair []) m
        , bench "foldrWithKey'" $ whnf (M.foldrWithKey' consPair []) m
        , bench "update absent" $ whnf (upd Just evens) m_odd
        , bench "update present" $ whnf (upd Just evens) m_even
        , bench "update delete" $ whnf (upd (const Nothing) evens) m
        , bench "updateLookupWithKey absent" $ whnf (upd' Just evens) m_odd
        , bench "updateLookupWithKey present" $ whnf (upd' Just evens) m_even
        , bench "updateLookupWithKey delete" $ whnf (upd' (const Nothing) evens) m
        , bench "mapMaybe" $ whnf (M.mapMaybe maybeDel) m
        , bench "mapMaybeWithKey" $ whnf (M.mapMaybeWithKey (const maybeDel)) m
        , bench "lookupIndex" $ whnf (lookupIndex keys) m
        , bench "union" $ whnf (M.union m_even) m_odd
        , bench "union_identical" $ whnf (M.union m_even) m_even
        , bench "union_sparse" $ whnf (M.union m_even) m_sparse
        , bench "union_into_sparse" $ whnf (M.union m_sparse) m_even
        , bench "difference" $ whnf (M.difference m) m_even
        , bench "intersection" $ whnf (M.intersection m) m_even
        , bench "split" $ whnf (M.split (bound `div` 2)) m
        , bench "fromList" $ whnf M.fromList elems
        , bench "fromList-desc" $ whnf M.fromList (reverse elems)
        , bench "fromAscList" $ whnf M.fromAscList elems
        , bench "fromDistinctAscList" $ whnf M.fromDistinctAscList elems
        , bench "fromDistinctAscList:fusion" $ whnf (\n -> M.fromDistinctAscList [(i,i) | i <- [1..n]]) bound
        , bench "fromDistinctDescList" $ whnf M.fromDistinctDescList elems_rev
        , bench "fromDistinctDescList:fusion" $ whnf (\n -> M.fromDistinctDescList [(i,i) | i <- [n,n-1..1]]) bound
        , bench "minView" $ whnf (\m' -> case M.minViewWithKey m' of {Nothing -> 0; Just ((k,v),m'') -> k+v+M.size m''}) (M.fromAscList $ zip [1..10::Int] [100..110::Int])
        , bench "eq" $ whnf (\m' -> m' == m') m -- worst case, compares everything
        , bench "compare" $ whnf (\m' -> compare m' m') m -- worst case, compares everything
        ]
  where
    bound = 2^12
    elems = zip keys values
    elems_even = zip evens evens
    elems_alts = zip evens odds
    elems_odd = zip odds odds
    elems_rev = reverse elems
    keys = [1..bound]
    evens = [2,4..bound]
    odds = [1,3..bound]
    values = [1..bound]
    sum k v1 v2 = k + v1 + v2
    consPair k v xs = (k, v) : xs

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z
{-# INLINE add3 #-}

lookup :: [Int] -> M.Map Int Int -> Int
lookup xs m = foldl' (\n k -> fromMaybe n (M.lookup k m)) 0 xs

atLookup :: [Int] -> M.Map Int Int -> Int
atLookup xs m = foldl' (\n k -> fromMaybe n (getConst (alterF Const k m))) 0 xs

newtype Consty a b = Consty { getConsty :: a }
instance Functor (Consty a) where
  fmap _ (Consty a) = Consty a

atLookupNoRules :: [Int] -> M.Map Int Int -> Int
atLookupNoRules xs m = foldl' (\n k -> fromMaybe n (getConsty (alterF Consty k m))) 0 xs

lookupIndex :: [Int] -> M.Map Int Int -> Int
lookupIndex xs m = foldl' (\n k -> fromMaybe n (M.lookupIndex k m)) 0 xs

ins :: [(Int, Int)] -> M.Map Int Int -> M.Map Int Int
ins xs m = foldl' (\m (k, v) -> M.insert k v m) m xs

atIns :: [(Int, Int)] -> M.Map Int Int -> M.Map Int Int
atIns xs m = foldl' (\m (k, v) -> runIdentity (alterF (\_ -> Identity (Just v)) k m)) m xs

newtype Ident a = Ident { runIdent :: a }
instance Functor Ident where
  fmap = coerce

atInsNoRules :: [(Int, Int)] -> M.Map Int Int -> M.Map Int Int
atInsNoRules xs m = foldl' (\m (k, v) -> runIdent (alterF (\_ -> Ident (Just v)) k m)) m xs

insWith :: [(Int, Int)] -> M.Map Int Int -> M.Map Int Int
insWith xs m = foldl' (\m (k, v) -> M.insertWith (+) k v m) m xs

insWithKey :: [(Int, Int)] -> M.Map Int Int -> M.Map Int Int
insWithKey xs m = foldl' (\m (k, v) -> M.insertWithKey add3 k v m) m xs

insWith' :: [(Int, Int)] -> M.Map Int Int -> M.Map Int Int
insWith' xs m = foldl' (\m (k, v) -> MS.insertWith (+) k v m) m xs

insWithKey' :: [(Int, Int)] -> M.Map Int Int -> M.Map Int Int
insWithKey' xs m = foldl' (\m (k, v) -> MS.insertWithKey add3 k v m) m xs

data PairS a b = PS !a !b

insLookupWithKey :: [(Int, Int)] -> M.Map Int Int -> (Int, M.Map Int Int)
insLookupWithKey xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = M.insertLookupWithKey add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

insLookupWithKey' :: [(Int, Int)] -> M.Map Int Int -> (Int, M.Map Int Int)
insLookupWithKey' xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = MS.insertLookupWithKey add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

del :: [Int] -> M.Map Int Int -> M.Map Int Int
del xs m = foldl' (\m k -> M.delete k m) m xs

atDel :: [Int] -> M.Map Int Int -> M.Map Int Int
atDel xs m = foldl' (\m k -> runIdentity (alterF (\_ -> Identity Nothing) k m)) m xs

atDelNoRules :: [Int] -> M.Map Int Int -> M.Map Int Int
atDelNoRules xs m = foldl' (\m k -> runIdent (alterF (\_ -> Ident Nothing) k m)) m xs

upd :: (Int -> Maybe Int) -> [Int] -> M.Map Int Int -> M.Map Int Int
upd f xs m = foldl' (\m k -> M.update f k m) m xs

upd' :: (Int -> Maybe Int) -> [Int] -> M.Map Int Int -> M.Map Int Int
upd' f xs m = foldl' (\m k -> snd $ M.updateLookupWithKey (\_ a -> f a) k m) m xs

alt :: (Maybe Int -> Maybe Int) -> [Int] -> M.Map Int Int -> M.Map Int Int
alt f xs m = foldl' (\m k -> M.alter f k m) m xs

atAlt :: (Maybe Int -> Maybe Int) -> [Int] -> M.Map Int Int -> M.Map Int Int
atAlt f xs m = foldl' (\m k -> runIdentity (alterF (Identity . f) k m)) m xs

atAltNoRules :: (Maybe Int -> Maybe Int) -> [Int] -> M.Map Int Int -> M.Map Int Int
atAltNoRules f xs m = foldl' (\m k -> runIdent (alterF (Ident . f) k m)) m xs

maybeDel :: Int -> Maybe Int
maybeDel n | n `mod` 3 == 0 = Nothing
           | otherwise      = Just n
