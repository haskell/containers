{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Applicative (Const(Const, getConst), pure)
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Main
import Data.Functor.Identity (Identity(runIdentity))
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

main = do
    let m = M.fromAscList elems :: M.Map Int Int
        m_even = M.fromAscList elems_even :: M.Map Int Int
        m_odd = M.fromAscList elems_odd :: M.Map Int Int
    evaluate $ rnf [m, m_even, m_odd]
    defaultMain
        [ bench "lookup absent" $ whnf (lookup evens) m_odd
        , bench "lookup present" $ whnf (lookup evens) m_even
        , bench "at lookup absent" $ whnf (atLookup evens) m_odd
        , bench "at lookup present" $ whnf (atLookup evens) m_even
        , bench "atLens lookup absent" $ whnf (atLensLookup evens) m_odd
        , bench "atLens lookup present" $ whnf (atLensLookup evens) m_even
        , bench "insert absent" $ whnf (ins elems_even) m_odd
        , bench "insert present" $ whnf (ins elems_even) m_even
        , bench "at insert absent" $ whnf (atIns elems_even) m_odd
        , bench "at insert present" $ whnf (atIns elems_even) m_even
        , bench "atLens insert absent" $ whnf (atLensIns elems_even) m_odd
        , bench "atLens insert present" $ whnf (atLensIns elems_even) m_even
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
        , bench "map" $ whnf (M.map (+ 1)) m
        , bench "mapWithKey" $ whnf (M.mapWithKey (+)) m
        , bench "foldlWithKey" $ whnf (ins elems) m
--         , bench "foldlWithKey'" $ whnf (M.foldlWithKey' sum 0) m
        , bench "foldrWithKey" $ whnf (M.foldrWithKey consPair []) m
        , bench "delete absent" $ whnf (del evens) m_odd
        , bench "delete present" $ whnf (del evens) m
        , bench "update absent" $ whnf (upd Just evens) m_odd
        , bench "update present" $ whnf (upd Just evens) m_even
        , bench "update delete" $ whnf (upd (const Nothing) evens) m
        , bench "updateLookupWithKey absent" $ whnf (upd' Just evens) m_odd
        , bench "updateLookupWithKey present" $ whnf (upd' Just evens) m_even
        , bench "updateLookupWithKey delete" $ whnf (upd' (const Nothing) evens) m
        , bench "alter absent"  $ whnf (alt id evens) m_odd
        , bench "alter insert"  $ whnf (alt (const (Just 1)) evens) m_odd
        , bench "alter update"  $ whnf (alt id evens) m_even
        , bench "alter delete"  $ whnf (alt (const Nothing) evens) m
        , bench "at alter absent" $ whnf (atAlt id evens) m_odd
        , bench "at alter insert" $ whnf (atAlt (const (Just 1)) evens) m_odd
        , bench "at alter update" $ whnf (atAlt id evens) m_even
        , bench "at alter delete" $ whnf (atAlt (const Nothing) evens) m
        , bench "atLens alter absent" $ whnf (atLensAlt id evens) m_odd
        , bench "atLens alter insert" $ whnf (atLensAlt (const (Just 1)) evens) m_odd
        , bench "atLens alter update" $ whnf (atLensAlt id evens) m_even
        , bench "atLens alter delete" $ whnf (atLensAlt (const Nothing) evens) m
        , bench "mapMaybe" $ whnf (M.mapMaybe maybeDel) m
        , bench "mapMaybeWithKey" $ whnf (M.mapMaybeWithKey (const maybeDel)) m
        , bench "lookupIndex" $ whnf (lookupIndex keys) m
        , bench "union" $ whnf (M.union m_even) m_odd
        , bench "difference" $ whnf (M.difference m) m_even
        , bench "intersection" $ whnf (M.intersection m) m_even
        , bench "split" $ whnf (M.split (bound `div` 2)) m
        , bench "fromList" $ whnf M.fromList elems
        , bench "fromList-desc" $ whnf M.fromList (reverse elems)
        , bench "fromAscList" $ whnf M.fromAscList elems
        , bench "fromDistinctAscList" $ whnf M.fromDistinctAscList elems
        ]
  where
    bound = 2^12
    elems = zip keys values
    elems_even = zip evens evens
    elems_odd = zip odds odds
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
atLookup xs m = foldl' (\n k -> fromMaybe n (getConst (M.at k Const m))) 0 xs

atLensLookup :: [Int] -> M.Map Int Int -> Int
atLensLookup xs m = foldl' (\n k -> fromMaybe n (getConst (atLens k Const m))) 0 xs

lookupIndex :: [Int] -> M.Map Int Int -> Int
lookupIndex xs m = foldl' (\n k -> fromMaybe n (M.lookupIndex k m)) 0 xs

ins :: [(Int, Int)] -> M.Map Int Int -> M.Map Int Int
ins xs m = foldl' (\m (k, v) -> M.insert k v m) m xs

atIns :: [(Int, Int)] -> M.Map Int Int -> M.Map Int Int
atIns xs m = foldl' (\m (k, v) -> runIdentity (M.at k (\_ -> pure (Just v)) m)) m xs

atLensIns :: [(Int, Int)] -> M.Map Int Int -> M.Map Int Int
atLensIns xs m = foldl' (\m (k, v) -> runIdentity (atLens k (\_ -> pure (Just v)) m)) m xs

insWith :: [(Int, Int)] -> M.Map Int Int -> M.Map Int Int
insWith xs m = foldl' (\m (k, v) -> M.insertWith (+) k v m) m xs

insWithKey :: [(Int, Int)] -> M.Map Int Int -> M.Map Int Int
insWithKey xs m = foldl' (\m (k, v) -> M.insertWithKey add3 k v m) m xs

insWith' :: [(Int, Int)] -> M.Map Int Int -> M.Map Int Int
insWith' xs m = foldl' (\m (k, v) -> M.insertWith' (+) k v m) m xs

insWithKey' :: [(Int, Int)] -> M.Map Int Int -> M.Map Int Int
insWithKey' xs m = foldl' (\m (k, v) -> M.insertWithKey' add3 k v m) m xs

data PairS a b = PS !a !b

insLookupWithKey :: [(Int, Int)] -> M.Map Int Int -> (Int, M.Map Int Int)
insLookupWithKey xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = M.insertLookupWithKey add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

insLookupWithKey' :: [(Int, Int)] -> M.Map Int Int -> (Int, M.Map Int Int)
insLookupWithKey' xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = M.insertLookupWithKey' add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

del :: [Int] -> M.Map Int Int -> M.Map Int Int
del xs m = foldl' (\m k -> M.delete k m) m xs

upd :: (Int -> Maybe Int) -> [Int] -> M.Map Int Int -> M.Map Int Int
upd f xs m = foldl' (\m k -> M.update f k m) m xs

upd' :: (Int -> Maybe Int) -> [Int] -> M.Map Int Int -> M.Map Int Int
upd' f xs m = foldl' (\m k -> snd $ M.updateLookupWithKey (\_ a -> f a) k m) m xs

alt :: (Maybe Int -> Maybe Int) -> [Int] -> M.Map Int Int -> M.Map Int Int
alt f xs m = foldl' (\m k -> M.alter f k m) m xs

atAlt :: (Maybe Int -> Maybe Int) -> [Int] -> M.Map Int Int -> M.Map Int Int
atAlt f xs m = foldl' (\m k -> runIdentity (M.at k (pure . f) m)) m xs

atLensAlt :: (Maybe Int -> Maybe Int) -> [Int] -> M.Map Int Int -> M.Map Int Int
atLensAlt f xs m = foldl' (\m k -> runIdentity (atLens k (pure . f) m)) m xs

-- implementation from Control.Lens.At for comparison
atLens :: (Functor f, Ord k) =>
          k -> (Maybe a -> f (Maybe a)) -> M.Map k a -> f (M.Map k a)
atLens k f m = (`fmap` f mx) $ \ mx' ->
  case mx' of
    Just x' -> M.insert k x' m
    Nothing ->
      case mx of
        Nothing -> m
        Just x  -> M.delete k m
  where mx = M.lookup k m
{-# INLINE atLens #-}

maybeDel :: Int -> Maybe Int
maybeDel n | n `mod` 3 == 0 = Nothing
           | otherwise      = Just n
