{-# LANGUAGE BangPatterns #-}
module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Main
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

instance (NFData k, NFData a) => NFData (M.Map k a) where
    rnf M.Tip = ()
    rnf (M.Bin _ k a l r) = rnf k `seq` rnf a `seq` rnf l `seq` rnf r

main = do
    let m = M.fromAscList elems :: M.Map Int Int
        m_even = M.fromAscList elems_even :: M.Map Int Int
        m_odd = M.fromAscList elems_odd :: M.Map Int Int
    defaultMainWith
        defaultConfig
        (liftIO . evaluate $ rnf [m, m_even, m_odd])
        [ bench "lookup absent" $ nf (lookup evens) m_odd
        , bench "lookup present" $ nf (lookup evens) m_even
        , bench "insert absent" $ nf (ins elems_even) m_odd
        , bench "insert present" $ nf (ins elems_even) m_even
        , bench "insertWith absent" $ nf (insWith elems_even) m_odd
        , bench "insertWith present" $ nf (insWith elems_even) m_even
        , bench "insertWith' absent" $ nf (insWith' elems_even) m_odd
        , bench "insertWith' present" $ nf (insWith' elems_even) m_even
        , bench "insertWithKey absent" $ nf (insWithKey elems_even) m_odd
        , bench "insertWithKey present" $ nf (insWithKey elems_even) m_even
        , bench "insertWithKey' absent" $ nf (insWithKey' elems_even) m_odd
        , bench "insertWithKey' present" $ nf (insWithKey' elems_even) m_even
        , bench "insertLookupWithKey absent" $
          nf (insLookupWithKey elems_even) m_odd
        , bench "insertLookupWithKey present" $
          nf (insLookupWithKey elems_even) m_even
        , bench "insertLookupWithKey' absent" $
          nf (insLookupWithKey' elems_even) m_odd
        , bench "insertLookupWithKey' present" $
          nf (insLookupWithKey' elems_even) m_even
        , bench "map" $ nf (M.map (+ 1)) m
        , bench "mapWithKey" $ nf (M.mapWithKey (+)) m
        , bench "foldlWithKey" $ nf (ins elems) m
--         , bench "foldlWithKey'" $ nf (M.foldlWithKey' sum 0) m
        , bench "foldrWithKey" $ nf (M.foldrWithKey consPair []) m
        , bench "delete absent" $ nf (del evens) m_odd
        , bench "delete present" $ nf (del evens) m
        , bench "update absent" $ nf (upd Just evens) m_odd
        , bench "update present" $ nf (upd Just evens) m_even
        , bench "update delete" $ nf (upd (const Nothing) evens) m
        , bench "updateLookupWithKey absent" $ nf (upd' Just evens) m_odd
        , bench "updateLookupWithKey present" $ nf (upd' Just evens) m_even
        , bench "updateLookupWithKey delete" $ nf (upd' (const Nothing) evens) m
        , bench "alter absent"  $ nf (alt id evens) m_odd
        , bench "alter insert"  $ nf (alt (const (Just 1)) evens) m_odd
        , bench "alter update"  $ nf (alt id evens) m_even
        , bench "alter delete"  $ nf (alt (const Nothing) evens) m
        , bench "mapMaybe" $ nf (M.mapMaybe maybeDel) m
        , bench "mapMaybeWithKey" $ nf (M.mapMaybeWithKey (const maybeDel)) m
        , bench "lookupIndex" $ nf (lookupIndex keys) m
        , bench "union" $ nf (M.union m_even) m_odd
        , bench "difference" $ nf (M.difference m) m_even
        , bench "intersection" $ nf (M.intersection m) m_even
        ]
  where
    bound = 2^10
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

lookupIndex :: [Int] -> M.Map Int Int -> Int
lookupIndex xs m = foldl' (\n k -> fromMaybe n (M.lookupIndex k m)) 0 xs

ins :: [(Int, Int)] -> M.Map Int Int -> M.Map Int Int
ins xs m = foldl' (\m (k, v) -> M.insert k v m) m xs

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

maybeDel :: Int -> Maybe Int
maybeDel n | n `mod` 3 == 0 = Nothing
           | otherwise      = Just n
