{-# LANGUAGE BangPatterns #-}
module Main where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Test.Tasty.Bench (bench, bgroup, defaultMain, whnf)
import Data.List (foldl')
import qualified Data.IntMap as M
import qualified Data.IntMap.Strict as MS
import qualified Data.IntSet as S
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import System.Random (StdGen, mkStdGen, random, randoms)
import Prelude hiding (lookup)

import Utils.Fold (foldBenchmarks, foldWithKeyBenchmarks)

main = do
    let m     = M.fromAscList elems_hits   :: M.IntMap Int
    let m'    = M.fromAscList elems_mid    :: M.IntMap Int
    let m''   = M.fromAscList elems_most   :: M.IntMap Int
    let m'''  = M.fromAscList elems_misses :: M.IntMap Int
    let m'''' = M.fromAscList elems_mixed  :: M.IntMap Int
        m_random = M.fromList elems_random
        s = S.fromList keys
        s_random2 = S.fromList keys_random2
    evaluate $
      rnf [elems_asc, elems_random, elems_randomDups, elems_fromListWorstCase]
    evaluate $ rnf [m, m', m'', m''', m'''']
    evaluate $ rnf m_random
    evaluate $ rnf [s, s_random2]
    defaultMain
        [ bench "lookup_hits" $ whnf (lookup keys) m
        , bench "lookup_half" $ whnf (lookup keys) m'
        , bench "lookup_most" $ whnf (lookup keys) m''
        , bench "lookup_misses" $ whnf (lookup keys'') m'''
        , bench "lookup_mixed" $ whnf (lookup keys) m''''
        , bench "insert" $ whnf (ins elems) M.empty
        , bench "insertWith empty" $ whnf (insWith elems) M.empty
        , bench "insertWith update" $ whnf (insWith elems) m
        , bench "insertWith' empty" $ whnf (insWith' elems) M.empty
        , bench "insertWith' update" $ whnf (insWith' elems) m
        , bench "insertWithKey empty" $ whnf (insWithKey elems) M.empty
        , bench "insertWithKey update" $ whnf (insWithKey elems) m
        , bench "insertWithKey' empty" $ whnf (insWithKey' elems) M.empty
        , bench "insertWithKey' update" $ whnf (insWithKey' elems) m
        , bench "insertLookupWithKey empty" $ whnf (insLookupWithKey elems) M.empty
        , bench "insertLookupWithKey update" $ whnf (insLookupWithKey elems) m
        , bench "map" $ whnf (M.map (+ 1)) m
        , bench "mapWithKey" $ whnf (M.mapWithKey (+)) m
        , bench "delete" $ whnf (del keys) m
        , bench "update" $ whnf (upd keys) m
        , bench "updateLookupWithKey" $ whnf (upd' keys) m
        , bench "alter"  $ whnf (alt keys) m
        , bench "mapMaybe" $ whnf (M.mapMaybe maybeDel) m
        , bench "mapMaybeWithKey" $ whnf (M.mapMaybeWithKey (const maybeDel)) m
        , bench "fromList:asc" $ whnf M.fromList elems_asc
        , bench "mapKeys:asc" $ whnf (M.mapKeys (+1)) m
        , bench "mapKeys:random" $ whnf (M.mapKeys (fst . random . mkStdGen)) m
        , bench "mapKeysWith:asc:dups" $ whnf (M.mapKeysWith (+) (`div` 2)) m
        , bench "mapKeysMonotonic" $ whnf (M.mapKeysMonotonic (+1)) m
        , bench "fromList:asc:fusion" $
            whnf (\n -> M.fromList (unitValues [1..n])) bound
        , bench "fromList:random" $ whnf M.fromList elems_random
        , bench "fromList:random:fusion" $
            whnf (\(n,g) -> M.fromList (take n (unitValues (randoms g)))) (bound,gen)
        , bench "fromList:randomDups" $ whnf M.fromList elems_randomDups
        , bench "fromList:randomDups:fusion" $
            whnf
              (\(n,g) -> M.fromList (take n (unitValues (map word8ToInt (randoms g)))))
              (bound,gen)
        , bench "fromListWith:randomDups" $ whnf (M.fromListWith seq) elems_randomDups
        , bench "fromListWith:randomDups:fusion" $
            whnf
              (\(n,g) -> M.fromListWith seq (take n (unitValues (map word8ToInt (randoms g)))))
              (bound,gen)
        , bench "fromList:worstCase" $ whnf M.fromList elems_fromListWorstCase
        , bench "fromAscList" $ whnf M.fromAscList elems_asc
        , bench "fromAscList:fusion" $
            whnf (\n -> M.fromAscList (unitValues [1..n])) bound
        , bench "minView" $ whnf (maybe 0 (\((k,v), m) -> k+v+M.size m) . M.minViewWithKey)
                    (M.fromList $ zip [1..10] [1..10])
        , bench "spanAntitone" $ whnf (M.spanAntitone (<key_mid)) m
        , bench "split" $ whnf (M.split key_mid) m
        , bench "splitLookup" $ whnf (M.splitLookup key_mid) m
        , bench "eq" $ whnf (\m' -> m' == m') m -- worst case, compares everything
        , bench "compare" $ whnf (\m' -> compare m' m') m -- worst case, compares everything
        , bench "withoutKeys" $ -- dense, equal keys
            whnf (uncurry M.withoutKeys) (m, s)
        , bench "restrictKeys" $ -- dense, equal keys
            whnf (uncurry M.restrictKeys) (m, s)
        , bench "withoutKeys:random" $ -- large keys, no overlap
            whnf (uncurry M.withoutKeys) (m_random, s_random2)
        , bench "restrictKeys:random" $ -- large keys, no overlap
            whnf (uncurry M.restrictKeys) (m_random, s_random2)
        , bench "size" $ whnf M.size m
        , bench "compareSize:2" $ whnf (flip M.compareSize 2) m
        , bench "compareSize:n" $ whnf (flip M.compareSize bound) m
        , bgroup "folds" $ foldBenchmarks M.foldr M.foldl M.foldr' M.foldl' foldMap m
        , bgroup "folds with key" $
            foldWithKeyBenchmarks M.foldrWithKey M.foldlWithKey M.foldrWithKey' M.foldlWithKey' M.foldMapWithKey m
        ]
  where
    elems = elems_hits
    elems_hits   = zip keys values
    elems_mid    = zip (map (+ (bound `div` 2)) keys) values
    elems_most   = zip (map (+ (bound `div` 10)) keys) values
    elems_misses = zip (map (\x-> x * 2 + 1) keys) values
    elems_mixed = zip mixedKeys values
    elems_random = take bound (unitValues (randoms gen))
    elems_asc = unitValues [1..bound]
    -- Random elements in a small range to produce duplicates
    elems_randomDups = take bound (unitValues (map word8ToInt (randoms gen)))
    -- Worst case for the current fromList algorithm. Consider removing this
    -- test case if the algorithm changes.
    elems_fromListWorstCase =
      unitValues $
      take bound $
      concat
        [ take 63 (iterate (*2) 1)
        , take 63 (map negate (iterate (*2) 1))
        , interleave [1..] (map negate [1..])
        ]

    --------------------------------------------------------
    !bound = 2^12
    keys = [1..bound]
    keys' = fmap (+ 1000000) keys
    keys'' = fmap (* 2) [1..bound]
    mixedKeys = interleave keys keys'
    values = [1..bound]
    key_mid = bound `div` 2
    keys_random2 = take bound (randoms gen2)
    --------------------------------------------------------
    sum k v1 v2 = k + v1 + v2
    consPair k v xs = (k, v) : xs

------------------------------------------------------------
add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z
{-# INLINE add3 #-}

lookup :: [Int] -> M.IntMap Int -> Int
lookup xs m = foldl' (\n k -> fromMaybe n (M.lookup k m)) 0 xs

ins :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
ins xs m = foldl' (\m (k, v) -> M.insert k v m) m xs

insWith :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
insWith xs m = foldl' (\m (k, v) -> M.insertWith (+) k v m) m xs

insWithKey :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
insWithKey xs m = foldl' (\m (k, v) -> M.insertWithKey add3 k v m) m xs

insWith' :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
insWith' xs m = foldl' (\m (k, v) -> MS.insertWith (+) k v m) m xs

insWithKey' :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
insWithKey' xs m = foldl' (\m (k, v) -> MS.insertWithKey add3 k v m) m xs

data PairS a b = PS !a !b

insLookupWithKey :: [(Int, Int)] -> M.IntMap Int -> (Int, M.IntMap Int)
insLookupWithKey xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = M.insertLookupWithKey add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

del :: [Int] -> M.IntMap Int -> M.IntMap Int
del xs m = foldl' (\m k -> M.delete k m) m xs

upd :: [Int] -> M.IntMap Int -> M.IntMap Int
upd xs m = foldl' (\m k -> M.update Just k m) m xs

upd' :: [Int] -> M.IntMap Int -> M.IntMap Int
upd' xs m = foldl' (\m k -> snd $ M.updateLookupWithKey (\_ a -> Just a) k m) m xs

alt :: [Int] -> M.IntMap Int -> M.IntMap Int
alt xs m = foldl' (\m k -> M.alter id k m) m xs

maybeDel :: Int -> Maybe Int
maybeDel n | n `mod` 3 == 0 = Nothing
           | otherwise      = Just n

------------------------------------------------------------
interleave :: [Int] -> [Int] -> [Int]
interleave [] ys = ys
interleave (x:xs) (y:ys) = x : y : interleave xs ys

unitValues :: [Int] -> [(Int, ())]
unitValues = map (flip (,) ())
{-# INLINE unitValues #-}

gen, gen2 :: StdGen
gen = mkStdGen 42
gen2 = mkStdGen 90

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral
