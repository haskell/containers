{-# LANGUAGE BangPatterns #-}
module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Main
import Data.List (foldl')
import qualified Data.IntMap as M
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

instance (NFData a) => NFData (M.IntMap a) where
    rnf M.Nil = ()
    rnf (M.Tip x y) = rnf x `seq` rnf y 
    rnf (M.Bin p m l r) = rnf p `seq` rnf m `seq` rnf l `seq` rnf r

main = do
    let m = M.fromAscList elems :: M.IntMap Int
    defaultMainWith
        defaultConfig
        (liftIO . evaluate $ rnf [m])
        [ bench "lookup" $ nf (lookup keys) m
        , bench "insert" $ nf (ins elems) M.empty
{-      , bench "insertWith empty" $ nf (insWith elems) M.empty
        , bench "insertWith update" $ nf (insWith elems) m
   --     , bench "insertWith' empty" $ nf (insWith' elems) M.empty
   --     , bench "insertWith' update" $ nf (insWith' elems) m
        , bench "insertWithKey empty" $ nf (insWithKey elems) M.empty
        , bench "insertWithKey update" $ nf (insWithKey elems) m
   --     , bench "insertWithKey' empty" $ nf (insWithKey' elems) M.empty
   --     , bench "insertWithKey' update" $ nf (insWithKey' elems) m
        , bench "insertLookupWithKey empty" $
          nf (insLookupWithKey elems) M.empty
        , bench "insertLookupWithKey update" $
          nf (insLookupWithKey elems) m
--        , bench "insertLookupWithKey' empty" $
--          nf (insLookupWithKey' elems) M.empty
--        , bench "insertLookupWithKey' update" $
--          nf (insLookupWithKey' elems) m
-}
        , bench "map" $ nf (M.map (+ 1)) m
        , bench "mapWithKey" $ nf (M.mapWithKey (+)) m
        , bench "foldlWithKey" $ nf (ins elems) m
--        , bench "foldlWithKey'" $ nf (M.foldlWithKey' sum 0) m
--        , bench "foldrWithKey" $ nf (M.foldrWithKey consPair []) m
        , bench "delete" $ nf (del keys) m
        , bench "update" $ nf (upd keys) m
        , bench "updateLookupWithKey" $ nf (upd' keys) m
        , bench "alter"  $ nf (alt keys) m
        , bench "mapMaybe" $ nf (M.mapMaybe maybeDel) m
--        , bench "mapMaybeWithKey" $ nf (M.mapMaybeWithKey (const maybeDel)) m
        ]
  where
    elems = zip keys values
    keys = [1..2^12]
    values = [1..2^12]
    sum k v1 v2 = k + v1 + v2
    consPair k v xs = (k, v) : xs

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z
{-# INLINE add3 #-}

lookup :: [Int] -> M.IntMap Int -> Int
lookup xs m = foldl' (\n k -> fromMaybe n (M.lookup k m)) 0 xs

-- lookupIndex :: [Int] -> M.IntMap Int -> Int
-- lookupIndex xs m = foldl' (\n k -> fromMaybe n (M.lookupIndex k m)) 0 xs

ins :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
ins xs m = foldl' (\m (k, v) -> M.insert k v m) m xs

insWith :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
insWith xs m = foldl' (\m (k, v) -> M.insertWith (+) k v m) m xs

insWithKey :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
insWithKey xs m = foldl' (\m (k, v) -> M.insertWithKey add3 k v m) m xs

-- insWith' :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
-- insWith' xs m = foldl' (\m (k, v) -> M.insertWith' (+) k v m) m xs

-- insWithKey' :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
-- insWithKey' xs m = foldl' (\m (k, v) -> M.insertWithKey' add3 k v m) m xs

data PairS a b = PS !a !b

insLookupWithKey :: [(Int, Int)] -> M.IntMap Int -> (Int, M.IntMap Int)
insLookupWithKey xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = M.insertLookupWithKey add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

{-
insLookupWithKey' :: [(Int, Int)] -> M.Map Int Int -> (Int, M.Map Int Int)
insLookupWithKey' xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = M.insertLookupWithKey' add3 k v m
                        in PS (fromMaybe 0 n' + n) m'
-}

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
