{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module Main where

#if MIN_VERSION_base(4,9,0)
import Control.Applicative (Const(..))
#endif
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Criterion.Main (bench, defaultMain, whnf)
#if MIN_VERSION_base(4,8,0)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (..))
#endif
import Data.List (foldl')
import qualified Data.IntMap as M
import qualified Data.IntMap.Strict as MS
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

main = do
    let m = M.fromAscList elems :: M.IntMap Int
        m_even = M.fromAscList elems_even :: M.IntMap Int
        m_odd = M.fromAscList elems_odd :: M.IntMap Int
    evaluate $ rnf [m]
    defaultMain
        [ bench "lookup" $ whnf (lookup keys) m
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
        , bench "foldlWithKey" $ whnf (ins elems) m
        , bench "foldlWithKey'" $ whnf (M.foldlWithKey' sum 0) m
        , bench "foldrWithKey" $ whnf (M.foldrWithKey consPair []) m
        , bench "delete" $ whnf (del keys) m
        , bench "update" $ whnf (upd keys) m
        , bench "updateLookupWithKey" $ whnf (upd' keys) m
        , bench "alter"  $ whnf (alt keys) m
        , bench "alterF lookup absent" $ whnf (atLookup evens) m_odd
        , bench "alterF lookup present" $ whnf (atLookup evens) m_even
        , bench "alterF no rules lookup absent" $ whnf (atLookupNoRules evens) m_odd
        , bench "alterF no rules lookup present" $ whnf (atLookupNoRules evens) m_even
        , bench "alterF insert absent" $ whnf (atIns elems_even) m_odd
        , bench "alterF insert present" $ whnf (atIns elems_even) m_even
        , bench "alterF no rules insert absent" $ whnf (atInsNoRules elems_even) m_odd
        , bench "alterF no rules insert present" $ whnf (atInsNoRules elems_even) m_even
        , bench "alterF delete absent" $ whnf (atDel evens) m_odd
        , bench "alterF delete present" $ whnf (atDel evens) m
        , bench "alterF no rules delete absent" $ whnf (atDelNoRules evens) m_odd
        , bench "alterF no rules delete present" $ whnf (atDelNoRules evens) m
        , bench "alterF alter absent" $ whnf (atAlt id evens) m_odd
        , bench "alterF alter insert" $ whnf (atAlt (const (Just 1)) evens) m_odd
        , bench "alterF alter update" $ whnf (atAlt id evens) m_even
        , bench "alterF alter delete" $ whnf (atAlt (const Nothing) evens) m
        , bench "alterF no rules alter absent" $ whnf (atAltNoRules id evens) m_odd
        , bench "alterF no rules alter insert" $ whnf (atAltNoRules (const (Just 1)) evens) m_odd
        , bench "alterF no rules alter update" $ whnf (atAltNoRules id evens) m_even
        , bench "alterF no rules alter delete" $ whnf (atAltNoRules (const Nothing) evens) m
        , bench "mapMaybe" $ whnf (M.mapMaybe maybeDel) m
        , bench "mapMaybeWithKey" $ whnf (M.mapMaybeWithKey (const maybeDel)) m
        , bench "fromList" $ whnf M.fromList elems
        , bench "fromAscList" $ whnf M.fromAscList elems
        , bench "fromDistinctAscList" $ whnf M.fromDistinctAscList elems
        , bench "minView" $ whnf (maybe 0 (\((k,v), m) -> k+v+M.size m) . M.minViewWithKey)
                    (M.fromList $ zip [1..10] [1..10])
        ]
  where
    elems = zip keys values
    elems_even = zip evens evens
    elems_odd = zip odds odds
    keys = [1..2^12]
    evens = [2,4..2^12]
    odds = [1,3..2^12]
    values = [1..2^12]
    sum k v1 v2 = k + v1 + v2
    consPair k v xs = (k, v) : xs

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

newtype TestIdentity a = TestIdentity { runTestIdentity :: a }

instance Functor TestIdentity where
#if MIN_VERSION_base(4,8,0)
  fmap = coerce
#else
  fmap f (Ident a) = Ident (f a)
#endif

newtype TestConst a b = TestConst { getTestConst :: a }
instance Functor (TestConst a) where
  fmap _ (TestConst a) = TestConst a

atLookup :: [Int] -> M.IntMap Int -> Int
atLookup xs m = foldl' (\n k -> fromMaybe n (getConst (M.alterF Const k m))) 0 xs

atLookupNoRules :: [Int] -> M.IntMap Int -> Int
atLookupNoRules xs m =
  foldl' (\n k -> fromMaybe n (getTestConst (M.alterF TestConst k m))) 0 xs

atIns :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
atIns xs m =
  foldl' (\m (k, v) -> runIdentity (M.alterF (\_ -> Identity (Just v)) k m)) m xs

atInsNoRules :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
atInsNoRules xs m =
  foldl' (\m (k, v) -> runTestIdentity (M.alterF (\_ -> TestIdentity (Just v)) k m)) m xs

atDel :: [Int] -> M.IntMap Int -> M.IntMap Int
atDel xs m = foldl' (\m k -> runIdentity (M.alterF (\_ -> Identity Nothing) k m)) m xs

atDelNoRules :: [Int] -> M.IntMap Int -> M.IntMap Int
atDelNoRules xs m =
  foldl' (\m k -> runTestIdentity (M.alterF (\_ -> TestIdentity Nothing) k m)) m xs

atAlt :: (Maybe Int -> Maybe Int) -> [Int] -> M.IntMap Int -> M.IntMap Int
atAlt f xs m = foldl' (\m k -> runIdentity (M.alterF (Identity . f) k m)) m xs

atAltNoRules :: (Maybe Int -> Maybe Int) -> [Int] -> M.IntMap Int -> M.IntMap Int
atAltNoRules f xs m =
  foldl' (\m k -> runTestIdentity (M.alterF (TestIdentity . f) k m)) m xs

maybeDel :: Int -> Maybe Int
maybeDel n | n `mod` 3 == 0 = Nothing
           | otherwise      = Just n
