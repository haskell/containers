{-# LANGUAGE BangPatterns, CPP #-}
module LookupGE.Map where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Gauge (defaultMain, bench, bgroup, env, nf)
import Gauge (Benchmark)
import Data.List (foldl')
import Data.Map.Internal (Map(..))
import qualified Data.Map.Internal as M
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

benchmark :: Benchmark
benchmark =
    env (pure
            ( M.fromAscList elems_even :: M.Map Int Int
            , M.fromAscList elems_odd :: M.Map Int Int
            , M.fromAscList elems_large :: M.Map Int Int
            )
        )
        (\ms -> bgroup "lookupGE" [b f | b <- benches ms, f <- funs1])
  where
    bound = 2^10
    elems_even  = zip evens evens
    elems_odd   = zip odds odds
    elems_large = zip large large
    evens = [2,4..bound]
    odds  = [1,3..bound]
    large = [1,100..50*bound]
    benches ~(m_even, m_odd, m_large) =
          [ \(n,fun) -> bench (n++" present")  $ nf (fge fun evens) m_even
          , \(n,fun) -> bench (n++" absent")   $ nf (fge fun evens) m_odd
          , \(n,fun) -> bench (n++" far")      $ nf (fge fun odds)  m_large
          , \(n,fun) -> bench (n++" !present") $ nf (fge2 fun evens) m_even
          , \(n,fun) -> bench (n++" !absent")  $ nf (fge2 fun evens) m_odd
          , \(n,fun) -> bench (n++" !far")     $ nf (fge2 fun odds)  m_large
          ]
    funs1 = [ ("GE split",  lookupGE1)
            , ("GE caseof", lookupGE2)
            , ("GE Twan", lookupGE3)
            , ("GE Milan", lookupGE4) ]

fge :: (Int -> M.Map Int Int -> Maybe (Int,Int)) -> [Int] -> M.Map Int Int -> (Int,Int)
fge fun xs m = foldl' (\n k -> fromMaybe n (fun k m)) (0,0) xs

-- forcing values inside tuples!
fge2 :: (Int -> M.Map Int Int -> Maybe (Int,Int)) -> [Int] -> M.Map Int Int -> (Int,Int)
fge2 fun xs m = foldl' (\n@(!_, !_) k -> fromMaybe n (fun k m)) (0,0) xs

-------------------------------------------------------------------------------
-- Alternative implementations
-------------------------------------------------------------------------------

lookupGE1 :: Ord k => k -> Map k a -> Maybe (k,a)
lookupGE1 k m =
    case M.splitLookup k m of
        (_,Just v,_)  -> Just (k,v)
        (_,Nothing,r) -> findMinMaybe r
{-# INLINABLE lookupGE1 #-}

lookupGE2 :: Ord k => k -> Map k a -> Maybe (k,a)
lookupGE2 = go
  where
    go !_ Tip = Nothing
    go !k (Bin _ kx x l r) =
        case compare k kx of
            LT -> case go k l of
                    Nothing -> Just (kx,x)
                    ret -> ret
            GT -> go k r
            EQ -> Just (kx,x)
{-# INLINABLE lookupGE2 #-}

lookupGE3 :: Ord k => k -> Map k a -> Maybe (k,a)
lookupGE3 = go Nothing
  where
    go def !_ Tip = def
    go def !k (Bin _ kx x l r) =
        case compare k kx of
            LT -> go (Just (kx,x)) k l
            GT -> go def k r
            EQ -> Just (kx,x)
{-# INLINABLE lookupGE3 #-}

lookupGE4 :: Ord k => k -> Map k a -> Maybe (k,a)
lookupGE4 k = k `seq` goNothing
  where
    goNothing Tip = Nothing
    goNothing (Bin _ kx x l r) = case compare k kx of
                                   LT -> goJust kx x l
                                   EQ -> Just (kx, x)
                                   GT -> goNothing r

    goJust ky y Tip = Just (ky, y)
    goJust ky y (Bin _ kx x l r) = case compare k kx of
                                     LT -> goJust kx x l
                                     EQ -> Just (kx, x)
                                     GT -> goJust ky y r
{-# INLINABLE lookupGE4 #-}

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

findMinMaybe :: Map k a -> Maybe (k,a)
findMinMaybe (Bin _ kx x Tip _)  = Just (kx,x)
findMinMaybe (Bin _ _  _ l _)    = findMinMaybe l
findMinMaybe Tip                 = Nothing

#ifdef TESTING
-------------------------------------------------------------------------------
-- Properties:
-------------------------------------------------------------------------------

prop_lookupGE12 :: Int -> [Int] -> Bool
prop_lookupGE12 x xs = case fromList $ zip xs xs of m -> lookupGE1 x m == lookupGE2 x m

prop_lookupGE13 :: Int -> [Int] -> Bool
prop_lookupGE13 x xs = case fromList $ zip xs xs of m -> lookupGE1 x m == lookupGE3 x m

prop_lookupGE14 :: Int -> [Int] -> Bool
prop_lookupGE14 x xs = case fromList $ zip xs xs of m -> lookupGE1 x m == lookupGE4 x m
#endif
