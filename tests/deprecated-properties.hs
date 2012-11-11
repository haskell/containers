{-# LANGUAGE CPP #-}

-- This module tests the deprecated properties of Data.Map and Data.IntMap,
-- because these cannot be tested in either map-properties or
-- intmap-properties, as these modules are designed to work with the .Lazy and
-- .Strict modules.

import qualified Data.Map as M
import qualified Data.Map.Strict as SM
import qualified Data.IntMap as IM
import qualified Data.IntMap.Strict as SIM

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Text.Show.Functions ()

default (Int)

main :: IO ()
main = defaultMain
         [ testProperty "Data.Map.insertWith' as Strict.insertWith" prop_mapInsertWith'Strict
         , testProperty "Data.Map.insertWith' undefined value" prop_mapInsertWith'Undefined
         , testProperty "Data.Map.insertWithKey' as Strict.insertWithKey" prop_mapInsertWithKey'Strict
         , testProperty "Data.Map.insertWithKey' undefined value" prop_mapInsertWithKey'Undefined
         , testProperty "Data.Map.insertLookupWithKey' as Strict.insertLookupWithKey" prop_mapInsertLookupWithKey'Strict
         , testProperty "Data.Map.insertLookupWithKey' undefined value" prop_mapInsertLookupWithKey'Undefined
         , testProperty "Data.IntMap.insertWith' as Strict.insertWith" prop_intmapInsertWith'Strict
         , testProperty "Data.IntMap.insertWith' undefined value" prop_intmapInsertWith'Undefined
         , testProperty "Data.IntMap.insertWithKey' as Strict.insertWithKey" prop_intmapInsertWithKey'Strict
         , testProperty "Data.IntMap.insertWithKey' undefined value" prop_intmapInsertWithKey'Undefined
         ]


---------- Map properties ----------

prop_mapInsertWith'Strict :: [(Int, Int)] -> (Int -> Int -> Int) -> [(Int, Int)] -> Bool
prop_mapInsertWith'Strict xs f kxxs =
  let m = M.fromList xs
      insertList ins = foldr (\(kx, x) -> ins f kx x) m kxxs
  in insertList M.insertWith' == insertList SM.insertWith

prop_mapInsertWith'Undefined :: [(Int, Int)] -> Bool
prop_mapInsertWith'Undefined xs =
  let m = M.fromList xs
      f _ x = x * 33
      insertList ins = foldr (\(kx, _) -> ins f kx undefined) m xs
  in insertList M.insertWith' == insertList M.insertWith

prop_mapInsertWithKey'Strict :: [(Int, Int)] -> (Int -> Int -> Int -> Int) -> [(Int, Int)] -> Bool
prop_mapInsertWithKey'Strict xs f kxxs =
  let m = M.fromList xs
      insertList ins = foldr (\(kx, x) -> ins f kx x) m kxxs
  in insertList M.insertWithKey' == insertList SM.insertWithKey

prop_mapInsertWithKey'Undefined :: [(Int, Int)] -> Bool
prop_mapInsertWithKey'Undefined xs =
  let m = M.fromList xs
      f k _ x = (k + x) * 33
      insertList ins = foldr (\(kx, _) -> ins f kx undefined) m xs
  in insertList M.insertWithKey' == insertList M.insertWithKey

prop_mapInsertLookupWithKey'Strict :: [(Int, Int)] -> (Int -> Int -> Int -> Int) -> [(Int, Int)] -> Bool
prop_mapInsertLookupWithKey'Strict xs f kxxs =
  let m = M.fromList xs
      insertLookupList insLkp = scanr (\(kx, x) (_, mp) -> insLkp f kx x mp) (Nothing, m) kxxs
  in insertLookupList M.insertLookupWithKey' == insertLookupList SM.insertLookupWithKey

prop_mapInsertLookupWithKey'Undefined :: [(Int, Int)] -> Bool
prop_mapInsertLookupWithKey'Undefined xs =
  let m = M.fromList xs
      f k _ x = (k + x) * 33
      insertLookupList insLkp = scanr (\(kx, _) (_, mp) -> insLkp f kx undefined mp) (Nothing, m) xs
  in insertLookupList M.insertLookupWithKey' == insertLookupList M.insertLookupWithKey


---------- IntMap properties ----------

prop_intmapInsertWith'Strict :: [(Int, Int)] -> (Int -> Int -> Int) -> [(Int, Int)] -> Bool
prop_intmapInsertWith'Strict xs f kxxs =
  let m = IM.fromList xs
      insertList ins = foldr (\(kx, x) -> ins f kx x) m kxxs
  in insertList IM.insertWith' == insertList SIM.insertWith

prop_intmapInsertWith'Undefined :: [(Int, Int)] -> Bool
prop_intmapInsertWith'Undefined xs =
  let m = IM.fromList xs
      f _ x = x * 33
      insertList ins = foldr (\(kx, _) -> ins f kx undefined) m xs
  in insertList IM.insertWith' == insertList IM.insertWith

prop_intmapInsertWithKey'Strict :: [(Int, Int)] -> (Int -> Int -> Int -> Int) -> [(Int, Int)] -> Bool
prop_intmapInsertWithKey'Strict xs f kxxs =
  let m = IM.fromList xs
      insertList ins = foldr (\(kx, x) -> ins f kx x) m kxxs
  in insertList IM.insertWithKey' == insertList SIM.insertWithKey

prop_intmapInsertWithKey'Undefined :: [(Int, Int)] -> Bool
prop_intmapInsertWithKey'Undefined xs =
  let m = IM.fromList xs
      f k _ x = (k + x) * 33
      insertList ins = foldr (\(kx, _) -> ins f kx undefined) m xs
  in insertList IM.insertWithKey' == insertList IM.insertWithKey
