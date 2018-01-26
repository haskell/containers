module Main where

import Data.List (nub, nubBy)
import Data.Containers.ListUtils
import Test.Framework
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMain
         [   testProperty "nubOrd"     prop_nubOrd
           , testProperty "nubOrdOn"   prop_nubOrdOn
           , testProperty "nubInt"     prop_nubInt
           , testProperty "nubIntOn"     prop_nubIntOn
         ]


prop_nubOrd :: [Int] -> Bool
prop_nubOrd xs = nubOrd xs == nub xs

prop_nubInt :: [Int] -> Bool
prop_nubInt xs = nubInt xs == nub xs

prop_nubOrdOn :: [(Int,Int)] -> Bool
prop_nubOrdOn xs = nubOrdOn snd xs == nubBy (\x y -> snd x == snd y) xs

prop_nubIntOn :: [(Int,Int)] -> Bool
prop_nubIntOn xs = nubIntOn snd xs == nubBy (\x y -> snd x == snd y) xs
