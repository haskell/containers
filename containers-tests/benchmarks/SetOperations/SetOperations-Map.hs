module Main where

import Data.Map as C
import SetOperations

import Utils.Random (shuffle)

main :: IO ()
main = benchmark (\xs -> fromList [(x, x) | x <- shuffle xs]) True
  [ ("union", C.union)
  , ("difference", C.difference)
  , ("intersection", C.intersection)
  , ("symmetricDifference", C.symmetricDifference)
  ]
