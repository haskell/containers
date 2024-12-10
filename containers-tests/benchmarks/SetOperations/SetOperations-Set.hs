module Main where

import Data.Set as C
import SetOperations

import Utils.Random (shuffle)

main :: IO ()
main = benchmark (fromList . shuffle) True
  [ ("union", C.union)
  , ("difference", C.difference)
  , ("intersection", C.intersection)
  , ("symmetricDifference", C.symmetricDifference)
  ]
