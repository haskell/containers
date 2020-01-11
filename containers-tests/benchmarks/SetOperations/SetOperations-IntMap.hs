module Main where

import Data.IntMap as C
import SetOperations

main = benchmark (\xs -> fromList [(x, x) | x <- xs]) True
    [ ("union", C.union)
    , ("unionWith", C.unionWith (+))
    , ("difference", C.difference)
    , ("differenceWith (keep)", C.differenceWith (\a b -> Just (a + b)))
    , ("differenceWith (delete)", C.differenceWith (\_ _ -> Nothing))
    , ("intersection", C.intersection)
    , ("intersectionWith", C.intersectionWith (+))
    ]
