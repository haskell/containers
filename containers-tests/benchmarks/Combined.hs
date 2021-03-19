module Main where

import Map
import LookupGE.Map
import SetOperations.Map
import Set

import Gauge

main = do
    defaultMain
        [ Map.benchmark
        , bgroup "Map"
              [ SetOperations.Map.benchmark
              , LookupGE.Map.benchmark
              ]
        , Set.benchmark
        , bgroup "IntMap" []
        , bgroup "IntSet" []
        , bgroup "Sequence" []
        ]
