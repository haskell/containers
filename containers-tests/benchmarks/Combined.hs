module Main where

import Map
import LookupGE.Map
import SetOperations.Map

import Gauge

main = do
    defaultMain
        [ Map.benchmark
        , bgroup "Map"
              [ SetOperations.Map.benchmark
              , LookupGE.Map.benchmark
              ]
        , bgroup "Set" []
        , bgroup "IntMap" []
        , bgroup "IntSet" []
        , bgroup "Sequence" []
        ]
