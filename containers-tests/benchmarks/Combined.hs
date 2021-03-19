module Main where

import Map
import SetOperations.Map

import Gauge

main = do
    defaultMain
        [ Map.benchmarks
        , SetOperations.Map.benchmarks
        , bgroup "Set" []
        , bgroup "IntMap" []
        , bgroup "IntSet" []
        , bgroup "Sequence" []
        ]
