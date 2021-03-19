module SetOperations.Map (benchmark) where

import Data.Map as C
import qualified SetOperations.Util

benchmark =
    SetOperations.Util.benchmark
        (\xs -> fromList [(x, x) | x <- xs]) True [("union", C.union), ("difference", C.difference), ("intersection", C.intersection)]
