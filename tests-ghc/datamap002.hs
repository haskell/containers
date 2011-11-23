
-- In 6.12 this failed

module Main (main) where

import Data.Map

main :: IO ()
main = print $ valid $ deleteMin $ deleteMin
     $ fromList [ (i, ()) | i <- [0,2,5,1,6,4,8,9,7,11,10,3] ]

