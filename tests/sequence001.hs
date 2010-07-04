
module Main where

import Data.Sequence

main :: IO ()
main = do print $ dropWhileL (< 3) $ fromList [1..5]
          print $ dropWhileR (> 3) $ fromList [1..5]

