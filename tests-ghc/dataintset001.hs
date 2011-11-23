
{-
Through 6.8.1 this printed False, should be True.
-}

module Main (main) where

import Data.IntSet

main :: IO ()
main = print $ isProperSubsetOf (fromList [2,3]) $ fromList [2,3,4]
