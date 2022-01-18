{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

import qualified Data.List as List
import Test.Tasty
import Test.Tasty.QuickCheck
import Utils.Containers.Internal.BitUtil (wordSize)
import Utils.Containers.Internal.BitQueue
    ( BitQueue
    , emptyQB
    , snocQB
    , buildQ
    , toListQ )

default (Int)

main :: IO ()
main = defaultMain $ testGroup "bitqueue-properties" $ map testNum [0..(wordSize - 2)]

testNum :: Int -> TestTree
testNum n = testProperty ("Size "++show n) (prop_n n)

prop_n :: Int -> Gen Bool
prop_n n = checkList <$> vectorOf n (arbitrary :: Gen Bool)
  where
    checkList :: [Bool] -> Bool
    checkList values = toListQ q == values
      where
        q :: BitQueue
        !q = buildQ $ List.foldl' snocQB emptyQB values
