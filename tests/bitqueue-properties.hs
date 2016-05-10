{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative (Const(Const, getConst), pure, (<$>))
import qualified Data.List as List
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test, Testable)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Text.Show.Functions ()
import Data.Utils.BitQueue
    ( BitQueue
    , emptyQB
    , snocQB
    , buildQ
    , unconsQ
    , toListQ )

default (Int)

main :: IO ()
main = defaultMain $ map testNum [0..126]

testNum :: Int -> Test
testNum n = testProperty ("Size "++show n) (prop_n n)

prop_n :: Int -> Gen Bool
prop_n n = checkList <$> vectorOf n (arbitrary :: Gen Bool)
  where
    checkList :: [Bool] -> Bool
    checkList values = toListQ q == values
      where
        !q = buildQ $ List.foldl' snocQB emptyQB values
