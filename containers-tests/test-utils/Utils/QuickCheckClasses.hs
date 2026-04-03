module Utils.QuickCheckClasses
  ( testLaws
  ) where

import Test.QuickCheck.Classes.Base (Laws(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)

testLaws :: Laws -> TestTree
testLaws (Laws name tests) = testProperties name tests
