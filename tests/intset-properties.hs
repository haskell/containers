{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- QuickCheck properties for Data.IntSet
-- > ghc -DTESTING -fforce-recomp -O2 --make -fhpc -i..  intset-properties.hs

import Data.Bits ((.&.))
import Data.IntSet
import Data.List (nub,sort)
import qualified Data.List as List
import qualified Data.Set as Set
import Prelude hiding (lookup, null, map ,filter)
import Test.QuickCheck hiding ((.&.))

main :: IO ()
main = do
    q $ label "prop_Single" prop_Single
    q $ label "prop_InsertDelete" prop_InsertDelete
    q $ label "prop_UnionInsert" prop_UnionInsert
    q $ label "prop_UnionAssoc" prop_UnionAssoc
    q $ label "prop_UnionComm" prop_UnionComm
    q $ label "prop_Diff" prop_Diff
    q $ label "prop_Int" prop_Int
    q $ label "prop_Ordered" prop_Ordered
    q $ label "prop_List" prop_List
    q $ label "prop_MaskPow2" prop_MaskPow2
    q $ label "prop_Prefix" prop_Prefix
    q $ label "prop_LeftRight" prop_LeftRight
    q $ label "prop_isProperSubsetOf" prop_isProperSubsetOf
    q $ label "prop_isProperSubsetOf2" prop_isProperSubsetOf2
  where
    q :: Testable prop => prop -> IO ()
    q = quickCheckWith args
{--------------------------------------------------------------------
  QuickCheck
--------------------------------------------------------------------}

args :: Args
args = stdArgs { maxSuccess = 500
               , maxDiscard = 500
               }

{--------------------------------------------------------------------
  Arbitrary, reasonably balanced trees
--------------------------------------------------------------------}
instance Arbitrary IntSet where
  arbitrary = do{ xs <- arbitrary
                ; return (fromList xs)
                }


{--------------------------------------------------------------------
  Single, Insert, Delete
--------------------------------------------------------------------}
prop_Single :: Int -> Bool
prop_Single x
  = (insert x empty == singleton x)

prop_InsertDelete :: Int -> IntSet -> Property
prop_InsertDelete k t
  = not (member k t) ==> delete k (insert k t) == t


{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
prop_UnionInsert :: Int -> IntSet -> Bool
prop_UnionInsert x t
  = union t (singleton x) == insert x t

prop_UnionAssoc :: IntSet -> IntSet -> IntSet -> Bool
prop_UnionAssoc t1 t2 t3
  = union t1 (union t2 t3) == union (union t1 t2) t3

prop_UnionComm :: IntSet -> IntSet -> Bool
prop_UnionComm t1 t2
  = (union t1 t2 == union t2 t1)

prop_Diff :: [Int] -> [Int] -> Bool
prop_Diff xs ys
  =  toAscList (difference (fromList xs) (fromList ys))
    == List.sort ((List.\\) (nub xs)  (nub ys))

prop_Int :: [Int] -> [Int] -> Bool
prop_Int xs ys
  =  toAscList (intersection (fromList xs) (fromList ys))
    == List.sort (nub ((List.intersect) (xs)  (ys)))

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
prop_Ordered
  = forAll (choose (5,100)) $ \n ->
    let xs = concat [[i-n,i-n]|i<-[0..2*n :: Int]]
    in fromAscList xs == fromList xs

prop_List :: [Int] -> Bool
prop_List xs
  = (sort (nub xs) == toAscList (fromList xs))

{--------------------------------------------------------------------
  Bin invariants
--------------------------------------------------------------------}
powersOf2 :: IntSet
powersOf2 = fromList [2^i | i <- [0..63]]

-- Check the invariant that the mask is a power of 2.
prop_MaskPow2 :: IntSet -> Bool
prop_MaskPow2 (Bin _ msk left right) = member msk powersOf2 && prop_MaskPow2 left && prop_MaskPow2 right
prop_MaskPow2 _ = True

-- Check that the prefix satisfies its invariant.
prop_Prefix :: IntSet -> Bool
prop_Prefix s@(Bin prefix msk left right) = all (\elem -> match elem prefix msk) (toList s) && prop_Prefix left && prop_Prefix right
prop_Prefix _ = True

-- Check that the left elements don't have the mask bit set, and the right
-- ones do.
prop_LeftRight :: IntSet -> Bool
prop_LeftRight (Bin _ msk left right) = and [x .&. msk == 0 | x <- toList left] && and [x .&. msk == msk | x <- toList right]
prop_LeftRight _ = True

{--------------------------------------------------------------------
  IntSet operations are like Set operations
--------------------------------------------------------------------}
toSet :: IntSet -> Set.Set Int
toSet = Set.fromList . toList

-- Check that IntSet.isProperSubsetOf is the same as Set.isProperSubsetOf.
prop_isProperSubsetOf :: IntSet -> IntSet -> Bool
prop_isProperSubsetOf a b = isProperSubsetOf a b == Set.isProperSubsetOf (toSet a) (toSet b)

-- In the above test, isProperSubsetOf almost always returns False (since a
-- random set is almost never a subset of another random set).  So this second
-- test checks the True case.
prop_isProperSubsetOf2 :: IntSet -> IntSet -> Bool
prop_isProperSubsetOf2 a b = isProperSubsetOf a c == (a /= c) where
  c = union a b
