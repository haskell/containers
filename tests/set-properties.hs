{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- QuickCheck properties for Data.Set
-- > ghc -DTESTING -fforce-recomp -O2 --make -fhpc -i..  set-properties.hs

import Data.List (nub,sort)
import qualified Data.List as List
import Data.Set
import Prelude hiding (lookup, null, map ,filter)
import Test.QuickCheck

main :: IO ()
main = do
    q $ label "prop_Valid" prop_Valid
    q $ label "prop_Single" prop_Single
    q $ label "prop_Single" prop_Single
    q $ label "prop_InsertValid" prop_InsertValid
    q $ label "prop_InsertValid" prop_InsertValid
    q $ label "prop_InsertDelete" prop_InsertDelete
    q $ label "prop_InsertDelete" prop_InsertDelete
    q $ label "prop_DeleteValid" prop_DeleteValid
    q $ label "prop_DeleteValid" prop_DeleteValid
    q $ label "prop_Join" prop_Join
    q $ label "prop_Join" prop_Join
    q $ label "prop_Merge" prop_Merge
    q $ label "prop_Merge" prop_Merge
    q $ label "prop_UnionValid" prop_UnionValid
    q $ label "prop_UnionValid" prop_UnionValid
    q $ label "prop_UnionInsert" prop_UnionInsert
    q $ label "prop_UnionInsert" prop_UnionInsert
    q $ label "prop_UnionAssoc" prop_UnionAssoc
    q $ label "prop_UnionAssoc" prop_UnionAssoc
    q $ label "prop_UnionComm" prop_UnionComm
    q $ label "prop_UnionComm" prop_UnionComm
    q $ label "prop_DiffValid" prop_DiffValid
    q $ label "prop_Diff" prop_Diff
    q $ label "prop_Diff" prop_Diff
    q $ label "prop_IntValid" prop_IntValid
    q $ label "prop_Int" prop_Int
    q $ label "prop_Int" prop_Int
    q $ label "prop_Ordered" prop_Ordered
    q $ label "prop_List" prop_List
    q $ label "prop_List" prop_List
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
instance (Enum a) => Arbitrary (Set a) where
    arbitrary = sized (arbtree 0 maxkey)
      where maxkey = 10000

arbtree :: (Enum a) => Int -> Int -> Int -> Gen (Set a)
arbtree lo hi n = do t <- gentree lo hi n
                     if balanced t then return t else arbtree lo hi n
  where gentree lo hi n
          | n <= 0    = return Tip
          | lo >= hi  = return Tip
          | otherwise = do  i  <- choose (lo,hi)
                            m  <- choose (1,70)
                            let (ml,mr) | m==(1::Int) = (1,2)
                                        | m==2        = (2,1)
                                        | m==3        = (1,1)
                                        | otherwise   = (2,2)
                            l  <- gentree lo (i-1) (n `div` ml)
                            r  <- gentree (i+1) hi (n `div` mr)
                            return (bin (toEnum i) l r)

{--------------------------------------------------------------------
  Valid tree's
--------------------------------------------------------------------}
forValid :: (Enum a,Show a,Testable b) => (Set a -> b) -> Property
forValid f = forAll arbitrary $ \t ->
--    classify (balanced t) "balanced" $
    classify (size t == 0) "empty" $
    classify (size t > 0  && size t <= 10) "small" $
    classify (size t > 10 && size t <= 64) "medium" $
    classify (size t > 64) "large" $
    balanced t ==> f t

forValidUnitTree :: Testable a => (Set Int -> a) -> Property
forValidUnitTree f = forValid f

prop_Valid :: Property
prop_Valid = forValidUnitTree $ \t -> valid t

{--------------------------------------------------------------------
  Single, Insert, Delete
--------------------------------------------------------------------}
prop_Single :: Int -> Bool
prop_Single x = (insert x empty == singleton x)

prop_InsertValid :: Int -> Property
prop_InsertValid k = forValidUnitTree $ \t -> valid (insert k t)

prop_InsertDelete :: Int -> Set Int -> Property
prop_InsertDelete k t = not (member k t) ==> delete k (insert k t) == t

prop_DeleteValid :: Int -> Property
prop_DeleteValid k = forValidUnitTree $ \t -> valid (delete k (insert k t))

{--------------------------------------------------------------------
  Balance
--------------------------------------------------------------------}
prop_Join :: Int -> Property
prop_Join x = forValidUnitTree $ \t ->
    let (l,r) = split x t
    in valid (join x l r)

prop_Merge :: Int -> Property
prop_Merge x = forValidUnitTree $ \t ->
    let (l,r) = split x t
    in valid (merge l r)

{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
prop_UnionValid :: Property
prop_UnionValid
  = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (union t1 t2)

prop_UnionInsert :: Int -> Set Int -> Bool
prop_UnionInsert x t = union t (singleton x) == insert x t

prop_UnionAssoc :: Set Int -> Set Int -> Set Int -> Bool
prop_UnionAssoc t1 t2 t3 = union t1 (union t2 t3) == union (union t1 t2) t3

prop_UnionComm :: Set Int -> Set Int -> Bool
prop_UnionComm t1 t2 = (union t1 t2 == union t2 t1)

prop_DiffValid :: Property
prop_DiffValid = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (difference t1 t2)

prop_Diff :: [Int] -> [Int] -> Bool
prop_Diff xs ys = toAscList (difference (fromList xs) (fromList ys))
                  == List.sort ((List.\\) (nub xs)  (nub ys))

prop_IntValid :: Property
prop_IntValid = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (intersection t1 t2)

prop_Int :: [Int] -> [Int] -> Bool
prop_Int xs ys = toAscList (intersection (fromList xs) (fromList ys))
                 == List.sort (nub ((List.intersect) (xs)  (ys)))

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
prop_Ordered :: Property
prop_Ordered = forAll (choose (5,100)) $ \n ->
    let xs = [0..n::Int]
    in fromAscList xs == fromList xs

prop_List :: [Int] -> Bool
prop_List xs = (sort (nub xs) == toList (fromList xs))
