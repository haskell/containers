{-# LANGUAGE CPP #-}
import Control.Applicative (Const(..))
import Data.Bits ((.&.), popCount)
import Data.Word (Word)
import Data.IntSet
import Data.List (nub,sort)
import qualified Data.List as List
import Data.Maybe (listToMaybe)
import Data.Monoid (mempty)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import IntSetValidity (valid)
import Prelude hiding (lookup, null, map, filter, foldr, foldl, foldl', foldMap)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))

main :: IO ()
main = defaultMain $ testGroup "intset-properties"
                   [ testCase "lookupLT" test_lookupLT
                   , testCase "lookupGT" test_lookupGT
                   , testCase "lookupLE" test_lookupLE
                   , testCase "lookupGE" test_lookupGE
                   , testCase "split" test_split
                   , testCase "isProperSubsetOf" test_isProperSubsetOf
                   , testCase "compareSize" test_compareSize
                   , testProperty "prop_Valid" prop_Valid
                   , testProperty "prop_EmptyValid" prop_EmptyValid
                   , testProperty "prop_SingletonValid" prop_SingletonValid
                   , testProperty "prop_InsertIntoEmptyValid" prop_InsertIntoEmptyValid
                   , testProperty "prop_instanceEqIntSet" prop_instanceEqIntSet
                   , testProperty "prop_instanceOrdIntSet" prop_instanceOrdIntSet
                   , testProperty "prop_Single" prop_Single
                   , testProperty "prop_Member" prop_Member
                   , testProperty "prop_NotMember" prop_NotMember
                   , testProperty "prop_LookupLT" prop_LookupLT
                   , testProperty "prop_LookupGT" prop_LookupGT
                   , testProperty "prop_LookupLE" prop_LookupLE
                   , testProperty "prop_LookupGE" prop_LookupGE
                   , testProperty "prop_InsertDelete" prop_InsertDelete
                   , testProperty "prop_MemberFromList" prop_MemberFromList
                   , testProperty "prop_UnionInsert" prop_UnionInsert
                   , testProperty "prop_UnionAssoc" prop_UnionAssoc
                   , testProperty "prop_UnionComm" prop_UnionComm
                   , testProperty "prop_union" prop_union
                   , testProperty "prop_difference" prop_difference
                   , testProperty "prop_intersection" prop_intersection
                   , testProperty "prop_symmetricDifference" prop_symmetricDifference
                   , testProperty "prop_List" prop_List
                   , testProperty "prop_DescList" prop_DescList
                   , testProperty "prop_AscDescList" prop_AscDescList
                   , testProperty "prop_fromList" prop_fromList
                   , testProperty "prop_fromRange" prop_fromRange
                   , testProperty "prop_isProperSubsetOf" prop_isProperSubsetOf
                   , testProperty "prop_isProperSubsetOf2" prop_isProperSubsetOf2
                   , testProperty "prop_isSubsetOf" prop_isSubsetOf
                   , testProperty "prop_isSubsetOf2" prop_isSubsetOf2
                   , testProperty "prop_disjoint" prop_disjoint
                   , testProperty "prop_size" prop_size
                   , testProperty "prop_lookupMin" prop_lookupMin
                   , testProperty "prop_lookupMax" prop_lookupMax
                   , testProperty "prop_findMax" prop_findMax
                   , testProperty "prop_findMin" prop_findMin
                   , testProperty "prop_ord" prop_ord
                   , testProperty "prop_readShow" prop_readShow
                   , testProperty "prop_foldr" prop_foldr
                   , testProperty "prop_foldr'" prop_foldr'
                   , testProperty "prop_foldl" prop_foldl
                   , testProperty "prop_foldl'" prop_foldl'
                   , testProperty "prop_foldMap" prop_foldMap
                   , testProperty "prop_map" prop_map
                   , testProperty "prop_mapMonotonicId" prop_mapMonotonicId
                   , testProperty "prop_mapMonotonicLinear" prop_mapMonotonicLinear
                   , testProperty "prop_maxView" prop_maxView
                   , testProperty "prop_minView" prop_minView
                   , testProperty "prop_split" prop_split
                   , testProperty "prop_splitMember" prop_splitMember
                   , testProperty "prop_splitRoot" prop_splitRoot
                   , testProperty "prop_partition" prop_partition
                   , testProperty "prop_filter" prop_filter
                   , testProperty "takeWhileAntitone" prop_takeWhileAntitone
                   , testProperty "dropWhileAntitone" prop_dropWhileAntitone
                   , testProperty "spanAntitone" prop_spanAntitone
                   , testProperty "prop_alterF_list" prop_alterF_list
                   , testProperty "prop_alterF_const" prop_alterF_const
                   , testProperty "intersections" prop_intersections
                   , testProperty "intersections_lazy" prop_intersections_lazy
                   , testProperty "insert" prop_insert
                   , testProperty "delete" prop_delete
                   , testProperty "deleteMin" prop_deleteMin
                   , testProperty "deleteMax" prop_deleteMax
                   , testProperty "fromAscList" prop_fromAscList
                   , testProperty "fromDistinctAscList" prop_fromDistinctAscList
                   , testProperty "compareSize" prop_compareSize
                   ]

----------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------

test_lookupLT :: Assertion
test_lookupLT = do
    lookupLT 3 (fromList [3, 5]) @?= Nothing
    lookupLT 5 (fromList [3, 5]) @?= Just 3

test_lookupGT :: Assertion
test_lookupGT = do
   lookupGT 4 (fromList [3, 5]) @?= Just 5
   lookupGT 5 (fromList [3, 5]) @?= Nothing

test_lookupLE :: Assertion
test_lookupLE = do
   lookupLE 2 (fromList [3, 5]) @?= Nothing
   lookupLE 4 (fromList [3, 5]) @?= Just 3
   lookupLE 5 (fromList [3, 5]) @?= Just 5

test_lookupGE :: Assertion
test_lookupGE = do
   lookupGE 3 (fromList [3, 5]) @?= Just 3
   lookupGE 4 (fromList [3, 5]) @?= Just 5
   lookupGE 6 (fromList [3, 5]) @?= Nothing

test_split :: Assertion
test_split = do
   split 3 (fromList [1..5]) @?= (fromList [1,2], fromList [4,5])

test_isProperSubsetOf :: Assertion
test_isProperSubsetOf = do
    isProperSubsetOf (fromList [1]) (fromList [1,2]) @?= True
    isProperSubsetOf (fromList [1,2]) (fromList [1,2]) @?= False
    isProperSubsetOf (fromList [1,2]) (fromList [1]) @?= False

    isProperSubsetOf (fromList [-1]) (fromList [-1,2]) @?= True
    isProperSubsetOf (fromList [-1,2]) (fromList [-1,2]) @?= False
    isProperSubsetOf (fromList [-1,2]) (fromList [-1]) @?= False

    -- See Github #1007
    isProperSubsetOf (fromList [-65,-1]) (fromList [-65,-1,0]) @?= True

-- Check cases where there is a risk of overflow
test_compareSize :: Assertion
test_compareSize = do
  compareSize (fromList [1]) minBound @?= GT
  compareSize (fromList [1]) maxBound @?= LT

{--------------------------------------------------------------------
  Arbitrary, reasonably balanced trees
--------------------------------------------------------------------}
instance Arbitrary IntSet where
  arbitrary = fromList <$> oneof [arbitrary, fmap (fmap getLarge) arbitrary]
  shrink = fmap fromList . shrink . toAscList

{--------------------------------------------------------------------
  Valid IntMaps
--------------------------------------------------------------------}
forValid :: Testable a => (IntSet -> a) -> Property
forValid f = forAll arbitrary $ \t ->
    classify (size t == 0) "empty" $
    classify (size t > 0 && size t <= 10) "small" $
    classify (size t > 10 && size t <= 64) "medium" $
    classify (size t > 64) "large" $ f t

forValidUnitTree :: Testable a => (IntSet -> a) -> Property
forValidUnitTree f = forValid f

prop_Valid :: Property
prop_Valid = forValidUnitTree $ \t -> valid t

{--------------------------------------------------------------------
  Construction validity
--------------------------------------------------------------------}

prop_EmptyValid :: Property
prop_EmptyValid =
    valid empty

prop_SingletonValid :: Int -> Property
prop_SingletonValid x =
    valid (singleton x)

prop_InsertIntoEmptyValid :: Int -> Property
prop_InsertIntoEmptyValid x =
    valid (insert x empty)

{--------------------------------------------------------------------
  Instances for Eq and Ord
--------------------------------------------------------------------}

prop_instanceEqIntSet :: IntSet -> IntSet -> Bool
prop_instanceEqIntSet x y = (x == y) == (toAscList x == toAscList y)

prop_instanceOrdIntSet :: IntSet -> IntSet -> Bool
prop_instanceOrdIntSet x y = (compare x y) == (compare (toAscList x) (toAscList y))

{--------------------------------------------------------------------
  Single, Member, Insert, Delete, Member, FromList
--------------------------------------------------------------------}
prop_Single :: Int -> Bool
prop_Single x
  = (insert x empty == singleton x)

prop_Member :: [Int] -> Int -> Bool
prop_Member xs n =
  let m  = fromList xs
  in all (\k -> k `member` m == (k `elem` xs)) (n : xs)

prop_NotMember :: [Int] -> Int -> Bool
prop_NotMember xs n =
  let m  = fromList xs
  in all (\k -> k `notMember` m == (k `notElem` xs)) (n : xs)

prop_LookupLT :: Int -> IntSet -> Property
prop_LookupLT x xs = lookupLT x xs === List.find (<x) (toDescList xs)

prop_LookupGT :: Int -> IntSet -> Property
prop_LookupGT x xs = lookupGT x xs === List.find (>x) (toList xs)

prop_LookupLE :: Int -> IntSet -> Property
prop_LookupLE x xs = lookupLE x xs === List.find (<=x) (toDescList xs)

prop_LookupGE :: Int -> IntSet -> Property
prop_LookupGE x xs = lookupGE x xs === List.find (>=x) (toList xs)

prop_InsertDelete :: Int -> IntSet -> Property
prop_InsertDelete k t
  = not (member k t) ==>
      case delete k (insert k t) of
        t' -> valid t' .&&. t' === t

prop_MemberFromList :: [Int] -> Bool
prop_MemberFromList xs
  = all (`member` t) abs_xs && all ((`notMember` t) . negate) abs_xs
  where abs_xs = [abs x | x <- xs, x /= 0]
        t = fromList abs_xs

{--------------------------------------------------------------------
  Union, Difference and Intersection
--------------------------------------------------------------------}
prop_UnionInsert :: Int -> IntSet -> Property
prop_UnionInsert x t =
  case union t (singleton x) of
    t' ->
      valid t' .&&.
      t' === insert x t

prop_UnionAssoc :: IntSet -> IntSet -> IntSet -> Bool
prop_UnionAssoc t1 t2 t3
  = union t1 (union t2 t3) == union (union t1 t2) t3

prop_UnionComm :: IntSet -> IntSet -> Bool
prop_UnionComm t1 t2
  = (union t1 t2 == union t2 t1)

prop_union :: IntSet -> IntSet -> Property
prop_union xs ys =
  case union xs ys of
    t ->
      valid t .&&.
      toAscList t === List.nub (List.sort (toAscList xs ++ toAscList ys))

prop_difference :: IntSet -> IntSet -> Property
prop_difference xs ys =
  case difference xs ys of
    t ->
      valid t .&&.
      toAscList t === (toAscList xs List.\\ toAscList ys)

prop_intersection :: IntSet -> IntSet -> Property
prop_intersection xs ys =
  case intersection xs ys of
    t ->
      valid t .&&.
      toAscList t === (toAscList xs `List.intersect` toAscList ys)

prop_symmetricDifference :: IntSet -> IntSet -> Property
prop_symmetricDifference xs ys =
  case symmetricDifference xs ys of
    t ->
      valid t .&&.
      toAscList t ===
      List.sort (List.filter (`notElem` xs') ys' ++ List.filter (`notElem` ys') xs')
    where
      xs' = toAscList xs
      ys' = toAscList ys

prop_disjoint :: IntSet -> IntSet -> Bool
prop_disjoint a b = a `disjoint` b == null (a `intersection` b)

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

prop_List :: [Int] -> Bool
prop_List xs
  = (sort (nub xs) == toAscList (fromList xs))

prop_DescList :: [Int] -> Bool
prop_DescList xs = (reverse (sort (nub xs)) == toDescList (fromList xs))

prop_AscDescList :: [Int] -> Bool
prop_AscDescList xs = toAscList s == reverse (toDescList s)
  where s = fromList xs

prop_fromList :: [Int] -> Property
prop_fromList xs
  = case fromList xs of
      t -> valid t .&&.
           t === fromAscList sort_xs .&&.
           t === fromDistinctAscList nub_sort_xs .&&.
           t === List.foldr insert empty xs
  where sort_xs = sort xs
        nub_sort_xs = List.map List.head $ List.group sort_xs

prop_fromRange :: Property
prop_fromRange = forAll (scale (*100) arbitrary) go
  where
    go (l,h) = valid t .&&. t === fromAscList [l..h]
      where t = fromRange (l,h)

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

prop_isSubsetOf :: IntSet -> IntSet -> Bool
prop_isSubsetOf a b = isSubsetOf a b == Set.isSubsetOf (toSet a) (toSet b)

prop_isSubsetOf2 :: IntSet -> IntSet -> Bool
prop_isSubsetOf2 a b = isSubsetOf a (union a b)

prop_size :: IntSet -> Property
prop_size s = sz === foldl' (\i _ -> i + 1) (0 :: Int) s .&&.
              sz === List.length (toList s)
  where sz = size s

prop_lookupMin :: IntSet -> Property
prop_lookupMin s = lookupMin s === listToMaybe (toAscList s)

prop_lookupMax :: IntSet -> Property
prop_lookupMax s = lookupMax s === listToMaybe (toDescList s)

prop_findMax :: IntSet -> Property
prop_findMax s = not (null s) ==> findMax s == maximum (toList s)

prop_findMin :: IntSet -> Property
prop_findMin s = not (null s) ==> findMin s == minimum (toList s)

prop_ord :: IntSet -> IntSet -> Bool
prop_ord s1 s2 = s1 `compare` s2 == toList s1 `compare` toList s2

prop_readShow :: IntSet -> Bool
prop_readShow s = s == read (show s)

prop_foldr :: IntSet -> Property
prop_foldr s = foldr (:) [] s === toList s

prop_foldr' :: IntSet -> Property
prop_foldr' s = foldr' (:) [] s === toList s

prop_foldl :: IntSet -> Property
prop_foldl s = foldl (flip (:)) [] s === toDescList s

prop_foldl' :: IntSet -> Property
prop_foldl' s = foldl' (flip (:)) [] s === toDescList s

prop_foldMap :: IntSet -> Property
prop_foldMap s = foldMap (:[]) s === toList s

prop_map :: IntSet -> Bool
prop_map s = map id s == s

-- Note: we could generate an arbitrary strictly monotonic function by
-- restricting f using @\x y -> x < y ==> f x < f y@
-- but this will be inefficient given the probability of actually finding
-- a function that meets the criteria.
-- For now we settle on identity function and arbitrary linear functions
-- f x = a*x + b (with a being positive).
-- This might be insufficient to support any fancier implementation.
prop_mapMonotonicId :: IntSet -> Property
prop_mapMonotonicId s = mapMonotonic id s === map id s

prop_mapMonotonicLinear :: Positive Int -> Int -> IntSet -> Property
prop_mapMonotonicLinear (Positive a) b s =
  all ok (toList s) ==>
    mapMonotonic f s === map f s
  where
    f x = a*x + b
    ok x =  -- must not overflow
      fromIntegral (minBound :: Int) <= y && y <= fromIntegral (maxBound :: Int)
      where
        y = fromIntegral a * fromIntegral x + fromIntegral b :: Integer

prop_maxView :: IntSet -> Bool
prop_maxView s = case maxView s of
    Nothing -> null s
    Just (m,s') -> m == maximum (toList s) && s == insert m s' && m `notMember` s'

prop_minView :: IntSet -> Bool
prop_minView s = case minView s of
    Nothing -> null s
    Just (m,s') -> m == minimum (toList s) && s == insert m s' && m `notMember` s'

prop_split :: IntSet -> Int -> Property
prop_split s i = case split i s of
    (s1,s2) -> valid s1 .&&.
               valid s2 .&&.
               all (<i) (toList s1) .&&.
               all (>i) (toList s2) .&&.
               i `delete` s === union s1 s2

prop_splitMember :: IntSet -> Int -> Property
prop_splitMember s i = case splitMember i s of
    (s1,t,s2) -> valid s1 .&&.
                 valid s2 .&&.
                 all (<i) (toList s1) .&&.
                 all (>i) (toList s2) .&&.
                 t === i `member` s .&&.
                 i `delete` s === union s1 s2

prop_splitRoot :: IntSet -> Bool
prop_splitRoot s = loop ls && (s == unions ls)
 where
  ls = splitRoot s
  loop [] = True
  loop (s1:rst) = List.null
                  [ (x,y) | x <- toList s1
                          , y <- toList (unions rst)
                          , x > y ]

prop_partition :: IntSet -> Int -> Property
prop_partition s i = case partition odd s of
    (s1,s2) -> valid s1 .&&.
               valid s2 .&&.
               all odd (toList s1) .&&.
               all even (toList s2) .&&.
               s === s1 `union` s2

prop_filter :: IntSet -> Int -> Property
prop_filter s i =
  let parts = partition odd s
      odds = filter odd s
      evens = filter even s
  in valid odds .&&.
     valid evens .&&.
     parts === (odds, evens)

prop_takeWhileAntitone :: Int -> [Int] -> Property
prop_takeWhileAntitone x ys =
  let l = takeWhileAntitone (<x) (fromList ys)
  in  valid l .&&.
      l === fromList (List.filter (<x) ys)

prop_dropWhileAntitone :: Int -> [Int] -> Property
prop_dropWhileAntitone x ys =
  let r = dropWhileAntitone (<x) (fromList ys)
  in  valid r .&&.
      r === fromList (List.filter (>=x) ys)

prop_spanAntitone :: Int -> [Int] -> Property
prop_spanAntitone x ys =
  let (l, r) = spanAntitone (<x) (fromList ys)
  in  valid l .&&.
      valid r .&&.
      l === fromList (List.filter (<x) ys) .&&.
      r === fromList (List.filter (>=x) ys)

prop_alterF_list
    :: Fun Bool [Bool]
    -> Int
    -> IntSet
    -> Property
prop_alterF_list f k s =
        fmap toSet (alterF     (applyFun f) k s)
    ===             Set.alterF (applyFun f) k (toSet s)

prop_alterF_const
    :: Fun Bool Bool
    -> Int
    -> IntSet
    -> Property
prop_alterF_const f k s =
        getConst (alterF     (Const . applyFun f) k s        )
    === getConst (Set.alterF (Const . applyFun f) k (toSet s))

prop_intersections :: (IntSet, [IntSet]) -> Property
prop_intersections (s, ss) =
  intersections ss' === List.foldl' intersection s ss
  where
    ss' = s :| ss -- Work around missing Arbitrary NonEmpty instance

prop_intersections_lazy :: [IntSet] -> Property
prop_intersections_lazy ss = intersections ss' === empty
  where
    ss' = NE.fromList $ ss ++ [empty] ++ error "too strict"
                           --- ^ result will certainly be empty at this point,
                           --    so the rest of the list should not be demanded.

prop_insert :: Int -> IntSet -> Property
prop_insert x s =
  valid s' .&&.
  toList s' === (if x `List.elem` xs then xs else List.insert x xs)
  where
    s' = insert x s
    xs = toList s

prop_delete :: Int -> IntSet -> Property
prop_delete x s = valid s' .&&. toList s' === toList s List.\\ [x]
  where
    s' = delete x s

prop_deleteMin :: IntSet -> Property
prop_deleteMin s = toList (deleteMin s) === if null s then [] else tail (toList s)

prop_deleteMax :: IntSet -> Property
prop_deleteMax s = toList (deleteMax s) === if null s then [] else init (toList s)

prop_fromAscList :: [Int] -> Property
prop_fromAscList xs =
    valid t .&&.
    toList t === nubSortedXs
  where
    sortedXs = sort xs
    nubSortedXs = List.map NE.head $ NE.group sortedXs
    t = fromAscList sortedXs

prop_fromDistinctAscList :: [Int] -> Property
prop_fromDistinctAscList xs =
    valid t .&&.
    toList t === nubSortedXs
  where
    nubSortedXs = List.map NE.head $ NE.group $ sort xs
    t = fromDistinctAscList nubSortedXs

prop_compareSize :: IntSet -> Int -> Property
prop_compareSize t c = compareSize t c === compare (size t) c
