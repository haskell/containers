{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
--
-- QuickCheck properties for Data.Map
-- > ghc -DTESTING -fforce-recomp -O2 --make -fhpc -i..  map-properties.hs

--

import Data.Map
import Data.Monoid
import Data.Maybe hiding (mapMaybe)
import Data.Ord
import Data.Function
import Test.QuickCheck
import Text.Show.Functions
import Prelude hiding (lookup, null, map ,filter)
import qualified Prelude (map, filter)
import qualified Data.List as List

import Control.Applicative ((<$>),(<*>))
import Data.List (nub,sort)
import qualified Data.List as L ((\\),intersect)
import qualified Data.Set
-- import Data.SMap.Types
-- import Data.SMap.Balance
-- import Data.SMap.Internal
import Data.Maybe (isJust,fromJust)
import Prelude hiding (lookup,map,filter,null)
import qualified Prelude as P (map)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test, Testable)
import Test.QuickCheck

main = do
    q $ label   "prop_Valid"            prop_Valid
    q $ label   "prop_Single"           prop_Single
    q $ label   "prop_InsertValid"      prop_InsertValid
    q $ label   "prop_InsertDelete"     prop_InsertDelete
    q $ label   "prop_DeleteValid"      prop_DeleteValid
    q $ label   "prop_Join"             prop_Join
    q $ label   "prop_Merge"            prop_Merge
    q $ label   "prop_UnionValid"       prop_UnionValid
    q $ label   "prop_UnionInsert"      prop_UnionInsert
    q $ label   "prop_UnionAssoc"       prop_UnionAssoc
    q $ label   "prop_UnionComm"        prop_UnionComm
    q $ label   "prop_UnionWithValid"   prop_UnionWithValid 
    q $ label   "prop_UnionWith"        prop_UnionWith
    q $ label   "prop_DiffValid"        prop_DiffValid
    q $ label   "prop_Diff"             prop_Diff
    q $ label   "prop_Diff2"            prop_Diff2
    q $ label   "prop_IntValid"         prop_IntValid
    q $ label   "prop_Int"              prop_Int
    q $ label   "prop_Ordered"          prop_Ordered
    q $ label   "prop_List"             prop_List

    -- new tests
    q $ label   "prop_index"            prop_index
    q $ label   "prop_null"             prop_null
    q $ label   "prop_member"           prop_member
    q $ label   "prop_notmember"        prop_notmember
    q $ label   "prop_findWithDefault"  prop_findWithDefault
    q $ label   "prop_findIndex"        prop_findIndex
    q $ label   "prop_findMin"          prop_findMin
    q $ label   "prop_findMax"          prop_findMax
    q $ label   "prop_filter"           prop_filter
    q $ label   "prop_partition"        prop_partition
    q $ label   "prop_map"              prop_map
    q $ label   "prop_fmap"              prop_fmap
--    q $ label   "prop_mapkeys"          prop_mapkeys
    q $ label   "prop_foldr"            prop_foldr
    q $ label   "prop_foldl"            prop_foldl
--     q $ label   "prop_foldl'"           prop_foldl'
    q $ label   "prop_fold"           prop_fold
    q $ label   "prop_folWithKeyd"           prop_foldWithKey

    defaultMain tests

  where
    q :: Testable prop => prop -> IO ()
    q = quickCheckWith args


{--------------------------------------------------------------------
  Testing
--------------------------------------------------------------------}
testTree xs   = fromList [(x,"*") | x <- xs]
test1 = testTree [1..20]
test2 = testTree [30,29..10]
test3 = testTree [1,4,6,89,2323,53,43,234,5,79,12,9,24,9,8,423,8,42,4,8,9,3]


{--------------------------------------------------------------------
  QuickCheck
--------------------------------------------------------------------}

args = stdArgs {
                 maxSuccess = 500
               , maxDiscard = 500
               }

{-
qcheck prop
  = check config prop
  where
    config = Config
      { configMaxTest = 500
      , configMaxFail = 5000
      , configSize    = \n -> (div n 2 + 3)
      , configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ]
      }
-}


{--------------------------------------------------------------------
  Arbitrary, reasonably balanced trees
--------------------------------------------------------------------}
instance (Enum k,Arbitrary a) => Arbitrary (Map k a) where
  arbitrary = sized (arbtree 0 maxkey)
            where maxkey  = 10^5

--
-- requires access to internals
--
arbtree :: (Enum k,Arbitrary a) => Int -> Int -> Int -> Gen (Map k a)
arbtree lo hi n = do t <- gentree lo hi n
                     if balanced t then return t else arbtree lo hi n
  where gentree lo hi n
          | n <= 0        = return Tip
          | lo >= hi      = return Tip
          | otherwise     = do{ x  <- arbitrary
                              ; i  <- choose (lo,hi)
                              ; m  <- choose (1,70)
                              ; let (ml,mr)  | m==(1::Int)= (1,2)
                                             | m==2       = (2,1)
                                             | m==3       = (1,1)
                                             | otherwise  = (2,2)
                              ; l  <- gentree lo (i-1) (n `div` ml)
                              ; r  <- gentree (i+1) hi (n `div` mr)
                              ; return (bin (toEnum i) x l r)
                              }


{--------------------------------------------------------------------
  Valid tree's
--------------------------------------------------------------------}
forValid :: (Show k,Enum k,Show a,Arbitrary a,Testable b) => (Map k a -> b) -> Property
forValid f
  = forAll arbitrary $ \t -> 
--    classify (balanced t) "balanced" $
    classify (size t == 0) "empty" $
    classify (size t > 0  && size t <= 10) "small" $
    classify (size t > 10 && size t <= 64) "medium" $
    classify (size t > 64) "large" $
    balanced t ==> f t

forValidIntTree :: Testable a => (Map Int Int -> a) -> Property
forValidIntTree f
  = forValid f

forValidUnitTree :: Testable a => (Map Int () -> a) -> Property
forValidUnitTree f
  = forValid f


prop_Valid 
  = forValidUnitTree $ \t -> valid t

{--------------------------------------------------------------------
  Single, Insert, Delete
--------------------------------------------------------------------}
prop_Single :: Int -> Int -> Bool
prop_Single k x
  = (insert k x empty == singleton k x)

prop_InsertValid :: Int -> Property
prop_InsertValid k
  = forValidUnitTree $ \t -> valid (insert k () t)

prop_InsertDelete :: Int -> Map Int () -> Property
prop_InsertDelete k t
  = (lookup k t == Nothing) ==> delete k (insert k () t) == t

prop_DeleteValid :: Int -> Property
prop_DeleteValid k
  = forValidUnitTree $ \t -> 
    valid (delete k (insert k () t))

{--------------------------------------------------------------------
  Balance
--------------------------------------------------------------------}
prop_Join :: Int -> Property 
prop_Join k 
  = forValidUnitTree $ \t ->
    let (l,r) = split k t
    in valid (join k () l r)

prop_Merge :: Int -> Property 
prop_Merge k
  = forValidUnitTree $ \t ->
    let (l,r) = split k t
    in valid (merge l r)


{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
prop_UnionValid :: Property
prop_UnionValid
  = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (union t1 t2)

prop_UnionInsert :: Int -> Int -> Map Int Int -> Bool
prop_UnionInsert k x t
  = union (singleton k x) t == insert k x t

prop_UnionAssoc :: Map Int Int -> Map Int Int -> Map Int Int -> Bool
prop_UnionAssoc t1 t2 t3
  = union t1 (union t2 t3) == union (union t1 t2) t3

prop_UnionComm :: Map Int Int -> Map Int Int -> Bool
prop_UnionComm t1 t2
  = (union t1 t2 == unionWith (\x y -> y) t2 t1)

prop_UnionWithValid 
  = forValidIntTree $ \t1 ->
    forValidIntTree $ \t2 ->
    valid (unionWithKey (\k x y -> x+y) t1 t2)

prop_UnionWith :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_UnionWith xs ys
  = sum (elems (unionWith (+) (fromListWith (+) xs) (fromListWith (+) ys))) 
    == (sum (Prelude.map snd xs) + sum (Prelude.map snd ys))

prop_DiffValid
  = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (difference t1 t2)

prop_Diff :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_Diff xs ys
  =  List.sort (keys (difference (fromListWith (+) xs) (fromListWith (+) ys))) 
    == List.sort ((List.\\) (List.nub (Prelude.map fst xs))  (List.nub (Prelude.map fst ys)))

prop_Diff2 :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_Diff2 xs ys
  =  List.sort (keys ((\\) (fromListWith (+) xs) (fromListWith (+) ys))) 
    == List.sort ((List.\\) (List.nub (Prelude.map fst xs))  (List.nub (Prelude.map fst ys)))

prop_IntValid
  = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (intersection t1 t2)

prop_Int :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_Int xs ys
  =  List.sort (keys (intersection (fromListWith (+) xs) (fromListWith (+) ys))) 
    == List.sort (List.nub ((List.intersect) (Prelude.map fst xs)  (Prelude.map fst ys)))

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
prop_Ordered
  = forAll (choose (5,100)) $ \n ->
    let xs = [(x,()) | x <- [0..n::Int]] 
    in fromAscList xs == fromList xs

prop_List :: [Int] -> Bool
prop_List xs
  = (List.sort (List.nub xs) == [x | (x,()) <- toList (fromList [(x,()) | x <- xs])])

------------------------------------------------------------------------
-- New tests: compare against the list model (after nub on keys)

prop_index = \(xs :: [Int]) ->  length xs > 0  ==>
        let m  = fromList (zip xs xs)
        in xs == [ m ! i | i <- xs ]

prop_null (m :: Data.Map.Map Int Int) = Data.Map.null m == (size m == 0)

prop_member (xs :: [Int]) n = 
        let m  = fromList (zip xs xs)
        in (n `elem` xs) == (n `member` m)

prop_notmember (xs :: [Int]) n = 
        let m  = fromList (zip xs xs)
        in (n `notElem` xs) == (n `notMember` m)

prop_findWithDefault = \(ys :: [(Int, Int)]) ->  length ys > 0  ==>
        let m  = fromList xs
            xs = List.nubBy ((==) `on` fst) ys
        in 
           and [ findWithDefault 0 i m == j | (i,j) <- xs ]

prop_findIndex = \(ys :: [(Int, Int)]) ->  length ys > 0  ==>
        let m  = fromList ys
        in findIndex (fst (head ys)) m `seq` True

prop_lookupIndex = \(ys :: [(Int, Int)]) ->  length ys > 0  ==>
        let m  = fromList ys
        in isJust (lookupIndex (fst (head ys)) m)

prop_findMin = \(ys :: [(Int, Int)]) ->  length ys > 0  ==>
        let m  = fromList ys
            xs = List.nubBy ((==) `on` fst) (reverse ys) -- note.
        in findMin m == List.minimumBy (comparing fst) xs
    
prop_findMax = \(ys :: [(Int, Int)]) ->  length ys > 0  ==>
        let m  = fromList ys
            xs = List.nubBy ((==) `on` fst) (reverse ys) -- note.
        in findMax m == List.maximumBy (comparing fst) xs
    
prop_filter  = \p (ys :: [(Int, Int)]) ->  length ys > 0  ==>
    let m = fromList ys
        xs = List.nubBy ((==) `on` fst) (reverse ys) -- note.
    in
        Data.Map.filter p m == fromList (List.filter (p . snd) xs)

prop_partition = \p (ys :: [(Int, Int)]) ->  length ys > 0  ==>
    let m = fromList ys
        xs = List.nubBy ((==) `on` fst) (reverse ys) -- note.
    in
        Data.Map.partition p m == let (a,b) = (List.partition (p . snd) xs) in (fromList a, fromList b)

prop_map (f :: Int -> Int) (ys :: [(Int, Int)]) =
    let m = fromList ys
        xs = List.nubBy ((==) `on` fst) (reverse ys) -- note.
    in
        Data.Map.map f m == fromList [ (a, f b) | (a,b) <- xs ]

prop_fmap (f :: Int -> Int) (ys :: [(Int, Int)]) =
    let m = fromList ys
        xs = List.nubBy ((==) `on` fst) (reverse ys) -- note.
    in
        fmap f m == fromList [ (a, f b) | (a,b) <- xs ]

{-

-- mapkeys is hard, as we have to consider collisions of the index space.

prop_mapkeys (f :: Int -> Int) (ys :: [(Int, Int)]) =
    let m = fromList ys
        xs = List.nubBy ((==) `on` fst) (reverse ys) -- note.
    in
        Data.Map.mapKeys f m ==
        (fromList $
            {-List.nubBy ((==) `on` fst) $ reverse-} [ (f a, b) | (a,b) <- xs ])
-}


prop_foldr (n :: Int) (ys :: [(Int, Int)]) =
    let m = fromList ys
        xs = List.nubBy ((==) `on` fst) (reverse ys) -- note.
    in 
        fold (+) n m == List.foldr (+) n (List.map snd xs)
  where
    fold k = Data.Map.foldrWithKey (\_ x' z' -> k x' z')


prop_foldl (n :: Int) (ys :: [(Int, Int)]) =
    let m = fromList ys
        xs = List.nubBy ((==) `on` fst) (reverse ys) -- note.
    in 
        Data.Map.foldlWithKey (\a _ b -> a + b) n m == List.foldl (+) n (List.map snd xs)


-- prop_foldl' (n :: Int) (ys :: [(Int, Int)]) =
--     let m = fromList ys
--         xs = List.nubBy ((==) `on` fst) (reverse ys) -- note.
--     in 
--         Data.Map.foldlWithKey' (\a _ b -> a + b) n m == List.foldl' (+) n (List.map snd xs)


prop_fold (n :: Int) (ys :: [(Int, Int)]) =
    let m = fromList ys
        xs = List.nubBy ((==) `on` fst) (reverse ys) -- note.
    in 
        Data.Map.fold (+) n m == List.foldr (+) n (List.map snd xs)

prop_foldWithKey (n :: Int) (ys :: [(Int, Int)]) =
    let m = fromList ys
        xs = List.nubBy ((==) `on` fst) (reverse ys) -- note.
    in 
        Data.Map.foldWithKey (const (+)) n m == List.foldr (+) n (List.map snd xs)

------------------------------------------------------------------------

type UMap = Map Int ()
type IMap = Map Int Int
type SMap = Map Int String

----------------------------------------------------------------

tests :: [Test]
tests = [ testGroup "Test Case" [
               testCase "ticket4242" test_ticket4242
             , testCase "index"      test_index
             , testCase "size"       test_size
             , testCase "size2"      test_size2
             , testCase "member"     test_member
             , testCase "notMember"  test_notMember
             , testCase "lookup"     test_lookup
             , testCase "findWithDefault"     test_findWithDefault
             , testCase "empty" test_empty
             , testCase "mempty" test_mempty
             , testCase "singleton" test_singleton
             , testCase "insert" test_insert
             , testCase "insertWith" test_insertWith
             , testCase "insertWith'" test_insertWith'
             , testCase "insertWithKey" test_insertWithKey
             , testCase "insertWithKey'" test_insertWithKey'
             , testCase "insertLookupWithKey" test_insertLookupWithKey
             , testCase "insertLookupWithKey'" test_insertLookupWithKey'
             , testCase "delete" test_delete
             , testCase "adjust" test_adjust
             , testCase "adjustWithKey" test_adjustWithKey
             , testCase "update" test_update
             , testCase "updateWithKey" test_updateWithKey
             , testCase "updateLookupWithKey" test_updateLookupWithKey
             , testCase "alter" test_alter
             , testCase "union" test_union
             , testCase "mappend" test_mappend
             , testCase "unionWith" test_unionWith
             , testCase "unionWithKey" test_unionWithKey
             , testCase "unions" test_unions
             , testCase "mconcat" test_mconcat
             , testCase "unionsWith" test_unionsWith
             , testCase "difference" test_difference
             , testCase "differenceWith" test_differenceWith
             , testCase "differenceWithKey" test_differenceWithKey
             , testCase "intersection" test_intersection
             , testCase "intersectionWith" test_intersectionWith
             , testCase "intersectionWithKey" test_intersectionWithKey
             , testCase "map" test_map
             , testCase "mapWithKey" test_mapWithKey
             , testCase "mapAccum" test_mapAccum
             , testCase "mapAccumWithKey" test_mapAccumWithKey
             , testCase "mapAccumRWithKey" test_mapAccumRWithKey
             , testCase "mapKeys" test_mapKeys
             , testCase "mapKeysWith" test_mapKeysWith
             , testCase "mapKeysMonotonic" test_mapKeysMonotonic
             , testCase "fold" test_fold
             , testCase "foldWithKey" test_foldWithKey
             , testCase "elems" test_elems
             , testCase "keys" test_keys
             , testCase "keysSet" test_keysSet
             , testCase "associative" test_assocs
             , testCase "toList" test_toList
             , testCase "fromList" test_fromList
             , testCase "fromListWith" test_fromListWith
             , testCase "fromListWithKey" test_fromListWithKey
             , testCase "toAscList" test_toAscList
             , testCase "toDescList" test_toDescList
             , testCase "showTree" test_showTree
             , testCase "showTree'" test_showTree'
             , testCase "fromAscList" test_fromAscList
             , testCase "fromAscListWith" test_fromAscListWith
             , testCase "fromAscListWithKey" test_fromAscListWithKey
             , testCase "fromDistinctAscList" test_fromDistinctAscList
             , testCase "filter" test_filter
             , testCase "filterWithKey" test_filteWithKey
             , testCase "partition" test_partition
             , testCase "partitionWithKey" test_partitionWithKey
             , testCase "mapMaybe" test_mapMaybe
             , testCase "mapMaybeWithKey" test_mapMaybeWithKey
             , testCase "mapEither" test_mapEither
             , testCase "mapEitherWithKey" test_mapEitherWithKey
             , testCase "split" test_split
             , testCase "splitLookup" test_splitLookup
             , testCase "isSubmapOfBy" test_isSubmapOfBy
             , testCase "isSubmapOf" test_isSubmapOf
             , testCase "isProperSubmapOfBy" test_isProperSubmapOfBy
             , testCase "isProperSubmapOf" test_isProperSubmapOf
             , testCase "lookupIndex" test_lookupIndex
             , testCase "findIndex" test_findIndex
             , testCase "elemAt" test_elemAt
             , testCase "updateAt" test_updateAt
             , testCase "deleteAt" test_deleteAt
             , testCase "findMin" test_findMin
             , testCase "findMax" test_findMax
             , testCase "deleteMin" test_deleteMin
             , testCase "deleteMax" test_deleteMax
             , testCase "deleteFindMin" test_deleteFindMin
             , testCase "deleteFindMax" test_deleteFindMax
             , testCase "updateMin" test_updateMin
             , testCase "updateMax" test_updateMax
             , testCase "updateMinWithKey" test_updateMinWithKey
             , testCase "updateMaxWithKey" test_updateMaxWithKey
             , testCase "minView" test_minView
             , testCase "maxView" test_maxView
             , testCase "minViewWithKey" test_minViewWithKey
             , testCase "maxViewWithKey" test_maxViewWithKey
             , testCase "valid" test_valid
             ]
        , testGroup "Property Test" [
    --           testProperty "fromList"             prop_fromList
               testProperty "insert to singleton"  prop_singleton
    --         , testProperty "insert"               prop_insert
             , testProperty "insert then lookup"   prop_lookup
    --         , testProperty "insert then delete"   prop_insertDelete
    --         , testProperty "insert then delete2"  prop_insertDelete2
             , testProperty "delete non member"    prop_deleteNonMember
    --         , testProperty "deleteMin"            prop_deleteMin
    --         , testProperty "deleteMax"            prop_deleteMax
             , testProperty "split"                prop_split
    --         , testProperty "split then join"      prop_join
    --         , testProperty "split then merge"     prop_merge
    --         , testProperty "union"                prop_union
             , testProperty "union model"          prop_unionModel
             , testProperty "union singleton"      prop_unionSingleton
             , testProperty "union associative"    prop_unionAssoc
             , testProperty "fromAscList"          prop_ordered
             , testProperty "fromList then toList" prop_list
             , testProperty "unionWith"            prop_unionWith
    --         , testProperty "unionWith2"           prop_unionWith2
             , testProperty "union sum"            prop_unionSum
    --         , testProperty "difference"           prop_difference
             , testProperty "difference model"     prop_differenceModel
             , testProperty "intersection"         prop_intersection
             , testProperty "intersection model"   prop_intersectionModel
    --         , testProperty "alter"                prop_alter
             ]
        ]


----------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------

test_ticket4242 :: Assertion
test_ticket4242 = (valid $ deleteMin $ deleteMin $ fromList [ (i, ()) | i <- [0,2,5,1,6,4,8,9,7,11,10,3] :: [Int] ]) @?= True

----------------------------------------------------------------
-- Operators

test_index :: Assertion
test_index = fromList [(5,'a'), (3,'b')] ! 5 @?= 'a'

----------------------------------------------------------------
-- Query

test_size :: Assertion
test_size = do
    null (empty)           @?= True
    null (singleton 1 'a') @?= False

test_size2 :: Assertion
test_size2 = do
    size empty                                   @?= 0
    size (singleton 1 'a')                       @?= 1
    size (fromList([(1,'a'), (2,'c'), (3,'b')])) @?= 3

test_member :: Assertion
test_member = do
    member 5 (fromList [(5,'a'), (3,'b')]) @?= True
    member 1 (fromList [(5,'a'), (3,'b')]) @?= False

test_notMember :: Assertion
test_notMember = do
    notMember 5 (fromList [(5,'a'), (3,'b')]) @?= False
    notMember 1 (fromList [(5,'a'), (3,'b')]) @?= True

test_lookup :: Assertion
test_lookup = do
    employeeCurrency "John" @?= Just "Euro"
    employeeCurrency "Pete" @?= Nothing
  where
    employeeDept = fromList([("John","Sales"), ("Bob","IT")])
    deptCountry = fromList([("IT","USA"), ("Sales","France")])
    countryCurrency = fromList([("USA", "Dollar"), ("France", "Euro")])
    employeeCurrency :: String -> Maybe String
    employeeCurrency name = do
        dept <- lookup name employeeDept
        country <- lookup dept deptCountry
        lookup country countryCurrency

test_findWithDefault :: Assertion
test_findWithDefault = do
    findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) @?= 'x'
    findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) @?= 'a'

----------------------------------------------------------------
-- Construction

test_empty :: Assertion
test_empty = do
    (empty :: UMap)  @?= fromList []
    size empty @?= 0

test_mempty :: Assertion
test_mempty = do
    (mempty :: UMap)  @?= fromList []
    size (mempty :: UMap) @?= 0

test_singleton :: Assertion
test_singleton = do
    singleton 1 'a'        @?= fromList [(1, 'a')]
    size (singleton 1 'a') @?= 1

test_insert :: Assertion
test_insert = do
    insert 5 'x' (fromList [(5,'a'), (3,'b')]) @?= fromList [(3, 'b'), (5, 'x')]
    insert 7 'x' (fromList [(5,'a'), (3,'b')]) @?= fromList [(3, 'b'), (5, 'a'), (7, 'x')]
    insert 5 'x' empty                         @?= singleton 5 'x'

test_insertWith :: Assertion
test_insertWith = do
    insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "xxxa")]
    insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a"), (7, "xxx")]
    insertWith (++) 5 "xxx" empty                         @?= singleton 5 "xxx"

test_insertWith' :: Assertion
test_insertWith' = do
    insertWith' (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "xxxa")]
    insertWith' (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a"), (7, "xxx")]
    insertWith' (++) 5 "xxx" empty                         @?= singleton 5 "xxx"

test_insertWithKey :: Assertion
test_insertWithKey = do
    insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "5:xxx|a")]
    insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a"), (7, "xxx")]
    insertWithKey f 5 "xxx" empty                         @?= singleton 5 "xxx"
  where
    f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value

test_insertWithKey' :: Assertion
test_insertWithKey' = do
    insertWithKey' f 5 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "5:xxx|a")]
    insertWithKey' f 7 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a"), (7, "xxx")]
    insertWithKey' f 5 "xxx" empty                         @?= singleton 5 "xxx"
  where
    f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value

test_insertLookupWithKey :: Assertion
test_insertLookupWithKey = do
    insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) @?= (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
    insertLookupWithKey f 2 "xxx" (fromList [(5,"a"), (3,"b")]) @?= (Nothing,fromList [(2,"xxx"),(3,"b"),(5,"a")])
    insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) @?= (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
    insertLookupWithKey f 5 "xxx" empty                         @?= (Nothing,  singleton 5 "xxx")
  where
    f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value

test_insertLookupWithKey' :: Assertion
test_insertLookupWithKey' = do
    insertLookupWithKey' f 5 "xxx" (fromList [(5,"a"), (3,"b")]) @?= (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
    insertLookupWithKey' f 2 "xxx" (fromList [(5,"a"), (3,"b")]) @?= (Nothing,fromList [(2,"xxx"),(3,"b"),(5,"a")])
    insertLookupWithKey' f 7 "xxx" (fromList [(5,"a"), (3,"b")]) @?= (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
    insertLookupWithKey' f 5 "xxx" empty                         @?= (Nothing,  singleton 5 "xxx")
  where
    f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value

----------------------------------------------------------------
-- Delete/Update

test_delete :: Assertion
test_delete = do
    delete 5 (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"
    delete 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
    delete 5 empty                         @?= (empty :: IMap)

test_adjust :: Assertion
test_adjust = do
    adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "new a")]
    adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
    adjust ("new " ++) 7 empty                         @?= empty

test_adjustWithKey :: Assertion
test_adjustWithKey = do
    adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "5:new a")]
    adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
    adjustWithKey f 7 empty                         @?= empty
  where
    f key x = (show key) ++ ":new " ++ x

test_update :: Assertion
test_update = do
    update f 5 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "new a")]
    update f 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
    update f 3 (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"
  where
    f x = if x == "a" then Just "new a" else Nothing

test_updateWithKey :: Assertion
test_updateWithKey = do
    updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "5:new a")]
    updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
    updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"
 where
     f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing

test_updateLookupWithKey :: Assertion
test_updateLookupWithKey = do
    updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) @?= (Just "5:new a", fromList [(3, "b"), (5, "5:new a")])
    updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) @?= (Nothing,  fromList [(3, "b"), (5, "a")])
    updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) @?= (Just "b", singleton 5 "a")
  where
    f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing

test_alter :: Assertion
test_alter = do
    alter f 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
    alter f 5 (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"
    alter g 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a"), (7, "c")]
    alter g 5 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "c")]
  where
    f _ = Nothing
    g _ = Just "c"

----------------------------------------------------------------
-- Combine

test_union :: Assertion
test_union = union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= fromList [(3, "b"), (5, "a"), (7, "C")]

test_mappend :: Assertion
test_mappend = mappend (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= fromList [(3, "b"), (5, "a"), (7, "C")]

test_unionWith :: Assertion
test_unionWith = unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= fromList [(3, "b"), (5, "aA"), (7, "C")]

test_unionWithKey :: Assertion
test_unionWithKey = unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= fromList [(3, "b"), (5, "5:a|A"), (7, "C")]
  where
    f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value

test_unions :: Assertion
test_unions = do
    unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
        @?= fromList [(3, "b"), (5, "a"), (7, "C")]
    unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
        @?= fromList [(3, "B3"), (5, "A3"), (7, "C")]

test_mconcat :: Assertion
test_mconcat = do
    mconcat [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
        @?= fromList [(3, "b"), (5, "a"), (7, "C")]
    mconcat [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
        @?= fromList [(3, "B3"), (5, "A3"), (7, "C")]

test_unionsWith :: Assertion
test_unionsWith = unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
     @?= fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]

test_difference :: Assertion
test_difference = difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= singleton 3 "b"

test_differenceWith :: Assertion
test_differenceWith = differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
     @?= singleton 3 "b:B"
 where
   f al ar = if al== "b" then Just (al ++ ":" ++ ar) else Nothing

test_differenceWithKey :: Assertion
test_differenceWithKey = differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
     @?= singleton 3 "3:b|B"
  where
    f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing

test_intersection :: Assertion
test_intersection = intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= singleton 5 "a"


test_intersectionWith :: Assertion
test_intersectionWith = intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= singleton 5 "aA"

test_intersectionWithKey :: Assertion
test_intersectionWithKey = intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= singleton 5 "5:a|A"
  where
    f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar

----------------------------------------------------------------
-- Traversal

test_map :: Assertion
test_map = map (++ "x") (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "bx"), (5, "ax")]

test_mapWithKey :: Assertion
test_mapWithKey = mapWithKey f (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "3:b"), (5, "5:a")]
  where
    f key x = (show key) ++ ":" ++ x

test_mapAccum :: Assertion
test_mapAccum = mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) @?= ("Everything: ba", fromList [(3, "bX"), (5, "aX")])
  where
    f a b = (a ++ b, b ++ "X")

test_mapAccumWithKey :: Assertion
test_mapAccumWithKey = mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) @?= ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])
  where
    f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")

test_mapAccumRWithKey :: Assertion
test_mapAccumRWithKey = mapAccumRWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) @?= ("Everything: 5-a 3-b", fromList [(3, "bX"), (5, "aX")])
  where
    f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")

test_mapKeys :: Assertion
test_mapKeys = do
    mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        @?= fromList [(4, "b"), (6, "a")]
    mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) @?= singleton 1 "c"
    mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) @?= singleton 3 "c"

test_mapKeysWith :: Assertion
test_mapKeysWith = do
    mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) @?= singleton 1 "cdab"
    mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) @?= singleton 3 "cdab"

test_mapKeysMonotonic :: Assertion
test_mapKeysMonotonic = do
    mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) @?= fromList [(6, "b"), (10, "a")]
    valid (mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")])) @?= True
    valid (mapKeysMonotonic (\ _ -> 1)     (fromList [(5,"a"), (3,"b")])) @?= False

test_fold :: Assertion
test_fold = fold f 0 (fromList [(5,"a"), (3,"bbb")]) @?= 4
  where
    f a len = len + (length a)

test_foldWithKey :: Assertion
test_foldWithKey = foldWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) @?= "Map: (5:a)(3:b)"
  where
    f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"

----------------------------------------------------------------
-- Conversion

test_elems :: Assertion
test_elems = do
    elems (fromList [(5,"a"), (3,"b")]) @?= ["b","a"]
    elems (empty :: UMap) @?= []

test_keys :: Assertion
test_keys = do
    keys (fromList [(5,"a"), (3,"b")]) @?= [3,5]
    keys (empty :: UMap) @?= []

test_keysSet :: Assertion
test_keysSet = do
    keysSet (fromList [(5,"a"), (3,"b")]) @?= Data.Set.fromList [3,5]
    keysSet (empty :: UMap) @?= Data.Set.empty

test_assocs :: Assertion
test_assocs = do
    assocs (fromList [(5,"a"), (3,"b")]) @?= [(3,"b"), (5,"a")]
    assocs (empty :: UMap) @?= []

----------------------------------------------------------------
-- Lists

test_toList :: Assertion
test_toList = do
    toList (fromList [(5,"a"), (3,"b")]) @?= [(3,"b"), (5,"a")]
    toList (empty :: SMap) @?= []

test_fromList :: Assertion
test_fromList = do
    fromList [] @?= (empty :: SMap)
    fromList [(5,"a"), (3,"b"), (5, "c")] @?= fromList [(5,"c"), (3,"b")]
    fromList [(5,"c"), (3,"b"), (5, "a")] @?= fromList [(5,"a"), (3,"b")]

test_fromListWith :: Assertion
test_fromListWith = do
    fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] @?= fromList [(3, "ab"), (5, "aba")]
    fromListWith (++) [] @?= (empty :: SMap)

test_fromListWithKey :: Assertion
test_fromListWithKey = do
    fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] @?= fromList [(3, "3ab"), (5, "5a5ba")]
    fromListWithKey f [] @?= (empty :: SMap)
  where
    f k a1 a2 = (show k) ++ a1 ++ a2

----------------------------------------------------------------
-- Ordered lists

test_toAscList :: Assertion
test_toAscList = toAscList (fromList [(5,"a"), (3,"b")]) @?= [(3,"b"), (5,"a")]

test_toDescList :: Assertion
test_toDescList = toDescList (fromList [(5,"a"), (3,"b")]) @?= [(5,"a"), (3,"b")]

test_showTree :: Assertion
test_showTree =
       (let t = fromDistinctAscList [(x,()) | x <- [1..5]]
        in showTree t) @?= "4:=()\n+--2:=()\n|  +--1:=()\n|  +--3:=()\n+--5:=()\n"

test_showTree' :: Assertion
test_showTree' =
       (let t = fromDistinctAscList [(x,()) | x <- [1..5]]
        in s t ) @?= "+--5:=()\n|\n4:=()\n|\n|  +--3:=()\n|  |\n+--2:=()\n   |\n   +--1:=()\n" 
   where
    showElem k x  = show k ++ ":=" ++ show x

    s = showTreeWith showElem False True


test_fromAscList :: Assertion
test_fromAscList = do
    fromAscList [(3,"b"), (5,"a")]          @?= fromList [(3, "b"), (5, "a")]
    fromAscList [(3,"b"), (5,"a"), (5,"b")] @?= fromList [(3, "b"), (5, "b")]
    valid (fromAscList [(3,"b"), (5,"a"), (5,"b")]) @?= True
    valid (fromAscList [(5,"a"), (3,"b"), (5,"b")]) @?= False

test_fromAscListWith :: Assertion
test_fromAscListWith = do
    fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] @?= fromList [(3, "b"), (5, "ba")]
    valid (fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")]) @?= True
    valid (fromAscListWith (++) [(5,"a"), (3,"b"), (5,"b")]) @?= False

test_fromAscListWithKey :: Assertion
test_fromAscListWithKey = do
    fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")] @?= fromList [(3, "b"), (5, "5:b5:ba")]
    valid (fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")]) @?= True
    valid (fromAscListWithKey f [(5,"a"), (3,"b"), (5,"b"), (5,"b")]) @?= False
  where
    f k a1 a2 = (show k) ++ ":" ++ a1 ++ a2

test_fromDistinctAscList :: Assertion
test_fromDistinctAscList = do
    fromDistinctAscList [(3,"b"), (5,"a")] @?= fromList [(3, "b"), (5, "a")]
    valid (fromDistinctAscList [(3,"b"), (5,"a")])          @?= True
    valid (fromDistinctAscList [(3,"b"), (5,"a"), (5,"b")]) @?= False

----------------------------------------------------------------
-- Filter

test_filter :: Assertion
test_filter = do
    filter (> "a") (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"
    filter (> "x") (fromList [(5,"a"), (3,"b")]) @?= empty
    filter (< "a") (fromList [(5,"a"), (3,"b")]) @?= empty

test_filteWithKey :: Assertion
test_filteWithKey = filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"

test_partition :: Assertion
test_partition = do
    partition (> "a") (fromList [(5,"a"), (3,"b")]) @?= (singleton 3 "b", singleton 5 "a")
    partition (< "x") (fromList [(5,"a"), (3,"b")]) @?= (fromList [(3, "b"), (5, "a")], empty)
    partition (> "x") (fromList [(5,"a"), (3,"b")]) @?= (empty, fromList [(3, "b"), (5, "a")])

test_partitionWithKey :: Assertion
test_partitionWithKey = do
    partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) @?= (singleton 5 "a", singleton 3 "b")
    partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) @?= (fromList [(3, "b"), (5, "a")], empty)
    partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) @?= (empty, fromList [(3, "b"), (5, "a")])

test_mapMaybe :: Assertion
test_mapMaybe = mapMaybe f (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "new a"
  where
    f x = if x == "a" then Just "new a" else Nothing

test_mapMaybeWithKey :: Assertion
test_mapMaybeWithKey = mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "key : 3"
  where
    f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing

test_mapEither :: Assertion
test_mapEither = do
    mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
        @?= (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
    mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
        @?= ((empty :: SMap), fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
 where
   f a = if a < "c" then Left a else Right a

test_mapEitherWithKey :: Assertion
test_mapEitherWithKey = do
    mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
     @?= (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
    mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
     @?= ((empty :: SMap), fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])
  where
    f k a = if k < 5 then Left (k * 2) else Right (a ++ a)

test_split :: Assertion
test_split = do
    split 2 (fromList [(5,"a"), (3,"b")]) @?= (empty, fromList [(3,"b"), (5,"a")])
    split 3 (fromList [(5,"a"), (3,"b")]) @?= (empty, singleton 5 "a")
    split 4 (fromList [(5,"a"), (3,"b")]) @?= (singleton 3 "b", singleton 5 "a")
    split 5 (fromList [(5,"a"), (3,"b")]) @?= (singleton 3 "b", empty)
    split 6 (fromList [(5,"a"), (3,"b")]) @?= (fromList [(3,"b"), (5,"a")], empty)

test_splitLookup :: Assertion
test_splitLookup = do
    splitLookup 2 (fromList [(5,"a"), (3,"b")]) @?= (empty, Nothing, fromList [(3,"b"), (5,"a")])
    splitLookup 3 (fromList [(5,"a"), (3,"b")]) @?= (empty, Just "b", singleton 5 "a")
    splitLookup 4 (fromList [(5,"a"), (3,"b")]) @?= (singleton 3 "b", Nothing, singleton 5 "a")
    splitLookup 5 (fromList [(5,"a"), (3,"b")]) @?= (singleton 3 "b", Just "a", empty)
    splitLookup 6 (fromList [(5,"a"), (3,"b")]) @?= (fromList [(3,"b"), (5,"a")], Nothing, empty)

----------------------------------------------------------------
-- Submap

test_isSubmapOfBy :: Assertion
test_isSubmapOfBy = do
    isSubmapOfBy (==) (fromList [('a',1)]) (fromList [('a',1),('b',2)]) @?= True
    isSubmapOfBy (<=) (fromList [('a',1)]) (fromList [('a',1),('b',2)]) @?= True
    isSubmapOfBy (==) (fromList [('a',1),('b',2)]) (fromList [('a',1),('b',2)]) @?= True
    isSubmapOfBy (==) (fromList [('a',2)]) (fromList [('a',1),('b',2)]) @?= False
    isSubmapOfBy (<)  (fromList [('a',1)]) (fromList [('a',1),('b',2)]) @?= False
    isSubmapOfBy (==) (fromList [('a',1),('b',2)]) (fromList [('a',1)]) @?= False

test_isSubmapOf :: Assertion
test_isSubmapOf = do
    isSubmapOf (fromList [('a',1)]) (fromList [('a',1),('b',2)]) @?= True
    isSubmapOf (fromList [('a',1),('b',2)]) (fromList [('a',1),('b',2)]) @?= True
    isSubmapOf (fromList [('a',2)]) (fromList [('a',1),('b',2)]) @?= False
    isSubmapOf (fromList [('a',1),('b',2)]) (fromList [('a',1)]) @?= False

test_isProperSubmapOfBy :: Assertion
test_isProperSubmapOfBy = do
    isProperSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)]) @?= True
    isProperSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)]) @?= True
    isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)]) @?= False
    isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)]) @?= False
    isProperSubmapOfBy (<)  (fromList [(1,1)])       (fromList [(1,1),(2,2)]) @?= False

test_isProperSubmapOf :: Assertion
test_isProperSubmapOf = do
    isProperSubmapOf (fromList [(1,1)]) (fromList [(1,1),(2,2)]) @?= True
    isProperSubmapOf (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)]) @?= False
    isProperSubmapOf (fromList [(1,1),(2,2)]) (fromList [(1,1)]) @?= False

----------------------------------------------------------------
-- Indexed

test_lookupIndex :: Assertion
test_lookupIndex = do
    isJust (lookupIndex 2 (fromList [(5,"a"), (3,"b")]))   @?= False
    fromJust (lookupIndex 3 (fromList [(5,"a"), (3,"b")])) @?= 0
    fromJust (lookupIndex 5 (fromList [(5,"a"), (3,"b")])) @?= 1
    isJust (lookupIndex 6 (fromList [(5,"a"), (3,"b")]))   @?= False

test_findIndex :: Assertion
test_findIndex = do
    findIndex 3 (fromList [(5,"a"), (3,"b")]) @?= 0
    findIndex 5 (fromList [(5,"a"), (3,"b")]) @?= 1

test_elemAt :: Assertion
test_elemAt = do
    elemAt 0 (fromList [(5,"a"), (3,"b")]) @?= (3,"b")
    elemAt 1 (fromList [(5,"a"), (3,"b")]) @?= (5, "a")

test_updateAt :: Assertion
test_updateAt = do
    updateAt (\ _ _ -> Just "x") 0    (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "x"), (5, "a")]
    updateAt (\ _ _ -> Just "x") 1    (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "x")]
    updateAt (\_ _  -> Nothing)  0    (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"
    updateAt (\_ _  -> Nothing)  1    (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"
--    updateAt (\_ _  -> Nothing)  7    (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"

test_deleteAt :: Assertion
test_deleteAt = do
    deleteAt 0  (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"
    deleteAt 1  (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"

----------------------------------------------------------------
-- Min/Max

test_findMin :: Assertion
test_findMin = findMin (fromList [(5,"a"), (3,"b")]) @?= (3,"b")

test_findMax :: Assertion
test_findMax = findMax (fromList [(5,"a"), (3,"b")]) @?= (5,"a")

test_deleteMin :: Assertion
test_deleteMin = do
    deleteMin (fromList [(5,"a"), (3,"b"), (7,"c")]) @?= fromList [(5,"a"), (7,"c")]
    deleteMin (empty :: SMap) @?= empty

test_deleteMax :: Assertion
test_deleteMax = do
    deleteMax (fromList [(5,"a"), (3,"b"), (7,"c")]) @?= fromList [(3,"b"), (5,"a")]
    deleteMax (empty :: SMap) @?= empty

test_deleteFindMin :: Assertion
test_deleteFindMin = deleteFindMin (fromList [(5,"a"), (3,"b"), (10,"c")]) @?= ((3,"b"), fromList[(5,"a"), (10,"c")])

test_deleteFindMax :: Assertion
test_deleteFindMax = deleteFindMax (fromList [(5,"a"), (3,"b"), (10,"c")]) @?= ((10,"c"), fromList [(3,"b"), (5,"a")])

test_updateMin :: Assertion
test_updateMin = do
    updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "Xb"), (5, "a")]
    updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"

test_updateMax :: Assertion
test_updateMax = do
    updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "Xa")]
    updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"

test_updateMinWithKey :: Assertion
test_updateMinWithKey = do
    updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) @?= fromList [(3,"3:b"), (5,"a")]
    updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"

test_updateMaxWithKey :: Assertion
test_updateMaxWithKey = do
    updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) @?= fromList [(3,"b"), (5,"5:a")]
    updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"

test_minView :: Assertion
test_minView = do
    minView (fromList [(5,"a"), (3,"b")]) @?= Just ("b", singleton 5 "a")
    minView (empty :: SMap) @?= Nothing

test_maxView :: Assertion
test_maxView = do
    maxView (fromList [(5,"a"), (3,"b")]) @?= Just ("a", singleton 3 "b")
    maxView (empty :: SMap) @?= Nothing

test_minViewWithKey :: Assertion
test_minViewWithKey = do
    minViewWithKey (fromList [(5,"a"), (3,"b")]) @?= Just ((3,"b"), singleton 5 "a")
    minViewWithKey (empty :: SMap) @?= Nothing

test_maxViewWithKey :: Assertion
test_maxViewWithKey = do
    maxViewWithKey (fromList [(5,"a"), (3,"b")]) @?= Just ((5,"a"), singleton 3 "b")
    maxViewWithKey (empty :: SMap) @?= Nothing

----------------------------------------------------------------
-- Debug

test_valid :: Assertion
test_valid = do
    valid (fromAscList [(3,"b"), (5,"a")]) @?= True
    valid (fromAscList [(5,"a"), (3,"b")]) @?= False

----------------------------------------------------------------
-- QuickCheck
----------------------------------------------------------------

prop_fromList :: UMap -> Bool
prop_fromList t = valid t

prop_singleton :: Int -> Int -> Bool
prop_singleton k x = insert k x empty == singleton k x

prop_insert :: Int -> UMap -> Bool
prop_insert k t = valid $ insert k () t

prop_lookup :: Int -> UMap -> Bool
prop_lookup k t = lookup k (insert k () t) /= Nothing

prop_insertDelete :: Int -> UMap -> Bool
prop_insertDelete k t = valid $ delete k (insert k () t)

prop_insertDelete2 :: Int -> UMap -> Property
prop_insertDelete2 k t = (lookup k t == Nothing) ==> (delete k (insert k () t) == t)

prop_deleteNonMember :: Int -> UMap -> Property
prop_deleteNonMember k t = (lookup k t == Nothing) ==> (delete k t == t)

prop_deleteMin :: UMap -> Bool
prop_deleteMin t = valid $ deleteMin $ deleteMin t

prop_deleteMax :: UMap -> Bool
prop_deleteMax t = valid $ deleteMax $ deleteMax t

----------------------------------------------------------------

prop_split :: Int -> UMap -> Property
prop_split k t = (lookup k t /= Nothing) ==> let (r,l) = split k t
                                             in (valid r, valid l) == (True, True)

prop_join :: Int -> UMap -> Bool
prop_join k t = let (l,r) = split k t
                in valid (join k () l r)

prop_merge :: Int -> UMap -> Bool
prop_merge k t = let (l,r) = split k t
                 in valid (merge l r)

----------------------------------------------------------------

prop_union :: UMap -> UMap -> Bool
prop_union t1 t2 = valid (union t1 t2)

prop_unionModel :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_unionModel xs ys
  = sort (keys (union (fromList xs) (fromList ys)))
    == sort (nub (P.map fst xs ++ P.map fst ys))

prop_unionSingleton :: IMap -> Int -> Int -> Bool
prop_unionSingleton t k x = union (singleton k x) t == insert k x t

prop_unionAssoc :: IMap -> IMap -> IMap -> Bool
prop_unionAssoc t1 t2 t3 = union t1 (union t2 t3) == union (union t1 t2) t3

prop_unionWith :: IMap -> IMap -> Bool
prop_unionWith t1 t2 = (union t1 t2 == unionWith (\_ y -> y) t2 t1)

prop_unionWith2 :: IMap -> IMap -> Bool
prop_unionWith2 t1 t2 = valid (unionWithKey (\_ x y -> x+y) t1 t2)

prop_unionSum :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_unionSum xs ys
  = sum (elems (unionWith (+) (fromListWith (+) xs) (fromListWith (+) ys)))
    == (sum (P.map snd xs) + sum (P.map snd ys))

prop_difference :: IMap -> IMap -> Bool
prop_difference t1 t2 = valid (difference t1 t2)

prop_differenceModel :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_differenceModel xs ys
  = sort (keys (difference (fromListWith (+) xs) (fromListWith (+) ys)))
    == sort ((L.\\) (nub (P.map fst xs)) (nub (P.map fst ys)))

prop_intersection :: IMap -> IMap -> Bool
prop_intersection t1 t2 = valid (intersection t1 t2)

prop_intersectionModel :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_intersectionModel xs ys
  = sort (keys (intersection (fromListWith (+) xs) (fromListWith (+) ys)))
    == sort (nub ((L.intersect) (P.map fst xs) (P.map fst ys)))

----------------------------------------------------------------

prop_ordered :: Property
prop_ordered
  = forAll (choose (5,100)) $ \n ->
    let xs = [(x,()) | x <- [0..n::Int]]
    in fromAscList xs == fromList xs

prop_list :: [Int] -> Bool
prop_list xs = (sort (nub xs) == [x | (x,()) <- toList (fromList [(x,()) | x <- xs])])

----------------------------------------------------------------

prop_alter :: UMap -> Int -> Bool
prop_alter t k = balanced t' && case lookup k t of
    Just _  -> (size t - 1) == size t' && lookup k t' == Nothing
    Nothing -> (size t + 1) == size t' && lookup k t' /= Nothing
  where
    t' = alter f k t
    f Nothing   = Just ()
    f (Just ()) = Nothing
