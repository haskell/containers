{-# LANGUAGE CPP #-}
import qualified Data.IntSet as IntSet
import Data.List (nub, sort, sortBy)
import qualified Data.List as List
import Data.Maybe
import Data.Set
import Data.Set.Internal (link, merge)
import Prelude hiding (lookup, null, map, filter, foldr, foldl, foldl', all, take, drop, splitAt)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function (apply)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Data.Foldable (all)
import Data.Ord (Down(..), comparing)
import Control.Applicative (liftA2)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Utils.ArbitrarySetMap (mkArbSet, setFromList)

main :: IO ()
main = defaultMain $ testGroup "set-properties"
                   [ testCase "lookupLT" test_lookupLT
                   , testCase "lookupGT" test_lookupGT
                   , testCase "lookupLE" test_lookupLE
                   , testCase "lookupGE" test_lookupGE
                   , testCase "lookupIndex" test_lookupIndex
                   , testCase "findIndex" test_findIndex
                   , testCase "elemAt" test_elemAt
                   , testCase "deleteAt" test_deleteAt
                   , testProperty "prop_Valid" prop_Valid
                   , testProperty "prop_Single" prop_Single
                   , testProperty "prop_Member" prop_Member
                   , testProperty "prop_NotMember" prop_NotMember
                   , testProperty "prop_LookupLT" prop_LookupLT
                   , testProperty "prop_LookupGT" prop_LookupGT
                   , testProperty "prop_LookupLE" prop_LookupLE
                   , testProperty "prop_LookupGE" prop_LookupGE
                   , testProperty "prop_InsertValid" prop_InsertValid
                   , testProperty "prop_InsertDelete" prop_InsertDelete
                   , testProperty "prop_InsertBiased" prop_InsertBiased
                   , testProperty "prop_DeleteValid" prop_DeleteValid
                   , testProperty "alterF" prop_alterF
                   , testProperty "alterF/delete" prop_alterF_delete
                   , testProperty "alterF/insert" prop_alterF_insert
                   , testProperty "alterF/member" prop_alterF_member
                   , testProperty "alterF/four" prop_alterF_four
                   , testProperty "alterF/valid" prop_alterF_valid
                   , testProperty "prop_Link" prop_Link
                   , testProperty "prop_Merge" prop_Merge
                   , testProperty "prop_UnionValid" prop_UnionValid
                   , testProperty "prop_UnionInsert" prop_UnionInsert
                   , testProperty "prop_UnionAssoc" prop_UnionAssoc
                   , testProperty "prop_UnionComm" prop_UnionComm
                   , testProperty "prop_UnionBiased" prop_UnionBiased
                   , testProperty "prop_DiffValid" prop_DiffValid
                   , testProperty "prop_Diff" prop_Diff
                   , testProperty "prop_IntValid" prop_IntValid
                   , testProperty "prop_Int" prop_Int
                   , testProperty "prop_IntBiased" prop_IntBiased
                   , testProperty "prop_Ordered" prop_Ordered
                   , testProperty "prop_DescendingOrdered" prop_DescendingOrdered
                   , testProperty "prop_List" prop_List
                   , testProperty "prop_DescList" prop_DescList
                   , testProperty "prop_AscDescList" prop_AscDescList
                   , testProperty "prop_fromList" prop_fromList
                   , testProperty "prop_fromAscList" prop_fromAscList
                   , testProperty "prop_fromDistinctAscList" prop_fromDistinctAscList
                   , testProperty "prop_fromDescList" prop_fromDescList
                   , testProperty "prop_fromDistinctDescList" prop_fromDistinctDescList
                   , testProperty "prop_isProperSubsetOf" prop_isProperSubsetOf
                   , testProperty "prop_isProperSubsetOf2" prop_isProperSubsetOf2
                   , testProperty "prop_isSubsetOf" prop_isSubsetOf
                   , testProperty "prop_isSubsetOf2" prop_isSubsetOf2
                   , testProperty "prop_symmetricDifference" prop_symmetricDifference
                   , testProperty "prop_disjoint" prop_disjoint
                   , testProperty "prop_size" prop_size
                   , testProperty "prop_lookupMax" prop_lookupMax
                   , testProperty "prop_lookupMin" prop_lookupMin
                   , testProperty "prop_findMax" prop_findMax
                   , testProperty "prop_findMin" prop_findMin
                   , testProperty "prop_ord" prop_ord
                   , testProperty "prop_readShow" prop_readShow
                   , testProperty "prop_foldR" prop_foldR
                   , testProperty "prop_foldR'" prop_foldR'
                   , testProperty "prop_foldL" prop_foldL
                   , testProperty "prop_foldL'" prop_foldL'
                   , testProperty "prop_map" prop_map
                   , testProperty "prop_map2" prop_map2
                   , testProperty "prop_mapMonotonic" prop_mapMonotonic
                   , testProperty "prop_maxView" prop_maxView
                   , testProperty "prop_minView" prop_minView
                   , testProperty "prop_split" prop_split
                   , testProperty "prop_splitMember" prop_splitMember
                   , testProperty "prop_splitRoot" prop_splitRoot
                   , testProperty "prop_partition" prop_partition
                   , testProperty "prop_filter" prop_filter
                   , testProperty "takeWhileAntitone"    prop_takeWhileAntitone
                   , testProperty "dropWhileAntitone"    prop_dropWhileAntitone
                   , testProperty "spanAntitone"         prop_spanAntitone
                   , testProperty "take"                 prop_take
                   , testProperty "drop"                 prop_drop
                   , testProperty "splitAt"              prop_splitAt
                   , testProperty "powerSet"             prop_powerSet
                   , testProperty "cartesianProduct"     prop_cartesianProduct
                   , testProperty "disjointUnion"        prop_disjointUnion
                   , testProperty "eq" prop_eq
                   , testProperty "compare" prop_compare
                   , testProperty "intersections" prop_intersections
                   , testProperty "intersections_lazy" prop_intersections_lazy
                   , testProperty "insert" prop_insert
                   , testProperty "delete" prop_delete
                   , testProperty "deleteMin" prop_deleteMin
                   , testProperty "deleteMax" prop_deleteMax
                   , testProperty "findIndex" prop_findIndex
                   , testProperty "lookupIndex" prop_lookupIndex
                   , testProperty "elemAt" prop_elemAt
                   , testProperty "deleteAt" prop_deleteAt
                   ]

-- A type with a peculiar Eq instance designed to make sure keys
-- come from where they're supposed to.
data OddEq a = OddEq a Bool deriving (Show)

getOddEq :: OddEq a -> (a, Bool)
getOddEq (OddEq b a) = (b, a)
instance Arbitrary a => Arbitrary (OddEq a) where
  arbitrary = OddEq <$> arbitrary <*> arbitrary
instance Eq a => Eq (OddEq a) where
  OddEq x _ == OddEq y _ = x == y
instance Ord a => Ord (OddEq a) where
  OddEq x _ `compare` OddEq y _ = x `compare` y

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

{--------------------------------------------------------------------
  Indexed
--------------------------------------------------------------------}

test_lookupIndex :: Assertion
test_lookupIndex = do
    isJust   (lookupIndex 2 (fromList [5,3])) @?= False
    fromJust (lookupIndex 3 (fromList [5,3])) @?= 0
    fromJust (lookupIndex 5 (fromList [5,3])) @?= 1
    isJust   (lookupIndex 6 (fromList [5,3])) @?= False

test_findIndex :: Assertion
test_findIndex = do
    findIndex 3 (fromList [5,3]) @?= 0
    findIndex 5 (fromList [5,3]) @?= 1

test_elemAt :: Assertion
test_elemAt = do
    elemAt 0 (fromList [5,3]) @?= 3
    elemAt 1 (fromList [5,3]) @?= 5

test_deleteAt :: Assertion
test_deleteAt = do
    deleteAt 0 (fromList [5,3]) @?= singleton 5
    deleteAt 1 (fromList [5,3]) @?= singleton 3

{--------------------------------------------------------------------
  Arbitrary, reasonably balanced trees
--------------------------------------------------------------------}

-- | The IsInt class lets us constrain a type variable to be Int in an entirely
-- standard way. The constraint @ IsInt a @ is essentially equivalent to the
-- GHC-only constraint @ a ~ Int @, but @ IsInt @ requires manual intervention
-- to use. If ~ is ever standardized, we should certainly use it instead.
-- Earlier versions used an Enum constraint, but this is confusing because
-- not all Enum instances will work properly for the Arbitrary instance here.
class (Show a, Read a, Integral a, Arbitrary a) => IsInt a where
  fromIntF :: f Int -> f a

instance IsInt Int where
  fromIntF = id

-- | Convert an Int to any instance of IsInt
fromInt :: IsInt a => Int -> a
fromInt = runIdentity . fromIntF . Identity

{- We don't actually need this, but we can add it if we ever do
toIntF :: IsInt a => g a -> g Int
toIntF = unf . fromIntF . F $ id

newtype F g a b = F {unf :: g b -> a}

toInt :: IsInt a => a -> Int
toInt = runIdentity . toIntF . Identity -}


-- How much the minimum value of an arbitrary set should vary
positionFactor :: Int
positionFactor = 1

-- How much the gap between consecutive elements in an arbitrary
-- set should vary
gapRange :: Int
gapRange = 5

instance IsInt a => Arbitrary (Set a) where
  arbitrary = sized (\sz0 -> do
        sz <- choose (0, sz0)
        middle <- choose (-positionFactor * (sz + 1), positionFactor * (sz + 1))
        let shift = (sz * (gapRange) + 1) `quot` 2
            start = middle - shift
        t <- evalStateT (mkArbSet step sz) start
        if valid t then pure t else error "Test generated invalid tree!")
    where
      step = do
        i <- get
        diff <- lift $ choose (1, gapRange)
        let i' = i + diff
        put i'
        pure (fromInt i')

data TwoSets = TwoSets (Set Int) (Set Int) deriving (Show)

data TwoLists a = TwoLists [a] [a]

data Options2 = One2 | Two2 | Both2 deriving (Bounded, Enum)
instance Arbitrary Options2 where
  arbitrary = arbitraryBoundedEnum

-- We produce two lists from a simple "universe". This instance
-- is intended to give good results when the two lists are then
-- combined with each other; if other elements are used with them,
-- they may or may not behave particularly well.
instance IsInt a => Arbitrary (TwoLists a) where
  arbitrary = sized $ \sz0 -> do
    sz <- choose (0, sz0)
    let universe = [0,3..3*(fromInt sz - 1)]
    divide2Gen universe

instance Arbitrary TwoSets where
  arbitrary = do
    TwoLists l r <- arbitrary
    TwoSets <$> setFromList l <*> setFromList r

divide2Gen :: [a] -> Gen (TwoLists a)
divide2Gen [] = pure (TwoLists [] [])
divide2Gen (x : xs) = do
  way <- arbitrary
  TwoLists ls rs <- divide2Gen xs
  case way of
    One2 -> pure (TwoLists (x : ls) rs)
    Two2 -> pure (TwoLists ls (x : rs))
    Both2 -> pure (TwoLists (x : ls) (x : rs))

{--------------------------------------------------------------------
  Valid trees
--------------------------------------------------------------------}
forValid :: (IsInt a,Testable b) => (Set a -> b) -> Property
forValid f = forAll arbitrary $ \t ->
    classify (size t == 0) "empty" $
    classify (size t > 0  && size t <= 10) "small" $
    classify (size t > 10 && size t <= 64) "medium" $
    classify (size t > 64) "large" $ f t

forValidUnitTree :: Testable a => (Set Int -> a) -> Property
forValidUnitTree f = forValid f

prop_Valid :: Property
prop_Valid = forValidUnitTree $ \t -> valid t

{--------------------------------------------------------------------
  Single, Member, Insert, Delete
--------------------------------------------------------------------}
prop_Single :: Int -> Bool
prop_Single x = (insert x empty == singleton x)

prop_Member :: [Int] -> Int -> Bool
prop_Member xs n =
  let m  = fromList xs
  in all (\k -> k `member` m == (k `elem` xs)) (n : xs)

prop_NotMember :: [Int] -> Int -> Bool
prop_NotMember xs n =
  let m  = fromList xs
  in all (\k -> k `notMember` m == (k `notElem` xs)) (n : xs)

prop_LookupLT :: Int -> Set Int -> Property
prop_LookupLT x xs = lookupLT x xs === List.find (<x) (toDescList xs)

prop_LookupGT :: Int -> Set Int -> Property
prop_LookupGT x xs = lookupGT x xs === List.find (>x) (toList xs)

prop_LookupLE :: Int -> Set Int -> Property
prop_LookupLE x xs = lookupLE x xs === List.find (<=x) (toDescList xs)

prop_LookupGE :: Int -> Set Int -> Property
prop_LookupGE x xs = lookupGE x xs === List.find (>=x) (toList xs)

prop_InsertValid :: Int -> Property
prop_InsertValid k = forValidUnitTree $ \t -> valid (insert k t)

prop_InsertDelete :: Int -> Set Int -> Property
prop_InsertDelete k t = not (member k t) ==> delete k (insert k t) == t

prop_InsertBiased :: Int -> Set Int -> Bool
prop_InsertBiased k t = (k, True) `member` kt
  where
    t' = mapMonotonic (`OddEq` False) t
    kt' = insert (OddEq k True) t'
    kt = mapMonotonic getOddEq kt'

prop_DeleteValid :: Int -> Property
prop_DeleteValid k = forValidUnitTree $ \t -> valid (delete k (insert k t))

{--------------------------------------------------------------------
  alterF
--------------------------------------------------------------------}

newtype Ident a = Ident { runIdent :: a }
instance Functor Ident where
  fmap f (Ident a) = Ident (f a)

newtype Consty a b = Consty { getConsty :: a}
instance Functor (Consty a) where
  fmap _ (Consty a) = Consty a

data Four a = Four a a a a
  deriving (Eq, Show)
instance Functor Four where
  fmap f (Four a b c d) = Four (f a) (f b) (f c) (f d)

four :: Bool -> Four Bool
               -- insert  delete  id     toggle
four True  = Four True    False   True   False
four False = Four True    False   False  True

toggle :: Ord a => a -> Set a -> Set a
toggle k s =
    if member k s
        then delete k s
        else insert k s

prop_alterF :: Fun Bool [Bool] -> Int -> Set Int -> Property
prop_alterF f k s = fmap (member k) (alterF (apply f) k s) === apply f (member k s)

prop_alterF_insert :: Int -> Set Int -> Property
prop_alterF_insert k s = runIdent (alterF (const (Ident True)) k s) === insert k s

prop_alterF_delete :: Int -> Set Int -> Property
prop_alterF_delete k s = runIdent (alterF (const (Ident False)) k s) === delete k s

prop_alterF_member :: Int -> Set Int -> Property
prop_alterF_member k s = getConsty (alterF (\b -> Consty b) k s) === member k s

prop_alterF_four :: Int -> Set Int -> Property
prop_alterF_four k s = alterF four k s === Four (insert k s) (delete k s) s (toggle k s)

prop_alterF_valid :: Int -> Set Int -> Property
prop_alterF_valid k s = fmap valid (alterF four k s) === Four True True True True

{--------------------------------------------------------------------
  Balance
--------------------------------------------------------------------}
prop_Link :: Int -> Property
prop_Link x = forValidUnitTree $ \t ->
    let (l,r) = split x t
    in valid (link x l r)

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

prop_UnionComm :: TwoSets -> Bool
prop_UnionComm (TwoSets t1 t2) = (union t1 t2 == union t2 t1)

prop_UnionBiased :: TwoSets -> Property
prop_UnionBiased (TwoSets l r) = union l' r' === union l' (difference r' l')
  where
    l' = mapMonotonic (`OddEq` False) l
    r' = mapMonotonic (`OddEq` True) r

prop_IntBiased :: TwoSets -> Bool
prop_IntBiased (TwoSets l r) = all (\(OddEq _ b) -> not b) l'r'
  where
    l' = mapMonotonic (`OddEq` False) l
    r' = mapMonotonic (`OddEq` True) r
    l'r' = intersection l' r'

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

prop_symmetricDifference :: Set Int -> Set Int -> Property
prop_symmetricDifference xs ys =
  valid zs .&&.
  toAscList zs ===
  List.sort (List.filter (`notElem` xs') ys' ++ List.filter (`notElem` ys') xs')
  where
    zs = symmetricDifference xs ys
    xs' = toAscList xs
    ys' = toAscList ys

prop_disjoint :: Set Int -> Set Int -> Bool
prop_disjoint a b = a `disjoint` b == null (a `intersection` b)

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
prop_Ordered :: Property
prop_Ordered = forAll (choose (5,100)) $ \n ->
    let xs = [0..n::Int]
    in fromAscList xs === fromList xs

prop_DescendingOrdered :: Property
prop_DescendingOrdered = forAll (choose (5,100)) $ \n ->
    let xs = [n,n-1..0::Int]
    in fromDescList xs === fromList xs

prop_List :: [Int] -> Bool
prop_List xs = (sort (nub xs) == toList (fromList xs))

prop_DescList :: [Int] -> Bool
prop_DescList xs = (reverse (sort (nub xs)) == toDescList (fromList xs))

prop_AscDescList :: [Int] -> Bool
prop_AscDescList xs = toAscList s == reverse (toDescList s)
  where s = fromList xs

prop_fromList :: [Int] -> Property
prop_fromList xs =
           valid t .&&.
           t === List.foldr insert empty xs
  where t = fromList xs

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

prop_fromDescList :: [Int] -> Property
prop_fromDescList xs =
    valid t .&&.
    toList t === reverse nubDownSortedXs
  where
    downSortedXs = sortBy (comparing Down) xs
    nubDownSortedXs = List.map NE.head $ NE.group downSortedXs
    t = fromDescList downSortedXs

prop_fromDistinctDescList :: [Int] -> Property
prop_fromDistinctDescList xs =
    valid t .&&.
    toList t === reverse nubDownSortedXs
  where
    nubDownSortedXs = List.map NE.head $ NE.group $ sortBy (comparing Down) xs
    t = fromDistinctDescList nubDownSortedXs

{--------------------------------------------------------------------
  Set operations are like IntSet operations
--------------------------------------------------------------------}
toIntSet :: Set Int -> IntSet.IntSet
toIntSet = IntSet.fromList . toList

-- Check that Set Int.isProperSubsetOf is the same as Set.isProperSubsetOf.
prop_isProperSubsetOf :: TwoSets -> Bool
prop_isProperSubsetOf (TwoSets a b) = isProperSubsetOf a b == IntSet.isProperSubsetOf (toIntSet a) (toIntSet b)

-- In the above test, isProperSubsetOf almost always returns False (since a
-- random set is almost never a subset of another random set).  So this second
-- test checks the True case.
prop_isProperSubsetOf2 :: TwoSets -> Bool
prop_isProperSubsetOf2 (TwoSets a b) = isProperSubsetOf a c == (a /= c) where
  c = union a b

prop_isSubsetOf :: TwoSets -> Bool
prop_isSubsetOf (TwoSets a b) = isSubsetOf a b == IntSet.isSubsetOf (toIntSet a) (toIntSet b)

prop_isSubsetOf2 :: TwoSets -> Bool
prop_isSubsetOf2 (TwoSets a b) = isSubsetOf a (union a b)

prop_size :: Set Int -> Bool
prop_size s = size s == List.length (toList s)

prop_findMax :: Set Int -> Property
prop_findMax s = not (null s) ==> findMax s == maximum (toList s)

prop_findMin :: Set Int -> Property
prop_findMin s = not (null s) ==> findMin s == minimum (toList s)

prop_lookupMin :: Set Int -> Property
prop_lookupMin m = lookupMin m === (fst <$> minView m)

prop_lookupMax :: Set Int -> Property
prop_lookupMax m = lookupMax m === (fst <$> maxView m)

prop_ord :: TwoSets -> Bool
prop_ord (TwoSets s1 s2) = s1 `compare` s2 == toList s1 `compare` toList s2

prop_readShow :: Set Int -> Bool
prop_readShow s = s == read (show s)

prop_foldR :: Set Int -> Bool
prop_foldR s = foldr (:) [] s == toList s

prop_foldR' :: Set Int -> Bool
prop_foldR' s = foldr' (:) [] s == toList s

prop_foldL :: Set Int -> Bool
prop_foldL s = foldl (flip (:)) [] s == List.foldl (flip (:)) [] (toList s)

prop_foldL' :: Set Int -> Bool
prop_foldL' s = foldl' (flip (:)) [] s == List.foldl' (flip (:)) [] (toList s)

prop_map :: Set Int -> Bool
prop_map s = map id s == s

prop_map2 :: Fun Int Int -> Fun Int Int -> Set Int -> Property
prop_map2 f g s = map (apply f) (map (apply g) s) === map (apply f . apply g) s

prop_mapMonotonic :: Set Int -> Property
prop_mapMonotonic s = mapMonotonic id s === s

prop_maxView :: Set Int -> Bool
prop_maxView s = case maxView s of
    Nothing -> null s
    Just (m,s') -> m == maximum (toList s) && s == insert m s' && m `notMember` s'

prop_minView :: Set Int -> Bool
prop_minView s = case minView s of
    Nothing -> null s
    Just (m,s') -> m == minimum (toList s) && s == insert m s' && m `notMember` s'

prop_split :: Set Int -> Int -> Bool
prop_split s i = case split i s of
    (s1,s2) -> all (<i) (toList s1) && all (>i) (toList s2) && i `delete` s == union s1 s2

prop_splitMember :: Set Int -> Int -> Bool
prop_splitMember s i = case splitMember i s of
    (s1,t,s2) -> all (<i) (toList s1) && all (>i) (toList s2) && t == i `member` s && i `delete` s == union s1 s2

prop_splitRoot :: Set Int -> Bool
prop_splitRoot s = loop ls && (s == unions ls)
 where
  ls = splitRoot s
  loop [] = True
  loop (s1:rst) = List.null
                  [ (x,y) | x <- toList s1
                          , y <- toList (unions rst)
                          , x > y ]

prop_partition :: Set Int -> Int -> Bool
prop_partition s i = case partition odd s of
    (s1,s2) -> all odd (toList s1) && all even (toList s2) && s == s1 `union` s2

prop_filter :: Set Int -> Int -> Bool
prop_filter s i = partition odd s == (filter odd s, filter even s)

prop_take :: Int -> Set Int -> Property
prop_take n xs = valid taken .&&.
                 taken === fromDistinctAscList (List.take n (toList xs))
  where
    taken = take n xs

prop_drop :: Int -> Set Int -> Property
prop_drop n xs = valid dropped .&&.
                 dropped === fromDistinctAscList (List.drop n (toList xs))
  where
    dropped = drop n xs

prop_splitAt :: Int -> Set Int -> Property
prop_splitAt n xs = valid taken .&&.
                    valid dropped .&&.
                    taken === take n xs .&&.
                    dropped === drop n xs
  where
    (taken, dropped) = splitAt n xs

prop_takeWhileAntitone :: [Either Int Int] -> Property
prop_takeWhileAntitone xs' = valid tw .&&. tw === filter isLeft xs
  where
    xs = fromList xs'
    tw = takeWhileAntitone isLeft xs

prop_dropWhileAntitone :: [Either Int Int] -> Property
prop_dropWhileAntitone xs' = valid tw .&&. tw === filter (not . isLeft) xs
  where
    xs = fromList xs'
    tw = dropWhileAntitone isLeft xs

prop_spanAntitone :: [Either Int Int] -> Property
prop_spanAntitone xs' = valid tw .&&. valid dw
                        .&&. tw === takeWhileAntitone isLeft xs
                        .&&. dw === dropWhileAntitone isLeft xs
  where
    xs = fromList xs'
    (tw, dw) = spanAntitone isLeft xs

prop_powerSet :: Property
prop_powerSet = forAll (resize 10 arbitrary :: Gen (Set Int)) $ \xs ->
   -- We don't actually have to check on the values directly, because the power
   -- set is the *only* one that can be produced by a function with the type of
   -- `powerSet` and satisfy the criteria below. In particular, the `valid ps`
   -- test ensures that we haven't duplicated any subsets, while the size test
   -- ensures that we haven't omitted any. Parametricity ensures that we
   -- haven't produced any elements out of thin air.
   let ps = powerSet xs
   in valid ps .&&. all valid ps .&&. size ps === 2^size xs

prop_cartesianProduct :: Set Int -> Set Int -> Property
prop_cartesianProduct xs ys =
  valid cp .&&. toList cp === liftA2 (,) (toList xs) (toList ys)
  where cp = cartesianProduct xs ys

prop_disjointUnion :: Set Int -> Set Int -> Property
prop_disjointUnion xs ys =
  valid du .&&. du === union (mapMonotonic Left xs) (mapMonotonic Right ys)
  where du = disjointUnion xs ys

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

prop_eq :: Set Int -> Set Int -> Property
prop_eq s1 s2 = (s1 == s2) === (toList s1 == toList s2)

prop_compare :: Set Int -> Set Int -> Property
prop_compare s1 s2 = compare s1 s2 === compare (toList s1) (toList s2)

prop_intersections :: (Set Int, [Set Int]) -> Property
prop_intersections (s, ss) =
  intersections ss' === List.foldl' intersection s ss
  where
    ss' = s :| ss -- Work around missing Arbitrary NonEmpty instance

prop_intersections_lazy :: [Set Int] -> Property
prop_intersections_lazy ss = intersections ss' === empty
  where
    ss' = NE.fromList $ ss ++ [empty] ++ error "too strict"
                           --- ^ result will certainly be empty at this point,
                           --    so the rest of the list should not be demanded.

prop_insert :: Int -> Set Int -> Property
prop_insert x s =
  valid s' .&&.
  toList s' === (if x `List.elem` xs then xs else List.insert x xs)
  where
    s' = insert x s
    xs = toList s

prop_delete :: Int -> Set Int -> Property
prop_delete x s = valid s' .&&. toList s' === toList s List.\\ [x]
  where
    s' = delete x s

prop_deleteMin :: Set Int -> Property
prop_deleteMin s = toList (deleteMin s) === if null s then [] else tail (toList s)

prop_deleteMax :: Set Int -> Property
prop_deleteMax s = toList (deleteMax s) === if null s then [] else init (toList s)

prop_findIndex :: Int -> Set Int -> Property
prop_findIndex x s = x `member` s ==>
  Just (findIndex x s) === List.findIndex (==x) (toList s)

prop_lookupIndex :: Int -> Set Int -> Property
prop_lookupIndex x s = lookupIndex x s === List.findIndex (==x) (toList s)

prop_elemAt :: Int -> Set Int -> Property
prop_elemAt i s = 0 <= i && i < size s ==>
  elemAt i s === toList s !! i

prop_deleteAt :: Int -> Set Int -> Property
prop_deleteAt i s = 0 <= i && i < size s ==>
  valid s' .&&.
  toList s' === [x | (j, x) <- zip [0..] (toList s), i /= j]
  where
    s' = deleteAt i s
