{-# LANGUAGE CPP #-}

#ifdef STRICT
import Data.IntMap.Strict as Data.IntMap
import Data.IntMap.Strict.Internal (traverseMaybeWithKey)
import Data.IntMap.Merge.Strict
#else
import Data.IntMap.Lazy as Data.IntMap
import Data.IntMap.Internal (traverseMaybeWithKey)
import Data.IntMap.Merge.Lazy
#endif
import Data.IntMap.Internal.Debug (showTree)
import Data.IntSet.Internal.IntTreeCommons (Prefix(..), nomatch)
import IntMapValidity (hasPrefix, hasPrefixSimple, valid)

import Control.Applicative (Applicative(..))
import Control.Monad ((<=<))
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import Data.Monoid
import Data.Maybe hiding (mapMaybe)
import qualified Data.Maybe as Maybe (mapMaybe)
import Data.Ord
import Data.Foldable (foldMap)
import Data.Function
import Data.Traversable (Traversable(traverse), foldMapDefault)
import Prelude hiding (lookup, null, map, filter, foldr, foldl, foldl')
import qualified Prelude (map, filter)

import Data.List (nub,sort)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.IntSet as IntSet
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function (apply)
import Test.QuickCheck.Poly (A, B, C, OrdA)

default (Int)

main :: IO ()
main = defaultMain $ testGroup "intmap-properties"
         [
               testCase "index"      test_index
             , testCase "index_lookup" test_index_lookup
             , testCase "size"       test_size
             , testCase "size2"      test_size2
             , testCase "member"     test_member
             , testCase "notMember"  test_notMember
             , testCase "lookup"     test_lookup
             , testCase "findWithDefault"     test_findWithDefault
             , testCase "lookupLT"   test_lookupLT
             , testCase "lookupGT"   test_lookupGT
             , testCase "lookupLE"   test_lookupLE
             , testCase "lookupGE"   test_lookupGE
             , testCase "empty" test_empty
             , testCase "mempty" test_mempty
             , testCase "singleton" test_singleton
             , testCase "insert" test_insert
             , testCase "insertWith" test_insertWith
             , testCase "insertWithKey" test_insertWithKey
             , testCase "insertLookupWithKey" test_insertLookupWithKey
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
             , testCase "elems" test_elems
             , testCase "keys" test_keys
             , testCase "assocs" test_assocs
             , testCase "keysSet" test_keysSet
             , testCase "keysSet" test_fromSet
             , testCase "toList" test_toList
             , testCase "fromList" test_fromList
             , testCase "fromListWith" test_fromListWith
             , testCase "fromListWithKey" test_fromListWithKey
             , testCase "toAscList" test_toAscList
             , testCase "toDescList" test_toDescList
             , testCase "showTree" test_showTree
             , testCase "fromAscList" test_fromAscList
             , testCase "fromAscListWith" test_fromAscListWith
             , testCase "fromAscListWithKey" test_fromAscListWithKey
             , testCase "fromDistinctAscList" test_fromDistinctAscList
             , testCase "filter" test_filter
             , testCase "filterKeys" test_filterKeys
             , testCase "filterWithKey" test_filterWithKey
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
             , testCase "lookupMin" test_lookupMin
             , testCase "lookupMax" test_lookupMax
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
             , testCase "minimum" test_minimum
             , testCase "maximum" test_maximum
             , testCase "compareSize" test_compareSize
             , testProperty "valid"                prop_valid
             , testProperty "hasPrefix"            prop_hasPrefix
             , testProperty "empty valid"          prop_emptyValid
             , testProperty "insert to singleton"  prop_singleton
             , testProperty "insert then lookup"   prop_insertLookup
             , testProperty "insert then delete"   prop_insertDelete
             , testProperty "delete non member"    prop_deleteNonMember
             , testProperty "union model"          prop_unionModel
             , testProperty "union singleton"      prop_unionSingleton
             , testProperty "union associative"    prop_unionAssoc
             , testProperty "union+unionWith"      prop_unionWith
             , testProperty "union sum"            prop_unionSum
             , testProperty "difference model"     prop_differenceModel
             , testProperty "intersection model"   prop_intersectionModel
             , testProperty "intersectionWith model" prop_intersectionWithModel
             , testProperty "intersectionWithKey model" prop_intersectionWithKeyModel
             , testProperty "symmetricDifference"  prop_symmetricDifference
             , testProperty "mergeWithKey model"   prop_mergeWithKeyModel
             , testProperty "merge valid"          prop_merge_valid
             , testProperty "mergeA effects"       prop_mergeA_effects
             , testProperty "fromList then toList" prop_list
             , testProperty "toDescList"           prop_descList
             , testProperty "toAscList+toDescList" prop_ascDescList
             , testProperty "fromList"             prop_fromList
             , testProperty "alter"                prop_alter
             , testProperty "index"                prop_index
             , testProperty "index_lookup"         prop_index_lookup
             , testProperty "null"                 prop_null
             , testProperty "size"                 prop_size
             , testProperty "member"               prop_member
             , testProperty "notmember"            prop_notmember
             , testProperty "lookup"               prop_lookup
             , testProperty "find"                 prop_find
             , testProperty "findWithDefault"      prop_findWithDefault
             , testProperty "lookupLT"             prop_lookupLT
             , testProperty "lookupGT"             prop_lookupGT
             , testProperty "lookupLE"             prop_lookupLE
             , testProperty "lookupGE"             prop_lookupGE
             , testProperty "disjoint"             prop_disjoint
             , testProperty "compose"              prop_compose
             , testProperty "lookupMin"            prop_lookupMin
             , testProperty "lookupMax"            prop_lookupMax
             , testProperty "findMin"              prop_findMin
             , testProperty "findMax"              prop_findMax
             , testProperty "deleteMin"            prop_deleteMinModel
             , testProperty "deleteMax"            prop_deleteMaxModel
             , testProperty "filter"               prop_filter
             , testProperty "filterKeys"           prop_filterKeys
             , testProperty "filterWithKey"        prop_filterWithKey
             , testProperty "partition"            prop_partition
             , testProperty "takeWhileAntitone"    prop_takeWhileAntitone
             , testProperty "dropWhileAntitone"    prop_dropWhileAntitone
             , testProperty "spanAntitone"         prop_spanAntitone
             , testProperty "map"                  prop_map
             , testProperty "fmap"                 prop_fmap
             , testProperty "mapkeys"              prop_mapkeys
             , testProperty "split"                prop_splitModel
             , testProperty "splitLookup"          prop_splitLookup
             , testProperty "splitRoot"            prop_splitRoot
             , testProperty "foldr"                prop_foldr
             , testProperty "foldr'"               prop_foldr'
             , testProperty "foldl"                prop_foldl
             , testProperty "foldl'"               prop_foldl'
             , testProperty "foldr==foldMap"       prop_foldrEqFoldMap
             , testProperty
                 "foldrWithKey==foldMapWithKey"
                 prop_foldrWithKeyEqFoldMapWithKey
             , testProperty
                 "prop_FoldableTraversableCompat"
                 prop_FoldableTraversableCompat
             , testProperty "keysSet"              prop_keysSet
             , testProperty "fromSet"              prop_fromSet
             , testProperty "restrictKeys"         prop_restrictKeys
             , testProperty "withoutKeys"          prop_withoutKeys
             , testProperty "traverseWithKey identity"              prop_traverseWithKey_identity
             , testProperty "traverseWithKey->mapWithKey"           prop_traverseWithKey_degrade_to_mapWithKey
             , testProperty "traverseMaybeWithKey identity"         prop_traverseMaybeWithKey_identity
             , testProperty "traverseMaybeWithKey->mapMaybeWithKey" prop_traverseMaybeWithKey_degrade_to_mapMaybeWithKey
             , testProperty "traverseMaybeWithKey->traverseWithKey" prop_traverseMaybeWithKey_degrade_to_traverseWithKey
             , testProperty "isProperSubmapOfBy"   prop_isProperSubmapOfBy
             , testProperty "isSubmapOfBy"         prop_isSubmapOfBy
             , testProperty "insert"               prop_insert
             , testProperty "delete"               prop_delete
             , testProperty "insertWith"           prop_insertWith
             , testProperty "insertWithKey"        prop_insertWithKey
             , testProperty "insertLookupWithKey"  prop_insertLookupWithKey
             , testProperty "adjust"               prop_adjust
             , testProperty "adjustWithKey"        prop_adjustWithKey
             , testProperty "update"               prop_update
             , testProperty "updateWithKey"        prop_updateWithKey
             , testProperty "upsert"               prop_upsert
             , testProperty "updateLookupWithKey"  prop_updateLookupWithKey
             , testProperty "differenceWith"       prop_differenceWith
             , testProperty "differenceWithKey"    prop_differenceWithKey
             , testProperty "partitionWithKey"     prop_partitionWithKey
             , testProperty "updateMin"            prop_updateMin
             , testProperty "updateMax"            prop_updateMax
             , testProperty "updateMinWithKey"     prop_updateMinWithKey
             , testProperty "updateMaxWithKey"     prop_updateMaxWithKey
             , testProperty "minViewWithKey"       prop_minViewWithKey
             , testProperty "maxViewWithKey"       prop_maxViewWithKey
             , testProperty "minView"              prop_minView
             , testProperty "maxView"              prop_maxView
             , testProperty "mapEither"            prop_mapEither
             , testProperty "mapEitherWithKey"     prop_mapEitherWithKey
             , testProperty "mapAccum"             prop_mapAccum
             , testProperty "mapAccumWithKey"      prop_mapAccumWithKey
             , testProperty "mapAccumRWithKey"     prop_mapAccumRWithKey
             , testProperty "mapKeysWith"          prop_mapKeysWith
             , testProperty "mapKeysMonotonic"     prop_mapKeysMonotonic
             , testProperty "compare"              prop_compare
             , testProperty "fromAscList"          prop_fromAscList
             , testProperty "fromAscListWith"      prop_fromAscListWith
             , testProperty "fromAscListWithKey"   prop_fromAscListWithKey
             , testProperty "fromDistinctAscList"  prop_fromDistinctAscList
             , testProperty "fromListWith"         prop_fromListWith
             , testProperty "fromListWithKey"      prop_fromListWithKey
             , testProperty "compareSize"          prop_compareSize
             ]

{--------------------------------------------------------------------
  Arbitrary, reasonably balanced trees
--------------------------------------------------------------------}

instance Arbitrary a => Arbitrary (IntMap a) where
  arbitrary = oneof [go arbitrary, go (getLarge <$> arbitrary)]
    where
      go kgen = fromList <$> listOf ((,) <$> kgen <*> arbitrary)
  shrink = fmap fromList . shrink . toAscList

newtype NonEmptyIntMap a = NonEmptyIntMap {getNonEmptyIntMap :: IntMap a} deriving (Eq, Show)

instance Arbitrary a => Arbitrary (NonEmptyIntMap a) where
  arbitrary = oneof [go arbitrary, go (getLarge <$> arbitrary)]
    where
      go kgen = NonEmptyIntMap . fromList <$> listOf1 ((,) <$> kgen <*> arbitrary)
  shrink =
    fmap (NonEmptyIntMap . fromList) .
    List.filter (not . List.null) .
    shrink .
    toAscList .
    getNonEmptyIntMap


------------------------------------------------------------------------

type UMap = IntMap ()
type IMap = IntMap Int
type SMap = IntMap String

----------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------

----------------------------------------------------------------
-- Operators

test_index :: Assertion
test_index = do
    fromList [(5,'a'), (3,'b')] ! 5 @?= 'a'

    fromList [(5,'a'), (-3,'b')] ! (-3) @?= 'b'

test_index_lookup :: Assertion
test_index_lookup = do
    fromList [(5,'a'), (3,'b')] !? 1 @?= Nothing
    fromList [(5,'a'), (3,'b')] !? 5 @?= Just 'a'

    fromList [(5,'a'), (-3,'b')] !? 1 @?= Nothing
    fromList [(5,'a'), (-3,'b')] !? 5 @?= Just 'a'
    fromList [(5,'a'), (-3,'b')] !? (-3) @?= Just 'b'

----------------------------------------------------------------
-- Query

test_size :: Assertion
test_size = do
    null (empty)           @?= True
    null (singleton 1 'a') @?= False

    null (singleton (-1) 'a') @?= False

test_size2 :: Assertion
test_size2 = do
    size empty                                   @?= 0
    size (singleton 1 'a')                       @?= 1
    size (fromList([(1,'a'), (2,'c'), (3,'b')])) @?= 3

    size (fromList [(-2, '?'),(5,'a'), (3,'b')]) @?= 3

test_member :: Assertion
test_member = do
    member 5 (fromList [(5,'a'), (3,'b')]) @?= True
    member 1 (fromList [(5,'a'), (3,'b')]) @?= False

    member 5    (fromList [(5,'a'), (-3,'b')]) @?= True
    member 1    (fromList [(5,'a'), (-3,'b')]) @?= False
    member (-3) (fromList [(5,'a'), (-3,'b')]) @?= True

test_notMember :: Assertion
test_notMember = do
    notMember 5 (fromList [(5,'a'), (3,'b')]) @?= False
    notMember 1 (fromList [(5,'a'), (3,'b')]) @?= True

    notMember 5    (fromList [(5,'a'), (-3,'b')]) @?= False
    notMember 1    (fromList [(5,'a'), (-3,'b')]) @?= True
    notMember (-3) (fromList [(5,'a'), (-3,'b')]) @?= False

test_lookup :: Assertion
test_lookup = do
    employeeCurrency 1      @?= Just 1
    employeeCurrency 2      @?= Just 2
    employeeCurrency 3      @?= Just 3
    employeeCurrency 4      @?= Just 4
    employeeCurrency 5      @?= Nothing
    employeeCurrency (2^10) @?= Just 42
    employeeCurrency 6      @?= Nothing
  where
    employeeDept    = fromList [(1,2), (2, 14), (3, 10), (4, 18), (2^10, 100)]
    deptCountry     = fromList [(1,1), (14, 14), (10, 10), (18, 18), (100, 100), (2,2)]
    countryCurrency = fromList [(1, 2), (2, 1), (14, 2), (10, 3), (18, 4), (100, 42)]
    employeeCurrency :: Int -> Maybe Int
    employeeCurrency name = do
        dept <- lookup name employeeDept
        country <- lookup dept deptCountry
        lookup country countryCurrency

test_findWithDefault :: Assertion
test_findWithDefault = do
    findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) @?= 'x'
    findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) @?= 'a'

    findWithDefault 'x' 1    (fromList [(5,'a'), (-3,'b')]) @?= 'x'
    findWithDefault 'x' 5    (fromList [(5,'a'), (-3,'b')]) @?= 'a'
    findWithDefault 'x' (-3) (fromList [(5,'a'), (-3,'b')]) @?= 'b'



test_lookupLT :: Assertion
test_lookupLT = do
    lookupLT 3 (fromList [(3,'a'), (5,'b')]) @?= Nothing
    lookupLT 4 (fromList [(3,'a'), (5,'b')]) @?= Just (3, 'a')

    lookupLT (-3) (fromList [(5,'a'), (-3,'b')]) @?= Nothing
    lookupLT (-2) (fromList [(5,'a'), (-3,'b')]) @?= Just (-3, 'b')
    lookupLT 4    (fromList [(5,'a'), (-3,'b')]) @?= Just (-3, 'b')
    lookupLT 6    (fromList [(5,'a'), (-3,'b')]) @?= Just (5, 'a')

test_lookupGT :: Assertion
test_lookupGT = do
    lookupGT 4 (fromList [(3,'a'), (5,'b')]) @?= Just (5, 'b')
    lookupGT 5 (fromList [(3,'a'), (5,'b')]) @?= Nothing

    lookupGT (-4) (fromList [(5,'a'), (-3,'b')]) @?= Just (-3, 'b')
    lookupGT (-3) (fromList [(5,'a'), (-3,'b')]) @?= Just (5, 'a')
    lookupGT 4    (fromList [(5,'a'), (-3,'b')]) @?= Just (5, 'a')
    lookupGT 5    (fromList [(5,'a'), (-3,'b')]) @?= Nothing

test_lookupLE :: Assertion
test_lookupLE = do
    lookupLE 2 (fromList [(3,'a'), (5,'b')]) @?= Nothing
    lookupLE 4 (fromList [(3,'a'), (5,'b')]) @?= Just (3, 'a')
    lookupLE 5 (fromList [(3,'a'), (5,'b')]) @?= Just (5, 'b')

    lookupLE (-4) (fromList [(5,'a'), (-3,'b')]) @?= Nothing
    lookupLE (-3) (fromList [(5,'a'), (-3,'b')]) @?= Just (-3, 'b')
    lookupLE 4    (fromList [(5,'a'), (-3,'b')]) @?= Just (-3, 'b')
    lookupLE 5    (fromList [(5,'a'), (-3,'b')]) @?= Just (5, 'a')
    lookupLE 6    (fromList [(5,'a'), (-3,'b')]) @?= Just (5, 'a')

test_lookupGE :: Assertion
test_lookupGE = do
    lookupGE 3 (fromList [(3,'a'), (5,'b')]) @?= Just (3, 'a')
    lookupGE 4 (fromList [(3,'a'), (5,'b')]) @?= Just (5, 'b')
    lookupGE 6 (fromList [(3,'a'), (5,'b')]) @?= Nothing

    lookupGE (-4) (fromList [(5,'a'), (-3,'b')]) @?= Just (-3, 'b')
    lookupGE (-3) (fromList [(5,'a'), (-3,'b')]) @?= Just (-3, 'b')
    lookupGE (-2) (fromList [(5,'a'), (-3,'b')]) @?= Just (5, 'a')
    lookupGE 5    (fromList [(5,'a'), (-3,'b')]) @?= Just (5, 'a')
    lookupGE 6    (fromList [(5,'a'), (-3,'b')]) @?= Nothing

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

    singleton (-1) 'a'        @?= fromList [(-1, 'a')]
    size (singleton (-1) 'a') @?= 1

test_insert :: Assertion
test_insert = do
    insert 5 'x' (fromList [(5,'a'), (3,'b')]) @?= fromList [(3, 'b'), (5, 'x')]
    insert 7 'x' (fromList [(5,'a'), (3,'b')]) @?= fromList [(3, 'b'), (5, 'a'), (7, 'x')]
    insert 5 'x' empty                         @?= singleton 5 'x'

    insert 5    'x' (fromList [(5,'a'), (-3,'b')]) @?= fromList [(-3, 'b'), (5, 'x')]
    insert 7    'x' (fromList [(5,'a'), (-3,'b')]) @?= fromList [(-3, 'b'), (5, 'a'), (7, 'x')]
    insert (-3) 'x' empty                          @?= singleton (-3) 'x'
    insert (-3) 'x' (fromList [(5,'a'), (-3,'b')]) @?= fromList [(-3, 'x'), (5, 'a')]
    insert (-7) 'x' (fromList [(5,'a'), (-3,'b')]) @?= fromList [(-3, 'b'), (5, 'a'), (-7, 'x')]

test_insertWith :: Assertion
test_insertWith = do
    insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "xxxa")]
    insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a"), (7, "xxx")]
    insertWith (++) 5 "xxx" empty                         @?= singleton 5 "xxx"

    insertWith (++) 5 "xxx"    (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "xxxa")]
    insertWith (++) 7 "xxx"    (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a"), (7, "xxx")]
    insertWith (++) (-3) "xxx" empty                          @?= singleton (-3) "xxx"
    insertWith (++) (-3) "xxx" (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "xxxb"), (5, "a")]
    insertWith (++) (-7) "xxx" (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a"), (-7, "xxx")]

test_insertWithKey :: Assertion
test_insertWithKey = do
    insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "5:xxx|a")]
    insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a"), (7, "xxx")]
    insertWithKey f 5 "xxx" empty                         @?= singleton 5 "xxx"

    insertWithKey f 5 "xxx"    (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "5:xxx|a")]
    insertWithKey f 7 "xxx"    (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a"), (7, "xxx")]
    insertWithKey f (-3) "xxx" empty                          @?= singleton (-3) "xxx"
    insertWithKey f (-3) "xxx" (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "-3:xxx|b"), (5, "a")]
    insertWithKey f (-7) "xxx" (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a"), (-7, "xxx")]
  where
    f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value

test_insertLookupWithKey :: Assertion
test_insertLookupWithKey = do
    insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) @?= (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
    insertLookupWithKey f 2 "xxx" (fromList [(5,"a"), (3,"b")]) @?= (Nothing,fromList [(2,"xxx"),(3,"b"),(5,"a")])
    insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) @?= (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
    insertLookupWithKey f 5 "xxx" empty                         @?= (Nothing,  singleton 5 "xxx")

    insertLookupWithKey f 5 "xxx"    (fromList [(5,"a"), (-3,"b")]) @?= (Just "a", fromList [(-3, "b"), (5, "5:xxx|a")])
    insertLookupWithKey f 7 "xxx"    (fromList [(5,"a"), (-3,"b")]) @?= (Nothing,  fromList [(-3, "b"), (5, "a"), (7, "xxx")])
    insertLookupWithKey f (-3) "xxx" empty                          @?= (Nothing,  singleton (-3) "xxx")
    insertLookupWithKey f (-3) "xxx" (fromList [(5,"a"), (-3,"b")]) @?= (Just "b", fromList [(-3, "-3:xxx|b"), (5, "a")])
    insertLookupWithKey f (-7) "xxx" (fromList [(5,"a"), (-3,"b")]) @?= (Nothing,  fromList [(-3, "b"), (5, "a"), (-7, "xxx")])
  where
    f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value

----------------------------------------------------------------
-- Delete/Update

test_delete :: Assertion
test_delete = do
    delete 5 (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"
    delete 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
    delete 5 empty                         @?= (empty :: IMap)

    delete 5    (fromList [(5,"a"), (-3,"b")]) @?= singleton (-3) "b"
    delete 7    (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a")]
    delete (-3) empty                          @?= (empty :: IMap)
    delete (-3) (fromList [(5,"a"), (-3,"b")]) @?= singleton 5 "a"
    delete (-7) (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a")]

test_adjust :: Assertion
test_adjust = do
    adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "new a")]
    adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
    adjust ("new " ++) 7 empty                         @?= empty

    adjust ("new " ++) 5    (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "new a")]
    adjust ("new " ++) 7    (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a")]
    adjust ("new " ++) (-3) empty                          @?= empty
    adjust ("new " ++) (-3) (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "new b"), (5, "a")]
    adjust ("new " ++) (-7) (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a")]

test_adjustWithKey :: Assertion
test_adjustWithKey = do
    adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "5:new a")]
    adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
    adjustWithKey f 7 empty                         @?= empty

    adjustWithKey f 5    (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "5:new a")]
    adjustWithKey f 7    (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a")]
    adjustWithKey f (-3) empty                          @?= empty
    adjustWithKey f (-3) (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "-3:new b"), (5, "a")]
    adjustWithKey f (-7) (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a")]
  where
    f key x = (show key) ++ ":new " ++ x

test_update :: Assertion
test_update = do
    update f 5 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "new a")]
    update f 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
    update f 3 (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"

    update f 5    (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "new a")]
    update f 7    (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a")]
    update f (-3) (fromList [(5,"a"), (-3,"b")]) @?= singleton 5 "a"
    update f (-7) (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a")]
  where
    f x = if x == "a" then Just "new a" else Nothing

test_updateWithKey :: Assertion
test_updateWithKey = do
    updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "5:new a")]
    updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
    updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"

    updateWithKey f 5    (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "5:new a")]
    updateWithKey f 7    (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a")]
    updateWithKey f (-3) (fromList [(5,"a"), (-3,"b")]) @?= singleton 5 "a"
    updateWithKey f (-7) (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a")]
 where
     f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing

test_updateLookupWithKey :: Assertion
test_updateLookupWithKey = do
    updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) @?= (Just "a", fromList [(3, "b"), (5, "5:new a")])
    updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) @?= (Nothing,  fromList [(3, "b"), (5, "a")])
    updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) @?= (Just "b", singleton 5 "a")

    updateLookupWithKey f 5    (fromList [(5,"a"), (-3,"b")]) @?= (Just "a", fromList [(-3, "b"), (5, "5:new a")])
    updateLookupWithKey f 7    (fromList [(5,"a"), (-3,"b")]) @?= (Nothing,  fromList [(-3, "b"), (5, "a")])
    updateLookupWithKey f (-3) (fromList [(5,"a"), (-3,"b")]) @?= (Just "b", singleton 5 "a")
    updateLookupWithKey f (-7) (fromList [(5,"a"), (-3,"b")]) @?= (Nothing,  fromList [(-3, "b"), (5, "a")])
  where
    f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing

test_alter :: Assertion
test_alter = do
    alter f 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
    alter f 5 (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"
    alter g 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a"), (7, "c")]
    alter g 5 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "c")]

    alter f 7    (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a")]
    alter f 5    (fromList [(5,"a"), (-3,"b")]) @?= singleton (-3) "b"
    alter f (-7) (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a")]
    alter f (-3) (fromList [(5,"a"), (-3,"b")]) @?= singleton (5) "a"
    alter g 7    (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a"), (7, "c")]
    alter g 5    (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "c")]
    alter g (-7) (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "a"), (-7, "c")]
    alter g (-3) (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "c"), (5, "a")]
  where
    f _ = Nothing
    g _ = Just "c"

----------------------------------------------------------------
-- Combine

test_union :: Assertion
test_union = do
    union (fromList [(5, "a"), (3, "b")])  (fromList [(5, "A"), (7, "C")]) @?= fromList [(3, "b"), (5, "a"), (7, "C")]
    union (fromList [(5, "a"), (-3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= fromList [(-3, "b"), (5, "a"), (7, "C")]

test_mappend :: Assertion
test_mappend = do
    mappend (fromList [(5, "a"), (3, "b")])  (fromList [(5, "A"), (7, "C")]) @?= fromList [(3, "b"), (5, "a"), (7, "C")]
    mappend (fromList [(5, "a"), (-3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= fromList [(-3, "b"), (5, "a"), (7, "C")]

test_unionWith :: Assertion
test_unionWith = do
    unionWith (++) (fromList [(5, "a"), (3, "b")])  (fromList [(5, "A"), (7, "C")]) @?= fromList [(3, "b"), (5, "aA"), (7, "C")]
    unionWith (++) (fromList [(5, "a"), (-3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= fromList [(-3, "b"), (5, "aA"), (7, "C")]

test_unionWithKey :: Assertion
test_unionWithKey = do
    unionWithKey f (fromList [(5, "a"), (3, "b")])  (fromList [(5, "A"), (7, "C")]) @?= fromList [(3, "b"), (5, "5:a|A"), (7, "C")]
    unionWithKey f (fromList [(5, "a"), (-3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= fromList [(-3, "b"), (5, "5:a|A"), (7, "C")]
  where
    f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value

test_unions :: Assertion
test_unions = do
    unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
        @?= fromList [(3, "b"), (5, "a"), (7, "C")]
    unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
        @?= fromList [(3, "B3"), (5, "A3"), (7, "C")]

    unions [(fromList [(5, "a"), (-3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (-3, "B3")])]
        @?= fromList [(-3, "b"), (5, "a"), (7, "C")]
    unions [(fromList [(5, "A3"), (-3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (-3, "b")])]
        @?= fromList [(-3, "B3"), (5, "A3"), (7, "C")]

test_mconcat :: Assertion
test_mconcat = do
    mconcat [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
        @?= fromList [(3, "b"), (5, "a"), (7, "C")]
    mconcat [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
        @?= fromList [(3, "B3"), (5, "A3"), (7, "C")]

    mconcat [(fromList [(5, "a"), (-3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (-3, "B3")])]
        @?= fromList [(-3, "b"), (5, "a"), (7, "C")]
    mconcat [(fromList [(5, "A3"), (-3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (-3, "b")])]
        @?= fromList [(-3, "B3"), (5, "A3"), (7, "C")]

test_unionsWith :: Assertion
test_unionsWith = do
    unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
        @?= fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]
    unionsWith (++) [(fromList [(5, "a"), (-3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (-3, "B3")])]
        @?= fromList [(-3, "bB3"), (5, "aAA3"), (7, "C")]

test_difference :: Assertion
test_difference = do
    difference (fromList [(5, "a"), (3, "b")])  (fromList [(5, "A"), (7, "C")]) @?= singleton 3 "b"
    difference (fromList [(5, "a"), (-3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= singleton (-3) "b"

test_differenceWith :: Assertion
test_differenceWith = do
    differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
        @?= singleton 3 "b:B"
    differenceWith f (fromList [(5, "a"), (-3, "b")]) (fromList [(5, "A"), (-3, "B"), (7, "C")])
        @?= singleton (-3) "b:B"
 where
   f al ar = if al== "b" then Just (al ++ ":" ++ ar) else Nothing

test_differenceWithKey :: Assertion
test_differenceWithKey = do
    differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
        @?= singleton 3 "3:b|B"
    differenceWithKey f (fromList [(5, "a"), (-3, "b")]) (fromList [(5, "A"), (-3, "B"), (10, "C")])
        @?= singleton (-3) "-3:b|B"
  where
    f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing

test_intersection :: Assertion
test_intersection = do
    intersection (fromList [(5, "a"), (3, "b")])  (fromList [(5, "A"), (7, "C")]) @?= singleton 5 "a"
    intersection (fromList [(5, "a"), (-3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= singleton 5 "a"


test_intersectionWith :: Assertion
test_intersectionWith = do
    intersectionWith (++) (fromList [(5, "a"), (3, "b")])  (fromList [(5, "A"), (7, "C")]) @?= singleton 5 "aA"
    intersectionWith (++) (fromList [(5, "a"), (-3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= singleton 5 "aA"

test_intersectionWithKey :: Assertion
test_intersectionWithKey = do
    intersectionWithKey f (fromList [(5, "a"), (3, "b")])  (fromList [(5, "A"), (7, "C")]) @?= singleton 5 "5:a|A"
    intersectionWithKey f (fromList [(5, "a"), (-3, "b")]) (fromList [(5, "A"), (7, "C")]) @?= singleton 5 "5:a|A"
  where
    f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar

----------------------------------------------------------------
-- Traversal

test_map :: Assertion
test_map = do
    map (++ "x") (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "bx"), (5, "ax")]
    map (++ "x") (fromList [(5,"a"), (3,"b"), (-1,"c")])
            @?= fromList [(3, "bx"), (5, "ax"), (-1,"cx")]

test_mapWithKey :: Assertion
test_mapWithKey = do
    mapWithKey f (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "3:b"), (5, "5:a")]
    mapWithKey f (fromList [(5,"a"), (3,"b"), (-1,"c")])
            @?= fromList [(3, "3:b"), (5, "5:a"), (-1,"-1:c")]
  where
    f key x = (show key) ++ ":" ++ x

test_mapAccum :: Assertion
test_mapAccum = do
    mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) @?= ("Everything: ba", fromList [(3, "bX"), (5, "aX")])
    mapAccum f "Everything: " (fromList [(5,"a"), (3,"b"), (-1,"c")])
        @?= ("Everything: cba", fromList [(3, "bX"), (5, "aX"), (-1, "cX")])
  where
    f a b = (a ++ b, b ++ "X")

test_mapAccumWithKey :: Assertion
test_mapAccumWithKey = do
    mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) @?= ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])
    mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b"), (-1,"c")])
        @?= ("Everything: -1-c 3-b 5-a", fromList [(3, "bX"), (5, "aX"), (-1,"cX")])
  where
    f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")

test_mapAccumRWithKey :: Assertion
test_mapAccumRWithKey = do
    mapAccumRWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) @?= ("Everything: 5-a 3-b", fromList [(3, "bX"), (5, "aX")])
    mapAccumRWithKey f "Everything:" (fromList [(5,"a"), (3,"b"), (-1,"c")])
        @?= ("Everything: 5-a 3-b -1-c", fromList [(3, "bX"), (5, "aX"), (-1,"cX")])
  where
    f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")

test_mapKeys :: Assertion
test_mapKeys = do
    mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        @?= fromList [(4, "b"), (6, "a")]
    mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) @?= singleton 1 "c"
    mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) @?= singleton 3 "c"

    mapKeys (+ 1) (fromList [(5,"a"), (3,"b"), (-2,"c")])
            @?= fromList [(4, "b"), (6, "a"), (-1,"c")]
    mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (-4,"c")])
            @?= singleton 1 "d"
    mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (-3,"d"), (4,"c")])
            @?= singleton 3 "c"

test_mapKeysWith :: Assertion
test_mapKeysWith = do
    mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) @?= singleton 1 "cdab"
    mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) @?= singleton 3 "cdab"

    mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (-4,"c")]) @?= singleton 1 "dabc"
    mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (-2,"a"), (3,"d"), (4,"c")]) @?= singleton 3 "cdba"

test_mapKeysMonotonic :: Assertion
test_mapKeysMonotonic = do
    mapKeysMonotonic (+ 1) (fromList [(5,"a"), (3,"b")])          @?= fromList [(4, "b"), (6, "a")]
    mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) @?= fromList [(6, "b"), (10, "a")]

    mapKeysMonotonic (+ 1) (fromList [(5,"a"), (3,"b"), (-2,"c")])
        @?= fromList [(4, "b"), (6, "a"), (-1, "c")]
    mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b"), (-2,"c")])
        @?= fromList [(6, "b"), (10, "a"), (-4, "c")]

----------------------------------------------------------------
-- Conversion

test_elems :: Assertion
test_elems = do
    elems (fromList [(5,"a"), (3,"b")]) @?= ["b","a"]
    elems (fromList [(5,"a"), (-3,"b")]) @?= ["b","a"]
    elems (empty :: UMap) @?= []

test_keys :: Assertion
test_keys = do
    keys (fromList [(5,"a"), (3,"b")]) @?= [3,5]
    keys (fromList [(5,"a"), (-3,"b")]) @?= [-3,5]
    keys (empty :: UMap) @?= []

test_assocs :: Assertion
test_assocs = do
    assocs (fromList [(5,"a"), (3,"b")]) @?= [(3,"b"), (5,"a")]
    assocs (fromList [(5,"a"), (-3,"b")]) @?= [(-3,"b"), (5,"a")]
    assocs (empty :: UMap) @?= []

test_keysSet :: Assertion
test_keysSet = do
    keysSet (fromList [(5,"a"), (3,"b")]) @?= IntSet.fromList [3,5]
    keysSet (fromList [(5,"a"), (-3,"b")]) @?= IntSet.fromList [-3,5]
    keysSet (empty :: UMap) @?= IntSet.empty

test_fromSet :: Assertion
test_fromSet = do
   fromSet (\k -> replicate k 'a') (IntSet.fromList [3, 5]) @?= fromList [(5,"aaaaa"), (3,"aaa")]
   fromSet (\k -> replicate k 'a') (IntSet.fromList [-3, 2, 5]) @?= fromList [(5,"aaaaa"), (-3,""), (2,"aa")]
   fromSet undefined IntSet.empty @?= (empty :: IMap)

----------------------------------------------------------------
-- Lists

test_toList :: Assertion
test_toList = do
    toList (fromList [(5,"a"), (3,"b")]) @?= [(3,"b"), (5,"a")]
    toList (fromList [(5,"a"), (-3,"b")]) @?= [(-3,"b"), (5,"a")]
    toList (empty :: SMap) @?= []

test_fromList :: Assertion
test_fromList = do
    fromList [] @?= (empty :: SMap)
    fromList [(5,"a"), (3,"b"), (5, "c")] @?= fromList [(5,"c"), (3,"b")]
    fromList [(5,"c"), (3,"b"), (5, "a")] @?= fromList [(5,"a"), (3,"b")]

    fromList [(5,"a"), (-3,"b"), (5, "c")] @?= fromList [(5,"c"), (-3,"b")]
    fromList [(5,"c"), (-3,"b"), (5, "a")] @?= fromList [(5,"a"), (-3,"b")]

test_fromListWith :: Assertion
test_fromListWith = do
    fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] @?= fromList [(3, "ab"), (5, "aba")]
    fromListWith (++) [(5,"a"), (5,"b"), (-3,"b"), (-3,"a"), (5,"a")] @?= fromList [(-3, "ab"), (5, "aba")]
    fromListWith (++) [] @?= (empty :: SMap)

test_fromListWithKey :: Assertion
test_fromListWithKey = do
    fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] @?= fromList [(3, "3ab"), (5, "5a5ba")]
    fromListWithKey f [(5,"a"), (5,"b"), (-3,"b"), (-3,"a"), (5,"a")] @?= fromList [(-3, "-3ab"), (5, "5a5ba")]
    fromListWithKey f [] @?= (empty :: SMap)
  where
    f k a1 a2 = (show k) ++ a1 ++ a2

----------------------------------------------------------------
-- Ordered lists

test_toAscList :: Assertion
test_toAscList = do
    toAscList (fromList [(5,"a"), (3,"b")]) @?= [(3,"b"), (5,"a")]
    toAscList (fromList [(5,"a"), (-3,"b")]) @?= [(-3,"b"), (5,"a")]

test_toDescList :: Assertion
test_toDescList = do
    toDescList (fromList [(5,"a"), (3,"b")]) @?= [(5,"a"), (3,"b")]
    toDescList (fromList [(5,"a"), (-3,"b")]) @?= [(5,"a"), (-3,"b")]

test_showTree :: Assertion
test_showTree = do
    showTree posTree @?= expectedPosTree
    showTree negTree @?= expectedNegTree
  where mkAscTree ls = fromDistinctAscList [(x,()) | x <- ls]
        posTree = mkAscTree [1..5]
        negTree = mkAscTree [(-2)..2]
        expectedPosTree = unlines
            [ "*"
            , "+--*"
            , "|  +-- 1:=()"
            , "|  +--*"
            , "|     +-- 2:=()"
            , "|     +-- 3:=()"
            , "+--*"
            , "   +-- 4:=()"
            , "   +-- 5:=()"
            ]
        expectedNegTree = unlines
            [ "*"
            , "+--*"
            , "|  +--*"
            , "|  |  +-- 0:=()"
            , "|  |  +-- 1:=()"
            , "|  +-- 2:=()"
            , "+--*"
            , "   +-- -2:=()"
            , "   +-- -1:=()"
            ]

test_fromAscList :: Assertion
test_fromAscList = do
    fromAscList [(3,"b"), (5,"a")]          @?= fromList [(3, "b"), (5, "a")]
    fromAscList [(3,"b"), (5,"a"), (5,"b")] @?= fromList [(3, "b"), (5, "b")]

    fromAscList [(-3,"b"), (5,"a")]          @?= fromList [(-3, "b"), (5, "a")]
    fromAscList [(-3,"b"), (5,"a"), (5,"b")] @?= fromList [(-3, "b"), (5, "b")]


test_fromAscListWith :: Assertion
test_fromAscListWith = do
    fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] @?= fromList [(3, "b"), (5, "ba")]
    fromAscListWith (++) [(-3,"b"), (5,"a"), (5,"b")] @?= fromList [(-3, "b"), (5, "ba")]

test_fromAscListWithKey :: Assertion
test_fromAscListWithKey = do
    fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")] @?= fromList [(3, "b"), (5, "5:b5:ba")]
    fromAscListWithKey f [(-3,"b"), (5,"a"), (5,"b"), (5,"b")] @?= fromList [(-3, "b"), (5, "5:b5:ba")]
  where
    f k a1 a2 = (show k) ++ ":" ++ a1 ++ a2

test_fromDistinctAscList :: Assertion
test_fromDistinctAscList = do
    fromDistinctAscList [(3,"b"), (5,"a")] @?= fromList [(3, "b"), (5, "a")]
    fromDistinctAscList [(-3,"b"), (5,"a")] @?= fromList [(-3, "b"), (5, "a")]

----------------------------------------------------------------
-- Filter

test_filter :: Assertion
test_filter = do
    filter (> "a") (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"
    filter (> "x") (fromList [(5,"a"), (3,"b")]) @?= empty
    filter (< "a") (fromList [(5,"a"), (3,"b")]) @?= empty

    filter (> "a") (fromList [(5,"a"), (-3,"b")]) @?= singleton (-3) "b"
    filter (> "x") (fromList [(5,"a"), (-3,"b")]) @?= empty
    filter (< "a") (fromList [(5,"a"), (-3,"b")]) @?= empty

test_filterKeys :: Assertion
test_filterKeys = do
    filterKeys (> 4) (fromList [(5,"a"), (3,"b")])  @?= singleton 5 "a"
    filterKeys (> 4) (fromList [(5,"a"), (-3,"b")]) @?= singleton 5 "a"

test_filterWithKey :: Assertion
test_filterWithKey = do
    filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")])  @?= singleton 5 "a"
    filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (-3,"b")]) @?= singleton 5 "a"

test_partition :: Assertion
test_partition = do
    partition (> "a") (fromList [(5,"a"), (3,"b")]) @?= (singleton 3 "b", singleton 5 "a")
    partition (< "x") (fromList [(5,"a"), (3,"b")]) @?= (fromList [(3, "b"), (5, "a")], empty)
    partition (> "x") (fromList [(5,"a"), (3,"b")]) @?= (empty, fromList [(3, "b"), (5, "a")])

    partition (> "a") (fromList [(5,"a"), (-3,"b")]) @?= (singleton (-3) "b", singleton 5 "a")
    partition (< "x") (fromList [(5,"a"), (-3,"b")]) @?= (fromList [(-3, "b"), (5, "a")], empty)
    partition (> "x") (fromList [(5,"a"), (-3,"b")]) @?= (empty, fromList [(-3, "b"), (5, "a")])

test_partitionWithKey :: Assertion
test_partitionWithKey = do
    partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) @?= (singleton 5 "a", singleton 3 "b")
    partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) @?= (fromList [(3, "b"), (5, "a")], empty)
    partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) @?= (empty, fromList [(3, "b"), (5, "a")])

    partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (-3,"b")]) @?= (singleton 5 "a", singleton (-3) "b")
    partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (-3,"b")]) @?= (fromList [(-3, "b"), (5, "a")], empty)
    partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (-3,"b")]) @?= (empty, fromList [(-3, "b"), (5, "a")])

test_mapMaybe :: Assertion
test_mapMaybe = do
    mapMaybe f (fromList [(5,"a"), (3,"b")])  @?= singleton 5 "new a"
    mapMaybe f (fromList [(5,"a"), (-3,"b")]) @?= singleton 5 "new a"
  where
    f x = if x == "a" then Just "new a" else Nothing

test_mapMaybeWithKey :: Assertion
test_mapMaybeWithKey = do
    mapMaybeWithKey f (fromList [(5,"a"), (3,"b")])  @?= singleton 3 "key : 3"
    mapMaybeWithKey f (fromList [(5,"a"), (-3,"b")]) @?= singleton (-3) "key : -3"
  where
    f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing

test_mapEither :: Assertion
test_mapEither = do
    mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
        @?= (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
    mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
        @?= ((empty :: SMap), fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])

    mapEither f (fromList [(5,"a"), (-3,"b"), (1,"x"), (7,"z")])
        @?= (fromList [(-3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
    mapEither (\ a -> Right a) (fromList [(5,"a"), (-3,"b"), (1,"x"), (7,"z")])
        @?= ((empty :: SMap), fromList [(5,"a"), (-3,"b"), (1,"x"), (7,"z")])
 where
   f a = if a < "c" then Left a else Right a

test_mapEitherWithKey :: Assertion
test_mapEitherWithKey = do
    mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
     @?= (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
    mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
     @?= ((empty :: SMap), fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])

    mapEitherWithKey f (fromList [(5,"a"), (-3,"b"), (1,"x"), (7,"z")])
     @?= (fromList [(1,2), (-3,-6)], fromList [(5,"aa"), (7,"zz")])
    mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (-3,"b"), (1,"x"), (7,"z")])
     @?= ((empty :: SMap), fromList [(1,"x"), (-3,"b"), (5,"a"), (7,"z")])
  where
    f k a = if k < 5 then Left (k * 2) else Right (a ++ a)

test_split :: Assertion
test_split = do
    split 2 (fromList [(5,"a"), (3,"b")]) @?= (empty, fromList [(3,"b"), (5,"a")])
    split 3 (fromList [(5,"a"), (3,"b")]) @?= (empty, singleton 5 "a")
    split 4 (fromList [(5,"a"), (3,"b")]) @?= (singleton 3 "b", singleton 5 "a")
    split 5 (fromList [(5,"a"), (3,"b")]) @?= (singleton 3 "b", empty)
    split 6 (fromList [(5,"a"), (3,"b")]) @?= (fromList [(3,"b"), (5,"a")], empty)

    split (-4) (fromList [(5,"a"), (-3,"b")]) @?= (empty, fromList [(-3,"b"), (5,"a")])
    split (-3) (fromList [(5,"a"), (-3,"b")]) @?= (empty, singleton 5 "a")
    split 4 (fromList [(5,"a"), (-3,"b")]) @?= (singleton (-3) "b", singleton 5 "a")
    split 5 (fromList [(5,"a"), (-3,"b")]) @?= (singleton (-3) "b", empty)
    split 6 (fromList [(5,"a"), (-3,"b")]) @?= (fromList [(-3,"b"), (5,"a")], empty)

test_splitLookup :: Assertion
test_splitLookup = do
    splitLookup 2 (fromList [(5,"a"), (3,"b")]) @?= (empty, Nothing, fromList [(3,"b"), (5,"a")])
    splitLookup 3 (fromList [(5,"a"), (3,"b")]) @?= (empty, Just "b", singleton 5 "a")
    splitLookup 4 (fromList [(5,"a"), (3,"b")]) @?= (singleton 3 "b", Nothing, singleton 5 "a")
    splitLookup 5 (fromList [(5,"a"), (3,"b")]) @?= (singleton 3 "b", Just "a", empty)
    splitLookup 6 (fromList [(5,"a"), (3,"b")]) @?= (fromList [(3,"b"), (5,"a")], Nothing, empty)

    splitLookup (-4) (fromList [(5,"a"), (-3,"b")]) @?= (empty, Nothing, fromList [(-3,"b"), (5,"a")])
    splitLookup (-3) (fromList [(5,"a"), (-3,"b")]) @?= (empty, Just "b", singleton 5 "a")
    splitLookup 4 (fromList [(5,"a"), (-3,"b")]) @?= (singleton (-3) "b", Nothing, singleton 5 "a")
    splitLookup 5 (fromList [(5,"a"), (-3,"b")]) @?= (singleton (-3) "b", Just "a", empty)
    splitLookup 6 (fromList [(5,"a"), (-3,"b")]) @?= (fromList [(-3,"b"), (5,"a")], Nothing, empty)

----------------------------------------------------------------
-- Submap

test_isSubmapOfBy :: Assertion
test_isSubmapOfBy = do
    isSubmapOfBy (==) (fromList [(fromEnum 'a',1)]) (fromList [(fromEnum 'a',1),(fromEnum 'b',2)]) @?= True
    isSubmapOfBy (<=) (fromList [(fromEnum 'a',1)]) (fromList [(fromEnum 'a',1),(fromEnum 'b',2)]) @?= True
    isSubmapOfBy (==) (fromList [(fromEnum 'a',1),(fromEnum 'b',2)]) (fromList [(fromEnum 'a',1),(fromEnum 'b',2)]) @?= True
    isSubmapOfBy (==) (fromList [(fromEnum 'a',2)]) (fromList [(fromEnum 'a',1),(fromEnum 'b',2)]) @?= False
    isSubmapOfBy (<)  (fromList [(fromEnum 'a',1)]) (fromList [(fromEnum 'a',1),(fromEnum 'b',2)]) @?= False
    isSubmapOfBy (==) (fromList [(fromEnum 'a',1),(fromEnum 'b',2)]) (fromList [(fromEnum 'a',1)]) @?= False

    isSubmapOfBy (==) (fromList [(-1,1)]) (fromList [(-1,1),(2,2)]) @?= True
    isSubmapOfBy (<=) (fromList [(-1,1)]) (fromList [(-1,1),(2,2)]) @?= True
    isSubmapOfBy (==) (fromList [(-1,1),(2,2)]) (fromList [(-1,1),(2,2)]) @?= True
    isSubmapOfBy (==) (fromList [(-1,2)]) (fromList [(-1,1),(2,2)]) @?= False
    isSubmapOfBy (>)  (fromList [(-1,2)]) (fromList [(-1,1),(2,2)]) @?= True
    isSubmapOfBy (<)  (fromList [(-1,1)]) (fromList [(-1,1),(2,2)]) @?= False
    isSubmapOfBy (==) (fromList [(-1,1),(2,2)]) (fromList [(-1,1)]) @?= False

test_isSubmapOf :: Assertion
test_isSubmapOf = do
    isSubmapOf (fromList [(fromEnum 'a',1)]) (fromList [(fromEnum 'a',1),(fromEnum 'b',2)]) @?= True
    isSubmapOf (fromList [(fromEnum 'a',1),(fromEnum 'b',2)]) (fromList [(fromEnum 'a',1),(fromEnum 'b',2)]) @?= True
    isSubmapOf (fromList [(fromEnum 'a',2)]) (fromList [(fromEnum 'a',1),(fromEnum 'b',2)]) @?= False
    isSubmapOf (fromList [(fromEnum 'a',1),(fromEnum 'b',2)]) (fromList [(fromEnum 'a',1)]) @?= False

    isSubmapOf (fromList [(-1,1)]) (fromList [(-1,1),(2,2)]) @?= True
    isSubmapOf (fromList [(-1,1),(2,2)]) (fromList [(-1,1),(2,2)]) @?= True
    isSubmapOf (fromList [(-1,2)]) (fromList [(-1,1),(2,2)]) @?= False
    isSubmapOf (fromList [(-1,1),(2,2)]) (fromList [(-1,1)]) @?= False

test_isProperSubmapOfBy :: Assertion
test_isProperSubmapOfBy = do
    isProperSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)]) @?= True
    isProperSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)]) @?= True
    isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)]) @?= False
    isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)]) @?= False
    isProperSubmapOfBy (<)  (fromList [(1,1)])       (fromList [(1,1),(2,2)]) @?= False

    isProperSubmapOfBy (==) (fromList [(-1,1)]) (fromList [(-1,1),(2,2)]) @?= True
    isProperSubmapOfBy (<=) (fromList [(-1,1)]) (fromList [(-1,1),(2,2)]) @?= True
    isProperSubmapOfBy (==) (fromList [(-1,1),(2,2)]) (fromList [(-1,1),(2,2)]) @?= False
    isProperSubmapOfBy (==) (fromList [(-1,1),(2,2)]) (fromList [(-1,1)]) @?= False
    isProperSubmapOfBy (<)  (fromList [(-1,1)])       (fromList [(-1,1),(2,2)]) @?= False

    -- See Github #1007
    isProperSubmapOfBy (==) (fromList [(-3,1),(-1,1)]) (fromList [(-3,1),(-1,1),(0,1)]) @?= True

test_isProperSubmapOf :: Assertion
test_isProperSubmapOf = do
    isProperSubmapOf (fromList [(1,1)]) (fromList [(1,1),(2,2)]) @?= True
    isProperSubmapOf (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)]) @?= False
    isProperSubmapOf (fromList [(1,1),(2,2)]) (fromList [(1,1)]) @?= False

    isProperSubmapOf (fromList [(-1,1)]) (fromList [(-1,1),(2,2)]) @?= True
    isProperSubmapOf (fromList [(-1,1),(2,2)]) (fromList [(-1,1),(2,2)]) @?= False
    isProperSubmapOf (fromList [(-1,1),(2,2)]) (fromList [(-1,1)]) @?= False

    -- See Github #1007
    isProperSubmapOf (fromList [(-3,1),(-1,1)]) (fromList [(-3,1),(-1,1),(0,1)]) @?= True

----------------------------------------------------------------
-- Min/Max

test_lookupMin :: Assertion
test_lookupMin = do
  lookupMin (fromList [(5,"a"), (3,"b")])  @?= Just (3,"b")
  lookupMin (fromList [(5,"a"), (-3,"b")]) @?= Just (-3,"b")
  lookupMin (empty :: SMap) @?= Nothing

test_lookupMax :: Assertion
test_lookupMax = do
  lookupMax (fromList [(5,"a"), (3,"b")]) @?= Just (5,"a")
  lookupMax (fromList [(5,"a"), (-3,"b")]) @?= Just (5,"a")
  lookupMax (empty :: SMap) @?= Nothing

test_findMin :: Assertion
test_findMin = do
    findMin (fromList [(5,"a"), (3,"b")]) @?= (3,"b")
    findMin (fromList [(5,"a"), (-3,"b")]) @?= (-3,"b")

test_findMax :: Assertion
test_findMax = do
    findMax (fromList [(5,"a"), (3,"b")]) @?= (5,"a")
    findMax (fromList [(5,"a"), (-3,"b")]) @?= (5,"a")

test_deleteMin :: Assertion
test_deleteMin = do
    deleteMin (fromList [(5,"a"), (3,"b"), (7,"c")]) @?= fromList [(5,"a"), (7,"c")]
    deleteMin (fromList [(5,"a"), (-3,"b"), (7,"c")]) @?= fromList [(5,"a"), (7,"c")]
    deleteMin (empty :: SMap) @?= empty

test_deleteMax :: Assertion
test_deleteMax = do
    deleteMax (fromList [(5,"a"), (3,"b"), (7,"c")]) @?= fromList [(3,"b"), (5,"a")]
    deleteMax (fromList [(5,"a"), (-3,"b"), (7,"c")]) @?= fromList [(-3,"b"), (5,"a")]
    deleteMax (empty :: SMap) @?= empty

test_deleteFindMin :: Assertion
test_deleteFindMin = do
    deleteFindMin (fromList [(5,"a"), (3,"b"), (10,"c")]) @?= ((3,"b"), fromList[(5,"a"), (10,"c")])
    deleteFindMin (fromList [(5,"a"), (-3,"b"), (10,"c")]) @?= ((-3,"b"), fromList[(5,"a"), (10,"c")])

test_deleteFindMax :: Assertion
test_deleteFindMax = do
    deleteFindMax (fromList [(5,"a"), (3,"b"), (10,"c")]) @?= ((10,"c"), fromList [(3,"b"), (5,"a")])
    deleteFindMax (fromList [(5,"a"), (-3,"b"), (10,"c")]) @?= ((10,"c"), fromList [(-3,"b"), (5,"a")])

test_updateMin :: Assertion
test_updateMin = do
    updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "Xb"), (5, "a")]
    updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"

    updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "Xb"), (5, "a")]
    updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (-3,"b")]) @?= singleton 5 "a"

    updateMin (\ a -> Just ("X" ++ a)) (empty :: SMap) @?= empty
    updateMin (\ _ -> Nothing)         (empty :: SMap) @?= empty

test_updateMax :: Assertion
test_updateMax = do
    updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "Xa")]
    updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"

    updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3, "b"), (5, "Xa")]
    updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (-3,"b")]) @?= singleton (-3) "b"

    updateMax (\ a -> Just ("X" ++ a)) (empty :: SMap) @?= empty
    updateMax (\ _ -> Nothing)         (empty :: SMap) @?= empty

test_updateMinWithKey :: Assertion
test_updateMinWithKey = do
    updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) @?= fromList [(3,"3:b"), (5,"a")]
    updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"

    updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3,"-3:b"), (5,"a")]
    updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (-3,"b")]) @?= singleton 5 "a"

    updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (empty :: SMap) @?= empty
    updateMinWithKey (\ _ _ -> Nothing)                     (empty :: SMap) @?= empty

test_updateMaxWithKey :: Assertion
test_updateMaxWithKey = do
    updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) @?= fromList [(3,"b"), (5,"5:a")]
    updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"

    updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (-3,"b")]) @?= fromList [(-3,"b"), (5,"5:a")]
    updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (-3,"b")]) @?= singleton (-3) "b"

    updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (empty :: SMap) @?= empty
    updateMaxWithKey (\ _ _ -> Nothing)                     (empty :: SMap) @?= empty

test_minView :: Assertion
test_minView = do
    minView (fromList [(5,"a"), (3,"b")]) @?= Just ("b", singleton 5 "a")
    minView (fromList [(5,"a"), (-3,"b")]) @?= Just ("b", singleton 5 "a")
    minView (empty :: SMap) @?= Nothing

test_maxView :: Assertion
test_maxView = do
    maxView (fromList [(5,"a"), (3,"b")]) @?= Just ("a", singleton 3 "b")
    maxView (fromList [(5,"a"), (-3,"b")]) @?= Just ("a", singleton (-3) "b")
    maxView (empty :: SMap) @?= Nothing

test_minViewWithKey :: Assertion
test_minViewWithKey = do
    minViewWithKey (fromList [(5,"a"), (3,"b")]) @?= Just ((3,"b"), singleton 5 "a")
    minViewWithKey (fromList [(5,"a"), (-3,"b")]) @?= Just ((-3,"b"), singleton 5 "a")
    minViewWithKey (empty :: SMap) @?= Nothing

test_maxViewWithKey :: Assertion
test_maxViewWithKey = do
    maxViewWithKey (fromList [(5,"a"), (3,"b")]) @?= Just ((5,"a"), singleton 3 "b")
    maxViewWithKey (fromList [(5,"a"), (-3,"b")]) @?= Just ((5,"a"), singleton (-3) "b")
    maxViewWithKey (empty :: SMap) @?= Nothing

test_minimum :: Assertion
test_minimum = do
    getOW (minimum testOrdMap) @?= "min"
    minimum (elems testOrdMap) @?= minimum testOrdMap
  where getOW (OrdWith s _) = s

test_maximum :: Assertion
test_maximum = do
    getOW (maximum testOrdMap) @?= "max"
    maximum (elems testOrdMap) @?= maximum testOrdMap
  where getOW (OrdWith s _) = s

-- Check cases where there is a risk of overflow
test_compareSize :: Assertion
test_compareSize = do
  compareSize (fromList [(1,'a')]) minBound @?= GT
  compareSize (fromList [(1,'a')]) maxBound @?= LT

testOrdMap :: IntMap (OrdWith Int)
testOrdMap = fromList [(1,OrdWith "max" 1),(-1,OrdWith "min" 1)]

data OrdWith a = OrdWith String a
    deriving (Eq, Show)

instance Ord a => Ord (OrdWith a) where
    OrdWith _ a1 <= OrdWith _ a2 = a1 <= a2

----------------------------------------------------------------
-- Valid IntMaps
----------------------------------------------------------------

forValid :: Testable b => (SMap -> b) -> Property
forValid f = forAll arbitrary $ \t ->
    classify (size t == 0) "empty" $
    classify (size t > 0 && size t <= 10) "small" $
    classify (size t > 10 && size t <= 64) "medium" $
    classify (size t > 64) "large" $ f t

forValidUnitTree :: Testable b => (SMap -> b) -> Property
forValidUnitTree f = forValid f

prop_valid :: Property
prop_valid = forValidUnitTree $ \t -> valid t

prop_hasPrefix :: Int -> NonZero Int -> Property
prop_hasPrefix i (NonZero p) =
  hasPrefix i (Prefix p) === hasPrefixSimple i (Prefix p)

----------------------------------------------------------------
-- QuickCheck
----------------------------------------------------------------

prop_emptyValid :: Property
prop_emptyValid = valid empty

prop_singleton :: Int -> Int -> Property
prop_singleton k x =
  case singleton k x of
    s ->
      valid s .&&.
      s === insert k x empty

prop_insertLookup :: Int -> UMap -> Bool
prop_insertLookup k t = lookup k (insert k () t) /= Nothing

prop_insertDelete :: Int -> UMap -> Property
prop_insertDelete k t =
  lookup k t == Nothing ==>
    case delete k (insert k () t) of
      t' -> valid t' .&&. t' === t

prop_deleteNonMember :: Int -> UMap -> Property
prop_deleteNonMember k t = (lookup k t == Nothing) ==> (delete k t == t)

----------------------------------------------------------------

prop_unionModel :: [(Int,Int)] -> [(Int,Int)] -> Property
prop_unionModel xs ys =
  case union (fromList xs) (fromList ys) of
    t ->
      valid t .&&.
      sort (keys t) === sort (nub (Prelude.map fst xs ++ Prelude.map fst ys))

prop_unionSingleton :: IMap -> Int -> Int -> Bool
prop_unionSingleton t k x = union (singleton k x) t == insert k x t

prop_unionAssoc :: IMap -> IMap -> IMap -> Bool
prop_unionAssoc t1 t2 t3 = union t1 (union t2 t3) == union (union t1 t2) t3

prop_unionWith :: IMap -> IMap -> Bool
prop_unionWith t1 t2 = (union t1 t2 == unionWith (\_ y -> y) t2 t1)

prop_unionSum :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_unionSum xs ys
  = sum (elems (unionWith (+) (fromListWith (+) xs) (fromListWith (+) ys)))
    == (sum (Prelude.map snd xs) + sum (Prelude.map snd ys))

prop_differenceModel :: [(Int,Int)] -> [(Int,Int)] -> Property
prop_differenceModel xs ys =
  case difference (fromListWith (+) xs) (fromListWith (+) ys) of
    t ->
      valid t .&&.
      sort (keys t) === sort ((List.\\)
                                 (nub (Prelude.map fst xs))
                                 (nub (Prelude.map fst ys)))

prop_intersectionModel :: [(Int,Int)] -> [(Int,Int)] -> Property
prop_intersectionModel xs ys =
  case intersection (fromListWith (+) xs) (fromListWith (+) ys) of
    t ->
      valid t .&&.
      sort (keys t) === sort (nub ((List.intersect)
                                      (Prelude.map fst xs)
                                      (Prelude.map fst ys)))

prop_intersectionWithModel :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_intersectionWithModel xs ys
  = toList (intersectionWith f (fromList xs') (fromList ys'))
    == [(kx, f vx vy ) | (kx, vx) <- List.sort xs', (ky, vy) <- ys', kx == ky]
    where xs' = List.nubBy ((==) `on` fst) xs
          ys' = List.nubBy ((==) `on` fst) ys
          f l r = l + 2 * r

prop_intersectionWithKeyModel :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_intersectionWithKeyModel xs ys
  = toList (intersectionWithKey f (fromList xs') (fromList ys'))
    == [(kx, f kx vx vy) | (kx, vx) <- List.sort xs', (ky, vy) <- ys', kx == ky]
    where xs' = List.nubBy ((==) `on` fst) xs
          ys' = List.nubBy ((==) `on` fst) ys
          f k l r = k + 2 * l + 3 * r

prop_symmetricDifference :: IMap -> IMap -> Property
prop_symmetricDifference m1 m2 =
  valid m3 .&&.
  toAscList m3 ===
  List.sort (   List.filter ((`notElem` fmap fst kys) . fst) kxs
             ++ List.filter ((`notElem` fmap fst kxs) . fst) kys)
  where
    m3 = symmetricDifference m1 m2
    kxs = toAscList m1
    kys = toAscList m2

prop_disjoint :: UMap -> UMap -> Property
prop_disjoint m1 m2 = disjoint m1 m2 === null (intersection m1 m2)

prop_compose :: IMap -> IMap -> Int -> Property
prop_compose bc ab k = (compose bc ab !? k) === ((bc !?) <=< (ab !?)) k

-- TODO: the second argument should be simply an 'IntSet', but that
-- runs afoul of our orphan instance.
prop_restrictKeys :: IMap -> IMap -> Property
prop_restrictKeys m s0 =
    m `restrictKeys` s === filterWithKey (\k _ -> k `IntSet.member` s) m
  where
    s = keysSet s0

-- TODO: the second argument should be simply an 'IntSet', but that
-- runs afoul of our orphan instance.
prop_withoutKeys :: IMap -> IMap -> Property
prop_withoutKeys m s0 =
    m `withoutKeys` s === filterWithKey (\k _ -> k `IntSet.notMember` s) m
  where
    s = keysSet s0

prop_mergeWithKeyModel :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_mergeWithKeyModel xs ys
  = and [ testMergeWithKey f keep_x keep_y
        | f <- [ \_k x1  _x2 -> Just x1
               , \_k _x1 x2  -> Just x2
               , \_k _x1 _x2 -> Nothing
               , \k  x1  x2  -> if k `mod` 2 == 0 then Nothing else Just (2 * x1 + 3 * x2)
               ]
        , keep_x <- [ True, False ]
        , keep_y <- [ True, False ]
        ]

    where xs' = List.nubBy ((==) `on` fst) xs
          ys' = List.nubBy ((==) `on` fst) ys

          xm = fromList xs'
          ym = fromList ys'

          testMergeWithKey f keep_x keep_y
            = toList (mergeWithKey f (keep keep_x) (keep keep_y) xm ym) == emulateMergeWithKey f keep_x keep_y
              where keep False _ = empty
                    keep True  m = m

                    emulateMergeWithKey f keep_x keep_y
                      = Maybe.mapMaybe combine (sort $ List.union (List.map fst xs') (List.map fst ys'))
                        where combine k = case (List.lookup k xs', List.lookup k ys') of
                                            (Nothing, Just y) -> if keep_y then Just (k, y) else Nothing
                                            (Just x, Nothing) -> if keep_x then Just (k, x) else Nothing
                                            (Just x, Just y) -> (\v -> (k, v)) `fmap` f k x y

          -- We prevent inlining testMergeWithKey to disable the SpecConstr
          -- optimalization. There are too many call patterns here so several
          -- warnings are issued if testMergeWithKey gets inlined.
          {-# NOINLINE testMergeWithKey #-}

prop_merge_valid
    :: Fun (Key, A) (Maybe C)
    -> Fun (Key, B) (Maybe C)
    -> Fun (Key, A, B) (Maybe C)
    -> IntMap A
    -> IntMap B
    -> Property
prop_merge_valid whenMissingA whenMissingB whenMatched xs ys
  = valid m
  where
    m =
      merge
        (mapMaybeMissing (applyFun2 whenMissingA))
        (mapMaybeMissing (applyFun2 whenMissingB))
        (zipWithMaybeMatched (applyFun3 whenMatched))
        xs
        ys

-- This uses the instance
--     Monoid a => Applicative ((,) a)
-- to test that effects are sequenced in ascending key order.
prop_mergeA_effects :: UMap -> UMap -> Property
prop_mergeA_effects xs ys
  = effects === sort effects
  where
    (effects, _m) = mergeA whenMissing whenMissing whenMatched xs ys
    whenMissing = traverseMissing (\k _ -> ([k], ()))
    whenMatched = zipWithAMatched (\k _ _ -> ([k], ()))

----------------------------------------------------------------

prop_list :: [Int] -> Bool
prop_list xs = (sort (nub xs) == [x | (x,()) <- toList (fromList [(x,()) | x <- xs])])

prop_descList :: [Int] -> Bool
prop_descList xs = (reverse (sort (nub xs)) == [x | (x,()) <- toDescList (fromList [(x,()) | x <- xs])])

prop_ascDescList :: [Int] -> Bool
prop_ascDescList xs = toAscList m == reverse (toDescList m)
  where m = fromList $ zip xs $ repeat ()

prop_fromList :: [(Int, A)] -> Property
prop_fromList xs
  = case fromList xs of
      t -> valid t .&&.
           t === fromAscList sort_xs .&&.
           t === fromDistinctAscList nub_sort_xs .&&.
           t === List.foldl' (\t' (k,x) -> insert k x t') empty xs
  where sort_xs = List.sortBy (comparing fst) xs
        nub_sort_xs = List.map NE.last $ NE.groupBy ((==) `on` fst) sort_xs

----------------------------------------------------------------

prop_alter :: UMap -> Int -> Property
prop_alter t k = valid t' .&&. case lookup k t of
    Just _  -> (size t - 1) == size t' && lookup k t' == Nothing
    Nothing -> (size t + 1) == size t' && lookup k t' /= Nothing
  where
    t' = alter f k t
    f Nothing   = Just ()
    f (Just ()) = Nothing

------------------------------------------------------------------------
-- Compare against the list model (after nub on keys)

prop_index :: [Int] -> Property
prop_index xs = length xs > 0 ==>
  let m  = fromList (zip xs xs)
  in  xs == [ m ! i | i <- xs ]

prop_index_lookup :: [Int] -> Property
prop_index_lookup xs = length xs > 0 ==>
  let m  = fromList (zip xs xs)
  in  (Prelude.map Just xs) == [ m !? i | i <- xs ]

prop_null :: IMap -> Bool
prop_null m = null m == (size m == 0)

prop_size :: UMap -> Property
prop_size im = sz === foldl' (\i _ -> i + 1) (0 :: Int) im .&&.
               sz === List.length (toList im)
  where sz = size im

prop_member :: [Int] -> Int -> Bool
prop_member xs n =
  let m  = fromList (zip xs xs)
  in all (\k -> k `member` m == (k `elem` xs)) (n : xs)

prop_notmember :: [Int] -> Int -> Bool
prop_notmember xs n =
  let m  = fromList (zip xs xs)
  in all (\k -> k `notMember` m == (k `notElem` xs)) (n : xs)

prop_lookup :: [(Int, Int)] -> Int -> Bool
prop_lookup xs n =
  let xs' = List.nubBy ((==) `on` fst) xs
      m = fromList xs'
  in all (\k -> lookup k m == List.lookup k xs') (n : List.map fst xs')

prop_find :: [(Int, Int)] -> Bool
prop_find xs =
  let xs' = List.nubBy ((==) `on` fst) xs
      m = fromList xs'
  in all (\(k, v) -> m ! k == v) xs'

prop_findWithDefault :: [(Int, Int)] -> Int -> Int -> Bool
prop_findWithDefault xs n x =
  let xs' = List.nubBy ((==) `on` fst) xs
      m = fromList xs'
  in all (\k -> findWithDefault x k m == maybe x id (List.lookup k xs')) (n : List.map fst xs')

prop_lookupLT :: Int -> IntMap A -> Bool
prop_lookupLT k m = lookupLT k m == List.find ((<k) . fst) (toDescList m)

prop_lookupGT :: Int -> IntMap A -> Bool
prop_lookupGT k m = lookupGT k m == List.find ((>k) . fst) (toList m)

prop_lookupLE :: Int -> IntMap A -> Bool
prop_lookupLE k m = lookupLE k m == List.find ((<=k) . fst) (toDescList m)

prop_lookupGE :: Int -> IntMap A -> Bool
prop_lookupGE k m = lookupGE k m == List.find ((>=k) . fst) (toList m)

prop_lookupMin :: IntMap Int -> Property
prop_lookupMin im = lookupMin im === listToMaybe (toAscList im)

prop_lookupMax :: IntMap Int -> Property
prop_lookupMax im = lookupMax im === listToMaybe (toDescList im)

prop_findMin :: NonEmptyIntMap Int -> Property
prop_findMin (NonEmptyIntMap im) = findMin im === head (toAscList im)

prop_findMax :: NonEmptyIntMap Int -> Property
prop_findMax (NonEmptyIntMap im) = findMax im === head (toDescList im)

prop_deleteMinModel :: [(Int, Int)] -> Property
prop_deleteMinModel ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      m  = fromList xs
  in  toAscList (deleteMin m) == tail (sort xs)

prop_deleteMaxModel :: [(Int, Int)] -> Property
prop_deleteMaxModel ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      m  = fromList xs
  in  toAscList (deleteMax m) == init (sort xs)

prop_filter :: Fun Int Bool -> [(Int, Int)] -> Property
prop_filter p ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      m  = filter (apply p) (fromList xs)
  in  valid m .&&.
      m === fromList (List.filter (apply p . snd) xs)

prop_filterKeys :: Fun Int Bool -> IMap -> Property
prop_filterKeys fun m =
  valid m' .&&. toList m' === Prelude.filter (apply fun . fst) (toList m)
  where
    m' = filterKeys (apply fun) m

prop_filterWithKey :: Fun (Int, Int) Bool -> IMap -> Property
prop_filterWithKey fun m =
  valid m' .&&. toList m' === Prelude.filter (apply fun) (toList m)
  where
    m' = filterWithKey (applyFun2 fun) m

prop_partition :: Fun Int Bool -> [(Int, Int)] -> Property
prop_partition p ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      m@(l, r) = partition (apply p) (fromList xs)
  in  valid l .&&.
      valid r .&&.
      m === let (a,b) = (List.partition (apply p . snd) xs)
            in (fromList a, fromList b)

prop_takeWhileAntitone :: Int -> [(Int, Int)] -> Property
prop_takeWhileAntitone x ys =
  let l = takeWhileAntitone (<x) (fromList ys)
  in  valid l .&&.
      l === fromList (List.filter ((<x) . fst) ys)

prop_dropWhileAntitone :: Int -> [(Int, Int)] -> Property
prop_dropWhileAntitone x ys =
  let r = dropWhileAntitone (<x) (fromList ys)
  in  valid r .&&.
      r === fromList (List.filter ((>=x) . fst) ys)

prop_spanAntitone :: Int -> [(Int, Int)] -> Property
prop_spanAntitone x ys =
  let (l, r) = spanAntitone (<x) (fromList ys)
  in  valid l .&&.
      valid r .&&.
      l === fromList (List.filter ((<x) . fst) ys) .&&.
      r === fromList (List.filter ((>=x) . fst) ys)

prop_map :: Fun Int Int -> [(Int, Int)] -> Property
prop_map f ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      m  = fromList xs
  in  map (apply f) m == fromList [ (a, apply f b) | (a,b) <- xs ]

prop_fmap :: Fun Int Int -> [(Int, Int)] -> Property
prop_fmap f ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      m  = fromList xs
  in  fmap (apply f) m == fromList [ (a, apply f b) | (a,b) <- xs ]

prop_mapkeys :: Fun Int Int -> [(Int, Int)] -> Property
prop_mapkeys f ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      m  = fromList xs
  in  mapKeys (apply f) m == (fromList $ List.nubBy ((==) `on` fst) $ reverse [ (apply f a, b) | (a,b) <- sort xs])

prop_splitModel :: Int -> [(Int, Int)] -> Property
prop_splitModel n ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      (l, r) = split n $ fromList xs
  in  valid l .&&.
      valid r .&&.
      toAscList l === sort [(k, v) | (k,v) <- xs, k < n] .&&.
      toAscList r === sort [(k, v) | (k,v) <- xs, k > n]

prop_splitLookup :: Int -> [(Int, Int)] -> Property
prop_splitLookup n ys =
    let xs = List.nubBy ((==) `on` fst) ys
        (l, x, r) = splitLookup n (fromList xs)
    in  valid l .&&.
        valid r .&&.
        x === List.lookup n xs .&&.
        toAscList l === sort [(k, v) | (k,v) <- xs, k < n] .&&.
        toAscList r === sort [(k, v) | (k,v) <- xs, k > n]

prop_splitRoot :: IMap -> Bool
prop_splitRoot s = loop ls && (s == unions ls)
 where
  ls = splitRoot s
  loop [] = True
  loop (s1:rst) = List.null
                  [ (x,y) | x <- toList s1
                          , y <- toList (unions rst)
                          , x > y ]

prop_foldr :: Int -> [(Int, Int)] -> Property
prop_foldr n ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      m  = fromList xs
  in  foldr (+) n m == List.foldr (+) n (List.map snd xs) &&
      foldr (:) [] m == List.map snd (List.sort xs) &&
      foldrWithKey (\_ a b -> a + b) n m == List.foldr (+) n (List.map snd xs) &&
      foldrWithKey (\k _ b -> k + b) n m == List.foldr (+) n (List.map fst xs) &&
      foldrWithKey (\k x xs -> (k,x):xs) [] m == List.sort xs


prop_foldr' :: Int -> [(Int, Int)] -> Property
prop_foldr' n ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      m  = fromList xs
  in  foldr' (+) n m == List.foldr (+) n (List.map snd xs) &&
      foldr' (:) [] m == List.map snd (List.sort xs) &&
      foldrWithKey' (\_ a b -> a + b) n m == List.foldr (+) n (List.map snd xs) &&
      foldrWithKey' (\k _ b -> k + b) n m == List.foldr (+) n (List.map fst xs) &&
      foldrWithKey' (\k x xs -> (k,x):xs) [] m == List.sort xs

prop_foldl :: Int -> [(Int, Int)] -> Property
prop_foldl n ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      m  = fromList xs
  in  foldl (+) n m == List.foldr (+) n (List.map snd xs) &&
      foldl (flip (:)) [] m == reverse (List.map snd (List.sort xs)) &&
      foldlWithKey (\b _ a -> a + b) n m == List.foldr (+) n (List.map snd xs) &&
      foldlWithKey (\b k _ -> k + b) n m == List.foldr (+) n (List.map fst xs) &&
      foldlWithKey (\xs k x -> (k,x):xs) [] m == reverse (List.sort xs)

prop_foldl' :: Int -> [(Int, Int)] -> Property
prop_foldl' n ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      m  = fromList xs
  in  foldl' (+) n m == List.foldr (+) n (List.map snd xs) &&
      foldl' (flip (:)) [] m == reverse (List.map snd (List.sort xs)) &&
      foldlWithKey' (\b _ a -> a + b) n m == List.foldr (+) n (List.map snd xs) &&
      foldlWithKey' (\b k _ -> k + b) n m == List.foldr (+) n (List.map fst xs) &&
      foldlWithKey' (\xs k x -> (k,x):xs) [] m == reverse (List.sort xs)

prop_foldrEqFoldMap :: IntMap Int -> Property
prop_foldrEqFoldMap m =
  foldr (:) [] m === Data.Foldable.foldMap (:[]) m

prop_foldrWithKeyEqFoldMapWithKey :: IntMap Int -> Property
prop_foldrWithKeyEqFoldMapWithKey m =
  foldrWithKey (\k v -> ((k,v):)) [] m === foldMapWithKey (\k v -> ([(k,v)])) m

prop_FoldableTraversableCompat :: Fun A [B] -> IntMap A -> Property
prop_FoldableTraversableCompat fun m = foldMap f m === foldMapDefault f m
  where f = apply fun

prop_keysSet :: [(Int, Int)] -> Bool
prop_keysSet xs =
  keysSet (fromList xs) == IntSet.fromList (List.map fst xs)

prop_fromSet :: [(Int, Int)] -> Bool
prop_fromSet ys =
  let xs = List.nubBy ((==) `on` fst) ys
  in fromSet (\k -> fromJust $ List.lookup k xs) (IntSet.fromList $ List.map fst xs) == fromList xs

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity (f a)

prop_traverseWithKey_identity :: IntMap A -> Property
prop_traverseWithKey_identity mp = mp === newMap
  where Identity newMap = traverseWithKey (\_ -> Identity) mp

prop_traverseWithKey_degrade_to_mapWithKey :: Fun (Int, A) B -> IntMap A -> Property
prop_traverseWithKey_degrade_to_mapWithKey fun mp =
    mapWithKey f mp === newMap
  where f = applyFun2 fun
        g k v = Identity $ f k v
        Identity newMap = traverseWithKey g mp

prop_traverseMaybeWithKey_identity :: IntMap A -> Property
prop_traverseMaybeWithKey_identity mp = mp === newMap
  where Identity newMap = traverseMaybeWithKey (\_ -> Identity . Just) mp

prop_traverseMaybeWithKey_degrade_to_mapMaybeWithKey :: Fun (Int, A) (Maybe B) -> IntMap A -> Property
prop_traverseMaybeWithKey_degrade_to_mapMaybeWithKey fun mp =
    mapMaybeWithKey f mp === newMap
  where f = applyFun2 fun
        g k v = Identity $ f k v
        Identity newMap = traverseMaybeWithKey g mp

prop_traverseMaybeWithKey_degrade_to_traverseWithKey :: Fun (Int, A) B -> IntMap A -> Property
prop_traverseMaybeWithKey_degrade_to_traverseWithKey fun mp =
    traverseWithKey f mp === traverseMaybeWithKey g mp
        -- used (,) since its Applicative is monoidal in the left argument,
        -- so this also checks the order of traversing is the same.
  where f k v = (show k, applyFun2 fun k v)
        g k v = fmap Just $ f k v

prop_isProperSubmapOfBy :: Fun (A, A) Bool -> IntMap A -> IntMap A -> Property
prop_isProperSubmapOfBy f m1 m2 =
  isProperSubmapOfBy (applyFun2 f) m1 m2 ===
  (length xs == size m1 && size m1 < size m2)
  where
    xs = List.intersectBy
           (\(k1,x1) (k2,x2) -> k1 == k2 && applyFun2 f x1 x2)
           (assocs m1) (assocs m2)

prop_isSubmapOfBy :: Fun (A, A) Bool -> IntMap A -> IntMap A -> Property
prop_isSubmapOfBy f m1 m2 =
  isSubmapOfBy (applyFun2 f) m1 m2 ===
  (length xs == size m1)
  where
    xs = List.intersectBy
           (\(k1,x1) (k2,x2) -> k1 == k2 && applyFun2 f x1 x2)
           (assocs m1) (assocs m2)

prop_insert :: Int -> A -> IntMap A -> Property
prop_insert k x m = valid m' .&&. toList m' === kxs'
  where
    m' = insert k x m
    kxs = toList m
    kxs' = List.insertBy (comparing fst) (k,x) $
           maybe kxs (\y -> kxs List.\\ [(k,y)]) (List.lookup k kxs)

prop_delete :: Int -> IntMap A -> Property
prop_delete k m = valid m' .&&. toList m' === kxs'
  where
    m' = delete k m
    kxs = toList m
    kxs' = maybe kxs (\v -> kxs List.\\ [(k,v)]) (List.lookup k kxs)

prop_insertWith :: Fun (A, A) A -> Int -> A -> IntMap A -> Property
prop_insertWith f k x m = valid m' .&&. toList m' === kxs'
  where
    m' = insertWith (applyFun2 f) k x m
    kxs = toList m
    kxs' = if k `List.elem` (fmap fst kxs)
           then let g (k',x') = (k', if k == k' then applyFun2 f x x' else x')
                in fmap g kxs
           else List.insertBy (comparing fst) (k,x) kxs

prop_insertWithKey :: Fun (Int, A, A) A -> Int -> A -> IntMap A -> Property
prop_insertWithKey f k x m = valid m' .&&. toList m' === kxs'
  where
    m' = insertWithKey (applyFun3 f) k x m
    kxs = toList m
    kxs' = if k `List.elem` (fmap fst kxs)
           then let g (k',x') = (k', if k == k' then applyFun3 f k x x' else x')
                in fmap g kxs
           else List.insertBy (comparing fst) (k,x) kxs

prop_insertLookupWithKey
  :: Fun (Int, A, A) A -> Int -> A -> IntMap A -> Property
prop_insertLookupWithKey f k x m =
  valid m' .&&.
  r === (lookup k m, insertWith (applyFun3 f k) k x m)
  where
    r@(_, m') = insertLookupWithKey (applyFun3 f) k x m

prop_adjust :: Fun A A -> Int -> IntMap A -> Property
prop_adjust f k m =
  valid m' .&&.
  m' === maybe m (\x -> insert k (applyFun f x) m) (lookup k m)
  where
    m' = adjust (applyFun f) k m

prop_adjustWithKey :: Fun (Int, A) A -> Int -> IntMap A -> Property
prop_adjustWithKey f k m =
  valid m' .&&.
  m' === maybe m (\x -> insert k (applyFun2 f k x) m) (lookup k m)
  where
    m' = adjustWithKey (applyFun2 f) k m

prop_update :: Fun A (Maybe A) -> Int -> IntMap A -> Property
prop_update f k m = valid m' .&&. m' === m''
  where
    m' = update (applyFun f) k m
    m'' = case lookup k m of
      Nothing -> m
      Just x -> case applyFun f x of
        Nothing -> delete k m
        Just x' -> insert k x' m

prop_updateWithKey :: Fun (Int, A) (Maybe A) -> Int -> IntMap A -> Property
prop_updateWithKey f k m = valid m' .&&. m' === m''
  where
    m' = updateWithKey (applyFun2 f) k m
    m'' = case lookup k m of
      Nothing -> m
      Just x -> case applyFun2 f k x of
        Nothing -> delete k m
        Just x' -> insert k x' m

prop_upsert :: Fun (Maybe A) A -> Int -> IntMap A -> Property
prop_upsert f k m = valid m' .&&. m' == m''
  where
    m' = upsert (applyFun f) k m
    m'' = insert k (applyFun f (lookup k m)) m

prop_updateLookupWithKey
  :: Fun (Int, A) (Maybe A) -> Int -> IntMap A -> Property
prop_updateLookupWithKey f k m = valid m' .&&. r === (lookup k m, m'')
  where
    r@(_, m') = updateLookupWithKey (applyFun2 f) k m
    m'' = updateWithKey (applyFun2 f) k m

prop_differenceWith
  :: Fun (A, B) (Maybe A) -> IntMap A -> IntMap B -> Property
prop_differenceWith f m1 m2 =
  valid m' .&&.
  toList m' === Maybe.mapMaybe f' kxs
  where
    m' = differenceWith (applyFun2 f) m1 m2
    kxs = toList m1
    kys = toList m2
    f' (k,x) = case List.lookup k kys of
      Nothing -> Just (k,x)
      Just y -> (,) k <$> applyFun2 f x y

prop_differenceWithKey
  :: Fun (Int, A, B) (Maybe A) -> IntMap A -> IntMap B -> Property
prop_differenceWithKey f m1 m2 =
  valid m' .&&.
  toList m' === Maybe.mapMaybe f' kxs
  where
    m' = differenceWithKey (applyFun3 f) m1 m2
    kxs = toList m1
    kys = toList m2
    f' (k,x) = case List.lookup k kys of
      Nothing -> Just (k,x)
      Just y -> (,) k <$> applyFun3 f k x y

prop_partitionWithKey :: Fun (Int, A) Bool -> IntMap A -> Property
prop_partitionWithKey f m =
  valid m1 .&&.
  valid m2 .&&.
  (toList m1, toList m2) === List.partition (applyFun f) (toList m)
  where
    (m1, m2) = partitionWithKey (applyFun2 f) m

prop_updateMin :: Fun A (Maybe A) -> IntMap A -> Property
prop_updateMin f m =
  valid m' .&&.
  toList m' === case toList m of
    [] -> []
    (k,x):xs -> Foldable.toList ((,) k <$> applyFun f x) ++ xs
  where
    m' = updateMin (applyFun f) m

prop_updateMax :: Fun A (Maybe A) -> IntMap A -> Property
prop_updateMax f m =
  valid m' .&&.
  toDescList m' === case toDescList m of
    [] -> []
    (k,x):xs -> Foldable.toList ((,) k <$> applyFun f x) ++ xs
  where
    m' = updateMax (applyFun f) m

prop_updateMinWithKey :: Fun (Int, A) (Maybe A) -> IntMap A -> Property
prop_updateMinWithKey f m =
  valid m' .&&.
  toList m' === case toList m of
    [] -> []
    (k,x):xs -> Foldable.toList ((,) k <$> applyFun2 f k x) ++ xs
  where
    m' = updateMinWithKey (applyFun2 f) m

prop_updateMaxWithKey :: Fun (Int, A) (Maybe A) -> IntMap A -> Property
prop_updateMaxWithKey f m =
  valid m' .&&.
  toDescList m' === case toDescList m of
    [] -> []
    (k,x):xs -> Foldable.toList ((,) k <$> applyFun2 f k x) ++ xs
  where
    m' = updateMaxWithKey (applyFun2 f) m

prop_minViewWithKey :: IntMap A -> Property
prop_minViewWithKey m = case minViewWithKey m of
  Nothing -> property $ null m
  Just ((k,x),m') -> valid m .&&. toList m === (k,x) : toList m'

prop_maxViewWithKey :: IntMap A -> Property
prop_maxViewWithKey m = case maxViewWithKey m of
  Nothing -> property $ null m
  Just ((k,x),m') -> valid m .&&. toDescList m === (k,x) : toDescList m'

prop_minView :: IntMap A -> Property
prop_minView m = case minView m of
  Nothing -> property $ null m
  Just (x,m') | ~(_,x'):xs' <- toList m ->
    valid m' .&&. x == x' .&&. toList m' === xs'

prop_maxView :: IntMap A -> Property
prop_maxView m = case maxView m of
  Nothing -> property $ null m
  Just (x,m') | ~(_,x'):xs' <- toDescList m ->
    valid m' .&&. x == x' .&&. toDescList m' === xs'

prop_mapEither :: Fun A (Either B C) -> IntMap A -> Property
prop_mapEither f m =
  valid m1 .&&.
  valid m2 .&&.
  (toList m1, toList m2) === Either.partitionEithers (List.map f' (toList m))
  where
    (m1, m2) = mapEither (applyFun f) m
    f' (k,x) = case applyFun f x of
      Left y -> Left (k,y)
      Right z -> Right (k,z)

prop_mapEitherWithKey :: Fun (Int, A) (Either B C) -> IntMap A -> Property
prop_mapEitherWithKey f m =
  valid m1 .&&.
  valid m2 .&&.
  (toList m1, toList m2) === Either.partitionEithers (List.map f' (toList m))
  where
    (m1, m2) = mapEitherWithKey (applyFun2 f) m
    f' (k,x) = case applyFun2 f k x of
      Left y -> Left (k,y)
      Right z -> Right (k,z)

prop_mapAccum :: Fun (A, B) (A, C) -> A -> IntMap B -> Property
prop_mapAccum f z0 m =
  valid m' .&&.
  (z, toList m') === List.mapAccumL f' z0 (toList m)
  where
    (z, m') = mapAccum (applyFun2 f) z0 m
    f' z' (k,x) = (,) k <$> applyFun2 f z' x

prop_mapAccumWithKey :: Fun (A, Int, B) (A, C) -> A -> IntMap B -> Property
prop_mapAccumWithKey f z0 m =
  valid m' .&&.
  (z, toList m') === List.mapAccumL f' z0 (toList m)
  where
    (z, m') = mapAccumWithKey (applyFun3 f) z0 m
    f' z' (k,x) = (,) k <$> applyFun3 f z' k x

prop_mapAccumRWithKey :: Fun (A, Int, B) (A, C) -> A -> IntMap B -> Property
prop_mapAccumRWithKey f z0 m =
  valid m' .&&.
  (z, toList m') === List.mapAccumR f' z0 (toList m)
  where
    (z, m') = mapAccumRWithKey (applyFun3 f) z0 m
    f' z' (k,x) = (,) k <$> applyFun3 f z' k x

prop_mapKeysWith :: Fun (A, A) A -> Fun Int Int -> IntMap A -> Property
prop_mapKeysWith f g m =
  valid m' .&&.
  toList m' ===
    toList (fromListWith (applyFun2 f) [(applyFun g k, x) | (k,x) <- toList m])
  where
    m' = mapKeysWith (applyFun2 f) (applyFun g) m

prop_mapKeysMonotonic :: Positive Int -> Int -> IntMap A -> Property
prop_mapKeysMonotonic (Positive a) b m =
  all ok (keys m) ==>
    valid m' .&&.
    toList m' == [(f k, x) | (k,x) <- toList m]
  where
    m' = mapKeysMonotonic f m
    f x = x * a + b
    ok x =  -- must not overflow
      fromIntegral (minBound :: Int) <= y && y <= fromIntegral (maxBound :: Int)
      where
        y = fromIntegral a * fromIntegral x + fromIntegral b :: Integer

prop_compare :: IntMap OrdA -> IntMap OrdA -> Property
prop_compare m1 m2 = compare m1 m2 === compare (toList m1) (toList m2)

prop_fromAscList :: [(Int, A)] -> Property
prop_fromAscList kxs =
  valid t .&&.
  t === fromList sortedKxs
  where
    sortedKxs = List.sortBy (comparing fst) kxs
    t = fromAscList sortedKxs

prop_fromAscListWith :: Fun (A, A) A -> [(Int, A)] -> Property
prop_fromAscListWith f kxs =
  valid t .&&.
  t === fromListWith (applyFun2 f) sortedKxs
  where
    sortedKxs = List.sortBy (comparing fst) kxs
    t = fromAscListWith (applyFun2 f) sortedKxs

prop_fromAscListWithKey :: Fun (Int, A, A) A -> [(Int, A)] -> Property
prop_fromAscListWithKey f kxs =
  valid t .&&.
  t === fromListWithKey (applyFun3 f) sortedKxs
  where
    sortedKxs = List.sortBy (comparing fst) kxs
    t = fromAscListWithKey (applyFun3 f) sortedKxs

prop_fromDistinctAscList :: [(Int, A)] -> Property
prop_fromDistinctAscList kxs =
  valid t .&&.
  toList t === nubSortedKxs
  where
    nubSortedKxs =
      List.map NE.head $
      NE.groupBy ((==) `on` fst) $
      List.sortBy (comparing fst) kxs
    t = fromDistinctAscList nubSortedKxs

prop_fromListWith :: Fun (A, A) A -> [(Int, A)] -> Property
prop_fromListWith f kxs =
  valid m .&&.
  m === List.foldl' (\m' (k,x) -> insertWith (applyFun2 f) k x m') empty kxs
  where
    m = fromListWith (applyFun2 f) kxs

prop_fromListWithKey :: Fun (Int, A, A) A -> [(Int, A)] -> Property
prop_fromListWithKey f kxs =
  valid m .&&.
  m === List.foldl' (\m' (k,x) -> insertWith (applyFun3 f k) k x m') empty kxs
  where
    m = fromListWithKey (applyFun3 f) kxs

prop_compareSize :: IntMap A -> Int -> Property
prop_compareSize t c = compareSize t c === compare (size t) c
