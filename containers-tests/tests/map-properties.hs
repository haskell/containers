{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

#ifdef STRICT
import Data.Map.Strict as Data.Map
import Data.Map.Merge.Strict
#else
import Data.Map.Lazy as Data.Map
import Data.Map.Merge.Lazy
#endif
import Data.Map.Internal (Map, link2, link)
import Data.Map.Internal.Debug (showTree, showTreeWith, balanced)

import Control.Applicative (Const(Const, getConst), pure, (<$>), (<*>), (<|>))
import Control.Arrow ((&&&))
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Lazy
import Control.Monad ((<=<))
import qualified Data.Either as Either
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Monoid
import Data.Maybe hiding (mapMaybe)
import qualified Data.Maybe as Maybe (mapMaybe)
import Data.Ord
import Data.Semigroup (Arg(..))
import Data.Function
import Data.Functor
import qualified Data.Foldable as Foldable
import qualified Data.Bifoldable as Bifoldable
import Prelude hiding (lookup, null, map, filter, foldr, foldl, foldl', take, drop, splitAt)
import qualified Prelude

import Data.List (nub,sort)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function (apply)
import Test.QuickCheck.Poly (A, B, C, OrdA)

import Utils.ArbitrarySetMap (mkArbMap)

default (Int)

apply3 :: Fun (a,b,c) d -> a -> b -> c -> d
apply3 f a b c = apply f (a, b, c)

apply2 :: Fun (a,b) c -> a -> b -> c
apply2 f a b = apply f (a, b)

main :: IO ()
main = defaultMain $ testGroup "map-properties"
         [ testCase "ticket4242" test_ticket4242
         , testCase "index"      test_index
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
         , testCase "at" test_at
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
         , testCase "argSet" test_argSet
         , testCase "fromSet" test_fromSet
         , testCase "fromArgSet" test_fromArgSet
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
         , testCase "fromDistinctDescList" test_fromDistinctDescList
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
         , testProperty "valid"                prop_valid
         , testProperty "insert to singleton"  prop_singleton
         , testProperty "insert then lookup"   prop_insertLookup
         , testProperty "insert then delete"   prop_insertDelete
         , testProperty "insert then delete2"  prop_insertDelete2
         , testProperty "delete non member"    prop_deleteNonMember
         , testProperty "deleteMin"            prop_deleteMin
         , testProperty "deleteMax"            prop_deleteMax
         , testProperty "split"                prop_split
         , testProperty "splitRoot"            prop_splitRoot
         , testProperty "split then link"      prop_link
         , testProperty "split then link2"     prop_link2
         , testProperty "union"                prop_union
         , testProperty "union model"          prop_unionModel
         , testProperty "union singleton"      prop_unionSingleton
         , testProperty "union associative"    prop_unionAssoc
         , testProperty "union+unionWith"      prop_unionWith
         , testProperty "unionWith"            prop_unionWith2
         , testProperty "union sum"            prop_unionSum
         , testProperty "difference"           prop_difference
         , testProperty "difference model"     prop_differenceModel
         , testProperty "withoutKeys"          prop_withoutKeys
         , testProperty "intersection"         prop_intersection
         , testProperty "restrictKeys"         prop_restrictKeys
         , testProperty "intersection model"   prop_intersectionModel
         , testProperty "intersectionWith"     prop_intersectionWith
         , testProperty "intersectionWithModel" prop_intersectionWithModel
         , testProperty "intersectionWithKey"  prop_intersectionWithKey
         , testProperty "intersectionWithKeyModel" prop_intersectionWithKeyModel
         , testProperty "symmetricDifference"  prop_symmetricDifference
         , testProperty "disjoint"             prop_disjoint
         , testProperty "compose"              prop_compose
         , testProperty "differenceMerge"   prop_differenceMerge
         , testProperty "unionWithKeyMerge"   prop_unionWithKeyMerge
         , testProperty "mergeWithKey model"   prop_mergeWithKeyModel
         , testProperty "mergeA effects"       prop_mergeA_effects
         , testProperty "fromAscList"          prop_fromAscList
         , testProperty "fromAscListWith"      prop_fromAscListWith
         , testProperty "fromAscListWithKey"   prop_fromAscListWithKey
         , testProperty "fromDistinctAscList"  prop_fromDistinctAscList
         , testProperty "fromDescList"         prop_fromDescList
         , testProperty "fromDescListWith"     prop_fromDescListWith
         , testProperty "fromDescListWithKey"  prop_fromDescListWithKey
         , testProperty "fromDistinctDescList" prop_fromDistinctDescList
         , testProperty "fromList then toList" prop_list
         , testProperty "toDescList"           prop_descList
         , testProperty "toAscList+toDescList" prop_ascDescList
         , testProperty "fromList"             prop_fromList
         , testProperty "fromListWith"         prop_fromListWith
         , testProperty "fromListWithKey"      prop_fromListWithKey
         , testProperty "alter"                prop_alter
         , testProperty "alterF/alter"         prop_alterF_alter
         , testProperty "alterF/alter/noRULES" prop_alterF_alter_noRULES
         , testProperty "alterF/lookup"        prop_alterF_lookup
         , testProperty "alterF/lookup/noRULES" prop_alterF_lookup_noRULES
         , testProperty "index"                prop_index
         , testProperty "null"                 prop_null
         , testProperty "member"               prop_member
         , testProperty "notmember"            prop_notmember
         , testProperty "lookup"               prop_lookup
         , testProperty "find"                 prop_find
         , testProperty "findWithDefault"      prop_findWithDefault
         , testProperty "lookupLT"             prop_lookupLT
         , testProperty "lookupGT"             prop_lookupGT
         , testProperty "lookupLE"             prop_lookupLE
         , testProperty "lookupGE"             prop_lookupGE
         , testProperty "findIndex"            prop_findIndex
         , testProperty "lookupIndex"          prop_lookupIndex
         , testProperty "findMin"              prop_findMin
         , testProperty "findMax"              prop_findMax
         , testProperty "deleteMin"            prop_deleteMinModel
         , testProperty "deleteMax"            prop_deleteMaxModel
         , testProperty "filter"               prop_filter
         , testProperty "filterKeys"           prop_filterKeys
         , testProperty "filterWithKey"        prop_filterWithKey
         , testProperty "partition"            prop_partition
         , testProperty "map"                  prop_map
         , testProperty "fmap"                 prop_fmap
         , testProperty "mapKeys"              prop_mapKeys
         , testProperty "mapKeysWith"          prop_mapKeysWith
         , testProperty "mapKeysMonotonic"     prop_mapKeysMonotonic
         , testProperty "split"                prop_splitModel
         , testProperty "fold"                 prop_fold
         , testProperty "foldMap"              prop_foldMap
         , testProperty "foldMapWithKey"       prop_foldMapWithKey
         , testProperty "foldr"                prop_foldr
         , testProperty "foldrWithKey"         prop_foldrWithKey
         , testProperty "foldr'"               prop_foldr'
         , testProperty "foldrWithKey'"        prop_foldrWithKey'
         , testProperty "foldl"                prop_foldl
         , testProperty "foldlWithKey"         prop_foldlWithKey
         , testProperty "foldl'"               prop_foldl'
         , testProperty "foldlWithKey'"        prop_foldlWithKey'
         , testProperty "bifold"               prop_bifold
         , testProperty "bifoldMap"            prop_bifoldMap
         , testProperty "bifoldr"              prop_bifoldr
         , testProperty "bifoldr'"             prop_bifoldr'
         , testProperty "bifoldl"              prop_bifoldl
         , testProperty "bifoldl'"             prop_bifoldl'
         , testProperty "keysSet"              prop_keysSet
         , testProperty "argSet"               prop_argSet
         , testProperty "fromSet"              prop_fromSet
         , testProperty "fromSetA eval order"  prop_fromSetA_action_order
         , testProperty "fromArgSet"           prop_fromArgSet
         , testProperty "takeWhileAntitone"    prop_takeWhileAntitone
         , testProperty "dropWhileAntitone"    prop_dropWhileAntitone
         , testProperty "spanAntitone"         prop_spanAntitone
         , testProperty "take"                 prop_take
         , testProperty "drop"                 prop_drop
         , testProperty "splitAt"              prop_splitAt
         , testProperty "lookupMin"            prop_lookupMin
         , testProperty "lookupMax"            prop_lookupMax
         , testProperty "eq"                   prop_eq
         , testProperty "compare"              prop_compare
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
         , testProperty "elemAt"               prop_elemAt
         , testProperty "updateAt"             prop_updateAt
         , testProperty "deleteAt"             prop_deleteAt
         , testProperty "updateMin"            prop_updateMin
         , testProperty "updateMax"            prop_updateMax
         , testProperty "updateMinWithKey"     prop_updateMinWithKey
         , testProperty "updateMaxWithKey"     prop_updateMaxWithKey
         , testProperty "minViewWithKey"       prop_minViewWithKey
         , testProperty "maxViewWithKey"       prop_maxViewWithKey
         , testProperty "minView"              prop_minView
         , testProperty "maxView"              prop_maxView
         , testProperty "isSubmapOf"           prop_isSubmapOf
         , testProperty "isSubmapOfBy"         prop_isSubmapOfBy
         , testProperty "isProperSubmapOf"     prop_isProperSubmapOf
         , testProperty "isProperSubmapOfBy"   prop_isProperSubmapOfBy
         , testProperty "differenceWith"       prop_differenceWith
         , testProperty "differenceWithKey"    prop_differenceWithKey
         , testProperty "partitionWithKey"     prop_partitionWithKey
         , testProperty "mapMaybe"             prop_mapMaybe
         , testProperty "mapMaybeWithKey"      prop_mapMaybeWithKey
         , testProperty "traverseMaybeWithKey" prop_traverseMaybeWithKey
         , testProperty "mapEither"            prop_mapEither
         , testProperty "mapEitherWithKey"     prop_mapEitherWithKey
         , testProperty "mapWithKey"           prop_mapWithKey
         , testProperty "traverse"             prop_traverse
         , testProperty "traverseWithKey"      prop_traverseWithKey
         , testProperty "mapAccum"             prop_mapAccum
         , testProperty "mapAccumWithKey"      prop_mapAccumWithKey
         , testProperty "mapAccumRWithKey"     prop_mapAccumRWithKey
         ]

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


-- How much the minimum key of an arbitrary map should vary
positionFactor :: Int
positionFactor = 1

-- How much the gap between consecutive keys in an arbitrary
-- map should vary
gapRange :: Int
gapRange = 5

instance (IsInt k, Arbitrary v) => Arbitrary (Map k v) where
  arbitrary = sized (\sz0 -> do
        sz <- choose (0, sz0)
        middle <- choose (-positionFactor * (sz + 1), positionFactor * (sz + 1))
        let shift = (sz * (gapRange) + 1) `quot` 2
            start = middle - shift
        t <- evalStateT (mkArbMap step sz) start
        if valid t then pure t else error "Test generated invalid tree!")
    where
      step = do
        i <- get
        diff <- lift $ choose (1, gapRange)
        let i' = i + diff
        put i'
        pure (fromInt i')

-- A type with a peculiar Eq instance designed to make sure keys
-- come from where they're supposed to.
data OddEq a = OddEq a Bool deriving (Show)

instance Arbitrary a => Arbitrary (OddEq a) where
  arbitrary = OddEq <$> arbitrary <*> arbitrary
instance Eq a => Eq (OddEq a) where
  OddEq x _ == OddEq y _ = x == y
instance Ord a => Ord (OddEq a) where
  OddEq x _ `compare` OddEq y _ = x `compare` y

------------------------------------------------------------------------

type UMap = Map Int ()
type IMap = Map Int Int
type SMap = Map Int String

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

test_lookupLT :: Assertion
test_lookupLT = do
    lookupLT 3 (fromList [(3,'a'), (5,'b')]) @?= Nothing
    lookupLT 4 (fromList [(3,'a'), (5,'b')]) @?= Just (3, 'a')

test_lookupGT :: Assertion
test_lookupGT = do
    lookupGT 4 (fromList [(3,'a'), (5,'b')]) @?= Just (5, 'b')
    lookupGT 5 (fromList [(3,'a'), (5,'b')]) @?= Nothing

test_lookupLE :: Assertion
test_lookupLE = do
    lookupLE 2 (fromList [(3,'a'), (5,'b')]) @?= Nothing
    lookupLE 4 (fromList [(3,'a'), (5,'b')]) @?= Just (3, 'a')
    lookupLE 5 (fromList [(3,'a'), (5,'b')]) @?= Just (5, 'b')

test_lookupGE :: Assertion
test_lookupGE = do
    lookupGE 3 (fromList [(3,'a'), (5,'b')]) @?= Just (3, 'a')
    lookupGE 4 (fromList [(3,'a'), (5,'b')]) @?= Just (5, 'b')
    lookupGE 6 (fromList [(3,'a'), (5,'b')]) @?= Nothing

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

test_insertWithKey :: Assertion
test_insertWithKey = do
    insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "5:xxx|a")]
    insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a"), (7, "xxx")]
    insertWithKey f 5 "xxx" empty                         @?= singleton 5 "xxx"
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

test_at :: Assertion
test_at = do
    employeeCurrency "John" @?= Just "Euro"
    employeeCurrency "Pete" @?= Nothing
    atAlter f 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a")]
    atAlter f 5 (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"
    atAlter g 7 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "a"), (7, "c")]
    atAlter g 5 (fromList [(5,"a"), (3,"b")]) @?= fromList [(3, "b"), (5, "c")]
  where
    f _ = Nothing
    g _ = Just "c"
    employeeDept = fromList([("John","Sales"), ("Bob","IT")])
    deptCountry = fromList([("IT","USA"), ("Sales","France")])
    countryCurrency = fromList([("USA", "Dollar"), ("France", "Euro")])
    employeeCurrency :: String -> Maybe String
    employeeCurrency name = do
        dept <- atLookup name employeeDept
        country <- atLookup dept deptCountry
        atLookup country countryCurrency

-- This version of atAlter will rewrite to alterFIdentity
-- if the rules fire.
atAlter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
atAlter f k m = runIdentity (alterF (pure . f) k m)

-- A version of atAlter that uses a private copy of Identity
-- to ensure that the adjustF/Identity rules don't fire and
-- we use the basic implementation.
atAlterNoRULES :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
atAlterNoRULES f k m = runIdent (alterF (Ident . f) k m)

newtype Ident a = Ident { runIdent :: a }
instance Functor Ident where
  fmap f (Ident a) = Ident (f a)

atLookup :: Ord k => k -> Map k a -> Maybe a
atLookup k m = getConst (alterF Const k m)

atLookupNoRULES :: Ord k => k -> Map k a -> Maybe a
atLookupNoRULES k m = getConsty (alterF Consty k m)

newtype Consty a b = Consty { getConsty :: a}
instance Functor (Consty a) where
  fmap _ (Consty a) = Consty a

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
    mapKeysMonotonic (+ 1) (fromList [(5,"a"), (3,"b")])          @?= fromList [(4, "b"), (6, "a")]
    mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) @?= fromList [(6, "b"), (10, "a")]
    valid (mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")])) @?= True
    valid (mapKeysMonotonic (\ _ -> 1)     (fromList [(5,"a"), (3,"b")])) @?= False

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

test_assocs :: Assertion
test_assocs = do
    assocs (fromList [(5,"a"), (3,"b")]) @?= [(3,"b"), (5,"a")]
    assocs (empty :: UMap) @?= []

test_keysSet :: Assertion
test_keysSet = do
    keysSet (fromList [(5,"a"), (3,"b")]) @?= Set.fromList [3,5]
    keysSet (empty :: UMap) @?= Set.empty

test_argSet :: Assertion
test_argSet = do
    argSet (fromList [(5,"a"), (3,"b")]) @?= Set.fromList [Arg 3 "b",Arg 5 "a"]
    argSet (empty :: UMap) @?= Set.empty

test_fromSet :: Assertion
test_fromSet = do
   fromSet (\k -> replicate k 'a') (Set.fromList [3, 5]) @?= fromList [(5,"aaaaa"), (3,"aaa")]
   fromSet undefined Set.empty @?= (empty :: IMap)

test_fromArgSet :: Assertion
test_fromArgSet = do
   fromArgSet (Set.fromList [Arg 3 "aaa", Arg 5 "aaaaa"]) @?= fromList [(5,"aaaaa"), (3,"aaa")]
   fromArgSet Set.empty @?= (empty :: IMap)

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

test_fromDistinctDescList :: Assertion
test_fromDistinctDescList = do
    fromDistinctDescList [(5,"a"), (3,"b")] @?= fromList [(3, "b"), (5, "a")]
    valid (fromDistinctDescList [(5,"a"), (3,"b")])          @?= True
    valid (fromDistinctDescList [(3,"b"), (5,"a"), (5,"b")]) @?= False

----------------------------------------------------------------
-- Filter

test_filter :: Assertion
test_filter = do
    filter (> "a") (fromList [(5,"a"), (3,"b")]) @?= singleton 3 "b"
    filter (> "x") (fromList [(5,"a"), (3,"b")]) @?= empty
    filter (< "a") (fromList [(5,"a"), (3,"b")]) @?= empty

test_filterKeys :: Assertion
test_filterKeys = filterKeys (> 4) (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"

test_filterWithKey :: Assertion
test_filterWithKey = filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) @?= singleton 5 "a"

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

prop_differenceMerge :: Fun (Int, A, B) (Maybe A) -> Map Int A -> Map Int B -> Property
prop_differenceMerge f m1 m2 =
  differenceWithKey (apply3 f) m1 m2 === merge preserveMissing dropMissing (zipWithMaybeMatched (apply3 f)) m1 m2

prop_unionWithKeyMerge :: Fun (Int, A, A) A -> Map Int A -> Map Int A -> Property
prop_unionWithKeyMerge f m1 m2 =
  unionWithKey (apply3 f) m1 m2 === unionWithKey' (apply3 f) m1 m2

unionWithKey' :: Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey' f = merge preserveMissing preserveMissing $
  zipWithMatched (\k a b -> f k a b)

prop_valid :: UMap -> Bool
prop_valid t = valid t

prop_singleton :: Int -> Int -> Bool
prop_singleton k x = insert k x empty == singleton k x

prop_insertLookup :: Int -> UMap -> Bool
prop_insertLookup k t = lookup k (insert k () t) /= Nothing

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

prop_lookupMin :: IMap -> Property
prop_lookupMin m = lookupMin m === (fst <$> minViewWithKey m)

prop_lookupMax :: IMap -> Property
prop_lookupMax m = lookupMax m === (fst <$> maxViewWithKey m)

----------------------------------------------------------------

prop_split :: Int -> UMap -> Bool
prop_split k t = let (r,l) = split k t
                 in (valid r, valid l) == (True, True)

prop_splitRoot :: UMap -> Bool
prop_splitRoot s = loop ls && (s == unions ls)
 where
  ls = splitRoot s
  loop [] = True
  loop (s1:rst) = List.null
                  [ (x,y) | x <- toList s1
                          , y <- toList (unions rst)
                          , x > y ]

prop_link :: Int -> UMap -> Bool
prop_link k t = let (l,r) = split k t
                in valid (link k () l r)

prop_link2 :: Int -> UMap -> Bool
prop_link2 k t = let (l,r) = split k t
                 in valid (link2 l r)

----------------------------------------------------------------

prop_union :: UMap -> UMap -> Bool
prop_union t1 t2 = valid (union t1 t2)

prop_unionModel :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_unionModel xs ys
  = sort (keys (union (fromList xs) (fromList ys)))
    == sort (nub (Prelude.map fst xs ++ Prelude.map fst ys))

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
    == (sum (Prelude.map snd xs) + sum (Prelude.map snd ys))

prop_difference :: IMap -> IMap -> Bool
prop_difference t1 t2 = valid (difference t1 t2)

prop_differenceModel :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_differenceModel xs ys
  = sort (keys (difference (fromListWith (+) xs) (fromListWith (+) ys)))
    == sort ((List.\\) (nub (Prelude.map fst xs)) (nub (Prelude.map fst ys)))

prop_restrictKeys :: IMap -> IMap -> Property
prop_restrictKeys m s0 = valid restricted .&&. (m `restrictKeys` s === filterWithKey (\k _ -> k `Set.member` s) m)
  where
    s = keysSet s0
    restricted = restrictKeys m s

prop_withoutKeys :: IMap -> IMap -> Property
prop_withoutKeys m s0 = valid reduced .&&. (m `withoutKeys` s === filterWithKey (\k _ -> k `Set.notMember` s) m)
  where
    s = keysSet s0
    reduced = withoutKeys m s

prop_intersection :: IMap -> IMap -> Bool
prop_intersection t1 t2 = valid (intersection t1 t2)

prop_intersectionModel :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_intersectionModel xs ys
  = sort (keys (intersection (fromListWith (+) xs) (fromListWith (+) ys)))
    == sort (nub ((List.intersect) (Prelude.map fst xs) (Prelude.map fst ys)))

prop_intersectionWith :: Fun (Int, Int) (Maybe Int) -> IMap -> IMap -> Bool
prop_intersectionWith f t1 t2 = valid (intersectionWith (apply2 f) t1 t2)

prop_intersectionWithModel :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_intersectionWithModel xs ys
  = toList (intersectionWith f (fromList xs') (fromList ys'))
    == [(kx, f vx vy) | (kx, vx) <- List.sort xs', (ky, vy) <- ys', kx == ky]
    where xs' = List.nubBy ((==) `on` fst) xs
          ys' = List.nubBy ((==) `on` fst) ys
          f l r = l + 2 * r

prop_intersectionWithKey :: Fun (Int, Int, Int) (Maybe Int) -> IMap -> IMap -> Bool
prop_intersectionWithKey f t1 t2 = valid (intersectionWithKey (apply3 f) t1 t2)

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
                                            (Nothing, Nothing) -> error "impossible"

          -- We prevent inlining testMergeWithKey to disable the SpecConstr
          -- optimalization. There are too many call patterns here so several
          -- warnings are issued if testMergeWithKey gets inlined.
          {-# NOINLINE testMergeWithKey #-}

-- This uses the instance
--     Monoid a => Applicative ((,) a)
-- to test that effects are sequenced in ascending key order.
prop_mergeA_effects :: WhenMissingSpec -> WhenMissingSpec -> WhenMatchedSpec -> UMap -> UMap -> Property
prop_mergeA_effects onlyLeft onlyRight both xs ys
  = effects === sort effects
  where
    (effects, _m) = mergeA (whenMissing onlyLeft) (whenMissing onlyRight) (whenMatched both) xs ys
    whenMissing spec = case spec of
      DropMissing -> dropMissing
      PreserveMissing -> preserveMissing
      FilterMissing -> filterMissing (\_ _ -> False)
      FilterAMissing -> filterAMissing (\k _ -> ([k], False))
      MapMissing -> mapMissing (\_ _ -> ())
      TraverseMissing -> traverseMissing (\k _ -> ([k], ()))
      MapMaybeMissing -> mapMaybeMissing (\_ _ -> Nothing)
      TraverseMaybeMissing -> traverseMaybeMissing (\k _ -> ([k], Nothing))
    whenMatched spec = case spec of
      ZipWithMatched -> zipWithMatched (\_ _ _ -> ())
      ZipWithAMatched -> zipWithAMatched (\k _ _ -> ([k], ()))
      ZipWithMaybeMatched -> zipWithMaybeMatched (\_ _ _ -> Nothing)
      ZipWithMaybeAMatched -> zipWithMaybeAMatched (\k _ _ -> ([k], Nothing))

data WhenMissingSpec
  = DropMissing
  | PreserveMissing
  | FilterMissing
  | FilterAMissing
  | MapMissing
  | TraverseMissing
  | MapMaybeMissing
  | TraverseMaybeMissing
  deriving (Bounded, Enum, Show)

instance Arbitrary WhenMissingSpec where
  arbitrary = arbitraryBoundedEnum

data WhenMatchedSpec
  = ZipWithMatched
  | ZipWithAMatched
  | ZipWithMaybeMatched
  | ZipWithMaybeAMatched
  deriving (Bounded, Enum, Show)

instance Arbitrary WhenMatchedSpec where
  arbitrary = arbitraryBoundedEnum

----------------------------------------------------------------

prop_list :: [Int] -> Bool
prop_list xs = (sort (nub xs) == [x | (x,()) <- toList (fromList [(x,()) | x <- xs])])

prop_descList :: [Int] -> Bool
prop_descList xs = (reverse (sort (nub xs)) == [x | (x,()) <- toDescList (fromList [(x,()) | x <- xs])])

prop_fromDescList :: [(Int, A)] -> Property
prop_fromDescList kxs =
    valid t .&&.
    t === fromList kxs
  where
    downSortedKxs = List.sortBy (comparing (Down . fst)) kxs
    t = fromDescList downSortedKxs

prop_fromDescListWith :: Fun (A, A) A -> [(Int, A)] -> Property
prop_fromDescListWith f kxs =
    valid t .&&.
    t === fromListWith (apply2 f) downSortedKxs
  where
    downSortedKxs = List.sortBy (comparing (Down . fst)) kxs
    t = fromDescListWith (apply2 f) downSortedKxs

prop_fromDescListWithKey :: Fun (Int, A, A) A -> [(Int, A)] -> Property
prop_fromDescListWithKey f kxs =
    valid t .&&.
    t === fromListWithKey (apply3 f) downSortedKxs
  where
    downSortedKxs = List.sortBy (comparing (Down . fst)) kxs
    t = fromDescListWithKey (apply3 f) downSortedKxs

prop_fromDistinctDescList :: [(Int, A)] -> Property
prop_fromDistinctDescList kxs =
    valid t .&&.
    toList t === reverse nubDownSortedKxs
  where
    nubDownSortedKxs =
      List.map NE.head $
      NE.groupBy ((==) `on` fst) $
      List.sortBy (comparing (Down . fst)) kxs
    t = fromDistinctDescList nubDownSortedKxs

prop_ascDescList :: [Int] -> Bool
prop_ascDescList xs = toAscList m == reverse (toDescList m)
  where m = fromList $ zip xs $ repeat ()

prop_fromList :: [Int] -> Bool
prop_fromList xs
  = case fromList (zip xs xs) of
      t -> t == fromAscList (zip sort_xs sort_xs) &&
           t == List.foldr (uncurry insert) empty (zip xs xs)
  where sort_xs = sort xs

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
    t === fromListWith (apply2 f) sortedKxs
  where
    sortedKxs = List.sortBy (comparing fst) kxs
    t = fromAscListWith (apply2 f) sortedKxs

prop_fromAscListWithKey :: Fun (Int, A, A) A -> [(Int, A)] -> Property
prop_fromAscListWithKey f kxs =
    valid t .&&.
    t === fromListWithKey (apply3 f) sortedKxs
  where
    sortedKxs = List.sortBy (comparing fst) kxs
    t = fromAscListWithKey (apply3 f) sortedKxs

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
  fromListWith (applyFun2 f) kxs ===
  List.foldl' (\m (kx, x) -> insertWith (applyFun2 f) kx x m) empty kxs

prop_fromListWithKey :: Fun (Int, A, A) A -> [(Int, A)] -> Property
prop_fromListWithKey f kxs =
  fromListWithKey (applyFun3 f) kxs ===
  List.foldl' (\m (kx, x) -> insertWithKey (applyFun3 f) kx x m) empty kxs

----------------------------------------------------------------

prop_alter :: UMap -> Int -> Bool
prop_alter t k = balanced t' && case lookup k t of
    Just _  -> (size t - 1) == size t' && lookup k t' == Nothing
    Nothing -> (size t + 1) == size t' && lookup k t' /= Nothing
  where
    t' = alter f k t
    f Nothing   = Just ()
    f (Just ()) = Nothing

prop_alterF_alter :: Fun (Maybe Int) (Maybe Int) -> Int -> IMap -> Bool
prop_alterF_alter f k m = valid altered && altered == alter (apply f) k m
  where altered = atAlter (apply f) k m

prop_alterF_alter_noRULES :: Fun (Maybe Int) (Maybe Int) -> Int -> IMap -> Bool
prop_alterF_alter_noRULES f k m = valid altered &&
                                  altered == alter (apply f) k m
  where altered = atAlterNoRULES (apply f) k m

prop_alterF_lookup :: Int -> IMap -> Bool
prop_alterF_lookup k m = atLookup k m == lookup k m

prop_alterF_lookup_noRULES :: Int -> IMap -> Bool
prop_alterF_lookup_noRULES k m = atLookupNoRULES k m == lookup k m

------------------------------------------------------------------------
-- Compare against the list model (after nub on keys)

prop_index :: [Int] -> Property
prop_index xs = length xs > 0 ==>
  let m  = fromList (zip xs xs)
  in  xs == [ m ! i | i <- xs ]

prop_null :: IMap -> Bool
prop_null m = null m == (size m == 0)

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

prop_lookupLT :: Int -> Map Int A -> Bool
prop_lookupLT k m = lookupLT k m == List.find ((<k) . fst) (toDescList m)

prop_lookupGT :: Int -> Map Int A -> Bool
prop_lookupGT k m = lookupGT k m == List.find ((>k) . fst) (toList m)

prop_lookupLE :: Int -> Map Int A -> Bool
prop_lookupLE k m = lookupLE k m == List.find ((<=k) . fst) (toDescList m)

prop_lookupGE :: Int -> Map Int A -> Bool
prop_lookupGE k m = lookupGE k m == List.find ((>=k) . fst) (toList m)

prop_findIndex :: [(Int, Int)] -> Property
prop_findIndex ys = length ys > 0 ==>
  let m = fromList ys
  in  findIndex (fst (head ys)) m `seq` True

prop_lookupIndex :: [(Int, Int)] -> Property
prop_lookupIndex ys = length ys > 0 ==>
  let m = fromList ys
  in  isJust (lookupIndex (fst (head ys)) m)

prop_findMin :: [(Int, Int)] -> Property
prop_findMin ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      m  = fromList xs
  in  findMin m == List.minimumBy (comparing fst) xs

prop_findMax :: [(Int, Int)] -> Property
prop_findMax ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      m  = fromList xs
  in  findMax m == List.maximumBy (comparing fst) xs

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
      m  = fromList xs
  in  filter (apply p) m == fromList (List.filter (apply p . snd) xs)

prop_filterKeys :: Fun Int Bool -> IMap -> Property
prop_filterKeys fun m =
  valid m' .&&. toList m' === Prelude.filter (apply fun . fst) (toList m)
  where
    m' = filterKeys (apply fun) m

prop_filterWithKey :: Fun (Int, Int) Bool -> IMap -> Property
prop_filterWithKey fun m =
  valid m' .&&. toList m' === Prelude.filter (apply fun) (toList m)
  where
    m' = filterWithKey (apply2 fun) m

prop_take :: Int -> Map Int Int -> Property
prop_take n xs = valid taken .&&.
                 taken === fromDistinctAscList (List.take n (toList xs))
  where
    taken = take n xs

prop_drop :: Int -> Map Int Int -> Property
prop_drop n xs = valid dropped .&&.
                 dropped === fromDistinctAscList (List.drop n (toList xs))
  where
    dropped = drop n xs

prop_splitAt :: Int -> Map Int Int -> Property
prop_splitAt n xs = valid taken .&&.
                    valid dropped .&&.
                    taken === take n xs .&&.
                    dropped === drop n xs
  where
    (taken, dropped) = splitAt n xs

prop_takeWhileAntitone :: [(Either Int Int, Int)] -> Property
prop_takeWhileAntitone xs' = valid tw .&&. (tw === filterWithKey (\k _ -> isLeft k) xs)
  where
    xs = fromList xs'
    tw = takeWhileAntitone isLeft xs

prop_dropWhileAntitone :: [(Either Int Int, Int)] -> Property
prop_dropWhileAntitone xs' = valid tw .&&. (tw === filterWithKey (\k _ -> not (isLeft k)) xs)
  where
    xs = fromList xs'
    tw = dropWhileAntitone isLeft xs

prop_spanAntitone :: [(Either Int Int, Int)] -> Property
prop_spanAntitone xs' = valid tw .&&. valid dw
                        .&&. (tw === takeWhileAntitone isLeft xs)
                        .&&. (dw === dropWhileAntitone isLeft xs)
  where
    xs = fromList xs'
    (tw, dw) = spanAntitone isLeft xs

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

prop_partition :: Fun Int Bool -> [(Int, Int)] -> Property
prop_partition p ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      m  = fromList xs
  in  partition (apply p) m == let (a,b) = (List.partition (apply p . snd) xs) in (fromList a, fromList b)

prop_map :: Fun Int Int -> [(Int, Int)] -> Property
prop_map f ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      m  = fromList xs
  in  map (apply f) m == fromList [ (a, apply f b) | (a,b) <- xs ]

prop_fmap :: Fun Int Int -> [(Int, Int)] -> Property
prop_fmap f ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      m  = fromList xs
  in  fmap (apply f) m == fromList [ (a, (apply f) b) | (a,b) <- xs ]

prop_mapKeys :: Fun Int Int -> Map Int A -> Property
prop_mapKeys f m =
  valid m' .&&.
  m' === fromList [(applyFun f k, x) | (k,x) <- toList m]
  where
    m' = mapKeys (applyFun f) m

prop_mapKeysWith :: Fun (A, A) A -> Fun Int Int -> Map Int A -> Property
prop_mapKeysWith f g m =
  valid m' .&&.
  m' === fromListWith (applyFun2 f) [(applyFun g k, x) | (k,x) <- toList m]
  where
    m' = mapKeysWith (applyFun2 f) (applyFun g) m

prop_mapKeysMonotonic :: Positive Integer -> Integer -> Map Int A -> Property
prop_mapKeysMonotonic (Positive p) q m =
  valid m' .&&.
  toList m' == [(f k, x) | (k,x) <- toList m]
  where
    m' = mapKeysMonotonic f m
    f x = fromIntegral x * p + q

prop_splitModel :: Int -> [(Int, Int)] -> Property
prop_splitModel n ys = length ys > 0 ==>
  let xs = List.nubBy ((==) `on` fst) ys
      (l, r) = split n $ fromList xs
  in  toAscList l == sort [(k, v) | (k,v) <- xs, k < n] &&
      toAscList r == sort [(k, v) | (k,v) <- xs, k > n]

prop_fold :: Map Int A -> Property
prop_fold = \m -> Foldable.fold (f <$> m) === Foldable.fold (f <$> elems m)
  where
    f v = [v]

prop_foldMap :: Map Int A -> Property
prop_foldMap = \m -> Foldable.foldMap f m === Foldable.foldMap f (elems m)
  where
    f v = [v]

prop_foldMapWithKey :: Map Int A -> Property
prop_foldMapWithKey = \m -> foldMapWithKey (curry f) m === Foldable.foldMap f (toList m)
  where
    f kv = [kv]

-- elems is implemented in terms of foldr, so we don't want to rely on it
-- when we're trying to test foldr.
prop_foldr :: Fun (A, B) B -> B -> [(Int, A)] -> Property
prop_foldr c n ys = foldr c' n m === Foldable.foldr c' n (snd <$> xs)
  where
    c' = curry (apply c)
    xs = List.sortBy (comparing fst) (List.nubBy ((==) `on` fst) ys)
    m  = fromList xs


-- toList is implemented in terms of foldrWithKey, so we don't want to rely on it
-- when we're trying to test foldrWithKey.
prop_foldrWithKey :: Fun (Int, A, B) B -> B -> [(Int, A)] -> Property
prop_foldrWithKey c n ys = foldrWithKey c' n m === Foldable.foldr (uncurry c') n xs
  where
    c' k v acc = apply c (k, v, acc)
    xs = List.sortBy (comparing fst) (List.nubBy ((==) `on` fst) ys)
    m  = fromList xs

prop_foldr' :: Fun (A, B) B -> B -> Map Int A -> Property
prop_foldr' c n m = foldr' c' n m === Foldable.foldr' c' n (elems m)
  where
    c' = curry (apply c)

prop_foldrWithKey' :: Fun (Int, A, B) B -> B -> Map Int A -> Property
prop_foldrWithKey' c n m = foldrWithKey' c' n m === Foldable.foldr' (uncurry c') n (toList m)
  where
    c' k v acc = apply c (k, v, acc)

prop_foldl :: Fun (B, A) B -> B -> Map Int A -> Property
prop_foldl c n m = foldl c' n m === Foldable.foldl c' n (elems m)
  where
    c' = curry (apply c)

prop_foldlWithKey :: Fun (B, Int, A) B -> B -> Map Int A -> Property
prop_foldlWithKey c n m = foldlWithKey c' n m === Foldable.foldl (uncurry . c') n (toList m)
  where
    c' acc k v = apply c (acc, k, v)

prop_foldl' :: Fun (B, A) B -> B -> Map Int A -> Property
prop_foldl' c n m = foldl' c' n m === Foldable.foldl' c' n (elems m)
  where
    c' = curry (apply c)

prop_foldlWithKey' :: Fun (B, Int, A) B -> B -> Map Int A -> Property
prop_foldlWithKey' c n m = foldlWithKey' c' n m === Foldable.foldl' (uncurry . c') n (toList m)
  where
    c' acc k v = apply c (acc, k, v)

prop_bifold :: Map Int Int -> Property
prop_bifold m = Bifoldable.bifold (mapKeys (:[]) ((:[]) <$> m)) === Foldable.fold ((\(k,v) -> [k,v]) <$> toList m)

prop_bifoldMap :: Map Int Int -> Property
prop_bifoldMap m = Bifoldable.bifoldMap (:[]) (:[]) m === Foldable.foldMap (\(k,v) -> [k,v]) (toList m)

prop_bifoldr :: Fun (Int, B) B -> Fun (A, B) B -> B -> Map Int A -> Property
prop_bifoldr ck cv n m = Bifoldable.bifoldr ck' cv' n m === Foldable.foldr c' n (toList m)
  where
    ck' = curry (apply ck)
    cv' = curry (apply cv)
    (k,v) `c'` acc = k `ck'` (v `cv'` acc)

prop_bifoldr' :: Fun (Int, B) B -> Fun (A, B) B -> B -> Map Int A -> Property
prop_bifoldr' ck cv n m = Bifoldable.bifoldr' ck' cv' n m === Foldable.foldr' c' n (toList m)
  where
    ck' = curry (apply ck)
    cv' = curry (apply cv)
    (k,v) `c'` acc = k `ck'` (v `cv'` acc)

prop_bifoldl :: Fun (B, Int) B -> Fun (B, A) B -> B -> Map Int A -> Property
prop_bifoldl ck cv n m = Bifoldable.bifoldl ck' cv' n m === Foldable.foldl c' n (toList m)
  where
    ck' = curry (apply ck)
    cv' = curry (apply cv)
    acc `c'` (k,v) = (acc `ck'` k) `cv'` v

prop_bifoldl' :: Fun (B, Int) B -> Fun (B, A) B -> B -> Map Int A -> Property
prop_bifoldl' ck cv n m = Bifoldable.bifoldl' ck' cv' n m === Foldable.foldl' c' n (toList m)
  where
    ck' = curry (apply ck)
    cv' = curry (apply cv)
    acc `c'` (k,v) = (acc `ck'` k) `cv'` v

prop_keysSet :: [OrdA] -> Property
prop_keysSet keys =
  keysSet (fromList (fmap (, ()) keys)) === Set.fromList keys

prop_argSet :: [(OrdA, B)] -> Property
prop_argSet xs =
  argSet (fromList xs) === Set.fromList (List.map (uncurry Arg) xs)

prop_fromSet :: [OrdA] -> Fun OrdA B -> Property
prop_fromSet keys funF =
  let f = apply funF
      m = fromSet f (Set.fromList keys)
  in
    valid m .&&.
    m === fromList (fmap (id &&& f) keys)

prop_fromSetA_action_order :: [OrdA] -> Fun OrdA B -> Property
prop_fromSetA_action_order keys funF =
  let set = Set.fromList keys
      setList = Set.toList set
      f = apply funF
      action = \k ->
        let v = f k
        in tell [v] $> v
      (writtenMap, writtenOutput) = runWriter (fromSetA action set)
  in
    valid writtenMap .&&.
    writtenOutput === List.map f setList .&&.
    toList writtenMap === fmap (id &&& f) setList

prop_fromArgSet :: [(OrdA, B)] -> Property
prop_fromArgSet ys =
  fromArgSet (Set.fromList $ List.map (uncurry Arg) ys) === fromList ys

prop_eq :: Map Int A -> Map Int A -> Property
prop_eq m1 m2 = (m1 == m2) === (toList m1 == toList m2)

prop_compare :: Map Int OrdA -> Map Int OrdA -> Property
prop_compare m1 m2 = compare m1 m2 === compare (toList m1) (toList m2)

prop_insert :: Int -> A -> Map Int A -> Property
prop_insert k x m = valid m' .&&. toList m' === kxs'
  where
    m' = insert k x m
    kxs = toList m
    kxs' = List.insertBy (comparing fst) (k,x) $
           maybe kxs (\y -> kxs List.\\ [(k,y)]) (List.lookup k kxs)

prop_delete :: Int -> Map Int A -> Property
prop_delete k m = valid m' .&&. toList m' === kxs'
  where
    m' = delete k m
    kxs = toList m
    kxs' = maybe kxs (\v -> kxs List.\\ [(k,v)]) (List.lookup k kxs)

prop_insertWith :: Fun (A, A) A -> Int -> A -> Map Int A -> Property
prop_insertWith f k x m = valid m' .&&. toList m' === kxs'
  where
    m' = insertWith (applyFun2 f) k x m
    kxs = toList m
    kxs' = if k `List.elem` (fmap fst kxs)
           then let g (k',x') = (k', if k == k' then applyFun2 f x x' else x')
                in fmap g kxs
           else List.insertBy (comparing fst) (k,x) kxs

prop_insertWithKey :: Fun (Int, A, A) A -> Int -> A -> Map Int A -> Property
prop_insertWithKey f k x m = valid m' .&&. toList m' === kxs'
  where
    m' = insertWithKey (applyFun3 f) k x m
    kxs = toList m
    kxs' = if k `List.elem` (fmap fst kxs)
           then let g (k',x') = (k', if k == k' then applyFun3 f k x x' else x')
                in fmap g kxs
           else List.insertBy (comparing fst) (k,x) kxs

prop_insertLookupWithKey
  :: Fun (Int, A, A) A -> Int -> A -> Map Int A -> Property
prop_insertLookupWithKey f k x m =
  valid m' .&&.
  r === (lookup k m, insertWith (applyFun3 f k) k x m)
  where
    r@(_, m') = insertLookupWithKey (applyFun3 f) k x m

prop_adjust :: Fun A A -> Int -> Map Int A -> Property
prop_adjust f k m =
  valid m' .&&.
  m' === maybe m (\x -> insert k (applyFun f x) m) (lookup k m)
  where
    m' = adjust (applyFun f) k m

prop_adjustWithKey :: Fun (Int, A) A -> Int -> Map Int A -> Property
prop_adjustWithKey f k m =
  valid m' .&&.
  m' === maybe m (\x -> insert k (applyFun2 f k x) m) (lookup k m)
  where
    m' = adjustWithKey (applyFun2 f) k m

prop_update :: Fun A (Maybe A) -> Int -> Map Int A -> Property
prop_update f k m = valid m' .&&. m' === m''
  where
    m' = update (applyFun f) k m
    m'' = case lookup k m of
      Nothing -> m
      Just x -> case applyFun f x of
        Nothing -> delete k m
        Just x' -> insert k x' m

prop_updateWithKey :: Fun (Int, A) (Maybe A) -> Int -> Map Int A -> Property
prop_updateWithKey f k m = valid m' .&&. m' === m''
  where
    m' = updateWithKey (applyFun2 f) k m
    m'' = case lookup k m of
      Nothing -> m
      Just x -> case applyFun2 f k x of
        Nothing -> delete k m
        Just x' -> insert k x' m

prop_upsert :: Fun (Maybe A) A -> Int -> Map Int A -> Property
prop_upsert f k m = valid m' .&&. m' == m''
  where
    m' = upsert (applyFun f) k m
    m'' = insert k (applyFun f (lookup k m)) m

prop_updateLookupWithKey
  :: Fun (Int, A) (Maybe A) -> Int -> Map Int A -> Property
prop_updateLookupWithKey f k m =
   valid m' .&&. r === (lookup k m' <|> lookup k m, m'')
  where
    r@(_, m') = updateLookupWithKey (applyFun2 f) k m
    m'' = updateWithKey (applyFun2 f) k m

prop_elemAt :: Int -> Map Int A -> Property
prop_elemAt i m = 0 <= i && i < size m ==>
  elemAt i m === toList m !! i

prop_updateAt :: Fun (Int, A) (Maybe A) -> Int -> Map Int A -> Property
prop_updateAt f i m = 0 <= i && i < size m ==>
  valid m' .&&.
  toList m' ===
    [ (k, x')
    | (j, (k, x)) <- zip [0..] (toList m)
    , x' <- if i == j then Foldable.toList (applyFun2 f k x) else [x]
    ]
  where
    m' = updateAt (applyFun2 f) i m

prop_deleteAt :: Int -> Map Int A -> Property
prop_deleteAt i m = 0 <= i && i < size m ==>
  valid m' .&&.
  toList m' === [kx | (j, kx) <- zip [0..] (toList m), i /= j]
  where
    m' = deleteAt i m

prop_updateMin :: Fun A (Maybe A) -> Map Int A -> Property
prop_updateMin f m =
  valid m' .&&.
  toList m' === case toList m of
    [] -> []
    (k,x):xs -> Foldable.toList ((,) k <$> applyFun f x) ++ xs
  where
    m' = updateMin (applyFun f) m

prop_updateMax :: Fun A (Maybe A) -> Map Int A -> Property
prop_updateMax f m =
  valid m' .&&.
  toDescList m' === case toDescList m of
    [] -> []
    (k,x):xs -> Foldable.toList ((,) k <$> applyFun f x) ++ xs
  where
    m' = updateMax (applyFun f) m

prop_updateMinWithKey :: Fun (Int, A) (Maybe A) -> Map Int A -> Property
prop_updateMinWithKey f m =
  valid m' .&&.
  toList m' === case toList m of
    [] -> []
    (k,x):xs -> Foldable.toList ((,) k <$> applyFun2 f k x) ++ xs
  where
    m' = updateMinWithKey (applyFun2 f) m

prop_updateMaxWithKey :: Fun (Int, A) (Maybe A) -> Map Int A -> Property
prop_updateMaxWithKey f m =
  valid m' .&&.
  toDescList m' === case toDescList m of
    [] -> []
    (k,x):xs -> Foldable.toList ((,) k <$> applyFun2 f k x) ++ xs
  where
    m' = updateMaxWithKey (applyFun2 f) m

prop_minViewWithKey :: Map Int A -> Property
prop_minViewWithKey m = case minViewWithKey m of
  Nothing -> property $ null m
  Just ((k,x),m') -> valid m .&&. toList m === (k,x) : toList m'

prop_maxViewWithKey :: Map Int A -> Property
prop_maxViewWithKey m = case maxViewWithKey m of
  Nothing -> property $ null m
  Just ((k,x),m') -> valid m .&&. toDescList m === (k,x) : toDescList m'

prop_minView :: Map Int A -> Property
prop_minView m = case minView m of
  Nothing -> property $ null m
  Just (x,m') -> valid m' .&&. kxs === (minKey, x) : toList m'
    where
      kxs = toList m
      minKey = minimum (fmap fst kxs)

prop_maxView :: Map Int A -> Property
prop_maxView m = case maxView m of
  Nothing -> property $ null m
  Just (x,m') -> valid m' .&&. kxs === (maxKey, x) : toDescList m'
    where
      kxs = toDescList m
      maxKey = maximum (fmap fst kxs)

prop_isSubmapOf :: Map Int A -> Map Int A -> Property
prop_isSubmapOf m1 m2 = isSubmapOf m1 m2 === all (`elem` kys) (toList m1)
  where
    kys = toList m2

prop_isSubmapOfBy :: Fun (A, B) Bool -> Map Int A -> Map Int B -> Property
prop_isSubmapOfBy f m1 m2 =
  isSubmapOfBy (applyFun2 f) m1 m2 ===
  all (\kx -> isJust (List.find (f' kx) kys)) (toList m1)
  where
    kys = toList m2
    f' (k1,x) (k2,y) = k1 == k2 && applyFun2 f x y

prop_isProperSubmapOf :: Map Int A -> Map Int A -> Property
prop_isProperSubmapOf m1 m2 =
  isProperSubmapOf m1 m2 ===
  (size m1 < size m2 && isSubmapOf m1 m2)

prop_isProperSubmapOfBy :: Fun (A, B) Bool -> Map Int A -> Map Int B -> Property
prop_isProperSubmapOfBy f m1 m2 =
  isProperSubmapOfBy (applyFun2 f) m1 m2 ===
  (size m1 < size m2 && isSubmapOfBy (applyFun2 f) m1 m2)

prop_differenceWith
  :: Fun (A, B) (Maybe A) -> Map Int A -> Map Int B -> Property
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
  :: Fun (Int, A, B) (Maybe A) -> Map Int A -> Map Int B -> Property
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

prop_partitionWithKey :: Fun (Int, A) Bool -> Map Int A -> Property
prop_partitionWithKey f m =
  valid m1 .&&.
  valid m2 .&&.
  (toList m1, toList m2) === List.partition (applyFun f) (toList m)
  where
    (m1, m2) = partitionWithKey (applyFun2 f) m

prop_mapMaybe :: Fun A (Maybe B) -> Map Int A -> Property
prop_mapMaybe f m =
  valid m' .&&.
  toList m' === Maybe.mapMaybe (\(k,x) -> (,) k <$> applyFun f x) (toList m)
  where
    m' = mapMaybe (applyFun f) m

prop_mapMaybeWithKey :: Fun (Int, A) (Maybe B) -> Map Int A -> Property
prop_mapMaybeWithKey f m =
  valid m' .&&.
  toList m' === Maybe.mapMaybe (\(k,x) -> (,) k <$> applyFun2 f k x) (toList m)
  where
    m' = mapMaybeWithKey (applyFun2 f) m

prop_traverseMaybeWithKey :: Fun (Int, A) (Maybe B) -> Map Int A -> Property
prop_traverseMaybeWithKey f m =
  valid m' .&&.
  toList m' ===
    Maybe.mapMaybe (\(k,x) -> (,) k <$> applyFun2 f k x) (toList m) .&&.
  kxs === toList m
  where
    (m', kxs) = runWriter (traverseMaybeWithKey f' m)
    f' k x = applyFun2 f k x <$ tell [(k,x)]

prop_mapEither :: Fun A (Either B C) -> Map Int A -> Property
prop_mapEither f m =
  valid m1 .&&.
  valid m2 .&&.
  (toList m1, toList m2) === Either.partitionEithers (List.map f' (toList m))
  where
    (m1, m2) = mapEither (applyFun f) m
    f' (k,x) = case applyFun f x of
      Left y -> Left (k,y)
      Right z -> Right (k,z)

prop_mapEitherWithKey :: Fun (Int, A) (Either B C) -> Map Int A -> Property
prop_mapEitherWithKey f m =
  valid m1 .&&.
  valid m2 .&&.
  (toList m1, toList m2) === Either.partitionEithers (List.map f' (toList m))
  where
    (m1, m2) = mapEitherWithKey (applyFun2 f) m
    f' (k,x) = case applyFun2 f k x of
      Left y -> Left (k,y)
      Right z -> Right (k,z)

prop_mapWithKey :: Fun (Int, A) B -> Map Int A -> Property
prop_mapWithKey f m =
  valid m' .&&.
  toList m' === List.map (\(k,x) -> (k, applyFun2 f k x)) (toList m)
  where
    m' = mapWithKey (applyFun2 f) m

prop_traverse :: Fun A B -> Map Int A -> Property
prop_traverse f m =
  valid m' .&&.
  toList m' === List.map (\(k,x) -> (k, applyFun f x)) (toList m) .&&.
  xs === elems m
  where
    (m', xs) = runWriter (traverse f' m)
    f' x = applyFun f x <$ tell [x]

prop_traverseWithKey :: Fun (Int, A) B -> Map Int A -> Property
prop_traverseWithKey f m =
  valid m' .&&.
  toList m' === List.map (\(k,x) -> (k, applyFun2 f k x)) (toList m) .&&.
  kxs === toList m
  where
    (m', kxs) = runWriter (traverseWithKey f' m)
    f' k x = applyFun2 f k x <$ tell [(k,x)]

prop_mapAccum :: Fun (A, B) (A, C) -> A -> Map Int B -> Property
prop_mapAccum f z0 m =
  valid m' .&&.
  (z, toList m') === List.mapAccumL f' z0 (toList m)
  where
    (z, m') = mapAccum (applyFun2 f) z0 m
    f' z' (k,x) = (,) k <$> applyFun2 f z' x

prop_mapAccumWithKey :: Fun (A, Int, B) (A, C) -> A -> Map Int B -> Property
prop_mapAccumWithKey f z0 m =
  valid m' .&&.
  (z, toList m') === List.mapAccumL f' z0 (toList m)
  where
    (z, m') = mapAccumWithKey (applyFun3 f) z0 m
    f' z' (k,x) = (,) k <$> applyFun3 f z' k x

prop_mapAccumRWithKey :: Fun (A, Int, B) (A, C) -> A -> Map Int B -> Property
prop_mapAccumRWithKey f z0 m =
  valid m' .&&.
  (z, toList m') === List.mapAccumR f' z0 (toList m)
  where
    (z, m') = mapAccumRWithKey (applyFun3 f) z0 m
    f' z' (k,x) = (,) k <$> applyFun3 f z' k x
