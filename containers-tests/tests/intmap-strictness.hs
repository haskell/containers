{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import qualified Data.Foldable as F
import Data.Function (on)
import Data.Functor.Compose
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)
import Test.ChasingBottoms.IsBottom
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Data.Tuple.Solo (Solo (MkSolo), getSolo)
import Test.QuickCheck
import Test.QuickCheck.Poly (A, B, C)
import Test.QuickCheck.Function (apply)

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap.Strict (IntMap, Key)
import qualified Data.IntMap.Strict as M
import qualified Data.IntMap as L
import qualified Data.IntMap.Merge.Strict as MMerge
import Data.IntMap.Merge.Lazy (WhenMatched, WhenMissing)
import qualified Data.IntMap.Merge.Lazy as LMerge
import Data.Containers.ListUtils

import Utils.MergeFunc (WhenMatchedFunc(..), WhenMissingFunc(..))
import Utils.NubSorted (NubSortedOnFst(..))
import Utils.Strictness
  (Bot(..), Func, Func2, Func3, applyFunc, applyFunc2, applyFunc3)

instance Arbitrary v => Arbitrary (IntMap v) where
    arbitrary = M.fromList `fmap` arbitrary
    shrink = map M.fromList . shrink . M.toList

instance Arbitrary IntSet where
  arbitrary = IntSet.fromList <$> arbitrary
  shrink = map IntSet.fromList . shrink . IntSet.toList

apply2 :: Fun (a, b) c -> a -> b -> c
apply2 f a b = apply f (a, b)

apply3 :: Fun (a, b, c) d -> a -> b -> c -> d
apply3 f a b c = apply f (a, b, c)

{--------------------------------------------------------------------
  Construction property tests
--------------------------------------------------------------------}

-- See Note [Overview of construction tests] in map-strictness.hs

-- See Note [Testing with lazy functions] in map-strictness.hs

prop_strictSingleton :: Key -> Bot A -> Property
prop_strictSingleton k (Bot x) = isBottom (M.singleton k x) === isBottom x

prop_lazySingleton :: Key -> Bot A -> Property
prop_lazySingleton k (Bot x) = isNotBottomProp (L.singleton k x)

prop_strictFromSet :: Func Key (Bot A) -> IntSet -> Property
prop_strictFromSet fun set =
  isBottom (M.fromSet f set) === any (isBottom . f) (IntSet.toList set)
  where
    f = applyFunc fun

prop_strictFromSetA :: Func Key (Bot A) -> IntSet -> Property
prop_strictFromSetA fun set =
  isBottom (getSolo (M.fromSetA (MkSolo . f) set)) === any (isBottom . f) (IntSet.toList set)
  where
    f = applyFunc fun

prop_lazyFromSet :: Func Key (Bot A) -> IntSet -> Property
prop_lazyFromSet fun set = isNotBottomProp (L.fromSet f set)
  where
    f = applyFunc fun

prop_lazyFromSetA :: Func Key (Bot A) -> IntSet -> Property
prop_lazyFromSetA fun set = isNotBottomProp (getSolo (L.fromSetA f set))
  where
  f = MkSolo . applyFunc fun

prop_fromSetA_equiv_strictness :: Func Int (Bot A) -> IntSet -> Property
prop_fromSetA_equiv_strictness fun set =
  -- strict fromSetA is the same as lazy and then forcing
  bottomOn (M.fromSetA f set) (fmap forceValues (L.fromSetA f set)) .&&.
  -- strict fromSetA is the same as lazy fromSetA composed with strictly applied
  -- wrapper
  bottomOn (M.fromSetA f set) (fmap getSolo . getCompose $ L.fromSetA (Compose . fmap (MkSolo $!) . f) set)
  where
  forceValues xs = foldr (\ !_ r -> r) () xs `seq` xs
  bottomOn = on (===) (isBottom . getSolo)
  f = MkSolo . applyFunc fun

prop_strictFromList :: [(Key, Bot A)] -> Property
prop_strictFromList kvs =
  isBottom (M.fromList kvs') === any (isBottom . snd) kvs'
  where
    kvs' = coerce kvs :: [(Key, A)]

prop_lazyFromList :: [(Key, Bot A)] -> Property
prop_lazyFromList kvs = isNotBottomProp (L.fromList kvs')
  where
    kvs' = coerce kvs :: [(Key, A)]

prop_strictFromListWith :: Func2 A A (Bot A) -> [(Key, Bot A)] -> Property
prop_strictFromListWith fun kvs =
  isBottom (M.fromListWith f kvs') ===
  isBottom (F.foldl' (\acc (k,x) -> M.insertWith f k x acc) M.empty kvs')
  where
    f = coerce (applyFunc2 fun)
    kvs' = coerce kvs :: [(Key, A)]

prop_lazyFromListWith :: Func2 A A (Bot A) -> [(Key, Bot A)] -> Property
prop_lazyFromListWith fun kvs = isNotBottomProp (L.fromListWith f kvs')
  where
    f = coerce (applyFunc2 fun)
    kvs' = coerce kvs :: [(Key, A)]

prop_strictFromListWithKey
  :: Func3 Key A A (Bot A) -> [(Key, Bot A)] -> Property
prop_strictFromListWithKey fun kvs =
  isBottom (M.fromListWithKey f kvs') ===
  isBottom (F.foldl' (\acc (k,x) -> M.insertWithKey f k x acc) M.empty kvs')
  where
    f = coerce (applyFunc3 fun)
    kvs' = coerce kvs :: [(Key, A)]

prop_lazyFromListWithKey
  :: Func3 Key A A (Bot A) -> [(Key, Bot A)] -> Property
prop_lazyFromListWithKey fun kvs = isNotBottomProp (L.fromListWithKey f kvs')
  where
    f = coerce (applyFunc3 fun)
    kvs' = coerce kvs :: [(Key, A)]

prop_strictFromAscList :: [(Key, Bot A)] -> Property
prop_strictFromAscList kvs =
  isBottom (M.fromAscList kvs') === isBottom (M.fromList kvs')
  where
    kvs' = List.sortBy (comparing fst) (coerce kvs) :: [(Key, A)]

prop_lazyFromAscList :: [(Key, Bot A)] -> Property
prop_lazyFromAscList kvs = isNotBottomProp (L.fromAscList kvs')
  where
    kvs' = List.sortBy (comparing fst) (coerce kvs) :: [(Key, A)]

prop_strictFromAscListWith :: Func2 A A (Bot A) -> [(Key, Bot A)] -> Property
prop_strictFromAscListWith fun kvs =
  isBottom (M.fromAscListWith f kvs') === isBottom (M.fromListWith f kvs')
  where
    f = coerce (applyFunc2 fun)
    kvs' = List.sortBy (comparing fst) (coerce kvs) :: [(Key, A)]

prop_lazyFromAscListWith :: Func2 A A (Bot A) -> [(Key, Bot A)] -> Property
prop_lazyFromAscListWith fun kvs = isNotBottomProp (L.fromAscListWith f kvs')
  where
    f = coerce (applyFunc2 fun)
    kvs' = List.sortBy (comparing fst) (coerce kvs) :: [(Key, A)]

prop_strictFromAscListWithKey
  :: Func3 Key A A (Bot A) -> [(Key, Bot A)] -> Property
prop_strictFromAscListWithKey fun kvs =
  isBottom (M.fromAscListWithKey f kvs') ===
  isBottom (M.fromListWithKey f kvs')
  where
    f = coerce (applyFunc3 fun)
    kvs' = List.sortBy (comparing fst) (coerce kvs) :: [(Key, A)]

prop_lazyFromAscListWithKey
  :: Func3 Key A A (Bot A) -> [(Key, Bot A)] -> Property
prop_lazyFromAscListWithKey fun kvs =
  isNotBottomProp (L.fromAscListWithKey f kvs')
  where
    f = coerce (applyFunc3 fun)
    kvs' = List.sortBy (comparing fst) (coerce kvs) :: [(Key, A)]

prop_strictFromDistinctAscList :: [(Key, Bot A)] -> Property
prop_strictFromDistinctAscList kvs =
  isBottom (M.fromDistinctAscList kvs') === isBottom (M.fromList kvs')
  where
    kvs' = uniqOn fst $ List.sortBy (comparing fst) (coerce kvs) :: [(Key, A)]

prop_lazyFromDistinctAscList :: [(Key, Bot A)] -> Property
prop_lazyFromDistinctAscList kvs = isNotBottomProp (L.fromDistinctAscList kvs')
  where
    kvs' = uniqOn fst $ List.sortBy (comparing fst) (coerce kvs) :: [(Key, A)]

prop_strictInsert :: Key -> Bot A -> IntMap A -> Property
prop_strictInsert k (Bot x) m = isBottom (M.insert k x m) === isBottom x

prop_lazyInsert :: Key -> Bot A -> IntMap A -> Property
prop_lazyInsert k (Bot x) m = isNotBottomProp (L.insert k x m)

prop_strictInsertWith
  :: Func2 A A (Bot A)
  -> Key
  -> Bot A
  -> IntMap A
  -> Property
prop_strictInsertWith fun k (Bot x) m =
  isBottom (M.insertWith f k x m) === isBottom (M.insertWithKey (const f) k x m)
  where
    f = coerce (applyFunc2 fun)

prop_lazyInsertWith
  :: Func2 A A (Bot A)
  -> Key
  -> Bot A
  -> IntMap A
  -> Property
prop_lazyInsertWith fun k (Bot x) m = isNotBottomProp (L.insertWith f k x m)
  where
    f = coerce (applyFunc2 fun)

prop_strictInsertWithKey
  :: Func3 Key A A (Bot A)
  -> Key
  -> Bot A
  -> IntMap A
  -> Property
prop_strictInsertWithKey fun k (Bot x) m =
  isBottom (M.insertWithKey f k x m) ===
  isBottom (maybe x (f k x) (M.lookup k m))
  where
    f = coerce (applyFunc3 fun)

prop_lazyInsertWithKey
  :: Func3 Key A A (Bot A)
  -> Key
  -> Bot A
  -> IntMap A
  -> Property
prop_lazyInsertWithKey fun k (Bot x) m =
  isNotBottomProp (L.insertWithKey f k x m)
  where
    f = coerce (applyFunc3 fun)

prop_strictInsertLookupWithKey
  :: Func3 Key A A (Bot A)
  -> Key
  -> Bot A
  -> IntMap A
  -> Property
prop_strictInsertLookupWithKey fun k (Bot x) m =
  isBottom (M.insertLookupWithKey f k x m) ===
  isBottom (maybe x (f k x) (M.lookup k m))
  where
    f = coerce (applyFunc3 fun)

prop_lazyInsertLookupWithKey
  :: Func3 Key A A (Bot A)
  -> Key
  -> Bot A
  -> IntMap A
  -> Property
prop_lazyInsertLookupWithKey fun k (Bot x) m =
  isNotBottomProp (L.insertLookupWithKey f k x m)
  where
    f = coerce (applyFunc3 fun)

prop_strictAdjust :: Func A (Bot A) -> Key -> IntMap A -> Property
prop_strictAdjust fun k m =
  isBottom (M.adjust f k m) === isBottom (M.alter (fmap f) k m)
  where
    f = coerce (applyFunc fun)

prop_lazyAdjust :: Func A (Bot A) -> Key -> IntMap A -> Property
prop_lazyAdjust fun k m = isNotBottomProp (L.adjust f k m)
  where
    f = coerce (applyFunc fun)

prop_strictAdjustWithKey
  :: Func2 Key A (Bot A) -> Key -> IntMap A -> Property
prop_strictAdjustWithKey fun k m =
 isBottom (M.adjustWithKey f k m) === isBottom (M.alter (fmap (f k)) k m)
  where
    f = coerce (applyFunc2 fun)

prop_lazyAdjustWithKey :: Func2 Key A (Bot A) -> Key -> IntMap A -> Property
prop_lazyAdjustWithKey fun k m = isNotBottomProp (L.adjustWithKey f k m)
  where
    f = coerce (applyFunc2 fun)

prop_strictUpdate :: Func A (Maybe (Bot A)) -> Key -> IntMap A -> Property
prop_strictUpdate fun k m =
  isBottom (M.update f k m) === isBottom (M.alter (>>= f) k m)
  where
    f = coerce (applyFunc fun)

prop_lazyUpdate :: Func A (Maybe (Bot A)) -> Key -> IntMap A -> Property
prop_lazyUpdate fun k m = isNotBottomProp (L.update f k m)
  where
    f = coerce (applyFunc fun)

prop_strictUpdateWithKey
  :: Func2 Key A (Maybe (Bot A)) -> Key -> IntMap A -> Property
prop_strictUpdateWithKey fun k m =
  isBottom (M.updateWithKey f k m) === isBottom (M.alter (>>= f k) k m)
  where
    f = coerce (applyFunc2 fun)

prop_lazyUpdateWithKey
  :: Func2 Key A (Maybe (Bot A)) -> Key -> IntMap A -> Property
prop_lazyUpdateWithKey fun k m = isNotBottomProp (L.updateWithKey f k m)
  where
    f = coerce (applyFunc2 fun)

prop_strictUpsert :: Func (Maybe A) (Bot A) -> Int -> IntMap A -> Property
prop_strictUpsert fun k m =
  isBottom (M.upsert f k m) === isBottom (M.alter (Just . f) k m)
  where
    f = coerce (applyFunc fun)

prop_lazyUpsert :: Func (Maybe A) (Bot A) -> Int -> IntMap A -> Property
prop_lazyUpsert fun k m = isNotBottomProp (L.upsert f k m)
  where
    f = coerce (applyFunc fun)

prop_strictUpdateLookupWithKey
  :: Func2 Key A (Maybe (Bot A)) -> Key -> IntMap A -> Property
prop_strictUpdateLookupWithKey fun k m =
  isBottom (M.updateLookupWithKey f k m) === isBottom (M.updateWithKey f k m)
  where
    f = coerce (applyFunc2 fun)

prop_lazyUpdateLookupWithKey
  :: Func2 Key A (Maybe (Bot A)) -> Key -> IntMap A -> Property
prop_lazyUpdateLookupWithKey fun k m =
  isNotBottomProp (L.updateLookupWithKey f k m)
  where
    f = coerce (applyFunc2 fun)

prop_strictAlter
  :: Func (Maybe A) (Maybe (Bot A)) -> Key -> IntMap A -> Property
prop_strictAlter fun k m =
  isBottom (M.alter f k m) ===
  maybe False isBottom (f (M.lookup k m))
  where
    f = coerce (applyFunc fun)

prop_lazyAlter
  :: Func (Maybe A) (Maybe (Bot A)) -> Key -> IntMap A -> Property
prop_lazyAlter fun k m = isNotBottomProp (L.alter f k m)
  where
    f = coerce (applyFunc fun)

prop_strictAlterF
  :: Func (Maybe A) (Identity (Maybe (Bot A))) -> Key -> IntMap A -> Property
prop_strictAlterF fun k m =
  isBottom (runIdentity (M.alterF f k m)) ===
  maybe False isBottom (runIdentity (f (M.lookup k m)))
  where
    f = coerce (applyFunc fun)

prop_lazyAlterF
  :: Func (Maybe A) (Identity (Maybe (Bot A))) -> Key -> IntMap A -> Property
prop_lazyAlterF fun k m = isNotBottomProp (runIdentity (L.alterF f k m))
  where
    f = coerce (applyFunc fun)

prop_strictUnionWith
  :: Func2 A A (Bot A) -> IntMap A -> IntMap A -> Property
prop_strictUnionWith fun m1 m2 =
  isBottom (M.unionWith f m1 m2) ===
  isBottom (M.unionWithKey (const f) m1 m2)
  where
    f = coerce (applyFunc2 fun)

prop_lazyUnionWith :: Func2 A A (Bot A) -> IntMap A -> IntMap A -> Property
prop_lazyUnionWith fun m1 m2 = isNotBottomProp (L.unionWith f m1 m2)
  where
    f = coerce (applyFunc2 fun)

prop_strictUnionWithKey
  :: Func3 Key A A (Bot A) -> IntMap A -> IntMap A -> Property
prop_strictUnionWithKey fun m1 m2 =
  isBottom (M.unionWithKey f m1 m2) ===
  isBottom (M.foldlWithKey' (\acc k x -> M.insertWithKey f k x acc) m2 m1)
  where
    f = coerce (applyFunc3 fun)

prop_lazyUnionWithKey
  :: Func3 Key A A (Bot A) -> IntMap A -> IntMap A -> Property
prop_lazyUnionWithKey fun m1 m2 = isNotBottomProp (L.unionWithKey f m1 m2)
  where
    f = coerce (applyFunc3 fun)

prop_strictUnionsWith :: Func2 A A (Bot A) -> [IntMap A] -> Property
prop_strictUnionsWith fun ms =
  isBottom (M.unionsWith f ms) ===
  isBottom (F.foldl' (M.unionWith f) M.empty ms)
  where
    f = coerce (applyFunc2 fun)

prop_lazyUnionsWith :: Func2 A A (Bot A) -> [IntMap A] -> Property
prop_lazyUnionsWith fun ms = isNotBottomProp (L.unionsWith f ms)
  where
    f = coerce (applyFunc2 fun)

prop_strictDifferenceWith
  :: Func2 A B (Maybe (Bot A))
  -> IntMap A
  -> IntMap B
  -> Property
prop_strictDifferenceWith fun m1 m2 =
  isBottom (M.differenceWith f m1 m2) ===
  isBottom (M.differenceWithKey (const f) m1 m2)
  where
    f = coerce (applyFunc2 fun)

prop_lazyDifferenceWith
  :: Func2 A B (Maybe (Bot A))
  -> IntMap A
  -> IntMap B
  -> Property
prop_lazyDifferenceWith fun m1 m2 = isNotBottomProp (L.differenceWith f m1 m2)
  where
    f = coerce (applyFunc2 fun)

prop_strictDifferenceWithKey
  :: Func3 Key A B (Maybe (Bot A))
  -> IntMap A
  -> IntMap B
  -> Property
prop_strictDifferenceWithKey fun m1 m2 =
  isBottom (M.differenceWithKey f m1 m2) ===
  isBottom
    (M.foldlWithKey'
      (\acc k x -> M.updateWithKey (\k' y -> f k' y x) k acc)
      m1
      m2)
  where
    f = coerce (applyFunc3 fun)

prop_lazyDifferenceWithKey
  :: Func3 Key A B (Maybe (Bot A))
  -> IntMap A
  -> IntMap B
  -> Property
prop_lazyDifferenceWithKey fun m1 m2 =
  isNotBottomProp (L.differenceWithKey f m1 m2)
  where
    f = coerce (applyFunc3 fun)

prop_strictIntersectionWith
  :: Func2 A B (Bot C)
  -> IntMap A
  -> IntMap B
  -> Property
prop_strictIntersectionWith fun m1 m2 =
  isBottom (M.intersectionWith f m1 m2) ===
  isBottom (M.intersectionWithKey (const f) m1 m2)
  where
    f = coerce (applyFunc2 fun) :: A -> B -> C

prop_lazyIntersectionWith
  :: Func2 A B (Bot C)
  -> IntMap A
  -> IntMap B
  -> Property
prop_lazyIntersectionWith fun m1 m2 =
  isNotBottomProp (L.intersectionWith f m1 m2)
  where
    f = coerce (applyFunc2 fun) :: A -> B -> C

prop_strictIntersectionWithKey
  :: Func3 Key A B (Bot C)
  -> IntMap A
  -> IntMap B
  -> Property
prop_strictIntersectionWithKey fun m1 m2 =
  isBottom (M.intersectionWithKey f m1 m2) ===
  isBottom
    (M.foldlWithKey'
      (\acc k x -> case M.lookup k m2 of
        Nothing -> acc
        Just y -> M.insert k (f k x y) acc)
      M.empty
      m1)
  where
    f = coerce (applyFunc3 fun) :: Key -> A -> B -> C

prop_lazyIntersectionWithKey
  :: Func3 Key A B (Bot C)
  -> IntMap A
  -> IntMap B
  -> Property
prop_lazyIntersectionWithKey fun m1 m2 =
  isNotBottomProp (L.intersectionWithKey f m1 m2)
  where
    f = coerce (applyFunc3 fun) :: Key -> A -> B -> C

prop_strictMergeWithKey
  :: Func3 Key A B (Maybe (Bot C))
  -> Fun A (Maybe C)
  -> Fun B (Maybe C)
  -> IntMap A
  -> IntMap B
  -> Property
prop_strictMergeWithKey fun12 fun1 fun2 m1 m2 =
  isBottom (M.mergeWithKey f12 (M.mapMaybe f1) (M.mapMaybe f2) m1 m2) ===
  any isBottom
    (mapMaybe (\(k, (x, y)) -> f12 k x y) $
     M.toList $
     M.intersectionWith (,) m1 m2)
  where
    f12 = coerce (applyFunc3 fun12) :: Key -> A -> B -> Maybe C
    f1 = applyFun fun1
    f2 = applyFun fun2

prop_lazyMergeWithKey
  :: Func3 Key A B (Maybe (Bot C))
  -> Fun A (Maybe C)
  -> Fun B (Maybe C)
  -> IntMap A
  -> IntMap B
  -> Property
prop_lazyMergeWithKey fun12 fun1 fun2 m1 m2 =
  isNotBottomProp (L.mergeWithKey f12 (L.mapMaybe f1) (L.mapMaybe f2) m1 m2)
  where
    f12 = coerce (applyFunc3 fun12) :: Key -> A -> B -> Maybe C
    f1 = applyFun fun1
    f2 = applyFun fun2

prop_strictMap :: Func A (Bot B) -> IntMap A -> Property
prop_strictMap fun m =
  isBottom (M.map f m) === isBottom (M.mapWithKey (const f) m)
  where
    f = coerce (applyFunc fun) :: A -> B

prop_lazyMap :: Func A (Bot B) -> IntMap A -> Property
prop_lazyMap fun m = isNotBottomProp (L.map f m)
  where
    f = coerce (applyFunc fun) :: A -> B

prop_strictMapWithKey :: Func2 Key A (Bot B) -> IntMap A -> Property
prop_strictMapWithKey fun m =
  isBottom (M.mapWithKey f m) ===
  any (isBottom . uncurry f) (M.toList m)
  where
    f = coerce (applyFunc2 fun) :: Key -> A -> B

prop_lazyMapWithKey :: Func2 Key A (Bot B) -> IntMap A -> Property
prop_lazyMapWithKey fun m = isNotBottomProp (L.mapWithKey f m)
  where
    f = coerce (applyFunc2 fun) :: Key -> A -> B

prop_strictTraverseWithKey
  :: Func2 Key A (Identity (Bot B)) -> IntMap A -> Property
prop_strictTraverseWithKey fun m =
  isBottom (runIdentity (M.traverseWithKey f m)) ===
  any (isBottom . runIdentity . uncurry f) (M.toList m)
  where
    f = coerce (applyFunc2 fun) :: Key -> A -> Identity B

prop_lazyTraverseWithKey
  :: Func2 Key A (Identity (Bot B)) -> IntMap A -> Property
prop_lazyTraverseWithKey fun m =
  isNotBottomProp (runIdentity (L.traverseWithKey f m))
  where
    f = coerce (applyFunc2 fun) :: Key -> A -> Identity B

prop_strictTraverseMaybeWithKey
  :: Func2 Key A (Identity (Maybe (Bot B))) -> IntMap A -> Property
prop_strictTraverseMaybeWithKey fun m =
  isBottom (runIdentity (M.traverseMaybeWithKey f m)) ===
  any (maybe False isBottom . runIdentity . uncurry f) (M.toList m)
  where
    f = coerce (applyFunc2 fun) :: Key -> A -> Identity (Maybe B)

prop_lazyTraverseMaybeWithKey
  :: Func2 Key A (Identity (Maybe (Bot B))) -> IntMap A -> Property
prop_lazyTraverseMaybeWithKey fun m =
  isNotBottomProp (runIdentity (L.traverseMaybeWithKey f m))
  where
    f = coerce (applyFunc2 fun) :: Key -> A -> Identity (Maybe B)

prop_strictMapAccum :: Func2 A B (A, Bot C) -> A -> IntMap B -> Property
prop_strictMapAccum fun z m =
  isBottom (snd (M.mapAccum f z m)) ===
  isBottom (snd (M.mapAccumWithKey (\acc _ -> f acc) z m))
  where
    f = coerce (applyFunc2 fun) :: A -> B -> (A, C)

prop_lazyMapAccum :: Func2 A B (A, Bot C) -> A -> IntMap B -> Property
prop_lazyMapAccum fun z m = isNotBottomProp (snd (L.mapAccum f z m))
  where
    f = coerce (applyFunc2 fun) :: A -> B -> (A, C)

prop_strictMapAccumWithKey
  :: Func3 A Key B (A, Bot C) -> A -> IntMap B -> Property
prop_strictMapAccumWithKey fun z m =
  isBottom (snd (M.mapAccumWithKey f z m)) ===
  isBottom (M.fromList $ snd $
    List.mapAccumL (\z' (k, x) -> fmap ((,) k) (f z' k x)) z (M.toList m))
  where
    f = coerce (applyFunc3 fun) :: A -> Key -> B -> (A, C)

prop_lazyMapAccumWithKey
  :: Func3 A Key B (A, Bot C) -> A -> IntMap B -> Property
prop_lazyMapAccumWithKey fun z m =
  isNotBottomProp (snd (L.mapAccumWithKey f z m))
  where
    f = coerce (applyFunc3 fun) :: A -> Key -> B -> (A, C)

prop_strictMapAccumRWithKey
  :: Func3 A Key B (A, Bot C) -> A -> IntMap B -> Property
prop_strictMapAccumRWithKey fun z m =
  isBottom (snd (M.mapAccumRWithKey f z m)) ===
  isBottom (M.fromList $ snd $
    List.mapAccumR (\z' (k, x) -> fmap ((,) k) (f z' k x)) z (M.toList m))
  where
    f = coerce (applyFunc3 fun) :: A -> Key -> B -> (A, C)

prop_lazyMapAccumRWithKey
  :: Func3 A Key B (A, Bot C) -> A -> IntMap B -> Property
prop_lazyMapAccumRWithKey fun z m =
  isNotBottomProp (snd (L.mapAccumRWithKey f z m))
  where
    f = coerce (applyFunc3 fun) :: A -> Key -> B -> (A, C)

prop_strictMapKeysWith
  :: Func2 A A (Bot A) -> Func Key Key -> IntMap A -> Property
prop_strictMapKeysWith fun kfun m =
  isBottom (M.mapKeysWith f kf m) ===
  isBottom (M.fromListWith f $ map (\(k, x) -> (kf k, x)) $ M.toList m)
  where
    f = coerce (applyFunc2 fun) :: A -> A -> A
    kf = applyFunc kfun

prop_lazyMapKeysWith
  :: Func2 A A (Bot A) -> Func Key Key -> IntMap A -> Property
prop_lazyMapKeysWith fun kfun m = isNotBottomProp (L.mapKeysWith f kf m)
  where
    f = coerce (applyFunc2 fun) :: A -> A -> A
    kf = applyFunc kfun

prop_strictMapMaybe :: Func A (Maybe (Bot B)) -> IntMap A -> Property
prop_strictMapMaybe fun m =
  isBottom (M.mapMaybe f m) === isBottom (M.mapMaybeWithKey (const f) m)
  where
    f = coerce (applyFunc fun) :: A -> Maybe B

prop_lazyMapMaybe :: Func A (Maybe (Bot B)) -> IntMap A -> Property
prop_lazyMapMaybe fun m = isNotBottomProp (L.mapMaybe f m)
  where
    f = coerce (applyFunc fun) :: A -> Maybe B

prop_strictMapMaybeWithKey
  :: Func2 Key A (Maybe (Bot B)) -> IntMap A -> Property
prop_strictMapMaybeWithKey fun m =
  isBottom (M.mapMaybeWithKey f m) ===
  isBottom (M.fromList $ mapMaybe (\(k, x) -> ((,) k) <$> f k x) $ M.toList m)
  where
    f = coerce (applyFunc2 fun) :: Key -> A -> Maybe B

prop_lazyMapMaybeWithKey
  :: Func2 Key A (Maybe (Bot B)) -> IntMap A -> Property
prop_lazyMapMaybeWithKey fun m = isNotBottomProp (L.mapMaybeWithKey f m)
  where
    f = coerce (applyFunc2 fun) :: Key -> A -> Maybe B

prop_strictMapEither
  :: Func A (Either (Bot B) (Bot C)) -> IntMap A -> Property
prop_strictMapEither fun m =
  isBottom (M.mapEither f m) ===
  isBottom (M.mapEitherWithKey (const f) m)
  where
    f = coerce (applyFunc fun) :: A -> Either B C

prop_lazyMapEither :: Func A (Either (Bot B) (Bot C)) -> IntMap A -> Property
prop_lazyMapEither fun m =
  property $ case L.mapEither f m of
    (m1, m2) -> not (isBottom m1 || isBottom m2)
  where
    f = coerce (applyFunc fun) :: A -> Either B C

prop_strictMapEitherWithKey
 :: Func2 Key A (Either (Bot B) (Bot C)) -> IntMap A -> Property
prop_strictMapEitherWithKey fun m =
  isBottom (M.mapEitherWithKey f m) ===
  isBottom
    ((\(!_, !_) -> ()) $ -- Strict in both
     bimap M.fromList M.fromList $
     partitionEithers $
     map (\(k, x) -> bimap ((,) k) ((,) k) (f k x)) $
     M.toList m)
  where
    f = coerce (applyFunc2 fun) :: Key -> A -> Either B C

prop_lazyMapEitherWithKey
 :: Func2 Key A (Either (Bot B) (Bot C)) -> IntMap A -> Property
prop_lazyMapEitherWithKey fun m =
  property $ case L.mapEitherWithKey f m of
    (m1, m2) -> not (isBottom m1 || isBottom m2)
  where
    f = coerce (applyFunc2 fun) :: Key -> A -> Either B C

prop_strictUpdateMin :: Func A (Bot A) -> IntMap A -> Property
prop_strictUpdateMin fun m =
  isBottom (M.updateMin (Just . f) m) ===
  isBottom (M.updateMinWithKey (const (Just . f)) m)
  where
    f = coerce (applyFunc fun)

prop_lazyUpdateMin :: Func A (Bot A) -> IntMap A -> Property
prop_lazyUpdateMin fun m = isNotBottomProp (L.updateMin (Just . f) m)
  where
    f = coerce (applyFunc fun)

prop_strictUpdateMinWithKey :: Func2 Key A (Bot A) -> IntMap A -> Property
prop_strictUpdateMinWithKey fun m =
  isBottom (M.updateMinWithKey (\k x -> Just (f k x)) m) ===
  maybe False (isBottom . uncurry f) (M.lookupMin m)
  where
    f = coerce (applyFunc2 fun)

prop_lazyUpdateMinWithKey :: Func2 Key A (Bot A) -> IntMap A -> Property
prop_lazyUpdateMinWithKey fun m =
  isNotBottomProp (L.updateMinWithKey (\k x -> Just (f k x)) m)
  where
    f = coerce (applyFunc2 fun)

prop_strictUpdateMax :: Func A (Bot A) -> IntMap A -> Property
prop_strictUpdateMax fun m =
  isBottom (M.updateMax (Just . f) m) ===
  isBottom (M.updateMaxWithKey (const (Just . f)) m)
  where
    f = coerce (applyFunc fun)

prop_lazyUpdateMax :: Func A (Bot A) -> IntMap A -> Property
prop_lazyUpdateMax fun m = isNotBottomProp (L.updateMax (Just . f) m)
  where
    f = coerce (applyFunc fun)

prop_strictUpdateMaxWithKey :: Func2 Key A (Bot A) -> IntMap A -> Property
prop_strictUpdateMaxWithKey fun m =
  isBottom (M.updateMaxWithKey (\k x -> Just (f k x)) m) ===
  maybe False (isBottom . uncurry f) (M.lookupMax m)
  where
    f = coerce (applyFunc2 fun)

prop_lazyUpdateMaxWithKey :: Func2 Key A (Bot A) -> IntMap A -> Property
prop_lazyUpdateMaxWithKey fun m =
  isNotBottomProp (L.updateMaxWithKey (\k x -> Just (f k x)) m)
  where
    f = coerce (applyFunc2 fun)

prop_strictMerge
  :: WhenMissingFunc Key A (Bot C) C (Bot C)
  -> WhenMissingFunc Key B (Bot C) C (Bot C)
  -> WhenMatchedFunc Key A B (Bot C) C (Bot C)
  -> IntMap A
  -> IntMap B
  -> Property
prop_strictMerge misfun1 misfun2 matfun m1 m2 =
  isBottom (MMerge.merge mis1 mis2 mat m1 m2) ===
  any isBottom
    (catMaybes $ concat
      [ map (\(k, (x, y)) -> matf k x y) $
        M.toList $
        M.intersectionWith (,) m1 m2
      , map (uncurry misf1) $ M.toList $ M.difference m1 m2
      , map (uncurry misf2) $ M.toList $ M.difference m2 m1
      ])
  where
    misfun1' = coerce misfun1 :: WhenMissingFunc Key A C C C
    misfun2' = coerce misfun2 :: WhenMissingFunc Key B C C C
    matfun' = coerce matfun :: WhenMatchedFunc Key A B C C C
    mis1 = toStrictWhenMissing misfun1'
    mis2 = toStrictWhenMissing misfun2'
    mat = toStrictWhenMatched matfun'
    misf1 = whenMissingApplyStrict misfun1'
    misf2 = whenMissingApplyStrict misfun2'
    matf = whenMatchedApplyStrict  matfun'

prop_lazyMerge
  :: WhenMissingFunc Key A (Bot C) C (Bot C)
  -> WhenMissingFunc Key B (Bot C) C (Bot C)
  -> WhenMatchedFunc Key A B (Bot C) C (Bot C)
  -> IntMap A
  -> IntMap B
  -> Property
prop_lazyMerge misfun1 misfun2 matfun m1 m2 =
  isNotBottomProp (MMerge.merge mis1 mis2 mat m1 m2)
  where
    mis1 = toLazyWhenMissing (coerce misfun1 :: WhenMissingFunc Key A C C C)
    mis2 = toLazyWhenMissing (coerce misfun2 :: WhenMissingFunc Key B C C C)
    mat = toLazyWhenMatched (coerce matfun :: WhenMatchedFunc Key A B C C C)

prop_strictMergeA
  :: WhenMissingFunc Key A (Bot C) C (Bot C)
  -> WhenMissingFunc Key B (Bot C) C (Bot C)
  -> WhenMatchedFunc Key A B (Bot C) C (Bot C)
  -> IntMap A
  -> IntMap B
  -> Property
prop_strictMergeA misfun1 misfun2 matfun m1 m2 =
  isBottom (runIdentity (MMerge.mergeA mis1 mis2 mat m1 m2)) ===
  any isBottom
    (catMaybes $ concat
      [ map (\(k, (x, y)) -> matf k x y) $
        M.toList $
        M.intersectionWith (,) m1 m2
      , map (uncurry misf1) $ M.toList $ M.difference m1 m2
      , map (uncurry misf2) $ M.toList $ M.difference m2 m1
      ])
  where
    misfun1' = coerce misfun1 :: WhenMissingFunc Key A C C C
    misfun2' = coerce misfun2 :: WhenMissingFunc Key B C C C
    matfun' = coerce matfun :: WhenMatchedFunc Key A B C C C
    mis1 = toStrictWhenMissingA misfun1'
    mis2 = toStrictWhenMissingA misfun2'
    mat = toStrictWhenMatchedA matfun'
    misf1 = whenMissingApplyStrict misfun1'
    misf2 = whenMissingApplyStrict misfun2'
    matf = whenMatchedApplyStrict  matfun'

prop_lazyMergeA
  :: WhenMissingFunc Key A (Bot C) C (Bot C)
  -> WhenMissingFunc Key B (Bot C) C (Bot C)
  -> WhenMatchedFunc Key A B (Bot C) C (Bot C)
  -> IntMap A
  -> IntMap B
  -> Property
prop_lazyMergeA misfun1 misfun2 matfun m1 m2 =
  isNotBottomProp (runIdentity (LMerge.mergeA mis1 mis2 mat m1 m2))
  where
    mis1 = toLazyWhenMissingA (coerce misfun1 :: WhenMissingFunc Key A C C C)
    mis2 = toLazyWhenMissingA (coerce misfun2 :: WhenMissingFunc Key B C C C)
    mat = toLazyWhenMatchedA (coerce matfun :: WhenMatchedFunc Key A B C C C)

------------------------------------------------------------------------
-- * Properties

------------------------------------------------------------------------
-- ** Strict module

pSingletonKeyStrict :: Int -> Bool
pSingletonKeyStrict v = isBottom $ M.singleton (bottom :: Int) v

pSingletonValueStrict :: Int -> Bool
pSingletonValueStrict k = isBottom $ (M.singleton k (bottom :: Int))

pFindWithDefaultKeyStrict :: Int -> IntMap Int -> Bool
pFindWithDefaultKeyStrict def m = isBottom $ M.findWithDefault def bottom m

pFindWithDefaultValueStrict :: Int -> IntMap Int -> Bool
pFindWithDefaultValueStrict k m =
    M.member k m || (isBottom $ M.findWithDefault bottom k m)

pAdjustKeyStrict :: Fun Int Int -> IntMap Int -> Bool
pAdjustKeyStrict f m = isBottom $ M.adjust (apply f) bottom m

pAdjustValueStrict :: Int -> IntMap Int -> Bool
pAdjustValueStrict k m
    | k `M.member` m = isBottom $ M.adjust (const bottom) k m
    | otherwise       = case M.keys m of
        []     -> True
        (k':_) -> isBottom $ M.adjust (const bottom) k' m

pInsertKeyStrict :: Int -> IntMap Int -> Bool
pInsertKeyStrict v m = isBottom $ M.insert bottom v m

pInsertValueStrict :: Int -> IntMap Int -> Bool
pInsertValueStrict k m = isBottom $ M.insert k bottom m

pInsertWithKeyStrict :: Fun (Int, Int) Int -> Int -> IntMap Int -> Bool
pInsertWithKeyStrict f v m = isBottom $ M.insertWith (apply2 f) bottom v m

pInsertWithValueStrict :: Fun (Int, Int) Int -> Int -> Int -> IntMap Int
                       -> Bool
pInsertWithValueStrict f k v m
    | M.member k m = (isBottom $ M.insertWith (const2 bottom) k v m) &&
                     not (isBottom $ M.insertWith (const2 1) k bottom m)
    | otherwise    = isBottom $ M.insertWith (apply2 f) k bottom m

pInsertLookupWithKeyKeyStrict :: Fun (Int, Int, Int) Int -> Int -> IntMap Int
                              -> Bool
pInsertLookupWithKeyKeyStrict f v m = isBottom $ M.insertLookupWithKey (apply3 f) bottom v m

pInsertLookupWithKeyValueStrict :: Fun (Int, Int, Int) Int -> Int -> Int
                                -> IntMap Int -> Bool
pInsertLookupWithKeyValueStrict f k v m
    | M.member k m = (isBottom $ M.insertLookupWithKey (const3 bottom) k v m) &&
                     not (isBottom $ M.insertLookupWithKey (const3 1) k bottom m)
    | otherwise    = isBottom $ M.insertLookupWithKey (apply3 f) k bottom m

------------------------------------------------------------------------
-- test a corner case of fromAscList
--
-- If the list contains duplicate keys, then (only) the first of the
-- given values is not evaluated. This may change in the future, see
-- also https://github.com/haskell/containers/issues/473

pFromAscListLazy :: [Int] -> Bool
pFromAscListLazy ks = not . isBottom $ L.fromAscList elems
  where
    elems = [(k, v) | k <- nubInt ks, v <- [undefined, ()]]

pFromAscListStrict :: [Int] -> Bool
pFromAscListStrict ks
    | null ks   = not . isBottom $ M.fromAscList elems
    | otherwise = isBottom $ M.fromAscList elems
  where
    elems = [(k, v) | k <- nubInt ks, v <- [undefined, undefined, ()]]

{--------------------------------------------------------------------
  Folds
--------------------------------------------------------------------}

-- See Note [Testing strictness of folds] in map-strictness.hs

prop_foldrWithKey
  :: NubSortedOnFst Key (Bot A) -> Func3 Key A B (Bot B) -> Bot B -> Property
prop_foldrWithKey kvs fun (Bot z) =
  isBottom (M.foldrWithKey f z m) ===
  isBottom (F.foldr (uncurry f) z kvs')
  where
    kvs' = coerce kvs :: [(Key, A)]
    m = L.fromList kvs'
    f = coerce (applyFunc3 fun)

prop_foldr
  :: NubSortedOnFst Key (Bot A) -> Func2 A B (Bot B) -> Bot B -> Property
prop_foldr kvs fun (Bot z) =
  isBottom (M.foldr f z m) ===
  isBottom (F.foldr (f . snd) z kvs')
  where
    kvs' = coerce kvs :: [(Key, A)]
    m = L.fromList kvs'
    f = coerce (applyFunc2 fun)

prop_foldlWithKey
  :: NubSortedOnFst Key (Bot A) -> Func3 B Key A (Bot B) -> Bot B -> Property
prop_foldlWithKey kvs fun (Bot z) =
  isBottom (M.foldlWithKey f z m) ===
  isBottom (F.foldl (\z' (k,x) -> f z' k x) z kvs')
  where
    kvs' = coerce kvs :: [(Key, A)]
    m = L.fromList kvs'
    f = coerce (applyFunc3 fun)

prop_foldl
  :: NubSortedOnFst Key (Bot A) -> Func2 B A (Bot B) -> Bot B -> Property
prop_foldl kvs fun (Bot z) =
  isBottom (M.foldl f z m) ===
  isBottom (F.foldl (\z' (_,x) -> f z' x) z kvs')
  where
    kvs' = coerce kvs :: [(Key, A)]
    m = L.fromList kvs'
    f = coerce (applyFunc2 fun)

prop_foldrWithKey'
  :: NubSortedOnFst Key (Bot A) -> Func3 Key A B (Bot B) -> Bot B -> Property
prop_foldrWithKey' kvs fun (Bot z) =
  isBottom (M.foldrWithKey' f z m) ===
  isBottom (z `seq` F.foldr' (uncurry f) z kvs')
  where
    kvs' = coerce kvs :: [(Key, A)]
    m = L.fromList kvs'
    f = coerce (applyFunc3 fun)

prop_foldr'
  :: NubSortedOnFst Key (Bot A) -> Func2 A B (Bot B) -> Bot B -> Property
prop_foldr' kvs fun (Bot z) =
  isBottom (M.foldr' f z m) ===
  isBottom (z `seq` F.foldr' (f . snd) z kvs')
  where
    kvs' = coerce kvs :: [(Key, A)]
    m = L.fromList kvs'
    f = coerce (applyFunc2 fun)

prop_foldlWithKey'
  :: NubSortedOnFst Key (Bot A) -> Func3 B Key A (Bot B) -> Bot B -> Property
prop_foldlWithKey' kvs fun (Bot z) =
  isBottom (M.foldlWithKey' f z m) ===
  isBottom (F.foldl' (\z' (k,x) -> f z' k x) z kvs')
  where
    kvs' = coerce kvs :: [(Key, A)]
    m = L.fromList kvs'
    f = coerce (applyFunc3 fun)

prop_foldl'
  :: NubSortedOnFst Key (Bot A) -> Func2 B A (Bot B) -> Bot B -> Property
prop_foldl' kvs fun (Bot z) =
  isBottom (M.foldl' f z m) ===
  isBottom (F.foldl' (\z' (_,x) -> f z' x) z kvs')
  where
    kvs' = coerce kvs :: [(Key, A)]
    m = L.fromList kvs'
    f = coerce (applyFunc2 fun)

------------------------------------------------------------------------
-- * Test list

tests :: [TestTree]
tests =
    [
    -- Basic interface
      testGroup "IntMap.Strict"
      [ testProperty "singleton is key-strict" pSingletonKeyStrict
      , testProperty "singleton is value-strict" pSingletonValueStrict
      , testProperty "member is key-strict" $ keyStrict M.member
      , testProperty "lookup is key-strict" $ keyStrict M.lookup
      , testProperty "findWithDefault is key-strict" pFindWithDefaultKeyStrict
      , testProperty "findWithDefault is value-strict" pFindWithDefaultValueStrict
      , testProperty "! is key-strict" $ keyStrict (flip (M.!))
      , testProperty "!? is key-strict" $ keyStrict (flip (M.!?))
      , testProperty "delete is key-strict" $ keyStrict M.delete
      , testProperty "adjust is key-strict" pAdjustKeyStrict
      , testProperty "adjust is value-strict" pAdjustValueStrict
      , testProperty "insert is key-strict" pInsertKeyStrict
      , testProperty "insert is value-strict" pInsertValueStrict
      , testProperty "insertWith is key-strict" pInsertWithKeyStrict
      , testProperty "insertWith is value-strict" pInsertWithValueStrict
      , testProperty "insertLookupWithKey is key-strict"
        pInsertLookupWithKeyKeyStrict
      , testProperty "insertLookupWithKey is value-strict"
        pInsertLookupWithKeyValueStrict
      , testProperty "fromAscList is somewhat value-lazy" pFromAscListLazy
      , testProperty "fromAscList is somewhat value-strict" pFromAscListStrict
      , testProperty "foldrWithKey" prop_foldrWithKey
      , testProperty "foldr" prop_foldr
      , testProperty "foldlWithKey" prop_foldlWithKey
      , testProperty "foldl" prop_foldl
      , testProperty "foldrWithKey'" prop_foldrWithKey'
      , testProperty "foldr'" prop_foldr'
      , testProperty "foldlWithKey'" prop_foldlWithKey'
      , testProperty "foldl'" prop_foldl'
      ]
    , testGroup "Construction"
      [ testPropStrictLazy "singleton" prop_strictSingleton prop_lazySingleton
      , testPropStrictLazy "fromSet" prop_strictFromSet prop_lazyFromSet
      , testPropStrictLazy "fromSetA" prop_strictFromSetA prop_lazyFromSetA
      , testProperty       "fromSetA equivalences" prop_fromSetA_equiv_strictness
      , testPropStrictLazy "fromList" prop_strictFromList prop_lazyFromList
      , testPropStrictLazy "fromListWith" prop_strictFromListWith prop_lazyFromListWith
      , testPropStrictLazy "fromListWithKey" prop_strictFromListWithKey prop_lazyFromListWithKey
      , testPropStrictLazy "fromAscList" prop_strictFromAscList prop_lazyFromAscList
      , testPropStrictLazy "fromAscListWith" prop_strictFromAscListWith prop_lazyFromAscListWith
      , testPropStrictLazy "fromAscListWithKey" prop_strictFromAscListWithKey prop_lazyFromAscListWithKey
      , testPropStrictLazy "fromDistinctAscList" prop_strictFromDistinctAscList prop_lazyFromDistinctAscList
      , testPropStrictLazy "insert" prop_strictInsert prop_lazyInsert
      , testPropStrictLazy "insertWith" prop_strictInsertWith prop_lazyInsertWith
      , testPropStrictLazy "insertWithKey" prop_strictInsertWithKey prop_lazyInsertWithKey
      , testPropStrictLazy "insertLookupWithKey" prop_strictInsertLookupWithKey prop_lazyInsertLookupWithKey
      , testPropStrictLazy "adjust" prop_strictAdjust prop_lazyAdjust
      , testPropStrictLazy "adjustWithKey" prop_strictAdjustWithKey prop_lazyAdjustWithKey
      , testPropStrictLazy "update" prop_strictUpdate prop_lazyUpdate
      , testPropStrictLazy "upsert" prop_strictUpsert prop_lazyUpsert
      , testPropStrictLazy "updateWithKey" prop_strictUpdateWithKey prop_lazyUpdateWithKey
      , testPropStrictLazy "updateLookupWithKey" prop_strictUpdateLookupWithKey prop_lazyUpdateLookupWithKey
      , testPropStrictLazy "alter" prop_strictAlter prop_lazyAlter
      , testPropStrictLazy "alterF" prop_strictAlterF prop_lazyAlterF
      , testPropStrictLazy "unionWith" prop_strictUnionWith prop_lazyUnionWith
      , testPropStrictLazy "unionWithKey" prop_strictUnionWithKey prop_lazyUnionWithKey
      , testPropStrictLazy "unionsWith" prop_strictUnionsWith prop_lazyUnionsWith
      , testPropStrictLazy "differenceWith" prop_strictDifferenceWith prop_lazyDifferenceWith
      , testPropStrictLazy "differenceWithKey" prop_strictDifferenceWithKey prop_lazyDifferenceWithKey
      , testPropStrictLazy "intersectionWith" prop_strictIntersectionWith prop_lazyIntersectionWith
      , testPropStrictLazy "intersectionWithKey" prop_strictIntersectionWithKey prop_lazyIntersectionWithKey
      , testPropStrictLazy "mergeWithKey" prop_strictMergeWithKey prop_lazyMergeWithKey
      , testPropStrictLazy "map" prop_strictMap prop_lazyMap
      , testPropStrictLazy "mapWithKey" prop_strictMapWithKey prop_lazyMapWithKey
      , testPropStrictLazy "traverseWithKey" prop_strictTraverseWithKey prop_lazyTraverseWithKey
      , testPropStrictLazy "traverseMaybeWithKey" prop_strictTraverseMaybeWithKey prop_lazyTraverseMaybeWithKey
      , testPropStrictLazy "mapAccum" prop_strictMapAccum prop_lazyMapAccum
      , testPropStrictLazy "mapAccumWithKey" prop_strictMapAccumWithKey prop_lazyMapAccumWithKey
      , testPropStrictLazy "mapAccumRWithKey" prop_strictMapAccumRWithKey prop_lazyMapAccumRWithKey
      , testPropStrictLazy "mapKeysWith" prop_strictMapKeysWith prop_lazyMapKeysWith
      , testPropStrictLazy "mapMaybe" prop_strictMapMaybe prop_lazyMapMaybe
      , testPropStrictLazy "mapMaybeWithKey" prop_strictMapMaybeWithKey prop_lazyMapMaybeWithKey
      , testPropStrictLazy "mapEither" prop_strictMapEither prop_lazyMapEither
      , testPropStrictLazy "mapEitherWithKey" prop_strictMapEitherWithKey prop_lazyMapEitherWithKey
      , testPropStrictLazy "updateMin" prop_strictUpdateMin prop_lazyUpdateMin
      , testPropStrictLazy "updateMinWithKey" prop_strictUpdateMinWithKey prop_lazyUpdateMinWithKey
      , testPropStrictLazy "updateMax" prop_strictUpdateMax prop_lazyUpdateMax
      , testPropStrictLazy "updateMaxWithKey" prop_strictUpdateMaxWithKey prop_lazyUpdateMaxWithKey
      , testPropStrictLazy "merge" prop_strictMerge prop_lazyMerge
      , testPropStrictLazy "mergeA" prop_strictMergeA prop_lazyMergeA
      ]
    ]

testPropStrictLazy :: Testable a => String -> a -> a -> TestTree
testPropStrictLazy name strictTest lazyTest =
  testGroup name
  [ testProperty "strict" strictTest
  , testProperty "lazy" lazyTest
  ]

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain $ testGroup "intmap-strictness" tests

------------------------------------------------------------------------
-- * Utilities

keyStrict :: (Int -> IntMap Int -> a) -> IntMap Int -> Bool
keyStrict f m = isBottom $ f bottom m

isNotBottomProp :: a -> Property
isNotBottomProp = property . not . isBottom

const2 :: a -> b -> c -> a
const2 x _ _ = x

const3 :: a -> b -> c -> d -> a
const3 x _ _ _ = x

-- | Keep the first of adjacent equal elements.
uniqOn :: Eq b => (a -> b) -> [a] -> [a]
uniqOn f = map NE.head . NE.groupBy ((==) `on` f)

{--------------------------------------------------------------------
  Merge stuff
--------------------------------------------------------------------}

toStrictWhenMatched
  :: WhenMatchedFunc Key x y z z z2 -> WhenMatched Identity x y z2
toStrictWhenMatched wmf = case wmf of
  MaybeMatchedFunc fun -> MMerge.zipWithMaybeMatched (applyFunc3 fun)
  FmapMaybeMatchedFunc fun2 fun1 ->
    MMerge.mapWhenMatched (applyFunc fun2) $
    MMerge.zipWithMaybeMatched (applyFunc3 fun1)
  MatchedFunc fun -> MMerge.zipWithMatched (applyFunc3 fun)
  FmapMatchedFunc fun2 fun1 ->
    MMerge.mapWhenMatched (applyFunc fun2) $
    MMerge.zipWithMatched (applyFunc3 fun1)

toStrictWhenMatchedA
  :: WhenMatchedFunc Key x y z z z2 -> WhenMatched Identity x y z2
toStrictWhenMatchedA wmf = case wmf of
  MaybeMatchedFunc fun -> MMerge.zipWithMaybeAMatched (coerce (applyFunc3 fun))
  FmapMaybeMatchedFunc fun2 fun1 ->
    MMerge.mapWhenMatched (applyFunc fun2) $
    MMerge.zipWithMaybeAMatched (coerce (applyFunc3 fun1))
  MatchedFunc fun -> MMerge.zipWithAMatched (coerce (applyFunc3 fun))
  FmapMatchedFunc fun2 fun1 ->
    MMerge.mapWhenMatched (applyFunc fun2) $
    MMerge.zipWithAMatched (coerce (applyFunc3 fun1))

toLazyWhenMatched
  :: WhenMatchedFunc Key x y z z z2 -> WhenMatched Identity x y z2
toLazyWhenMatched wmf = case wmf of
  MaybeMatchedFunc fun -> LMerge.zipWithMaybeMatched (applyFunc3 fun)
  FmapMaybeMatchedFunc fun2 fun1 ->
    LMerge.mapWhenMatched (applyFunc fun2) $
    LMerge.zipWithMaybeMatched (applyFunc3 fun1)
  MatchedFunc fun -> LMerge.zipWithMatched (applyFunc3 fun)
  FmapMatchedFunc fun2 fun1 ->
    LMerge.mapWhenMatched (applyFunc fun2) $
    LMerge.zipWithMatched (applyFunc3 fun1)

toLazyWhenMatchedA
  :: WhenMatchedFunc Key x y z z z2 -> WhenMatched Identity x y z2
toLazyWhenMatchedA wmf = case wmf of
  MaybeMatchedFunc fun -> LMerge.zipWithMaybeAMatched (coerce (applyFunc3 fun))
  FmapMaybeMatchedFunc fun2 fun1 ->
    LMerge.mapWhenMatched (applyFunc fun2) $
    LMerge.zipWithMaybeAMatched (coerce (applyFunc3 fun1))
  MatchedFunc fun -> LMerge.zipWithAMatched (coerce (applyFunc3 fun))
  FmapMatchedFunc fun2 fun1 ->
    LMerge.mapWhenMatched (applyFunc fun2) $
    LMerge.zipWithAMatched (coerce (applyFunc3 fun1))

whenMatchedApplyStrict
  :: WhenMatchedFunc Key x y z z z2 -> Key -> x -> y -> Maybe z2
whenMatchedApplyStrict wmf = case wmf of
  MaybeMatchedFunc fun -> applyFunc3 fun
  FmapMaybeMatchedFunc fun2 fun1 ->
    \k x y ->
      (applyFunc fun2 $!) <$> -- Strict in the intermediate result
      applyFunc3 fun1 k x y
  MatchedFunc fun -> \k x y -> Just (applyFunc3 fun k x y)
  FmapMatchedFunc fun2 fun1 ->
    \k x y -> Just $
      applyFunc fun2 $! -- Strict in the intermediate result
      applyFunc3 fun1 k x y

toStrictWhenMissing :: WhenMissingFunc Key x y y y2 -> WhenMissing Identity x y2
toStrictWhenMissing wmf = case wmf of
  MapMaybeMissingFunc fun -> MMerge.mapMaybeMissing (applyFunc2 fun)
  FmapMapMaybeMissingFunc fun2 fun1 ->
    MMerge.mapWhenMissing (applyFunc fun2) $
    MMerge.mapMaybeMissing (applyFunc2 fun1)
  MapMissingFunc fun -> MMerge.mapMissing (applyFunc2 fun)
  FmapMapMissingFunc fun2 fun1 ->
    MMerge.mapWhenMissing (applyFunc fun2) $
    MMerge.mapMissing (applyFunc2 fun1)

toStrictWhenMissingA
  :: WhenMissingFunc Key x y y y2 -> WhenMissing Identity x y2
toStrictWhenMissingA wmf = case wmf of
  MapMaybeMissingFunc fun ->
    MMerge.traverseMaybeMissing (coerce (applyFunc2 fun))
  FmapMapMaybeMissingFunc fun2 fun1 ->
    MMerge.mapWhenMissing (applyFunc fun2) $
    MMerge.traverseMaybeMissing (coerce (applyFunc2 fun1))
  MapMissingFunc fun -> MMerge.traverseMissing (coerce (applyFunc2 fun))
  FmapMapMissingFunc fun2 fun1 ->
    MMerge.mapWhenMissing (applyFunc fun2) $
    MMerge.traverseMissing (coerce (applyFunc2 fun1))

toLazyWhenMissing :: WhenMissingFunc Key x y y y2 -> WhenMissing Identity x y2
toLazyWhenMissing wmf = case wmf of
  MapMaybeMissingFunc fun -> LMerge.mapMaybeMissing (applyFunc2 fun)
  FmapMapMaybeMissingFunc fun2 fun1 ->
    LMerge.mapWhenMissing (applyFunc fun2) $
    LMerge.mapMaybeMissing (applyFunc2 fun1)
  MapMissingFunc fun -> LMerge.mapMissing (applyFunc2 fun)
  FmapMapMissingFunc fun2 fun1 ->
    LMerge.mapWhenMissing (applyFunc fun2) $
    LMerge.mapMissing (applyFunc2 fun1)

toLazyWhenMissingA :: WhenMissingFunc Key x y y y2 -> WhenMissing Identity x y2
toLazyWhenMissingA wmf = case wmf of
  MapMaybeMissingFunc fun ->
    LMerge.traverseMaybeMissing (coerce (applyFunc2 fun))
  FmapMapMaybeMissingFunc fun2 fun1 ->
    LMerge.mapWhenMissing (applyFunc fun2) $
    LMerge.traverseMaybeMissing (coerce (applyFunc2 fun1))
  MapMissingFunc fun -> LMerge.traverseMissing (coerce (applyFunc2 fun))
  FmapMapMissingFunc fun2 fun1 ->
    LMerge.mapWhenMissing (applyFunc fun2) $
    LMerge.traverseMissing (coerce (applyFunc2 fun1))

whenMissingApplyStrict :: WhenMissingFunc Key x y y y2 -> Key -> x -> Maybe y2
whenMissingApplyStrict wmf = case wmf of
  MapMaybeMissingFunc fun -> applyFunc2 fun
  FmapMapMaybeMissingFunc fun2 fun1 ->
    \k x ->
      (applyFunc fun2 $!) <$> -- Strict in the intermediate result
      applyFunc2 fun1 k x
  MapMissingFunc fun -> \k x -> Just (applyFunc2 fun k x)
  FmapMapMissingFunc fun2 fun1 ->
    \k x -> Just $
      applyFunc fun2 $! -- Strict in the intermediate result
      applyFunc2 fun1 k x
