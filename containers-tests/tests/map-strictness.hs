{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import Data.Foldable as F
import Data.Function (on)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Ord (Down(..), comparing)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Semigroup (Arg(..))
import Test.ChasingBottoms.IsBottom (bottom, isBottom)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Poly (A, B, C, OrdA, OrdB)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as MMerge
import qualified Data.Map as L
import Data.Map.Merge.Lazy (WhenMatched, WhenMissing)
import qualified Data.Map.Merge.Lazy as LMerge
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Containers.ListUtils (nubOrd)

import Utils.ArbitrarySetMap (setFromList, mapFromKeysList)
import Utils.Strictness
  (Bot(..), Func(..), Func2(..), Func3(..), applyFunc, applyFunc2, applyFunc3)

#if __GLASGOW_HASKELL__ >= 806
import Utils.NoThunks
#endif

instance (Arbitrary k, Arbitrary v, Ord k) =>
         Arbitrary (Map k v) where
  arbitrary = do
    Sorted xs <- arbitrary
    m <- mapFromKeysList $ nubOrd xs

    -- Force the values to WHNF. Should use liftRnf2 when that's available.
    let !_ = foldr seq () m

    pure m

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = do
    Sorted xs <- arbitrary
    setFromList $ nubOrd xs

apply2 :: Fun (a, b) c -> a -> b -> c
apply2 f a b = apply f (a, b)

apply3 :: Fun (a, b, c) d -> a -> b -> c -> d
apply3 f a b c = apply f (a, b, c)

{--------------------------------------------------------------------
  Construction property tests
--------------------------------------------------------------------}

-- The primary purpose of these property tests is to ensure that
--
-- 1. Functions in Data.Map.Strict always force the value (as in key-value) to
--    WHNF when inserting it into a Map.
-- 2. Functions is Data.Map.Lazy do not force the value when inserting it into
--    a Map.
--
-- These are ensured by testing against bottom. If the function is strict in the
-- value, the attempt to create a Map with a bottom value will result in a Map
-- that is bottom. Otherwise, the Map will not be bottom.
--
-- Every function defined in Data.Map.Strict.Internal should have a property
-- test defined here. The corresponding function from Data.Map.Lazy should
-- also be tested to ensure that it is as lazy as expected.
--
-- Functions with implementations shared between Data.Map.Lazy and
-- Data.Map.Strict never insert values and only remove them or shuffle them
-- around. These functions do not need to be tested here.
--
-- For strict Map functions, the strictness is tested in one of two ways:
--
-- * Directly. For example, the result of `alter` is bottom if and only if the
--   key exists in the map and the alter function applied to the key's value
--   returns `Just bottom`.
-- * Indirectly against another equivalent function, which is also tested. For
--   instance, `adjust f` is expected to be equivalent to `alter (fmap f)`, and
--   is tested to be so in strictness.

prop_strictSingleton :: OrdA -> Bot A -> Property
prop_strictSingleton k (Bot x) = isBottom (M.singleton k x) === isBottom x

prop_lazySingleton :: OrdA -> Bot A -> Property
prop_lazySingleton k (Bot x) = isNotBottomP (L.singleton k x)

prop_strictFromSet :: Func OrdA (Bot A) -> Set OrdA -> Property
prop_strictFromSet fun set =
  isBottom (M.fromSet f set) === any (isBottom . f) (Set.toList set)
  where
    f = coerce (applyFunc fun) :: OrdA -> A

prop_lazyFromSet :: Func OrdA (Bot A) -> Set OrdA -> Property
prop_lazyFromSet fun set = isNotBottomP (L.fromSet f set)
  where
    f = coerce (applyFunc fun) :: OrdA -> A

prop_strictFromArgSet :: Func OrdA (Bot A) -> Set OrdA -> Property
prop_strictFromArgSet fun set =
  isBottom (M.fromArgSet set') ===
  any (\(Arg _ x) -> isBottom x) (Set.toList set')
  where
    f = coerce (applyFunc fun) :: OrdA -> A
    -- Workaround for missing Arbitrary (Arg a b)
    set' = Set.map (\x -> Arg x (f x)) set

prop_lazyFromArgSet :: Func OrdA (Bot A) -> Set OrdA -> Property
prop_lazyFromArgSet fun set = isNotBottomP (L.fromArgSet set')
  where
    f = coerce (applyFunc fun) :: OrdA -> A
    -- Workaround for missing Arbitrary (Arg a b)
    set' = Set.map (\x -> Arg x (f x)) set

prop_strictFromList :: [(OrdA, Bot A)] -> Property
prop_strictFromList kvs =
  isBottom (M.fromList kvs') === any (isBottom . snd) kvs'
  where
    kvs' = coerce kvs :: [(OrdA, A)]

prop_lazyFromList :: [(OrdA, Bot A)] -> Property
prop_lazyFromList kvs = isNotBottomP $ isBottom (L.fromList kvs')
  where
    kvs' = coerce kvs :: [(OrdA, A)]

prop_strictFromListWith :: Func2 A A (Bot A) -> [(OrdA, Bot A)] -> Property
prop_strictFromListWith fun kvs =
  isBottom (M.fromListWith f kvs') ===
  isBottom (F.foldl' (\acc (k,x) -> M.insertWith f k x acc) M.empty kvs')
  where
    f = coerce (applyFunc2 fun)
    kvs' = coerce kvs :: [(OrdA, A)]

prop_lazyFromListWith :: Func2 A A (Bot A) -> [(OrdA, Bot A)] -> Property
prop_lazyFromListWith fun kvs = isNotBottomP (L.fromListWith f kvs')
  where
    f = coerce (applyFunc2 fun)
    kvs' = coerce kvs :: [(OrdA, A)]

prop_strictFromListWithKey
  :: Func3 OrdA A A (Bot A) -> [(OrdA, Bot A)] -> Property
prop_strictFromListWithKey fun kvs =
  isBottom (M.fromListWithKey f kvs') ===
  isBottom (F.foldl' (\acc (k,x) -> M.insertWithKey f k x acc) M.empty kvs')
  where
    f = coerce (applyFunc3 fun)
    kvs' = coerce kvs :: [(OrdA, A)]

prop_lazyFromListWithKey
  :: Func3 OrdA A A (Bot A) -> [(OrdA, Bot A)] -> Property
prop_lazyFromListWithKey fun kvs = isNotBottomP (L.fromListWithKey f kvs')
  where
    f = coerce (applyFunc3 fun)
    kvs' = coerce kvs :: [(OrdA, A)]

prop_strictFromAscList :: [(OrdA, Bot A)] -> Property
prop_strictFromAscList kvs =
  isBottom (M.fromAscList kvs') === isBottom (M.fromList kvs')
  where
    kvs' = List.sortBy (comparing fst) (coerce kvs) :: [(OrdA, A)]

prop_lazyFromAscList :: [(OrdA, Bot A)] -> Property
prop_lazyFromAscList kvs = isNotBottomP (L.fromAscList kvs')
  where
    kvs' = List.sortBy (comparing fst) (coerce kvs) :: [(OrdA, A)]

prop_strictFromAscListWith :: Func2 A A (Bot A) -> [(OrdA, Bot A)] -> Property
prop_strictFromAscListWith fun kvs =
  isBottom (M.fromAscListWith f kvs') === isBottom (M.fromListWith f kvs')
  where
    f = coerce (applyFunc2 fun)
    kvs' = List.sortBy (comparing fst) (coerce kvs) :: [(OrdA, A)]

prop_lazyFromAscListWith :: Func2 A A (Bot A) -> [(OrdA, Bot A)] -> Property
prop_lazyFromAscListWith fun kvs = isNotBottomP (L.fromAscListWith f kvs')
  where
    f = coerce (applyFunc2 fun)
    kvs' = List.sortBy (comparing fst) (coerce kvs) :: [(OrdA, A)]

prop_strictFromAscListWithKey
  :: Func3 OrdA A A (Bot A) -> [(OrdA, Bot A)] -> Property
prop_strictFromAscListWithKey fun kvs =
  isBottom (M.fromAscListWithKey f kvs') ===
  isBottom (M.fromListWithKey f kvs')
  where
    f = coerce (applyFunc3 fun)
    kvs' = List.sortBy (comparing fst) (coerce kvs) :: [(OrdA, A)]

prop_lazyFromAscListWithKey
  :: Func3 OrdA A A (Bot A) -> [(OrdA, Bot A)] -> Property
prop_lazyFromAscListWithKey fun kvs = isNotBottomP (L.fromAscListWithKey f kvs')
  where
    f = coerce (applyFunc3 fun)
    kvs' = List.sortBy (comparing fst) (coerce kvs) :: [(OrdA, A)]

prop_strictFromDistinctAscList :: [(OrdA, Bot A)] -> Property
prop_strictFromDistinctAscList kvs =
  isBottom (M.fromDistinctAscList kvs') === isBottom (M.fromList kvs')
  where
    kvs' = uniqOn fst $ List.sortBy (comparing fst) (coerce kvs) :: [(OrdA, A)]

prop_lazyFromDistinctAscList :: [(OrdA, Bot A)] -> Property
prop_lazyFromDistinctAscList kvs = isNotBottomP (L.fromDistinctAscList kvs')
  where
    kvs' = uniqOn fst $ List.sortBy (comparing fst) (coerce kvs) :: [(OrdA, A)]

prop_strictFromDescList :: [(OrdA, Bot A)] -> Property
prop_strictFromDescList kvs =
  isBottom (M.fromDescList kvs') === isBottom (M.fromList kvs')
  where
    kvs' = List.sortBy (comparing (Down . fst)) (coerce kvs) :: [(OrdA, A)]

prop_lazyFromDescList :: [(OrdA, Bot A)] -> Property
prop_lazyFromDescList kvs = isNotBottomP (L.fromDescList kvs')
  where
    kvs' = List.sortBy (comparing (Down . fst)) (coerce kvs) :: [(OrdA, A)]

prop_strictFromDescListWith :: Func2 A A (Bot A) -> [(OrdA, Bot A)] -> Property
prop_strictFromDescListWith fun kvs =
  isBottom (M.fromDescListWith f kvs') === isBottom (M.fromListWith f kvs')
  where
    f = coerce (applyFunc2 fun)
    kvs' = List.sortBy (comparing (Down . fst)) (coerce kvs) :: [(OrdA, A)]

prop_lazyFromDescListWith :: Func2 A A (Bot A) -> [(OrdA, Bot A)] -> Property
prop_lazyFromDescListWith fun kvs = isNotBottomP (L.fromDescListWith f kvs')
  where
    f = coerce (applyFunc2 fun)
    kvs' = List.sortBy (comparing (Down . fst)) (coerce kvs) :: [(OrdA, A)]

prop_strictFromDescListWithKey
  :: Func3 OrdA A A (Bot A) -> [(OrdA, Bot A)] -> Property
prop_strictFromDescListWithKey fun kvs =
  isBottom (M.fromDescListWithKey f kvs') ===
  isBottom (M.fromListWithKey f kvs')
  where
    f = coerce (applyFunc3 fun)
    kvs' = List.sortBy (comparing (Down . fst)) (coerce kvs) :: [(OrdA, A)]

prop_lazyFromDescListWithKey
  :: Func3 OrdA A A (Bot A) -> [(OrdA, Bot A)] -> Property
prop_lazyFromDescListWithKey fun kvs =
  isNotBottomP (L.fromDescListWithKey f kvs')
  where
    f = coerce (applyFunc3 fun)
    kvs' = List.sortBy (comparing (Down . fst)) (coerce kvs) :: [(OrdA, A)]

prop_strictFromDistinctDescList :: [(OrdA, Bot A)] -> Property
prop_strictFromDistinctDescList kvs =
  isBottom (M.fromDistinctDescList kvs') === isBottom (M.fromList kvs')
  where
    kvs' =
      uniqOn fst $
      List.sortBy (comparing (Down . fst)) (coerce kvs) :: [(OrdA, A)]

prop_lazyFromDistinctDescList :: [(OrdA, Bot A)] -> Property
prop_lazyFromDistinctDescList kvs = isNotBottomP (L.fromDistinctDescList kvs')
  where
    kvs' =
      uniqOn fst $
      List.sortBy (comparing (Down . fst)) (coerce kvs) :: [(OrdA, A)]

prop_strictInsert :: OrdA -> Bot A -> Map OrdA A -> Property
prop_strictInsert k (Bot x) m = isBottom (M.insert k x m) === isBottom x

prop_lazyInsert :: OrdA -> Bot A -> Map OrdA A -> Property
prop_lazyInsert k (Bot x) m = isNotBottomP (L.insert k x m)

prop_strictInsertWith
  :: Func2 A A (Bot A)
  -> OrdA
  -> Bot A
  -> Map OrdA A
  -> Property
prop_strictInsertWith fun k (Bot x) m =
  isBottom (M.insertWith f k x m) === isBottom (M.insertWithKey (const f) k x m)
  where
    f = coerce (applyFunc2 fun)

prop_lazyInsertWith
  :: Func2 A A (Bot A)
  -> OrdA
  -> Bot A
  -> Map OrdA A
  -> Property
prop_lazyInsertWith fun k (Bot x) m = isNotBottomP (L.insertWith f k x m)
  where
    f = coerce (applyFunc2 fun)

prop_strictInsertWithKey
  :: Func3 OrdA A A (Bot A)
  -> OrdA
  -> Bot A
  -> Map OrdA A
  -> Property
prop_strictInsertWithKey fun k (Bot x) m =
  isBottom (M.insertWithKey f k x m) ===
  isBottom (maybe x (f k x) (M.lookup k m))
  where
    f = coerce (applyFunc3 fun)

prop_lazyInsertWithKey
  :: Func3 OrdA A A (Bot A)
  -> OrdA
  -> Bot A
  -> Map OrdA A
  -> Property
prop_lazyInsertWithKey fun k (Bot x) m = isNotBottomP (L.insertWithKey f k x m)
  where
    f = coerce (applyFunc3 fun)

prop_strictInsertLookupWithKey
  :: Func3 OrdA A A (Bot A)
  -> OrdA
  -> Bot A
  -> Map OrdA A
  -> Property
prop_strictInsertLookupWithKey fun k (Bot x) m =
  isBottom (M.insertLookupWithKey f k x m) ===
  isBottom (maybe x (f k x) (M.lookup k m))
  where
    f = coerce (applyFunc3 fun)

prop_lazyInsertLookupWithKey
  :: Func3 OrdA A A (Bot A)
  -> OrdA
  -> Bot A
  -> Map OrdA A
  -> Property
prop_lazyInsertLookupWithKey fun k (Bot x) m =
  isNotBottomP (L.insertLookupWithKey f k x m)
  where
    f = coerce (applyFunc3 fun)

prop_strictAdjust :: Func A (Bot A) -> OrdA -> Map OrdA A -> Property
prop_strictAdjust fun k m =
  isBottom (M.adjust f k m) === isBottom (M.alter (fmap f) k m)
  where
    f = coerce (applyFunc fun)

prop_lazyAdjust :: Func A (Bot A) -> OrdA -> Map OrdA A -> Property
prop_lazyAdjust fun k m = isNotBottomP (L.adjust f k m)
  where
    f = coerce (applyFunc fun)

prop_strictAdjustWithKey
  :: Func2 OrdA A (Bot A) -> OrdA -> Map OrdA A -> Property
prop_strictAdjustWithKey fun k m =
 isBottom (M.adjustWithKey f k m) === isBottom (M.alter (fmap (f k)) k m)
  where
    f = coerce (applyFunc2 fun)

prop_lazyAdjustWithKey
  :: Func2 OrdA A (Bot A) -> OrdA -> Map OrdA A -> Property
prop_lazyAdjustWithKey fun k m = isNotBottomP (L.adjustWithKey f k m)
  where
    f = coerce (applyFunc2 fun)

prop_strictUpdate
  :: Func A (Maybe (Bot A)) -> OrdA -> Map OrdA A -> Property
prop_strictUpdate fun k m =
  isBottom (M.update f k m) === isBottom (M.alter (>>= f) k m)
  where
    f = coerce (applyFunc fun)

prop_lazyUpdate
  :: Func A (Maybe (Bot A)) -> OrdA -> Map OrdA A -> Property
prop_lazyUpdate fun k m = isNotBottomP (L.update f k m)
  where
    f = coerce (applyFunc fun)

prop_strictUpdateWithKey
  :: Func2 OrdA A (Maybe (Bot A)) -> OrdA -> Map OrdA A -> Property
prop_strictUpdateWithKey fun k m =
  isBottom (M.updateWithKey f k m) === isBottom (M.alter (>>= f k) k m)
  where
    f = coerce (applyFunc2 fun)

prop_lazyUpdateWithKey
  :: Func2 OrdA A (Maybe (Bot A)) -> OrdA -> Map OrdA A -> Property
prop_lazyUpdateWithKey fun k m = isNotBottomP (L.updateWithKey f k m)
  where
    f = coerce (applyFunc2 fun)

prop_strictUpdateLookupWithKey
  :: Func2 OrdA A (Maybe (Bot A)) -> OrdA -> Map OrdA A -> Property
prop_strictUpdateLookupWithKey fun k m =
  isBottom (M.updateLookupWithKey f k m) === isBottom (M.updateWithKey f k m)
  where
    f = coerce (applyFunc2 fun)

prop_lazyUpdateLookupWithKey
  :: Func2 OrdA A (Maybe (Bot A)) -> OrdA -> Map OrdA A -> Property
prop_lazyUpdateLookupWithKey fun k m =
  isNotBottomP (L.updateLookupWithKey f k m)
  where
    f = coerce (applyFunc2 fun)

prop_strictAlter
  :: Func (Maybe A) (Maybe (Bot A)) -> OrdA -> Map OrdA A -> Property
prop_strictAlter fun k m =
  isBottom (M.alter f k m) ===
  maybe False isBottom (f (M.lookup k m))
  where
    f = coerce (applyFunc fun)

prop_lazyAlter
  :: Func (Maybe A) (Maybe (Bot A)) -> OrdA -> Map OrdA A -> Property
prop_lazyAlter fun k m = isNotBottomP (L.alter f k m)
  where
    f = coerce (applyFunc fun)

prop_strictAlterF
  :: Func (Maybe A) (Identity (Maybe (Bot A))) -> OrdA -> Map OrdA A -> Property
prop_strictAlterF fun k m =
  isBottom (runIdentity (M.alterF f k m)) ===
  maybe False isBottom (runIdentity (f (M.lookup k m)))
  where
    f = coerce (applyFunc fun)

prop_lazyAlterF
  :: Func (Maybe A) (Identity (Maybe (Bot A))) -> OrdA -> Map OrdA A -> Property
prop_lazyAlterF fun k m = isNotBottomP (runIdentity (L.alterF f k m))
  where
    f = coerce (applyFunc fun)

prop_strictUnionWith
  :: Func2 A A (Bot A) -> Map OrdA A -> Map OrdA A -> Property
prop_strictUnionWith fun m1 m2 =
  isBottom (M.unionWith f m1 m2) ===
  isBottom (M.unionWithKey (const f) m1 m2)
  where
    f = coerce (applyFunc2 fun)

prop_lazyUnionWith
  :: Func2 A A (Bot A) -> Map OrdA A -> Map OrdA A -> Property
prop_lazyUnionWith fun m1 m2 = isNotBottomP (L.unionWith f m1 m2)
  where
    f = coerce (applyFunc2 fun)

prop_strictUnionWithKey
  :: Func3 OrdA A A (Bot A) -> Map OrdA A -> Map OrdA A -> Property
prop_strictUnionWithKey fun m1 m2 =
  isBottom (M.unionWithKey f m1 m2) ===
  isBottom (M.foldlWithKey' (\acc k x -> M.insertWithKey f k x acc) m2 m1)
  where
    f = coerce (applyFunc3 fun)

prop_lazyUnionWithKey
  :: Func3 OrdA A A (Bot A) -> Map OrdA A -> Map OrdA A -> Property
prop_lazyUnionWithKey fun m1 m2 = isNotBottomP (L.unionWithKey f m1 m2)
  where
    f = coerce (applyFunc3 fun)

prop_strictUnionsWith :: Func2 A A (Bot A) -> [Map OrdA A] -> Property
prop_strictUnionsWith fun ms =
  isBottom (M.unionsWith f ms) ===
  isBottom (F.foldl' (M.unionWith f) M.empty ms)
  where
    f = coerce (applyFunc2 fun)

prop_lazyUnionsWith :: Func2 A A (Bot A) -> [Map OrdA A] -> Property
prop_lazyUnionsWith fun ms = isNotBottomP (L.unionsWith f ms)
  where
    f = coerce (applyFunc2 fun)

prop_strictDifferenceWith
  :: Func2 A B (Maybe (Bot A))
  -> Map OrdA A
  -> Map OrdA B
  -> Property
prop_strictDifferenceWith fun m1 m2 =
  isBottom (M.differenceWith f m1 m2) ===
  isBottom (M.differenceWithKey (const f) m1 m2)
  where
    f = coerce (applyFunc2 fun)

prop_lazyDifferenceWith
  :: Func2 A B (Maybe (Bot A))
  -> Map OrdA A
  -> Map OrdA B
  -> Property
prop_lazyDifferenceWith fun m1 m2 = isNotBottomP (L.differenceWith f m1 m2)
  where
    f = coerce (applyFunc2 fun)

prop_strictDifferenceWithKey
  :: Func3 OrdA A B (Maybe (Bot A))
  -> Map OrdA A
  -> Map OrdA B
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
  :: Func3 OrdA A B (Maybe (Bot A))
  -> Map OrdA A
  -> Map OrdA B
  -> Property
prop_lazyDifferenceWithKey fun m1 m2 =
  isNotBottomP (L.differenceWithKey f m1 m2)
  where
    f = coerce (applyFunc3 fun)

prop_strictIntersectionWith
  :: Func2 A B (Bot C)
  -> Map OrdA A
  -> Map OrdA B
  -> Property
prop_strictIntersectionWith fun m1 m2 =
  isBottom (M.intersectionWith f m1 m2) ===
  isBottom (M.intersectionWithKey (const f) m1 m2)
  where
    f = coerce (applyFunc2 fun) :: A -> B -> C

prop_lazyIntersectionWith
  :: Func2 A B (Bot C)
  -> Map OrdA A
  -> Map OrdA B
  -> Property
prop_lazyIntersectionWith fun m1 m2 = isNotBottomP (L.intersectionWith f m1 m2)
  where
    f = coerce (applyFunc2 fun) :: A -> B -> C

prop_strictIntersectionWithKey
  :: Func3 OrdA A B (Bot C)
  -> Map OrdA A
  -> Map OrdA B
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
    f = coerce (applyFunc3 fun) :: OrdA -> A -> B -> C

prop_lazyIntersectionWithKey
  :: Func3 OrdA A B (Bot C)
  -> Map OrdA A
  -> Map OrdA B
  -> Property
prop_lazyIntersectionWithKey fun m1 m2 =
  isNotBottomP (L.intersectionWithKey f m1 m2)
  where
    f = coerce (applyFunc3 fun) :: OrdA -> A -> B -> C

prop_strictMergeWithKey
  :: Func3 OrdA A B (Maybe (Bot C))
  -> Fun A (Maybe C)
  -> Fun B (Maybe C)
  -> Map OrdA A
  -> Map OrdA B
  -> Property
prop_strictMergeWithKey fun12 fun1 fun2 m1 m2 =
  isBottom (M.mergeWithKey f12 (M.mapMaybe f1) (M.mapMaybe f2) m1 m2) ===
  any isBottom
    (mapMaybe (\(k, (x, y)) -> f12 k x y) $
     M.toList $
     M.intersectionWith (,) m1 m2)
  where
    f12 = coerce (applyFunc3 fun12) :: OrdA -> A -> B -> Maybe C
    f1 = applyFun fun1
    f2 = applyFun fun2

prop_lazyMergeWithKey
  :: Func3 OrdA A B (Maybe (Bot C))
  -> Fun A (Maybe C)
  -> Fun B (Maybe C)
  -> Map OrdA A
  -> Map OrdA B
  -> Property
prop_lazyMergeWithKey fun12 fun1 fun2 m1 m2 =
  isNotBottomP (L.mergeWithKey f12 (L.mapMaybe f1) (L.mapMaybe f2) m1 m2)
  where
    f12 = coerce (applyFunc3 fun12) :: OrdA -> A -> B -> Maybe C
    f1 = applyFun fun1
    f2 = applyFun fun2

prop_strictMap :: Func A (Bot B) -> Map OrdA A -> Property
prop_strictMap fun m =
  isBottom (M.map f m) === isBottom (M.mapWithKey (const f) m)
  where
    f = coerce (applyFunc fun) :: A -> B

prop_lazyMap :: Func A (Bot B) -> Map OrdA A -> Property
prop_lazyMap fun m = isNotBottomP (L.map f m)
  where
    f = coerce (applyFunc fun) :: A -> B

prop_strictMapWithKey :: Func2 OrdA A (Bot B) -> Map OrdA A -> Property
prop_strictMapWithKey fun m =
  isBottom (M.mapWithKey f m) ===
  any (isBottom . uncurry f) (M.toList m)
  where
    f = coerce (applyFunc2 fun) :: OrdA -> A -> B

prop_lazyMapWithKey :: Func2 OrdA A (Bot B) -> Map OrdA A -> Property
prop_lazyMapWithKey fun m = isNotBottomP (L.mapWithKey f m)
  where
    f = coerce (applyFunc2 fun) :: OrdA -> A -> B

prop_strictTraverseWithKey
  :: Func2 OrdA A (Identity (Bot B)) -> Map OrdA A -> Property
prop_strictTraverseWithKey fun m =
  isBottom (runIdentity (M.traverseWithKey f m)) ===
  any (isBottom . runIdentity . uncurry f) (M.toList m)
  where
    f = coerce (applyFunc2 fun) :: OrdA -> A -> Identity B

prop_lazyTraverseWithKey
  :: Func2 OrdA A (Identity (Bot B)) -> Map OrdA A -> Property
prop_lazyTraverseWithKey fun m =
  isNotBottomP (runIdentity (L.traverseWithKey f m))
  where
    f = coerce (applyFunc2 fun) :: OrdA -> A -> Identity B

prop_strictTraverseMaybeWithKey
  :: Func2 OrdA A (Identity (Maybe (Bot B))) -> Map OrdA A -> Property
prop_strictTraverseMaybeWithKey fun m =
  isBottom (runIdentity (M.traverseMaybeWithKey f m)) ===
  any (maybe False isBottom . runIdentity . uncurry f) (M.toList m)
  where
    f = coerce (applyFunc2 fun) :: OrdA -> A -> Identity (Maybe B)

prop_lazyTraverseMaybeWithKey
  :: Func2 OrdA A (Identity (Maybe (Bot B))) -> Map OrdA A -> Property
prop_lazyTraverseMaybeWithKey fun m =
  isNotBottomP (runIdentity (L.traverseMaybeWithKey f m))
  where
    f = coerce (applyFunc2 fun) :: OrdA -> A -> Identity (Maybe B)

prop_strictMapAccum
  :: Func2 A B (A, Bot C) -> A -> Map OrdA B -> Property
prop_strictMapAccum fun z m =
  isBottom (snd (M.mapAccum f z m)) ===
  isBottom (M.fromList $ snd $
    List.mapAccumL (\z' (k, x) -> fmap ((,) k) (f z' x)) z (M.toList m))
  where
    f = coerce (applyFunc2 fun) :: A -> B -> (A, C)

prop_lazyMapAccum
  :: Func2 A B (A, Bot C) -> A -> Map OrdA B -> Property
prop_lazyMapAccum fun z m = isNotBottomP (snd (L.mapAccum f z m))
  where
    f = coerce (applyFunc2 fun) :: A -> B -> (A, C)

prop_strictMapAccumWithKey
  :: Func3 A OrdA B (A, Bot C) -> A -> Map OrdA B -> Property
prop_strictMapAccumWithKey fun z m =
  isBottom (snd (M.mapAccumWithKey f z m)) ===
  isBottom (M.fromList $ snd $
    List.mapAccumL (\z' (k, x) -> fmap ((,) k) (f z' k x)) z (M.toList m))
  where
    f = coerce (applyFunc3 fun) :: A -> OrdA -> B -> (A, C)

prop_lazyMapAccumWithKey
  :: Func3 A OrdA B (A, Bot C) -> A -> Map OrdA B -> Property
prop_lazyMapAccumWithKey fun z m = isNotBottomP (snd (L.mapAccumWithKey f z m))
  where
    f = coerce (applyFunc3 fun) :: A -> OrdA -> B -> (A, C)

prop_strictMapAccumRWithKey
  :: Func3 A OrdA B (A, Bot C) -> A -> Map OrdA B -> Property
prop_strictMapAccumRWithKey fun z m =
  isBottom (snd (M.mapAccumRWithKey f z m)) ===
  isBottom (M.fromList $ snd $
    List.mapAccumR (\z' (k, x) -> fmap ((,) k) (f z' k x)) z (M.toList m))
  where
    f = coerce (applyFunc3 fun) :: A -> OrdA -> B -> (A, C)

prop_lazyMapAccumRWithKey
  :: Func3 A OrdA B (A, Bot C) -> A -> Map OrdA B -> Property
prop_lazyMapAccumRWithKey fun z m =
  isNotBottomP (snd (L.mapAccumRWithKey f z m))
  where
    f = coerce (applyFunc3 fun) :: A -> OrdA -> B -> (A, C)

prop_strictMapKeysWith
  :: Func2 A A (Bot A) -> Func OrdA OrdB -> Map OrdA A -> Property
prop_strictMapKeysWith fun kfun m =
  isBottom (M.mapKeysWith f kf m) ===
  isBottom (M.fromListWith f $ map (\(k, x) -> (kf k, x)) $ M.toList m)
  where
    f = coerce (applyFunc2 fun) :: A -> A -> A
    kf = applyFunc kfun

prop_lazyMapKeysWith
  :: Func2 A A (Bot A) -> Func OrdA OrdB -> Map OrdA A -> Property
prop_lazyMapKeysWith fun kfun m = isNotBottomP (L.mapKeysWith f kf m)
  where
    f = coerce (applyFunc2 fun) :: A -> A -> A
    kf = applyFunc kfun

prop_strictMapMaybe
  :: Func A (Maybe (Bot B)) -> Map OrdA A -> Property
prop_strictMapMaybe fun m =
  isBottom (M.mapMaybe f m) === isBottom (M.mapMaybeWithKey (const f) m)
  where
    f = coerce (applyFunc fun) :: A -> Maybe B

prop_lazyMapMaybe
  :: Func A (Maybe (Bot B)) -> Map OrdA A -> Property
prop_lazyMapMaybe fun m = isNotBottomP (L.mapMaybe f m)
  where
    f = coerce (applyFunc fun) :: A -> Maybe B

prop_strictMapMaybeWithKey
  :: Func2 OrdA A (Maybe (Bot B)) -> Map OrdA A -> Property
prop_strictMapMaybeWithKey fun m =
  isBottom (M.mapMaybeWithKey f m) ===
  isBottom (M.fromList $ mapMaybe (\(k, x) -> ((,) k) <$> f k x) $ M.toList m)
  where
    f = coerce (applyFunc2 fun) :: OrdA -> A -> Maybe B

prop_lazyMapMaybeWithKey
  :: Func2 OrdA A (Maybe (Bot B)) -> Map OrdA A -> Property
prop_lazyMapMaybeWithKey fun m = isNotBottomP (L.mapMaybeWithKey f m)
  where
    f = coerce (applyFunc2 fun) :: OrdA -> A -> Maybe B

prop_strictMapEither
  :: Func A (Either (Bot B) (Bot C)) -> Map OrdA A -> Property
prop_strictMapEither fun m =
  isBottom (M.mapEither f m) ===
  isBottom (M.mapEitherWithKey (const f) m)
  where
    f = coerce (applyFunc fun) :: A -> Either B C

prop_lazyMapEither
  :: Func A (Either (Bot B) (Bot C)) -> Map OrdA A -> Property
prop_lazyMapEither fun m =
  property $ case L.mapEither f m of
    (m1, m2) -> not (isBottom m1 || isBottom m2)
  where
    f = coerce (applyFunc fun) :: A -> Either B C

prop_strictMapEitherWithKey
 :: Func2 OrdA A (Either (Bot B) (Bot C)) -> Map OrdA A -> Property
prop_strictMapEitherWithKey fun m =
  isBottom (M.mapEitherWithKey f m) ===
  isBottom
    ((\(!_, !_) -> ()) $ -- Strict in both
     bimap M.fromList M.fromList $
     partitionEithers $
     map (\(k, x) -> bimap ((,) k) ((,) k) (f k x)) $
     M.toList m)
  where
    f = coerce (applyFunc2 fun) :: OrdA -> A -> Either B C

prop_lazyMapEitherWithKey
 :: Func2 OrdA A (Either (Bot B) (Bot C)) -> Map OrdA A -> Property
prop_lazyMapEitherWithKey fun m =
  property $ case L.mapEitherWithKey f m of
    (m1, m2) -> not (isBottom m1 || isBottom m2)
  where
    f = coerce (applyFunc2 fun) :: OrdA -> A -> Either B C

prop_strictUpdateAt :: Func2 OrdA A (Bot A) -> Map OrdA A -> Property
prop_strictUpdateAt fun m = not (M.null m) ==>
  forAll (choose (0, M.size m - 1)) $ \k ->
    isBottom (M.updateAt (\k' x -> Just (f k' x)) k m) ===
    isBottom (uncurry f (M.elemAt k m))
  where
    f = coerce (applyFunc2 fun)

prop_lazyUpdateAt :: Func2 OrdA A (Bot A) -> Map OrdA A -> Property
prop_lazyUpdateAt fun m = not (L.null m) ==>
  forAll (choose (0, L.size m - 1)) $ \k ->
    isNotBottomP (L.updateAt (\k' x -> Just (f k' x)) k m)
  where
    f = coerce (applyFunc2 fun)

prop_strictUpdateMin :: Func A (Bot A) -> Map OrdA A -> Property
prop_strictUpdateMin fun m =
  isBottom (M.updateMin (Just . f) m) ===
  isBottom (M.updateMinWithKey (\_ x -> Just (f x)) m)
  where
    f = coerce (applyFunc fun)

prop_lazyUpdateMin :: Func A (Bot A) -> Map OrdA A -> Property
prop_lazyUpdateMin fun m = isNotBottomP (L.updateMin (Just . f) m)
  where
    f = coerce (applyFunc fun)

prop_strictUpdateMinWithKey
  :: Func2 OrdA A (Bot A) -> Map OrdA A -> Property
prop_strictUpdateMinWithKey fun m = not (M.null m) ==>
  isBottom (M.updateMinWithKey (\k x -> Just (f k x)) m) ===
  isBottom (uncurry f (M.findMin m))
  where
    f = coerce (applyFunc2 fun)

prop_lazyUpdateMinWithKey
  :: Func2 OrdA A (Bot A) -> Map OrdA A -> Property
prop_lazyUpdateMinWithKey fun m = not (L.null m) ==>
  isNotBottomP (L.updateMinWithKey (\k x -> Just (f k x)) m)
  where
    f = coerce (applyFunc2 fun)

prop_strictUpdateMax :: Func A (Bot A) -> Map OrdA A -> Property
prop_strictUpdateMax fun m =
  isBottom (M.updateMax (Just . f) m) ===
  isBottom (M.updateMaxWithKey (\_ x -> Just (f x)) m)
  where
    f = coerce (applyFunc fun)

prop_lazyUpdateMax :: Func A (Bot A) -> Map OrdA A -> Property
prop_lazyUpdateMax fun m = isNotBottomP (L.updateMax (Just . f) m)
  where
    f = coerce (applyFunc fun)

prop_strictUpdateMaxWithKey
  :: Func2 OrdA A (Bot A) -> Map OrdA A -> Property
prop_strictUpdateMaxWithKey fun m = not (M.null m) ==>
  isBottom (M.updateMaxWithKey (\k x -> Just (f k x)) m) ===
  isBottom (uncurry f (M.findMax m))
  where
    f = coerce (applyFunc2 fun)

prop_lazyUpdateMaxWithKey
  :: Func2 OrdA A (Bot A) -> Map OrdA A -> Property
prop_lazyUpdateMaxWithKey fun m = not (M.null m) ==>
  isNotBottomP (L.updateMaxWithKey (\k x -> Just (f k x)) m)
  where
    f = coerce (applyFunc2 fun)

prop_strictMerge
  :: WhenMissingFunc OrdA A (Bot C) C (Bot C)
  -> WhenMissingFunc OrdA B (Bot C) C (Bot C)
  -> WhenMatchedFunc OrdA A B (Bot C) C (Bot C)
  -> Map OrdA A
  -> Map OrdA B
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
    misfun1' = coerce misfun1 :: WhenMissingFunc OrdA A C C C
    misfun2' = coerce misfun2 :: WhenMissingFunc OrdA B C C C
    matfun' = coerce matfun :: WhenMatchedFunc OrdA A B C C C
    mis1 = toStrictWhenMissing misfun1'
    mis2 = toStrictWhenMissing misfun2'
    mat = toStrictWhenMatched matfun'
    misf1 = whenMissingApplyStrict misfun1'
    misf2 = whenMissingApplyStrict misfun2'
    matf = whenMatchedApplyStrict  matfun'

prop_lazyMerge
  :: WhenMissingFunc OrdA A (Bot C) C (Bot C)
  -> WhenMissingFunc OrdA B (Bot C) C (Bot C)
  -> WhenMatchedFunc OrdA A B (Bot C) C (Bot C)
  -> Map OrdA A
  -> Map OrdA B
  -> Property
prop_lazyMerge misfun1 misfun2 matfun m1 m2 =
  isNotBottomP (MMerge.merge mis1 mis2 mat m1 m2)
  where
    mis1 = toLazyWhenMissing (coerce misfun1 :: WhenMissingFunc OrdA A C C C)
    mis2 = toLazyWhenMissing (coerce misfun2 :: WhenMissingFunc OrdA B C C C)
    mat = toLazyWhenMatched (coerce matfun :: WhenMatchedFunc OrdA A B C C C)

prop_strictMergeA
  :: WhenMissingFunc OrdA A (Bot C) C (Bot C)
  -> WhenMissingFunc OrdA B (Bot C) C (Bot C)
  -> WhenMatchedFunc OrdA A B (Bot C) C (Bot C)
  -> Map OrdA A
  -> Map OrdA B
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
    misfun1' = coerce misfun1 :: WhenMissingFunc OrdA A C C C
    misfun2' = coerce misfun2 :: WhenMissingFunc OrdA B C C C
    matfun' = coerce matfun :: WhenMatchedFunc OrdA A B C C C
    mis1 = toStrictWhenMissingA misfun1'
    mis2 = toStrictWhenMissingA misfun2'
    mat = toStrictWhenMatchedA matfun'
    misf1 = whenMissingApplyStrict misfun1'
    misf2 = whenMissingApplyStrict misfun2'
    matf = whenMatchedApplyStrict  matfun'

prop_lazyMergeA
  :: WhenMissingFunc OrdA A (Bot C) C (Bot C)
  -> WhenMissingFunc OrdA B (Bot C) C (Bot C)
  -> WhenMatchedFunc OrdA A B (Bot C) C (Bot C)
  -> Map OrdA A
  -> Map OrdA B
  -> Property
prop_lazyMergeA misfun1 misfun2 matfun m1 m2 =
  isNotBottomP (runIdentity (LMerge.mergeA mis1 mis2 mat m1 m2))
  where
    mis1 = toLazyWhenMissingA (coerce misfun1 :: WhenMissingFunc OrdA A C C C)
    mis2 = toLazyWhenMissingA (coerce misfun2 :: WhenMissingFunc OrdA B C C C)
    mat = toLazyWhenMatchedA (coerce matfun :: WhenMatchedFunc OrdA A B C C C)

------------------------------------------------------------------------
-- ** Strict module

pSingletonKeyStrict :: Int -> Bool
pSingletonKeyStrict v = isBottom $ M.singleton (bottom :: Int) v

pSingletonValueStrict :: Int -> Bool
pSingletonValueStrict k = isBottom $ (M.singleton k (bottom :: Int))

pFindWithDefaultKeyStrict :: Int -> Map Int Int -> Bool
pFindWithDefaultKeyStrict def m = isBottom $ M.findWithDefault def bottom m

pFindWithDefaultValueStrict :: Int -> Map Int Int -> Bool
pFindWithDefaultValueStrict k m =
    M.member k m || (isBottom $ M.findWithDefault bottom k m)

pAdjustKeyStrict :: Fun Int Int -> Map Int Int -> Bool
pAdjustKeyStrict f m = isBottom $ M.adjust (apply f) bottom m

pAdjustValueStrict :: Int -> Map Int Int -> Bool
pAdjustValueStrict k m
    | k `M.member` m = isBottom $ M.adjust (const bottom) k m
    | otherwise       = case M.keys m of
        []     -> True
        (k':_) -> isBottom $ M.adjust (const bottom) k' m

pInsertKeyStrict :: Int -> Map Int Int -> Bool
pInsertKeyStrict v m = isBottom $ M.insert bottom v m

pInsertValueStrict :: Int -> Map Int Int -> Bool
pInsertValueStrict k m = isBottom $ M.insert k bottom m

pInsertWithKeyStrict :: Fun (Int, Int) Int -> Int -> Map Int Int -> Bool
pInsertWithKeyStrict f v m = isBottom $ M.insertWith (apply2 f) bottom v m

pInsertWithValueStrict :: Fun (Int, Int) Int -> Int -> Int -> Map Int Int
                       -> Bool
pInsertWithValueStrict f k v m
    | M.member k m = (isBottom $ M.insertWith (const2 bottom) k v m) &&
                     not (isBottom $ M.insertWith (const2 1) k bottom m)
    | otherwise    = isBottom $ M.insertWith (apply2 f) k bottom m

pInsertLookupWithKeyKeyStrict :: Fun (Int, Int, Int) Int -> Int
                              -> Map Int Int -> Bool
pInsertLookupWithKeyKeyStrict f v m = isBottom $ M.insertLookupWithKey (apply3 f) bottom v m

pInsertLookupWithKeyValueStrict :: Fun (Int, Int, Int) Int -> Int -> Int
                                -> Map Int Int -> Bool
pInsertLookupWithKeyValueStrict f k v m
    | M.member k m = (isBottom $ M.insertLookupWithKey (const3 bottom) k v m) &&
                     not (isBottom $ M.insertLookupWithKey (const3 1) k bottom m)
    | otherwise    = isBottom $ M.insertLookupWithKey (apply3 f) k bottom m

#if __GLASGOW_HASKELL__ >= 806
pStrictFoldr' :: Map Int Int -> Property
pStrictFoldr' m = whnfHasNoThunks (M.foldr' (:) [] m)
#endif

#if __GLASGOW_HASKELL__ >= 806
pStrictFoldl' :: Map Int Int -> Property
pStrictFoldl' m = whnfHasNoThunks (M.foldl' (flip (:)) [] m)
#endif

#if __GLASGOW_HASKELL__ >= 806
pStrictFoldrWithKey' :: Map Int Int -> Property
pStrictFoldrWithKey' m = whnfHasNoThunks (M.foldrWithKey' (\_ a as -> a : as) [] m)
#endif

#if __GLASGOW_HASKELL__ >= 806
pStrictFoldlWithKey' :: Map Int Int -> Property
pStrictFoldlWithKey' m = whnfHasNoThunks (M.foldlWithKey' (\as _ a -> a : as) [] m)
#endif

------------------------------------------------------------------------
-- * Test list

tests :: [TestTree]
tests =
    [
    -- Basic interface
      testGroup "Map.Strict"
      [ testProperty "singleton is key-strict" pSingletonKeyStrict
      , testProperty "singleton is value-strict" pSingletonValueStrict
      , testProperty "member is key-strict" $ keyStrict M.member
      , testProperty "lookup is key-strict" $ keyStrict M.lookup
      , testProperty "findWithDefault is key-strict" pFindWithDefaultKeyStrict
      , testProperty "findWithDefault is value-strict" pFindWithDefaultValueStrict
      , testProperty "! is key-strict" $ keyStrict (flip (M.!))
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
#if __GLASGOW_HASKELL__ >= 806
      , testProperty "strict foldr'" pStrictFoldr'
      , testProperty "strict foldl'" pStrictFoldl'
      , testProperty "strict foldrWithKey'" pStrictFoldrWithKey'
      , testProperty "strict foldlWithKey'" pStrictFoldlWithKey'
#endif
      ]
    , testGroup "Map.Strict construction"
      [ testProperty "singleton" prop_strictSingleton
      , testProperty "fromSet" prop_strictFromSet
      , testProperty "fromArgSet" prop_strictFromArgSet
      , testProperty "fromList" prop_strictFromList
      , testProperty "fromListWith" prop_strictFromListWith
      , testProperty "fromListWithKey" prop_strictFromListWithKey
      , testProperty "fromAscList" prop_strictFromAscList
      , testProperty "fromAscListWith" prop_strictFromAscListWith
      , testProperty "fromAscListWithKey" prop_strictFromAscListWithKey
      , testProperty "fromDistinctAscList" prop_strictFromDistinctAscList
      , testProperty "fromDescList" prop_strictFromDescList
      , testProperty "fromDescListWith" prop_strictFromDescListWith
      , testProperty "fromDescListWithKey" prop_strictFromDescListWithKey
      , testProperty "fromDistinctDescList" prop_strictFromDistinctDescList
      , testProperty "insert" prop_strictInsert
      , testProperty "insertWith" prop_strictInsertWith
      , testProperty "insertWithKey" prop_strictInsertWithKey
      , testProperty "insertLookupWithKey" prop_strictInsertLookupWithKey
      , testProperty "adjust" prop_strictAdjust
      , testProperty "adjustWithKey" prop_strictAdjustWithKey
      , testProperty "update" prop_strictUpdate
      , testProperty "updateWithKey" prop_strictUpdateWithKey
      , testProperty "updateLookupWithKey" prop_strictUpdateLookupWithKey
      , testProperty "alter" prop_strictAlter
      , testProperty "alterF" prop_strictAlterF
      , testProperty "unionWith" prop_strictUnionWith
      , testProperty "unionWithKey" prop_strictUnionWithKey
      , testProperty "unionsWith" prop_strictUnionsWith
      , testProperty "differenceWith" prop_strictDifferenceWith
      , testProperty "differenceWithKey" prop_strictDifferenceWithKey
      , testProperty "intersectionWith" prop_strictIntersectionWith
      , testProperty "intersectionWithKey" prop_strictIntersectionWithKey
      , testProperty "mergeWithKey" prop_strictMergeWithKey
      , testProperty "map" prop_strictMap
      , testProperty "mapWithKey" prop_strictMapWithKey
      , testProperty "traverseWithKey" prop_strictTraverseWithKey
      , testProperty "traverseMaybeWithKey" prop_strictTraverseMaybeWithKey
      , testProperty "mapAccum" prop_strictMapAccum
      , testProperty "mapAccumWithKey" prop_strictMapAccumWithKey
      , testProperty "mapAccumRWithKey" prop_strictMapAccumRWithKey
      , testProperty "mapKeysWith" prop_strictMapKeysWith
      , testProperty "mapMaybe" prop_strictMapMaybe
      , testProperty "mapMaybeWithKey" prop_strictMapMaybeWithKey
      , testProperty "mapEither" prop_strictMapEither
      , testProperty "mapEitherWithKey" prop_strictMapEitherWithKey
      , testProperty "updateAt" prop_strictUpdateAt
      , testProperty "updateMin" prop_strictUpdateMin
      , testProperty "updateMinWithKey" prop_strictUpdateMinWithKey
      , testProperty "updateMax" prop_strictUpdateMax
      , testProperty "updateMaxWithKey" prop_strictUpdateMaxWithKey
      , testProperty "merge" prop_strictMerge
      , testProperty "mergeA" prop_strictMergeA
      ]
    , testGroup "Map.Lazy construction"
      [ testProperty "singleton" prop_lazySingleton
      , testProperty "fromSet" prop_lazyFromSet
      , testProperty "fromArgSet" prop_lazyFromArgSet
      , testProperty "fromList" prop_lazyFromList
      , testProperty "fromListWith" prop_lazyFromListWith
      , testProperty "fromListWithKey" prop_lazyFromListWithKey
      , testProperty "fromAscList" prop_lazyFromAscList
      , testProperty "fromAscListWith" prop_lazyFromAscListWith
      , testProperty "fromAscListWithKey" prop_lazyFromAscListWithKey
      , testProperty "fromDistinctAscList" prop_lazyFromDistinctAscList
      , testProperty "fromDescList" prop_lazyFromDescList
      , testProperty "fromDescListWith" prop_lazyFromDescListWith
      , testProperty "fromDescListWithKey" prop_lazyFromDescListWithKey
      , testProperty "fromDistinctDescList" prop_lazyFromDistinctDescList
      , testProperty "insert" prop_lazyInsert
      , testProperty "insertWith" prop_lazyInsertWith
      , testProperty "insertWithKey" prop_lazyInsertWithKey
      , testProperty "insertLookupWithKey" prop_lazyInsertLookupWithKey
      , testProperty "adjust" prop_lazyAdjust
      , testProperty "adjustWithKey" prop_lazyAdjustWithKey
      , testProperty "update" prop_lazyUpdate
      , testProperty "updateWithKey" prop_lazyUpdateWithKey
      , testProperty "updateLookupWithKey" prop_lazyUpdateLookupWithKey
      , testProperty "alter" prop_lazyAlter
      , testProperty "alterF" prop_lazyAlterF
      , testProperty "unionWith" prop_lazyUnionWith
      , testProperty "unionWithKey" prop_lazyUnionWithKey
      , testProperty "unionsWith" prop_lazyUnionsWith
      , testProperty "differenceWith" prop_lazyDifferenceWith
      , testProperty "differenceWithKey" prop_lazyDifferenceWithKey
      , testProperty "intersectionWith" prop_lazyIntersectionWith
      , testProperty "intersectionWithKey" prop_lazyIntersectionWithKey
      , testProperty "mergeWithKey" prop_lazyMergeWithKey
      , testProperty "map" prop_lazyMap
      , testProperty "mapWithKey" prop_lazyMapWithKey
      , testProperty "traverseWithKey" prop_lazyTraverseWithKey
      , testProperty "traverseMaybeWithKey" prop_lazyTraverseMaybeWithKey
      , testProperty "mapAccum" prop_lazyMapAccum
      , testProperty "mapAccumWithKey" prop_lazyMapAccumWithKey
      , testProperty "mapAccumRWithKey" prop_lazyMapAccumRWithKey
      , testProperty "mapKeysWith" prop_lazyMapKeysWith
      , testProperty "mapMaybe" prop_lazyMapMaybe
      , testProperty "mapMaybeWithKey" prop_lazyMapMaybeWithKey
      , testProperty "mapEither" prop_lazyMapEither
      , testProperty "mapEitherWithKey" prop_lazyMapEitherWithKey
      , testProperty "updateAt" prop_lazyUpdateAt
      , testProperty "updateMin" prop_lazyUpdateMin
      , testProperty "updateMinWithKey" prop_lazyUpdateMinWithKey
      , testProperty "updateMax" prop_lazyUpdateMax
      , testProperty "updateMaxWithKey" prop_lazyUpdateMaxWithKey
      , testProperty "merge" prop_lazyMerge
      , testProperty "mergeA" prop_lazyMergeA
      ]
    ]

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain $ testGroup "map-strictness" tests

------------------------------------------------------------------------
-- * Utilities

keyStrict :: (Int -> Map Int Int -> a) -> Map Int Int -> Bool
keyStrict f m = isBottom $ f bottom m

isNotBottomP :: a -> Property
isNotBottomP = property . not . isBottom

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

data WhenMatchedFunc k x y z a b
  = MaybeMatchedFunc (Func3 k x y (Maybe b))
  | FmapMaybeMatchedFunc (Func a b) (Func3 k x y (Maybe z))
  | MatchedFunc (Func3 k x y b)
  | FmapMatchedFunc (Func a b) (Func3 k x y z)
  deriving Show

instance
  ( CoArbitrary k, Function k
  , CoArbitrary x, Function x
  , CoArbitrary y, Function y
  , Arbitrary z
  , CoArbitrary a, Function a, Arbitrary a
  , Arbitrary b
  ) => Arbitrary (WhenMatchedFunc k x y z a b) where
  arbitrary = oneof
    [ MaybeMatchedFunc <$> arbitrary
    , FmapMaybeMatchedFunc <$> arbitrary <*> arbitrary
    , MatchedFunc <$> arbitrary
    , FmapMatchedFunc <$> arbitrary <*> arbitrary
    ]
  shrink wmf = case wmf of
    MaybeMatchedFunc fun -> MaybeMatchedFunc <$> shrink fun
    FmapMaybeMatchedFunc fun2 fun1 ->
      uncurry FmapMaybeMatchedFunc <$> shrink (fun2, fun1)
    MatchedFunc fun -> MatchedFunc <$> shrink fun
    FmapMatchedFunc fun2 fun1 ->
      uncurry FmapMatchedFunc <$> shrink (fun2, fun1)

toStrictWhenMatched
  :: WhenMatchedFunc k x y z z z2 -> WhenMatched Identity k x y z2
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
  :: WhenMatchedFunc k x y z z z2 -> WhenMatched Identity k x y z2
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
  :: WhenMatchedFunc k x y z z z2 -> WhenMatched Identity k x y z2
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
  :: WhenMatchedFunc k x y z z z2 -> WhenMatched Identity k x y z2
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
  :: WhenMatchedFunc k x y z z z2 -> k -> x -> y -> Maybe z2
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

data WhenMissingFunc k x y a b
  = MapMaybeMissingFunc (Func2 k x (Maybe b))
  | FmapMapMaybeMissingFunc (Func a b) (Func2 k x (Maybe a))
  | MapMissingFunc (Func2 k x b)
  | FmapMapMissingFunc (Func a b) (Func2 k x a)
  deriving Show

instance
  ( CoArbitrary k, Function k
  , CoArbitrary x, Function x
  , Arbitrary y
  , CoArbitrary a, Function a, Arbitrary a
  , Arbitrary b
  ) => Arbitrary (WhenMissingFunc k x y a b) where
  arbitrary = oneof
    [ MapMaybeMissingFunc <$> arbitrary
    , FmapMapMaybeMissingFunc <$> arbitrary <*> arbitrary
    , MapMissingFunc <$> arbitrary
    , FmapMapMissingFunc <$> arbitrary <*> arbitrary
    ]
  shrink wmf = case wmf of
    MapMaybeMissingFunc fun -> MapMaybeMissingFunc <$> shrink fun
    FmapMapMaybeMissingFunc fun2 fun1 ->
      uncurry FmapMapMaybeMissingFunc <$> shrink (fun2, fun1)
    MapMissingFunc fun -> MapMissingFunc <$> shrink fun
    FmapMapMissingFunc fun2 fun1 ->
      uncurry FmapMapMissingFunc <$> shrink (fun2, fun1)

toStrictWhenMissing :: WhenMissingFunc k x y y y2 -> WhenMissing Identity k x y2
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
  :: WhenMissingFunc k x y y y2 -> WhenMissing Identity k x y2
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

toLazyWhenMissing :: WhenMissingFunc k x y y y2 -> WhenMissing Identity k x y2
toLazyWhenMissing wmf = case wmf of
  MapMaybeMissingFunc fun -> LMerge.mapMaybeMissing (applyFunc2 fun)
  FmapMapMaybeMissingFunc fun2 fun1 ->
    LMerge.mapWhenMissing (applyFunc fun2) $
    LMerge.mapMaybeMissing (applyFunc2 fun1)
  MapMissingFunc fun -> LMerge.mapMissing (applyFunc2 fun)
  FmapMapMissingFunc fun2 fun1 ->
    LMerge.mapWhenMissing (applyFunc fun2) $
    LMerge.mapMissing (applyFunc2 fun1)

toLazyWhenMissingA :: WhenMissingFunc k x y y y2 -> WhenMissing Identity k x y2
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

whenMissingApplyStrict :: WhenMissingFunc k x y y y2 -> k -> x -> Maybe y2
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
