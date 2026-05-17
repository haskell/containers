{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Sequence.Internal
  ( Sized (..)
  , Seq (Seq)
  , FingerTree(..)
  , Node(..)
  , Elem(..)
  , Digit (..)
  , node2
  , node3
  , deep )

import Data.Sequence

import Control.Applicative (Applicative(..), liftA2)
import Control.Arrow ((***))
import Control.Monad.Trans.State.Strict
import qualified Control.Monad as Monad
import Data.Array (listArray)
import Data.Coerce (coerce)
import Data.Foldable (Foldable(foldl, foldl1, foldr, foldr1, foldMap, fold), all, sum, foldl', foldr')
import Data.Functor ((<$>), (<$))
import Data.Maybe (listToMaybe)
import qualified Data.Maybe as Maybe
import Data.Function (on)
import Data.Monoid (Monoid(..), All(..), Endo(..), Dual(..))
import Data.Proxy (Proxy(..))
import Data.Ord (comparing)
import Data.Semigroup (stimes, stimesMonoid)
import Data.Traversable (Traversable(traverse), sequenceA)
import Prelude hiding (
  lookup, null, length, take, drop, splitAt,
  foldl, foldl', foldl1, foldr, foldr1, scanl, scanl1, scanr, scanr1,
  filter, reverse, replicate, zip, zipWith, zip3, zipWith3,
  all, sum)
import qualified Prelude
import qualified Data.List
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((><))
import Test.QuickCheck.Function (apply)
import Test.QuickCheck.Poly (A, OrdA, B, OrdB, C)
import qualified Test.QuickCheck.Classes.Base as Laws
import Control.Monad.Zip (MonadZip (..))
import Control.DeepSeq (deepseq)
import Control.Monad.Fix (MonadFix (..))
import Test.Tasty.HUnit
import Test.ChasingBottoms.IsBottom (isBottom)
#if __GLASGOW_HASKELL__ >= 914
import qualified Language.Haskell.TH.Lift as TH
#else
import qualified Language.Haskell.TH.Syntax as TH
#endif

import Utils.Strictness (Bot(..), Func2, applyFunc2)
import Utils.QuickCheckClasses (testLaws)

main :: IO ()
main = defaultMain $ testGroup "seq-properties"
       [ test_lift
#if __GLASGOW_HASKELL__ >= 810
       , test_liftTyped
#endif
       , testProperty "fmap" prop_fmap
       , testProperty "(<$)" prop_constmap
       , testProperty "foldr" prop_foldr
       , testProperty "foldr'" prop_foldr'
       , testProperty "foldr1" prop_foldr1
       , testProperty "foldl" prop_foldl
       , testProperty "foldl'" prop_foldl'
       , testProperty "foldl1" prop_foldl1
       , testProperty "(==)" prop_equals
       , testProperty "compare" prop_compare
       , testProperty "mappend" prop_mappend
       , testProperty "singleton" prop_singleton
       , testProperty "(<|)" prop_cons
       , testProperty "(|>)" prop_snoc
       , testProperty "(><)" prop_append
       , testProperty "fromList" prop_fromList
       , testProperty "fromList_long" prop_fromList_long
       , testProperty "fromFunction" prop_fromFunction
       , testProperty "fromArray" prop_fromArray
       , testProperty "replicate" prop_replicate
       , testProperty "replicateA" prop_replicateA
       , testProperty "replicateM" prop_replicateM
       , testProperty "iterateN" prop_iterateN
       , testProperty "unfoldr" prop_unfoldr
       , testProperty "unfoldl" prop_unfoldl
       , testProperty "null" prop_null
       , testProperty "length" prop_length
       , testProperty "viewl" prop_viewl
       , testProperty "viewr" prop_viewr
       , testProperty "scanl" prop_scanl
       , testProperty "scanl1" prop_scanl1
       , testProperty "scanr" prop_scanr
       , testProperty "scanr1" prop_scanr1
       , testProperty "tails" prop_tails
       , testProperty "inits" prop_inits
       , testProperty "takeWhileL" prop_takeWhileL
       , testProperty "takeWhileR" prop_takeWhileR
       , testProperty "dropWhileL" prop_dropWhileL
       , testProperty "dropWhileR" prop_dropWhileR
       , testProperty "spanl" prop_spanl
       , testProperty "spanr" prop_spanr
       , testProperty "breakl" prop_breakl
       , testProperty "breakr" prop_breakr
       , testProperty "partition" prop_partition
       , testProperty "filter" prop_filter
       , testProperty "mapMaybe" prop_mapMaybe
       , testProperty "sort" prop_sort
       , testProperty "sortStable" prop_sortStable
       , testProperty "sortBy" prop_sortBy
       , testProperty "sortOn" prop_sortOn
       , testProperty "sortOnStable" prop_sortOnStable
       , testProperty "unstableSort" prop_unstableSort
       , testProperty "unstableSortBy" prop_unstableSortBy
       , testProperty "unstableSortOn" prop_unstableSortOn
       , testProperty "index" prop_index
       , testProperty "(!?)" prop_safeIndex
       , testProperty "adjust" prop_adjust
       , testProperty "adjust'" prop_adjust'
       , testProperty "insertAt" prop_insertAt
       , testProperty "deleteAt" prop_deleteAt
       , testProperty "update" prop_update
       , testProperty "take" prop_take
       , testProperty "drop" prop_drop
       , testProperty "splitAt" prop_splitAt
       , testProperty "chunksOf" prop_chunksOf
       , testProperty "elemIndexL" prop_elemIndexL
       , testProperty "elemIndicesL" prop_elemIndicesL
       , testProperty "elemIndexR" prop_elemIndexR
       , testProperty "elemIndicesR" prop_elemIndicesR
       , testProperty "findIndexL" prop_findIndexL
       , testProperty "findIndicesL" prop_findIndicesL
       , testProperty "findIndexR" prop_findIndexR
       , testProperty "findIndicesR" prop_findIndicesR
       , testProperty "foldlWithIndex" prop_foldlWithIndex
       , testProperty "foldrWithIndex" prop_foldrWithIndex
       , testProperty "mapWithIndex" prop_mapWithIndex
       , testProperty "foldMapWithIndex/foldlWithIndex" prop_foldMapWithIndexL
       , testProperty "foldMapWithIndex/foldrWithIndex" prop_foldMapWithIndexR
       , testProperty "traverseWithIndex" prop_traverseWithIndex
       , testProperty "reverse" prop_reverse
       , testProperty "zip" prop_zip
       , testProperty "zipWith" prop_zipWith
       , testProperty "zip3" prop_zip3
       , testProperty "zipWith3" prop_zipWith3
       , testProperty "zip4" prop_zip4
       , testProperty "zipWith4" prop_zipWith4
       , testProperty "mzip-naturality" prop_mzipNaturality
       , testProperty "mzip-preservation" prop_mzipPreservation
       , testProperty "munzip-lazy" prop_munzipLazy
       , testProperty "<*>" prop_ap
       , testProperty "<*> NOINLINE" prop_ap_NOINLINE
       , testProperty "liftA2" prop_liftA2
       , testProperty "*>" prop_then
       , testProperty "<*" prop_before
       , testProperty "cycleTaking" prop_cycleTaking
       , testProperty "intersperse" prop_intersperse
       , testProperty ">>=" prop_bind
       , testProperty "mfix" test_mfix
       , testProperty "Empty pattern" prop_empty_pat
       , testProperty "Empty constructor" prop_empty_con
       , testProperty "Left view pattern" prop_viewl_pat
       , testProperty "Left view constructor" prop_viewl_con
       , testProperty "Right view pattern" prop_viewr_pat
       , testProperty "Right view constructor" prop_viewr_con
       , testProperty "stimes" prop_stimes
       , testProperty "traverse" prop_traverse
       , testLaws $ Laws.eqLaws (Proxy :: Proxy (Seq A))
       , testLaws $ Laws.ordLaws (Proxy :: Proxy (Seq OrdA))
       , testLaws $ Laws.showLaws (Proxy :: Proxy (Seq A))
       , testLaws $ Laws.semigroupLaws (Proxy :: Proxy (Seq A))
       , testLaws $ Laws.monoidLaws (Proxy :: Proxy (Seq A))
-- Requires Arbitrary1 on GHC <8.6, skip
#if __GLASGOW_HASKELL__ >= 806
       , testLaws $ Laws.foldableLaws (Proxy :: Proxy Seq)
       , testLaws $ Laws.functorLaws (Proxy :: Proxy Seq)
       , testLaws $ Laws.traversableLaws (Proxy :: Proxy Seq)
       , testLaws $ Laws.applicativeLaws (Proxy :: Proxy Seq)
       , testLaws $ Laws.alternativeLaws (Proxy :: Proxy Seq)
       , testLaws $ Laws.monadLaws (Proxy :: Proxy Seq)
       , testLaws $ Laws.monadPlusLaws (Proxy :: Proxy Seq)
       , testLaws $ Laws.monadZipLaws (Proxy :: Proxy Seq)
#endif
       , testLaws $ Laws.isListLaws (Proxy :: Proxy (Seq A))
       , testGroup "strictness"
         [ testProperty "foldr" prop_strictness_foldr
         , testProperty "foldl" prop_strictness_foldl
         , testProperty "foldr'" prop_strictness_foldr'
         , testProperty "foldl'" prop_strictness_foldl'
         ]
       ]

------------------------------------------------------------------------
-- Arbitrary
------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Seq a) where
    arbitrary = Seq <$> arbitrary
    shrink (Seq x) = map Seq (shrink x)

instance Arbitrary a => Arbitrary (Elem a) where
    arbitrary = Elem <$> arbitrary

instance (Arbitrary a, Sized a) => Arbitrary (FingerTree a) where
    arbitrary = sized arb
      where
        arb :: (Arbitrary b, Sized b) => Int -> Gen (FingerTree b)
        arb 0 = return EmptyT
        arb 1 = Single <$> arbitrary
        arb n = do
            pr <- arbitrary
            sf <- arbitrary
            let n_pr = Prelude.length pr
                n_sf = Prelude.length sf
            -- adding n `div` 7 ensures that n_m >= 0, and makes more Singles
            let n_m = max (n `div` 7) ((n - n_pr - n_sf) `div` 3)
            m <- arb n_m
            return $ deep pr m sf

    shrink (Deep _ (One a) EmptyT (One b)) = [Single a, Single b]
    shrink (Deep _ pr m sf) =
        [deep pr' m sf | pr' <- shrink pr] ++
        [deep pr m' sf | m' <- shrink m] ++
        [deep pr m sf' | sf' <- shrink sf]
    shrink (Single x) = map Single (shrink x)
    shrink EmptyT = []

instance (Arbitrary a, Sized a) => Arbitrary (Node a) where
    arbitrary = oneof [
        node2 <$> arbitrary <*> arbitrary,
        node3 <$> arbitrary <*> arbitrary <*> arbitrary]

    shrink (Node2 _ a b) =
        [node2 a' b | a' <- shrink a] ++
        [node2 a b' | b' <- shrink b]
    shrink (Node3 _ a b c) =
        [node2 a b, node2 a c, node2 b c] ++
        [node3 a' b c | a' <- shrink a] ++
        [node3 a b' c | b' <- shrink b] ++
        [node3 a b c' | c' <- shrink c]

instance Arbitrary a => Arbitrary (Digit a) where
    arbitrary = oneof [
        One <$> arbitrary,
        Two <$> arbitrary <*> arbitrary,
        Three <$> arbitrary <*> arbitrary <*> arbitrary,
        Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary]

    shrink (One a) = map One (shrink a)
    shrink (Two a b) = [One a, One b]
    shrink (Three a b c) = [Two a b, Two a c, Two b c]
    shrink (Four a b c d) = [Three a b c, Three a b d, Three a c d, Three b c d]

------------------------------------------------------------------------
-- Valid trees
------------------------------------------------------------------------

class Valid a where
    valid :: a -> Bool

instance Valid (Elem a) where
    valid _ = True

instance Valid (Seq a) where
    valid (Seq xs) = valid xs

instance (Sized a, Valid a) => Valid (FingerTree a) where
    valid EmptyT = True
    valid (Single x) = valid x
    valid (Deep s pr m sf) =
        s == size pr + size m + size sf && valid pr && valid m && valid sf

instance (Sized a, Valid a) => Valid (Node a) where
    valid node = size node == sum (fmap size node) && all valid node

instance Valid a => Valid (Digit a) where
    valid = all valid

{--------------------------------------------------------------------
  The general plan is to compare each function with a list equivalent.
  Each operation should produce a valid tree representing the same
  sequence as produced by its list counterpart on corresponding inputs.
  (The list versions are often lazier, but these properties ignore
  strictness.)
--------------------------------------------------------------------}

-- Extra "polymorphic" test type
newtype D = D{ unD :: Integer }
  deriving ( Eq )

instance Show D where
  showsPrec n (D x) = showsPrec n x

instance Arbitrary D where
  arbitrary    = (D . (+1) . abs) `fmap` arbitrary
  shrink (D x) = [ D x' | x' <- shrink x, x' > 0 ]

instance CoArbitrary D where
  coarbitrary = coarbitrary . unD

-- instances

prop_fmap :: Fun A B -> Seq A -> Property
prop_fmap f xs =
  valid ys .&&.
  toList ys === map (applyFun f) (toList xs)
  where
    ys = fmap (applyFun f) xs

prop_constmap :: B -> Seq A -> Property
prop_constmap y xs =
  valid ys .&&.
  toList ys === map (const y) (toList xs)
  where
    ys = y <$ xs

prop_foldr :: Seq A -> Property
prop_foldr xs =
    foldr f z xs === Prelude.foldr f z (toList xs)
  where
    f = (:)
    z = []

prop_foldr' :: Seq A -> Property
prop_foldr' xs =
    foldr' f z xs === foldr' f z (toList xs)
  where
    f = (:)
    z = []

prop_foldr1 :: Seq Int -> Property
prop_foldr1 xs =
    not (null xs) ==> foldr1 f xs == Data.List.foldr1 f (toList xs)
  where f = (-)

prop_foldl :: Seq A -> Property
prop_foldl xs =
    foldl f z xs === Prelude.foldl f z (toList xs)
  where
    f = flip (:)
    z = []

prop_foldl' :: Seq A -> Property
prop_foldl' xs =
    foldl' f z xs === foldl' f z (toList xs)
  where
    f = flip (:)
    z = []

prop_foldl1 :: Seq Int -> Property
prop_foldl1 xs =
    not (null xs) ==> foldl1 f xs == Data.List.foldl1 f (toList xs)
  where f = (-)

prop_equals :: Seq A -> Seq A -> Property
prop_equals xs ys =
    (xs == ys) === (toList xs == toList ys)

prop_compare :: Seq OrdA -> Seq OrdA -> Property
prop_compare xs ys =
    compare xs ys === compare (toList xs) (toList ys)

prop_mappend :: Seq A -> Seq A -> Property
prop_mappend xs ys =
  valid zs .&&.
  toList zs === toList xs ++ toList ys
  where
    zs = mappend xs ys

-- * Construction

{-
    toList' empty ~= []
-}

prop_singleton :: A -> Property
prop_singleton x =
  valid xs .&&.
  toList xs === [x]
  where
    xs = singleton x

prop_cons :: A -> Seq A -> Property
prop_cons x xs =
  valid xs' .&&.
  toList xs' === x : toList xs
  where
    xs' = x <| xs

prop_snoc :: Seq A -> A -> Property
prop_snoc xs x =
  valid xs' .&&.
  toList xs' === toList xs ++ [x]
  where
    xs' = xs |> x

prop_append :: Seq A -> Seq A -> Property
prop_append xs ys =
  valid zs .&&.
  toList zs === toList xs ++ toList ys
  where
    zs = xs >< ys

prop_fromList :: [A] -> Property
prop_fromList xs =
  valid xs' .&&.
  toList xs' === xs
  where
    xs' = fromList xs

-- QuickCheck does not generate long lists by default (the current limit seems
-- to be 99), so we set the generator to use the max list length we want.
prop_fromList_long :: Property
prop_fromList_long = forAllShrink (resize 10000 arbitrary) shrink $ \xs ->
  let ys = fromList (xs :: [Bool])
  in valid ys .&&. toList ys === xs

prop_fromFunction :: NonNegative Int -> Fun Int A -> Property
prop_fromFunction (NonNegative n) f =
  valid xs .&&.
  toList xs === fmap (applyFun f) [0..n-1]
  where
    xs = fromFunction n (applyFun f)

prop_fromArray :: Int -> [A] -> Property
prop_fromArray startIdx xs =
  valid xs' .&&.
  toList xs' === xs
  where
    a = listArray (startIdx, startIdx + Prelude.length xs - 1) xs
    xs' = fromArray a

-- ** Repetition

prop_replicate :: NonNegative Int -> A -> Property
prop_replicate (NonNegative n) x =
  valid xs .&&.
  toList xs === Prelude.replicate n x
  where
    xs = replicate n x

prop_replicateA :: NonNegative Int -> Fun A A -> A -> Property
prop_replicateA (NonNegative n) f z =
  valid xs .&&.
  (toList xs, z') === runState (Monad.replicateM n act) z
  where
    act = get <* modify' (applyFun f)
    (xs, z') = runState (replicateA n act) z

prop_replicateM :: NonNegative Int -> Fun A A -> A -> Property
prop_replicateM (NonNegative n) f z =
  valid xs .&&.
  (toList xs, z') === runState (Monad.replicateM n act) z
  where
    act = get <* modify' (applyFun f)
    (xs, z') = runState (replicateM n act) z

-- ** Iterative construction

prop_iterateN :: NonNegative Int -> Fun A A -> A -> Property
prop_iterateN (NonNegative n) f x =
  valid xs .&&.
  toList xs === Prelude.take n (Prelude.iterate (applyFun f) x)
  where
    xs = iterateN n (applyFun f) x

prop_unfoldr :: [A] -> Property
prop_unfoldr z = valid xs .&&. toList xs === z
  where
    xs = unfoldr Data.List.uncons z

prop_unfoldl :: [A] -> Property
prop_unfoldl z = valid xs' .&&. toList xs' === Data.List.reverse z
  where
    xs' = unfoldl (fmap (\(x,xs) ->(xs,x)) . Data.List.uncons) z

-- * Deconstruction

-- ** Queries

prop_null :: Seq A -> Property
prop_null xs =
  null xs === Prelude.null (toList xs)

prop_length :: Seq A -> Property
prop_length xs =
  length xs === Prelude.length (toList xs)

-- ** Views

prop_viewl :: Seq A -> Property
prop_viewl xs =
    case viewl xs of
    EmptyL -> property $ Prelude.null (toList xs)
    x :< xs' -> valid xs' .&&. toList xs === x : toList xs'

prop_viewr :: Seq A -> Property
prop_viewr xs =
    case viewr xs of
    EmptyR -> property $ Prelude.null (toList xs)
    xs' :> x -> valid xs' .&&. toList xs === toList xs' ++ [x]

-- * Scans

prop_scanl :: Fun (B, A) B -> B -> Seq A -> Property
prop_scanl f z xs =
  valid ys .&&.
  toList ys === Data.List.scanl (applyFun2 f) z (toList xs)
  where
    ys = scanl (applyFun2 f) z xs

prop_scanl1 :: Fun (A, A) A -> Seq A -> Property
prop_scanl1 f xs =
  not (null xs) ==>
    valid xs' .&&.
    toList xs' === Data.List.scanl1 (applyFun2 f) (toList xs)
  where
    xs' = scanl1 (applyFun2 f) xs

prop_scanr :: Fun (A, B) B -> B -> Seq A -> Property
prop_scanr f z xs =
  valid ys .&&.
  toList ys === Data.List.scanr (applyFun2 f) z (toList xs)
  where
    ys = scanr (applyFun2 f) z xs

prop_scanr1 :: Fun (A, A) A -> Seq A -> Property
prop_scanr1 f xs =
  not (null xs) ==>
    valid xs' .&&.
    toList xs' === Data.List.scanr1 (applyFun2 f) (toList xs)
  where
    xs' = scanr1 (applyFun2 f) xs

-- * Sublists

prop_tails :: Seq A -> Property
prop_tails xs =
  conjoin (fmap valid (toList xss)) .&&.
  fmap toList (toList xss) === Data.List.tails (toList xs)
  where
    xss = tails xs

prop_inits :: Seq A -> Property
prop_inits xs =
  conjoin (fmap valid (toList xss)) .&&.
  fmap toList (toList xss) === Data.List.inits (toList xs)
  where
    xss = inits xs

-- ** Sequential searches

prop_takeWhileL :: Fun A Bool -> Seq A -> Property
prop_takeWhileL f xs =
  valid xs' .&&.
  toList xs' === Prelude.takeWhile (applyFun f) (toList xs)
  where
    xs' = takeWhileL (applyFun f) xs

prop_takeWhileR :: Fun A Bool -> Seq A -> Property
prop_takeWhileR f xs =
  valid xs' .&&.
  toList xs' ===
    Prelude.reverse
      (Prelude.takeWhile (applyFun f) (Prelude.reverse (toList xs)))
  where
    xs' = takeWhileR (applyFun f) xs

prop_dropWhileL :: Fun A Bool -> Seq A -> Property
prop_dropWhileL f xs =
  valid xs' .&&.
  toList xs' === Prelude.dropWhile (applyFun f) (toList xs)
  where
    xs' = dropWhileL (applyFun f) xs

prop_dropWhileR :: Fun A Bool -> Seq A -> Property
prop_dropWhileR f xs =
  valid xs' .&&.
  toList xs' ===
    Prelude.reverse
      (Prelude.dropWhile (applyFun f) (Prelude.reverse (toList xs)))
  where
    xs' = dropWhileR (applyFun f) xs

prop_spanl :: Fun A Bool -> Seq A -> Property
prop_spanl f xs =
  valid xs1 .&&.
  valid xs2 .&&.
  (toList xs1, toList xs2) === Data.List.span (applyFun f) (toList xs)
  where
    (xs1, xs2) = spanl (applyFun f) xs

prop_spanr :: Fun A Bool -> Seq A -> Property
prop_spanr f xs =
  valid xs1 .&&.
  valid xs2 .&&.
  (toList xs1, toList xs2) ===
    (Prelude.reverse *** Prelude.reverse)
      (Data.List.span (applyFun f) (Prelude.reverse (toList xs)))
  where
    (xs1, xs2) = spanr (applyFun f) xs

prop_breakl :: Fun A Bool -> Seq A -> Property
prop_breakl f xs =
  valid xs1 .&&.
  valid xs2 .&&.
  (toList xs1, toList xs2) === Data.List.break (applyFun f) (toList xs)
  where
    (xs1, xs2) = breakl (applyFun f) xs

prop_breakr :: Fun A Bool -> Seq A -> Property
prop_breakr f xs =
  valid xs1 .&&.
  valid xs2 .&&.
  (toList xs1, toList xs2) ===
    (Prelude.reverse *** Prelude.reverse)
      (Data.List.break (applyFun f) (Prelude.reverse (toList xs)))
  where
    (xs1, xs2) = breakr (applyFun f) xs

prop_partition :: Fun A Bool -> Seq A -> Property
prop_partition f xs =
  valid xs1 .&&.
  valid xs2 .&&.
  (toList xs1, toList xs2) === Data.List.partition (applyFun f) (toList xs)
  where
    (xs1, xs2) = partition (applyFun f) xs

prop_filter :: Fun A Bool -> Seq A -> Property
prop_filter f xs =
  valid xs' .&&.
  toList xs' === Prelude.filter (applyFun f) (toList xs)
  where
    xs' = filter (applyFun f) xs

prop_mapMaybe :: Fun A (Maybe B) -> Seq A -> Property
prop_mapMaybe f xs =
  valid ys .&&.
  toList ys === Maybe.mapMaybe (applyFun f) (toList xs)
  where
    ys = mapMaybe (applyFun f) xs

-- * Sorting

prop_sort :: Seq OrdA -> Property
prop_sort xs =
  valid xs' .&&.
  toList xs' === Data.List.sort (toList xs)
  where
    xs' = sort xs

data UnstableOrd = UnstableOrd OrdA A
    deriving (Show)

instance Eq UnstableOrd where
    x == y = compare x y == EQ

instance Ord UnstableOrd where
    compare (UnstableOrd x _) (UnstableOrd y _) = compare x y

instance Arbitrary UnstableOrd where
    arbitrary = liftA2 UnstableOrd arbitrary arbitrary
    shrink (UnstableOrd x y) =
        [ UnstableOrd x' y'
        | (x',y') <- shrink (x, y) ]

prop_sortStable :: Seq UnstableOrd -> Property
prop_sortStable xs =
  valid xs' .&&.
  fmap unignore (toList (sort xs)) ===
    fmap unignore (Data.List.sort (toList xs))
  where
    unignore (UnstableOrd x y) = (x, y)
    xs' = sort xs

prop_sortBy :: Seq (OrdA, B) -> Property
prop_sortBy xs =
  valid xs' .&&.
  toList xs' === Data.List.sortBy (comparing fst) (toList xs)
  where
    xs' = sortBy (comparing fst) xs

prop_sortOn :: Fun A OrdB -> Seq A -> Property
prop_sortOn f xs =
  valid xs' .&&.
  toList xs' === Data.List.sortOn (applyFun f) (toList xs)
  where
    xs' = sortOn (applyFun f) xs

prop_sortOnStable :: Fun A UnstableOrd -> Seq A -> Property
prop_sortOnStable f xs =
  valid xs' .&&.
  toList xs' === Data.List.sortOn (applyFun f) (toList xs)
  where
    xs' = sortOn (applyFun f) xs

prop_unstableSort :: Seq OrdA -> Property
prop_unstableSort xs =
  valid xs' .&&.
  toList xs' === Data.List.sort (toList xs)
  where
    xs' = unstableSort xs

prop_unstableSortBy :: Seq OrdA -> Property
prop_unstableSortBy xs =
  valid xs' .&&.
  toList xs' === Data.List.sort (toList xs)
  where
    xs' = unstableSortBy compare xs

prop_unstableSortOn :: Fun A OrdB -> Seq A -> Property
prop_unstableSortOn f xs =
  valid xs' .&&.
  conjoin (Data.List.zipWith ((<=) `on` applyFun f) xsl' (tail xsl'))
  where
    xs' = unstableSortOn (applyFun f) xs
    xsl' = toList xs'

-- * Indexing

prop_index :: Seq A -> Property
prop_index xs =
    not (null xs) ==> forAll (choose (0, length xs-1)) $ \ i ->
    index xs i === toList xs !! i

prop_safeIndex :: Seq A -> Property
prop_safeIndex xs =
    forAll (choose (-3, length xs + 3)) $ \i ->
    ((i < 0 || i >= length xs) .&&. lookup i xs === Nothing) .||.
    lookup i xs === Just (toList xs !! i)

prop_insertAt :: A -> Seq A -> Property
prop_insertAt x xs =
  forAll (choose (-3, length xs + 3)) $ \i ->
      let res = insertAt i x xs
      in valid res .&&. res === case splitAt i xs of (front, back) -> front >< x <| back

prop_deleteAt :: Seq A -> Property
prop_deleteAt xs =
  forAll (choose (-3, length xs + 3)) $ \i ->
      let res = deleteAt i xs
      in valid res .&&.
          (((0 <= i && i < length xs) .&&. res === case splitAt i xs of (front, back) -> front >< drop 1 back)
            .||. ((i < 0 || i >= length xs) .&&. res === xs))

prop_adjust :: Fun A A -> Int -> Seq A -> Property
prop_adjust f i xs =
  valid ys .&&.
  toList ys === adjustList (applyFun f) i (toList xs)
  where
    ys = adjust (applyFun f) i xs

prop_adjust' :: Fun A A -> Int -> Seq A -> Property
prop_adjust' f i xs =
  valid ys .&&.
  toList ys === adjustList (applyFun f) i (toList xs)
  where
    ys = adjust' (applyFun f) i xs

prop_update :: Int -> A -> Seq A -> Property
prop_update i x xs =
  valid ys .&&.
  toList ys === adjustList (const x) i (toList xs)
  where
    ys = update i x xs

prop_take :: Int -> Seq A -> Property
prop_take n xs =
  valid ys .&&.
  toList (take n xs) === Prelude.take n (toList xs)
  where
    ys = take n xs

prop_drop :: Int -> Seq A -> Property
prop_drop n xs =
  valid ys .&&.
  toList (drop n xs) === Prelude.drop n (toList xs)
  where
    ys = drop n xs

prop_splitAt :: Int -> Seq A -> Property
prop_splitAt n xs =
  valid ys .&&.
  valid zs .&&.
  (toList ys, toList zs) === Prelude.splitAt n (toList xs)
  where
    (ys, zs) = splitAt n xs

prop_chunksOf :: Seq A -> Property
prop_chunksOf xs =
  forAll (choose (1, length xs + 3)) $ \n ->
    let chunks = chunksOf n xs
    in valid chunks .&&.
       conjoin [valid c .&&. 1 <= length c && length c <= n | c <- toList chunks] .&&.
       fold chunks === xs

adjustList :: (a -> a) -> Int -> [a] -> [a]
adjustList f i xs =
    [if j == i then f x else x | (j, x) <- Prelude.zip [0..] xs]

-- ** Indexing with predicates
-- The elem* tests have poor coverage, but for find* we use predicates
-- of varying density.

prop_elemIndexL :: A -> Seq A -> Property
prop_elemIndexL x xs =
  elemIndexL x xs === Data.List.elemIndex x (toList xs)

prop_elemIndicesL :: A -> Seq A -> Property
prop_elemIndicesL x xs =
  elemIndicesL x xs === Data.List.elemIndices x (toList xs)

prop_elemIndexR :: A -> Seq A -> Property
prop_elemIndexR x xs =
  elemIndexR x xs === listToMaybe (Prelude.reverse (Data.List.elemIndices x (toList xs)))

prop_elemIndicesR :: A -> Seq A -> Property
prop_elemIndicesR x xs =
  elemIndicesR x xs === Prelude.reverse (Data.List.elemIndices x (toList xs))

prop_findIndexL :: Positive Int -> Seq Int -> Property
prop_findIndexL (Positive n) xs =
  findIndexL p xs === Data.List.findIndex p (toList xs)
  where p x = x `mod` n == 0

prop_findIndicesL :: Positive Int -> Seq Int -> Property
prop_findIndicesL (Positive n) xs =
  findIndicesL p xs === Data.List.findIndices p (toList xs)
  where p x = x `mod` n == 0

prop_findIndexR :: Positive Int -> Seq Int -> Property
prop_findIndexR (Positive n) xs =
  findIndexR p xs === listToMaybe (Prelude.reverse (Data.List.findIndices p (toList xs)))
  where p x = x `mod` n == 0

prop_findIndicesR :: Positive Int -> Seq Int -> Property
prop_findIndicesR (Positive n) xs =
  findIndicesR p xs === Prelude.reverse (Data.List.findIndices p (toList xs))
  where p x = x `mod` n == 0

-- * Folds

prop_foldlWithIndex :: Fun (B, Int, A) B -> B -> Seq A -> Property
prop_foldlWithIndex f z xs =
  foldlWithIndex (applyFun3 f) z xs ===
    Data.List.foldl
      (\z' (i,x) -> applyFun3 f z' i x)
      z
      (Data.List.zip [0..] (toList xs))

prop_foldrWithIndex :: Fun (Int, A, B) B -> B -> Seq A -> Property
prop_foldrWithIndex f z xs =
  foldrWithIndex (applyFun3 f) z xs ===
    Data.List.foldr
      (\(i,x) -> applyFun3 f i x)
      z
      (Data.List.zip [0..] (toList xs))

prop_foldMapWithIndexL :: Fun (B, Int, A) B -> B -> Seq A -> Property
prop_foldMapWithIndexL (Fun _ f) z t =
  foldlWithIndex f' z t ===
    appEndo (getDual (foldMapWithIndex (\i -> Dual . Endo . flip (flip f' i)) t)) z
  where f' b i a = f (b, i, a)

prop_foldMapWithIndexR :: Fun (Int, A, B) B -> B -> Seq A -> Property
prop_foldMapWithIndexR (Fun _ f) z t =
  foldrWithIndex f' z t ===
   appEndo (foldMapWithIndex (\i -> Endo . f' i) t) z
  where f' i a b = f (i, a, b)

-- * Transformations

prop_mapWithIndex :: Seq A -> Property
prop_mapWithIndex xs =
  valid ys .&&.
  toList ys === Data.List.zip [0..] (toList xs)
  where
    ys = mapWithIndex (,) xs

prop_traverseWithIndex :: Seq Int -> Property
prop_traverseWithIndex xs =
    runState (traverseWithIndex (\i x -> modify ((i,x) :)) xs) [] ===
    runState (sequenceA . mapWithIndex (\i x -> modify ((i,x) :)) $ xs) [] 

prop_reverse :: Seq A -> Property
prop_reverse xs =
  valid ys .&&.
  toList ys === Prelude.reverse (toList xs)
  where
    ys = reverse xs

-- ** Zips

prop_zip :: Seq A -> Seq B -> Property
prop_zip xs ys =
  valid zs .&&.
  toList zs === Prelude.zip (toList xs) (toList ys)
  where
    zs = zip xs ys

prop_zipWith :: Seq A -> Seq B -> Property
prop_zipWith xs ys =
  valid zs .&&.
  toList zs === Prelude.zip (toList xs) (toList ys)
  where
    zs = zipWith (,) xs ys

prop_zip3 :: Seq A -> Seq B -> Seq C -> Property
prop_zip3 xs ys zs =
  valid ts .&&.
  toList ts === Prelude.zip3 (toList xs) (toList ys) (toList zs)
  where
    ts = zip3 xs ys zs

prop_zipWith3 :: Seq A -> Seq B -> Seq C -> Property
prop_zipWith3 xs ys zs =
  valid ts .&&.
  toList ts === Prelude.zip3 (toList xs) (toList ys) (toList zs)
  where
    ts = zipWith3 (,,) xs ys zs

prop_zip4 :: Seq A -> Seq B -> Seq C -> Seq Int -> Property
prop_zip4 xs ys zs ts =
  valid us .&&.
  toList us === Data.List.zip4 (toList xs) (toList ys) (toList zs) (toList ts)
  where
    us = zip4 xs ys zs ts

prop_zipWith4 :: Seq A -> Seq B -> Seq C -> Seq Int -> Property
prop_zipWith4 xs ys zs ts =
  valid us .&&.
  toList us === Data.List.zip4 (toList xs) (toList ys) (toList zs) (toList ts)
  where
    us = zipWith4 (,,,) xs ys zs ts

-- This comes straight from the MonadZip documentation
prop_mzipNaturality :: Fun A C -> Fun B D -> Seq A -> Seq B -> Property
prop_mzipNaturality f g sa sb =
  fmap (apply f *** apply g) (mzip sa sb) ===
  mzip (apply f <$> sa) (apply g <$> sb)

-- This is a slight optimization of the MonadZip preservation
-- law that works because sequences don't have any decorations.
prop_mzipPreservation :: Fun A B -> Seq A -> Property
prop_mzipPreservation f sa =
  let sb = fmap (apply f) sa
  in munzip (mzip sa sb) === (sa, sb)

-- We want to ensure that
--
-- munzip xs = xs `seq` (fmap fst x, fmap snd x)
--
-- even in the presence of bottoms (alternatives are all balance-
-- fragile).
prop_munzipLazy :: Seq (Integer, B) -> Bool
prop_munzipLazy pairs = deepseq ((`seq` ()) <$> repaired) True
  where
    partialpairs = mapWithIndex (\i a -> update i err pairs) pairs
    firstPieces = fmap (fst . munzip) partialpairs
    repaired = mapWithIndex (\i s -> update i 10000 s) firstPieces
    err = error "munzip isn't lazy enough"

-- Applicative operations

prop_ap :: Seq A -> Seq B -> Property
prop_ap xs ys =
  valid zs .&&.
  toList zs === ( (,) <$> toList xs <*> toList ys )
  where
    zs = (,) <$> xs <*> ys

prop_ap_NOINLINE :: Seq A -> Seq B -> Property
prop_ap_NOINLINE xs ys =
  valid zs .&&.
  toList zs === ( (,) <$> toList xs <*> toList ys )
  where
    zs = ((,) <$> xs) `apNOINLINE` ys

{-# NOINLINE apNOINLINE #-}
apNOINLINE :: Seq (a -> b) -> Seq a -> Seq b
apNOINLINE fs xs = fs <*> xs

prop_liftA2 :: Seq A -> Seq B -> Property
prop_liftA2 xs ys = valid q .&&.
    toList q === liftA2 (,) (toList xs) (toList ys)
  where
    q = liftA2 (,) xs ys

prop_then :: Seq A -> Seq B -> Property
prop_then xs ys =
  valid zs .&&.
  toList zs === (toList xs *> toList ys)
  where
    zs = xs *> ys

-- We take only the length of the second sequence because
-- the implementation throws the rest away; there's no
-- point wasting test cases varying other aspects of that
-- argument.
prop_before :: Seq A -> NonNegative Int -> Property
prop_before xs (NonNegative lys) =
  valid zs .&&.
  toList zs === (toList xs <* toList ys)
  where
    ys = replicate lys ()
    zs = xs <* ys

prop_intersperse :: A -> Seq A -> Property
prop_intersperse x xs =
  valid xs' .&&.
  toList xs' === Data.List.intersperse x (toList xs)
  where
    xs' = intersperse x xs

prop_cycleTaking :: Int -> Seq A -> Property
prop_cycleTaking n xs =
  (n <= 0 || not (null xs)) ==>
    valid xs' .&&.
    toList xs' === Data.List.take n (Data.List.cycle (toList xs))
  where
    xs' = cycleTaking n xs

prop_empty_pat :: Seq A -> Bool
prop_empty_pat xs@Empty = null xs
prop_empty_pat xs = not (null xs)

prop_empty_con :: Bool
prop_empty_con = null Empty

prop_viewl_pat :: Seq A -> Property
prop_viewl_pat xs@(y :<| ys)
  | z :< zs <- viewl xs = y === z .&&. ys === zs
  | otherwise = property False
prop_viewl_pat xs = property $ null xs

prop_viewl_con :: A -> Seq A -> Property
prop_viewl_con x xs = x :<| xs === x <| xs

prop_viewr_pat :: Seq A -> Property
prop_viewr_pat xs@(ys :|> y)
  | zs :> z <- viewr xs = y === z .&&. ys === zs
  | otherwise = property False
prop_viewr_pat xs = property $ null xs

prop_viewr_con :: Seq A -> A -> Property
prop_viewr_con xs x = xs :|> x === xs |> x

-- Monad operations

prop_bind :: Seq A -> Fun A (Seq B) -> Property
prop_bind xs f =
  valid ys .&&.
  toList ys === (toList xs >>= toList . applyFun f)
  where
    ys = xs >>= applyFun f

-- Semigroup operations

prop_stimes :: NonNegative Int -> Seq A -> Property
prop_stimes (NonNegative n) s =
  stimes n s === stimesMonoid n s

-- MonadFix operation

-- It's exceedingly difficult to construct a proper QuickCheck
-- property for mfix because the function passed to it must be
-- lazy. The following property is really just a unit test in
-- disguise, and not a terribly meaningful one.
test_mfix :: Property
test_mfix = toList resS === resL
  where
    facty :: (Int -> Int) -> Int -> Int
    facty _ 0 = 1; facty f n = n * f (n - 1)

    resS :: Seq Int
    resS = fmap ($ 12) $ mfix (\f -> fromList [facty f, facty (+1), facty (+2)])

    resL :: [Int]
    resL = fmap ($ 12) $ mfix (\f -> [facty f, facty (+1), facty (+2)])

prop_traverse :: Fun A B -> Seq A -> Property
prop_traverse f xs =
  valid xs' .&&.
  toList xs' === fmap (applyFun f) (toList xs) .&&.
  ys === toList xs
  where
    (ys, xs') = traverse (\x -> ([x], applyFun f x)) xs

-- * Strictness tests

-- See Note [Testing strictness of folds] in map-strictness.hs

prop_strictness_foldr :: [A] -> Func2 A B (Bot B) -> Bot B -> Property
prop_strictness_foldr xs fun (Bot z) =
  isBottom (foldr f z s) ===
  isBottom (foldr f z xs)
  where
    s = fromList xs
    f = coerce (applyFunc2 fun) :: A -> B -> B

prop_strictness_foldl :: [A] -> Func2 B A (Bot B) -> Bot B -> Property
prop_strictness_foldl (xs) fun (Bot z) =
  isBottom (foldl f z s) ===
  isBottom (foldl f z xs)
  where
    s = fromList xs
    f = coerce (applyFunc2 fun) :: B -> A -> B

prop_strictness_foldr' :: [A] -> Func2 A B (Bot B) -> Bot B -> Property
prop_strictness_foldr' xs fun (Bot z) =
  isBottom (foldr' f z s) ===
  isBottom (z `seq` foldr' f z xs)
  where
    s = fromList xs
    f = coerce (applyFunc2 fun) :: A -> B -> B

prop_strictness_foldl' :: [A] -> Func2 B A (Bot B) -> Bot B -> Property
prop_strictness_foldl' xs fun (Bot z) =
  isBottom (foldl' f z s) ===
  isBottom (foldl' f z xs)
  where
    s = fromList xs
    f = coerce (applyFunc2 fun) :: B -> A -> B

-- Simple test monad

data M a = Action Int a
    deriving (Eq, Show)

instance Functor M where
    fmap f (Action n x) = Action n (f x)

instance Applicative M where
    pure x = Action 0 x
    Action m f <*> Action n x = Action (m+n) (f x)

instance Monad M where
    Action m x >>= f = let Action n y = f x in Action (m+n) y

instance Foldable M where
    foldMap f (Action _ x) = f x

instance Traversable M where
    traverse f (Action n x) = Action n <$> f x

-- ----------
--
-- Unit tests
--
-- ----------

test_lift :: TestTree
test_lift = testCase "lift" $ do
  (mempty :: Seq Int) @=? $([| $(TH.lift (fromList [] :: Seq Integer)) |])
  fromList [1..3 :: Int] @=? $([| $(TH.lift (fromList [1..3 :: Integer])) |])

#if __GLASGOW_HASKELL__ >= 810
test_liftTyped :: TestTree
test_liftTyped = testCase "liftTyped" $ do
  (mempty :: Seq Int) @=? $$([|| $$(TH.liftTyped (fromList [])) ||])
  fromList [1..3 :: Int] @=? $$([|| $$(TH.liftTyped (fromList [1..3])) ||])
#endif
