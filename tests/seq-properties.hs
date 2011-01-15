-- QuickCheck properties for Data.Sequence
-- > ghc -DTESTING -fforce-recomp -O2 --make -fhpc -i.. seq-properties.hs

module Main where

import Data.Sequence    -- needs to be compiled with -DTESTING for use here

import Test.QuickCheck hiding ((><))
import Test.QuickCheck.Poly

import Prelude hiding (
    null, length, take, drop, splitAt,
    foldl, foldl1, foldr, foldr1, scanl, scanl1, scanr, scanr1,
    filter, reverse, replicate, zip, zipWith, zip3, zipWith3)
import qualified Prelude
import qualified Data.List

import Control.Applicative (Applicative(..))
import Control.Arrow ((***))
import Data.Foldable (Foldable(..), toList)
import Data.Functor ((<$>), (<$))
import Data.Maybe
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable(traverse), sequenceA)

main :: IO ()
main = do
    q $ label "fmap" prop_fmap
    q $ label "(<$)" prop_constmap
    q $ label "foldr" prop_foldr
    q $ label "foldr1" prop_foldr1
    q $ label "foldl" prop_foldl
    q $ label "foldl1" prop_foldl1
    q $ label "(==)" prop_equals
    q $ label "compare" prop_compare
    q $ label "mappend" prop_mappend
    q $ label "singleton" prop_singleton
    q $ label "(<|)" prop_cons
    q $ label "(|>)" prop_snoc
    q $ label "(><)" prop_append
    q $ label "fromList" prop_fromList
    q $ label "replicate" prop_replicate
    q $ label "replicateA" prop_replicateA
    q $ label "replicateM" prop_replicateM
    q $ label "iterateN" prop_iterateN
    q $ label "unfoldr" prop_unfoldr
    q $ label "unfoldl" prop_unfoldl
    q $ label "null" prop_null
    q $ label "length" prop_length
    q $ label "viewl" prop_viewl
    q $ label "viewr" prop_viewr
    q $ label "scanl" prop_scanl
    q $ label "scanl1" prop_scanl1
    q $ label "scanr" prop_scanr
    q $ label "scanr1" prop_scanr1
    q $ label "tails" prop_tails
    q $ label "inits" prop_inits
    q $ label "takeWhileL" prop_takeWhileL
    q $ label "takeWhileR" prop_takeWhileR
    q $ label "dropWhileL" prop_dropWhileL
    q $ label "dropWhileR" prop_dropWhileR
    q $ label "spanl" prop_spanl
    q $ label "spanr" prop_spanr
    q $ label "breakl" prop_breakl
    q $ label "breakr" prop_breakr
    q $ label "partition" prop_partition
    q $ label "filter" prop_filter
    q $ label "sort" prop_sort
    q $ label "sortBy" prop_sortBy
    q $ label "unstableSort" prop_unstableSort
    q $ label "unstableSortBy" prop_unstableSortBy
    q $ label "index" prop_index
    q $ label "adjust" prop_adjust
    q $ label "update" prop_update
    q $ label "take" prop_take
    q $ label "drop" prop_drop
    q $ label "splitAt" prop_splitAt
    q $ label "elemIndexL" prop_elemIndexL
    q $ label "elemIndicesL" prop_elemIndicesL
    q $ label "elemIndexR" prop_elemIndexR
    q $ label "elemIndicesR" prop_elemIndicesR
    q $ label "findIndexL" prop_findIndexL
    q $ label "findIndicesL" prop_findIndicesL
    q $ label "findIndexR" prop_findIndexR
    q $ label "findIndicesR" prop_findIndicesR
    q $ label "foldlWithIndex" prop_foldlWithIndex
    q $ label "foldrWithIndex" prop_foldrWithIndex
    q $ label "mapWithIndex" prop_mapWithIndex
    q $ label "reverse" prop_reverse
    q $ label "zip" prop_zip
    q $ label "zipWith" prop_zipWith
    q $ label "zip3" prop_zip3
    q $ label "zipWith3" prop_zipWith3
    q $ label "zip4" prop_zip4
    q $ label "zipWith4" prop_zipWith4
  where
    q :: Testable prop => prop -> IO ()
    q = quickCheckWith args

{--------------------------------------------------------------------
  QuickCheck
--------------------------------------------------------------------}
 
args :: Args
args = stdArgs
    { maxSuccess = 500
    , maxDiscard = 500
    }

{--------------------------------------------------------------------
  The general plan is to compare each function with a list equivalent.
  Each operation should produce a valid tree representing the same
  sequence as produced by its list counterpart on corresponding inputs.
  (The list versions are often lazier, but these properties ignore
  strictness.)
--------------------------------------------------------------------}

-- utilities for partial conversions

infix 4 ~=

(~=) :: Eq a => Maybe a -> a -> Bool
(~=) = maybe (const False) (==)

-- Partial conversion of an output sequence to a list.
toList' :: Seq a -> Maybe [a]
toList' xs
  | valid xs = Just (toList xs)
  | otherwise = Nothing

toListList' :: Seq (Seq a) -> Maybe [[a]]
toListList' xss = toList' xss >>= mapM toList'

toListPair' :: (Seq a, Seq b) -> Maybe ([a], [b])
toListPair' (xs, ys) = (,) <$> toList' xs <*> toList' ys

-- instances

prop_fmap :: Seq Int -> Bool
prop_fmap xs =
    toList' (fmap f xs) ~= map f (toList xs)
  where f = (+100)

prop_constmap :: A -> Seq A -> Bool
prop_constmap x xs =
    toList' (x <$ xs) ~= map (const x) (toList xs)

prop_foldr :: Seq A -> Bool
prop_foldr xs =
    foldr f z xs == Prelude.foldr f z (toList xs)
  where
    f = (:)
    z = []

prop_foldr1 :: Seq Int -> Property
prop_foldr1 xs =
    not (null xs) ==> foldr1 f xs == Data.List.foldr1 f (toList xs)
  where f = (-)

prop_foldl :: Seq A -> Bool
prop_foldl xs =
    foldl f z xs == Prelude.foldl f z (toList xs)
  where
    f = flip (:)
    z = []

prop_foldl1 :: Seq Int -> Property
prop_foldl1 xs =
    not (null xs) ==> foldl1 f xs == Data.List.foldl1 f (toList xs)
  where f = (-)

prop_equals :: Seq OrdA -> Seq OrdA -> Bool
prop_equals xs ys =
    (xs == ys) == (toList xs == toList ys)

prop_compare :: Seq OrdA -> Seq OrdA -> Bool
prop_compare xs ys =
    compare xs ys == compare (toList xs) (toList ys)

prop_mappend :: Seq A -> Seq A -> Bool
prop_mappend xs ys =
    toList' (mappend xs ys) ~= toList xs ++ toList ys

-- * Construction

{-
    toList' empty ~= []
-}

prop_singleton :: A -> Bool
prop_singleton x =
    toList' (singleton x) ~= [x]

prop_cons :: A -> Seq A -> Bool
prop_cons x xs =
    toList' (x <| xs) ~= x : toList xs

prop_snoc :: Seq A -> A -> Bool
prop_snoc xs x =
    toList' (xs |> x) ~= toList xs ++ [x]

prop_append :: Seq A -> Seq A -> Bool
prop_append xs ys =
    toList' (xs >< ys) ~= toList xs ++ toList ys

prop_fromList :: [A] -> Bool
prop_fromList xs =
    toList' (fromList xs) ~= xs

-- ** Repetition

prop_replicate :: NonNegative Int -> A -> Bool
prop_replicate (NonNegative m) x =
    toList' (replicate n x) ~= Prelude.replicate n x
  where n = m `mod` 10000

prop_replicateA :: NonNegative Int -> Bool
prop_replicateA (NonNegative m) =
    traverse toList' (replicateA n a) ~= sequenceA (Prelude.replicate n a)
  where
    n = m `mod` 10000
    a = Action 1 0 :: M Int

prop_replicateM :: NonNegative Int -> Bool
prop_replicateM (NonNegative m) =
    traverse toList' (replicateM n a) ~= sequence (Prelude.replicate n a)
  where
    n = m `mod` 10000
    a = Action 1 0 :: M Int

-- ** Iterative construction

prop_iterateN :: NonNegative Int -> Int -> Bool
prop_iterateN (NonNegative m) x =
    toList' (iterateN n f x) ~= Prelude.take n (Prelude.iterate f x)
  where
    n = m `mod` 10000
    f = (+1)

prop_unfoldr :: [A] -> Bool
prop_unfoldr z =
    toList' (unfoldr f z) ~= Data.List.unfoldr f z
  where
    f [] = Nothing
    f (x:xs) = Just (x, xs)

prop_unfoldl :: [A] -> Bool
prop_unfoldl z =
    toList' (unfoldl f z) ~= Data.List.reverse (Data.List.unfoldr (fmap swap . f) z)
  where
    f [] = Nothing
    f (x:xs) = Just (xs, x)
    swap (x,y) = (y,x)

-- * Deconstruction

-- ** Queries

prop_null :: Seq A -> Bool
prop_null xs =
    null xs == Prelude.null (toList xs)

prop_length :: Seq A -> Bool
prop_length xs =
    length xs == Prelude.length (toList xs)

-- ** Views

prop_viewl :: Seq A -> Bool
prop_viewl xs =
    case viewl xs of
    EmptyL ->   Prelude.null (toList xs)
    x :< xs' -> valid xs' && toList xs == x : toList xs'

prop_viewr :: Seq A -> Bool
prop_viewr xs =
    case viewr xs of
    EmptyR ->   Prelude.null (toList xs)
    xs' :> x -> valid xs' && toList xs == toList xs' ++ [x]

-- * Scans

prop_scanl :: [A] -> Seq A -> Bool
prop_scanl z xs =
    toList' (scanl f z xs) ~= Data.List.scanl f z (toList xs)
  where f = flip (:)

prop_scanl1 :: Seq Int -> Property
prop_scanl1 xs =
    not (null xs) ==> toList' (scanl1 f xs) ~= Data.List.scanl1 f (toList xs)
  where f = (-)

prop_scanr :: [A] -> Seq A -> Bool
prop_scanr z xs =
    toList' (scanr f z xs) ~= Data.List.scanr f z (toList xs)
  where f = (:)

prop_scanr1 :: Seq Int -> Property
prop_scanr1 xs =
    not (null xs) ==> toList' (scanr1 f xs) ~= Data.List.scanr1 f (toList xs)
  where f = (-)

-- * Sublists

prop_tails :: Seq A -> Bool
prop_tails xs =
    toListList' (tails xs) ~= Data.List.tails (toList xs)

prop_inits :: Seq A -> Bool
prop_inits xs =
    toListList' (inits xs) ~= Data.List.inits (toList xs)

-- ** Sequential searches
-- We use predicates with varying density.

prop_takeWhileL :: Positive Int -> Seq Int -> Bool
prop_takeWhileL (Positive n) xs =
    toList' (takeWhileL p xs) ~= Prelude.takeWhile p (toList xs)
  where p x = x `mod` n == 0

prop_takeWhileR :: Positive Int -> Seq Int -> Bool
prop_takeWhileR (Positive n) xs =
    toList' (takeWhileR p xs) ~= Prelude.reverse (Prelude.takeWhile p (Prelude.reverse (toList xs)))
  where p x = x `mod` n == 0

prop_dropWhileL :: Positive Int -> Seq Int -> Bool
prop_dropWhileL (Positive n) xs =
    toList' (dropWhileL p xs) ~= Prelude.dropWhile p (toList xs)
  where p x = x `mod` n == 0

prop_dropWhileR :: Positive Int -> Seq Int -> Bool
prop_dropWhileR (Positive n) xs =
    toList' (dropWhileR p xs) ~= Prelude.reverse (Prelude.dropWhile p (Prelude.reverse (toList xs)))
  where p x = x `mod` n == 0

prop_spanl :: Positive Int -> Seq Int -> Bool
prop_spanl (Positive n) xs =
    toListPair' (spanl p xs) ~= Data.List.span p (toList xs)
  where p x = x `mod` n == 0

prop_spanr :: Positive Int -> Seq Int -> Bool
prop_spanr (Positive n) xs =
    toListPair' (spanr p xs) ~= (Prelude.reverse *** Prelude.reverse) (Data.List.span p (Prelude.reverse (toList xs)))
  where p x = x `mod` n == 0

prop_breakl :: Positive Int -> Seq Int -> Bool
prop_breakl (Positive n) xs =
    toListPair' (breakl p xs) ~= Data.List.break p (toList xs)
  where p x = x `mod` n == 0

prop_breakr :: Positive Int -> Seq Int -> Bool
prop_breakr (Positive n) xs =
    toListPair' (breakr p xs) ~= (Prelude.reverse *** Prelude.reverse) (Data.List.break p (Prelude.reverse (toList xs)))
  where p x = x `mod` n == 0

prop_partition :: Positive Int -> Seq Int -> Bool
prop_partition (Positive n) xs =
    toListPair' (partition p xs) ~= Data.List.partition p (toList xs)
  where p x = x `mod` n == 0

prop_filter :: Positive Int -> Seq Int -> Bool
prop_filter (Positive n) xs =
    toList' (filter p xs) ~= Prelude.filter p (toList xs)
  where p x = x `mod` n == 0

-- * Sorting

prop_sort :: Seq OrdA -> Bool
prop_sort xs =
    toList' (sort xs) ~= Data.List.sort (toList xs)

prop_sortBy :: Seq (OrdA, B) -> Bool
prop_sortBy xs =
    toList' (sortBy f xs) ~= Data.List.sortBy f (toList xs)
  where f (x1, _) (x2, _) = compare x1 x2

prop_unstableSort :: Seq OrdA -> Bool
prop_unstableSort xs =
    toList' (unstableSort xs) ~= Data.List.sort (toList xs)

prop_unstableSortBy :: Seq OrdA -> Bool
prop_unstableSortBy xs =
    toList' (unstableSortBy compare xs) ~= Data.List.sort (toList xs)

-- * Indexing

prop_index :: Seq A -> Property
prop_index xs =
    not (null xs) ==> forAll (choose (0, length xs-1)) $ \ i ->
    index xs i == toList xs !! i

prop_adjust :: Int -> Int -> Seq Int -> Bool
prop_adjust n i xs =
    toList' (adjust f i xs) ~= adjustList f i (toList xs)
  where f = (+n)

prop_update :: Int -> A -> Seq A -> Bool
prop_update i x xs =
    toList' (update i x xs) ~= adjustList (const x) i (toList xs)

prop_take :: Int -> Seq A -> Bool
prop_take n xs =
    toList' (take n xs) ~= Prelude.take n (toList xs)

prop_drop :: Int -> Seq A -> Bool
prop_drop n xs =
    toList' (drop n xs) ~= Prelude.drop n (toList xs)

prop_splitAt :: Int -> Seq A -> Bool
prop_splitAt n xs =
    toListPair' (splitAt n xs) ~= Prelude.splitAt n (toList xs)

adjustList :: (a -> a) -> Int -> [a] -> [a]
adjustList f i xs =
    [if j == i then f x else x | (j, x) <- Prelude.zip [0..] xs]

-- ** Indexing with predicates
-- The elem* tests have poor coverage, but for find* we use predicates
-- of varying density.

prop_elemIndexL :: A -> Seq A -> Bool
prop_elemIndexL x xs =
    elemIndexL x xs == Data.List.elemIndex x (toList xs)

prop_elemIndicesL :: A -> Seq A -> Bool
prop_elemIndicesL x xs =
    elemIndicesL x xs == Data.List.elemIndices x (toList xs)

prop_elemIndexR :: A -> Seq A -> Bool
prop_elemIndexR x xs =
    elemIndexR x xs == listToMaybe (Prelude.reverse (Data.List.elemIndices x (toList xs)))

prop_elemIndicesR :: A -> Seq A -> Bool
prop_elemIndicesR x xs =
    elemIndicesR x xs == Prelude.reverse (Data.List.elemIndices x (toList xs))

prop_findIndexL :: Positive Int -> Seq Int -> Bool
prop_findIndexL (Positive n) xs =
    findIndexL p xs == Data.List.findIndex p (toList xs)
  where p x = x `mod` n == 0

prop_findIndicesL :: Positive Int -> Seq Int -> Bool
prop_findIndicesL (Positive n) xs =
    findIndicesL p xs == Data.List.findIndices p (toList xs)
  where p x = x `mod` n == 0

prop_findIndexR :: Positive Int -> Seq Int -> Bool
prop_findIndexR (Positive n) xs =
    findIndexR p xs == listToMaybe (Prelude.reverse (Data.List.findIndices p (toList xs)))
  where p x = x `mod` n == 0

prop_findIndicesR :: Positive Int -> Seq Int -> Bool
prop_findIndicesR (Positive n) xs =
    findIndicesR p xs == Prelude.reverse (Data.List.findIndices p (toList xs))
  where p x = x `mod` n == 0

-- * Folds

prop_foldlWithIndex :: [(Int, A)] -> Seq A -> Bool
prop_foldlWithIndex z xs =
    foldlWithIndex f z xs == Data.List.foldl (uncurry . f) z (Data.List.zip [0..] (toList xs))
  where f ys n y = (n,y):ys

prop_foldrWithIndex :: [(Int, A)] -> Seq A -> Bool
prop_foldrWithIndex z xs =
    foldrWithIndex f z xs == Data.List.foldr (uncurry f) z (Data.List.zip [0..] (toList xs))
  where f n y ys = (n,y):ys

-- * Transformations

prop_mapWithIndex :: Seq A -> Bool
prop_mapWithIndex xs =
    toList' (mapWithIndex f xs) ~= map (uncurry f) (Data.List.zip [0..] (toList xs))
  where f = (,)

prop_reverse :: Seq A -> Bool
prop_reverse xs =
    toList' (reverse xs) ~= Prelude.reverse (toList xs)

-- ** Zips

prop_zip :: Seq A -> Seq B -> Bool
prop_zip xs ys =
    toList' (zip xs ys) ~= Prelude.zip (toList xs) (toList ys)

prop_zipWith :: Seq A -> Seq B -> Bool
prop_zipWith xs ys =
    toList' (zipWith f xs ys) ~= Prelude.zipWith f (toList xs) (toList ys)
  where f = (,)

prop_zip3 :: Seq A -> Seq B -> Seq C -> Bool
prop_zip3 xs ys zs =
    toList' (zip3 xs ys zs) ~= Prelude.zip3 (toList xs) (toList ys) (toList zs)

prop_zipWith3 :: Seq A -> Seq B -> Seq C -> Bool
prop_zipWith3 xs ys zs =
    toList' (zipWith3 f xs ys zs) ~= Prelude.zipWith3 f (toList xs) (toList ys) (toList zs)
  where f = (,,)

prop_zip4 :: Seq A -> Seq B -> Seq C -> Seq Int -> Bool
prop_zip4 xs ys zs ts =
    toList' (zip4 xs ys zs ts) ~= Data.List.zip4 (toList xs) (toList ys) (toList zs) (toList ts)

prop_zipWith4 :: Seq A -> Seq B -> Seq C -> Seq Int -> Bool
prop_zipWith4 xs ys zs ts =
    toList' (zipWith4 f xs ys zs ts) ~= Data.List.zipWith4 f (toList xs) (toList ys) (toList zs) (toList ts)
  where f = (,,,)

-- Simple test monad

data M a = Action Int a
    deriving (Eq, Show)

instance Functor M where
    fmap f (Action n x) = Action n (f x)

instance Applicative M where
    pure x = Action 0 x
    Action m f <*> Action n x = Action (m+n) (f x)

instance Monad M where
    return x = Action 0 x
    Action m x >>= f = let Action n y = f x in Action (m+n) y

instance Foldable M where
    foldMap f (Action _ x) = f x

instance Traversable M where
    traverse f (Action n x) = Action n <$> f x
