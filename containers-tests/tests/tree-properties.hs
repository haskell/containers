{-# LANGUAGE CPP #-}

import Data.Tree as T

import Control.Applicative (Const(Const, getConst), pure, (<$>), (<*>), liftA2)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function (apply)
import Test.QuickCheck.Poly (A, B, C, OrdA)
import Control.Monad.Fix (MonadFix (..))
import Control.Monad (ap)
import Data.Foldable (fold, foldl', toList)
import Data.Traversable (foldMapDefault)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup ((<>)))
#endif
#if MIN_VERSION_base(4,18,0)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Foldable1 as Foldable1
#endif

default (Int)

main :: IO ()
main = defaultMain $ testGroup "tree-properties"
         [
           testCase     "foldr"                    test_foldr
         , testProperty "monad_id1"                prop_monad_id1
         , testProperty "monad_id2"                prop_monad_id2
         , testProperty "monad_assoc"              prop_monad_assoc
         , testProperty "ap_ap"                    prop_ap_ap
         , testProperty "ap_liftA2"                prop_ap_liftA2
         , testProperty "monadFix_ls"              prop_monadFix_ls
         , testProperty "toList"                   prop_toList
         , testProperty "foldMap"                  prop_foldMap
         , testProperty "foldMap_structure"        prop_foldMap_structure
         , testProperty "foldl'"                   prop_foldl'
         , testProperty "foldr1"                   prop_foldr1
         , testProperty "foldl1"                   prop_foldl1
         , testProperty "foldr_infinite"           prop_foldr_infinite
         , testProperty "maximum"                  prop_maximum
         , testProperty "minimum"                  prop_minimum
         , testProperty "sum"                      prop_sum
         , testProperty "product"                  prop_product
#if MIN_VERSION_base(4,18,0)
         , testProperty "foldMap1_structure"       prop_foldMap1_structure
         , testProperty "toNonEmpty"               prop_toNonEmpty
         , testProperty "last"                     prop_last
         , testProperty "last_path"                prop_last_path
         , testProperty "foldrMap1"                prop_foldrMap1
         , testProperty "foldlMap1'"               prop_foldlMap1'
         , testProperty "foldlMap1"                prop_foldlMap1
#endif
         ]

{--------------------------------------------------------------------
  Arbitrary trees
--------------------------------------------------------------------}


-- This instance isn't balanced very well; the trees will probably tend
-- to lean left. But it's better than nothing and we can fix it later.
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized (fmap snd . arbtree)
    where
      arbtree :: Arbitrary a => Int -> Gen (Int, Tree a)
      arbtree 0 = fmap ((,) 1) $ Node <$> arbitrary <*> pure []
      arbtree n = do
        root <- arbitrary
        num_children <- choose (0, n - 1)
        (st, tl) <- go num_children
        return (1+st, Node root tl)

      go 0 = pure (0, [])
      go n = do
        (sh, hd) <- arbtree n
        (st, tl) <- go (n - sh)
        pure (sh + st, hd : tl)

#if defined(__GLASGOW_HASKELL__)
  shrink = genericShrink
#endif

----------------------------------------------------------------
-- Utilities
----------------------------------------------------------------

data Magma a
  = Inj a
  | Magma a :* Magma a
  deriving (Eq, Show)

-- Unlawful on purpose.
instance Semigroup (Magma a) where
  (<>) = (:*)

data UnitalMagma a
  = Unit
  | UInj a
  | UnitalMagma a :** UnitalMagma a
  deriving (Eq, Show)

-- Unlawful on purpose.
instance Semigroup (UnitalMagma a) where
  (<>) = (:**)

-- Unlawful on purpose.
instance Monoid (UnitalMagma a) where
  mempty = Unit
  mappend = (<>)

----------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------

test_foldr :: Assertion
test_foldr = do
  foldr (:) [] (Node 1 []) @?= [1]
  foldr (:) [] (Node 1 [Node 2 [Node 3 []]]) @?= [1..3]
  foldr (:) [] (Node 1 [Node 2 [Node 3 [], Node 4 []], Node 5 [Node 6 [], Node 7 []]]) @?= [1..7]

----------------------------------------------------------------
-- QuickCheck
----------------------------------------------------------------

apply2 :: Fun (a, b) c -> a -> b -> c
apply2 f a b = apply f (a, b)

prop_ap_ap :: Tree (Fun A B) -> Tree A -> Property
prop_ap_ap fs xs = (apply <$> fs <*> xs) === ((apply <$> fs) `ap` xs)

prop_ap_liftA2 :: Fun (A, B) C -> Tree A -> Tree B -> Property
prop_ap_liftA2 f as bs = (apply2 f <$> as <*> bs) === liftA2 (apply2 f) as bs

prop_monad_id1 :: Tree A -> Property
prop_monad_id1 t = (t >>= pure) === t

prop_monad_id2 :: A -> Fun A (Tree B) -> Property
prop_monad_id2 a f = (pure a >>= apply f) === apply f a

prop_monad_assoc :: Tree A -> Fun A (Tree B) -> Fun B (Tree C) -> Property
prop_monad_assoc ta atb btc =
  ((ta >>= apply atb) >>= apply btc)
  ===
  (ta >>= \a -> apply atb a >>= apply btc)

-- The left shrinking law
--
-- This test is kind of wonky and unprincipled, because it's
-- rather tricky to construct test cases!
-- This is the most important MonadFix law to test because it's the
-- least intuitive by far, and because it's the only one that's
-- sensitive to the Monad instance.
prop_monadFix_ls :: Int -> Tree Int -> Fun Int (Tree Int) -> Property
prop_monadFix_ls val ta ti =
  fmap ($ val) (mfix (\x -> ta >>= \y -> f x y))
  ===
  fmap ($ val) (ta >>= \y -> mfix (\x -> f x y))
  where
    fact :: Int -> (Int -> Int) -> Int -> Int
    fact x _ 0 = x + 1
    fact x f n = x + n * f ((n - 1) `mod` 23)

    f :: (Int -> Int) -> Int -> Tree (Int -> Int)
    f q y = let t = apply ti y
            in fmap (\w -> fact w q) t

prop_toList :: Tree A -> Property
prop_toList t = toList t === foldr (:) [] t

prop_foldMap :: Tree A -> Property
prop_foldMap t =
  foldMap (:[]) t === toList t .&&.
  foldMap (:[]) t === foldMapDefault (:[]) t

-- We use UnitalMagma with foldMap to test that the structure of the fold
-- follows that of the tree. This is desirable here because we can be more
-- efficient/lazy with some monoids, such as Data.Monoid.Last, compared
-- to a foldr-based foldMap.
prop_foldMap_structure :: Tree A -> Property
prop_foldMap_structure t =
  foldMap UInj t === foldTree (\x ys -> fold (UInj x : ys)) t

prop_foldl' :: Tree A -> Property
prop_foldl' t = foldl' (flip (:)) [] t === reverse (toList t)

prop_foldr1 :: Tree A -> Property
prop_foldr1 t = foldr1 (:*) (fmap Inj t) === foldr1 (:*) (map Inj (toList t))

prop_foldl1 :: Tree A -> Property
prop_foldl1 t = foldl1 (:*) (fmap Inj t) === foldl1 (:*) (map Inj (toList t))

prop_foldr_infinite :: NonNegative Int -> Property
prop_foldr_infinite (NonNegative n) =
    forAllShow genInf (const "<possibly infinite tree>") $
        \t -> length (take n (foldr (:) [] t)) <= n
  where
    genInf = Node () <$> oneof [listOf genInf, infiniteListOf genInf]

prop_maximum :: Tree OrdA -> Property
prop_maximum t = maximum t === maximum (toList t)

prop_minimum :: Tree OrdA -> Property
prop_minimum t = minimum t === minimum (toList t)

prop_sum :: Tree OrdA -> Property
prop_sum t = sum t === sum (toList t)

prop_product :: Tree OrdA -> Property
prop_product t = product t === product (toList t)

#if MIN_VERSION_base(4,18,0)
-- We use Magma with foldMap1 to test that the structure of the fold follows
-- that of the tree. This is desirable here because we can be more
-- efficient/lazy with some semigroups, such as Data.Semigroup.Last, compared
-- to a foldrMap1-based foldMap1.
prop_foldMap1_structure :: Tree A -> Property
prop_foldMap1_structure t =
  Foldable1.foldMap1 Inj t === foldTree (\x ys -> Foldable1.fold1 (Inj x :| ys)) t

prop_toNonEmpty :: Tree A -> Property
prop_toNonEmpty t = Foldable1.toNonEmpty t === NE.fromList (toList t)

prop_last :: Tree A -> Property
prop_last t = Foldable1.last t === NE.last (Foldable1.toNonEmpty t)

-- Tests that last only looks at the path going down to the last leaf.
prop_last_path :: Tree A -> Property
prop_last_path t = Foldable1.last (replace t) === Foldable1.last t
  where
    -- Replace all trees with bottom except for the last one.
    replace :: Tree a -> Tree a
    replace (Node x ts) = Node x (replaces ts)
    replaces :: [Tree a] -> [Tree a]
    replaces [] = []
    replaces [t] = [replace t]
    replaces (t:ts) = error "error tree" : replaces ts

prop_foldrMap1 :: Tree A -> Property
prop_foldrMap1 t =
    Foldable1.foldrMap1 Inj f t === Foldable1.foldrMap1 Inj f (Foldable1.toNonEmpty t)
  where
    f x z = Inj x :* z

prop_foldlMap1' :: Tree A -> Property
prop_foldlMap1' t =
    Foldable1.foldlMap1' Inj f t === Foldable1.foldlMap1' Inj f (Foldable1.toNonEmpty t)
  where
    f z x = z :* Inj x

prop_foldlMap1 :: Tree A -> Property
prop_foldlMap1 t =
    Foldable1.foldlMap1 Inj f t === Foldable1.foldlMap1 Inj f (Foldable1.toNonEmpty t)
  where
    f z x = z :* Inj x
#endif
