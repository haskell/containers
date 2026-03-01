{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE Trustworthy #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- = Multi-way Trees and Forests
--
-- The @'Tree' a@ type represents a lazy, possibly infinite, multi-way tree
-- (also known as a /rose tree/).
--
-- The @'Forest' a@ type represents a forest of @'Tree' a@s.
--
-----------------------------------------------------------------------------

module Data.Tree(

    -- * Trees and Forests
      Tree(..)
    , Forest
    , PostOrder(..)

    -- * Construction
    , unfoldTree
    , unfoldForest
    , unfoldTreeM
    , unfoldForestM
    , unfoldTreeM_BF
    , unfoldForestM_BF

    -- * Elimination
    , foldTree
    , flatten
    , levels
    , leaves
    , edges
    , pathsToRoot
    , pathsFromRoot

    -- * Ascii Drawings
    , drawTree
    , drawForest

    ) where

import Utils.Containers.Internal.Prelude as Prelude
import Prelude ()
import Data.Bits ((.&.))
import Data.Foldable (toList)
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Traversable (foldMapDefault)
import Control.Monad (liftM)
import Control.Monad.Fix (MonadFix (..), fix)
import Data.Sequence (Seq, empty, singleton, (<|), (|>), fromList,
            ViewL(..), ViewR(..), viewl, viewr)
import Control.DeepSeq (NFData(rnf),NFData1(liftRnf))

#ifdef __GLASGOW_HASKELL__
import Data.Data (Data)
import GHC.Generics (Generic, Generic1)
import qualified GHC.Exts
#  if __GLASGOW_HASKELL__ >= 914
import Language.Haskell.TH.Lift (Lift)
#  else
import Language.Haskell.TH.Syntax (Lift)
-- See Note [ Template Haskell Dependencies ]
import Language.Haskell.TH ()
#  endif
#endif

import Control.Monad.Zip (MonadZip (..))

#ifdef __GLASGOW_HASKELL__
import Data.Coerce (coerce)
#endif
import Data.Functor.Classes

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif

#if MIN_VERSION_base(4,18,0)
import qualified Data.Foldable1 as Foldable1
#endif

-- | Non-empty, possibly infinite, multi-way trees; also known as /rose trees/.
data Tree a = Node {
        rootLabel :: a,         -- ^ label value
        subForest :: [Tree a]   -- ^ zero or more child trees
    }
#ifdef __GLASGOW_HASKELL__
  deriving ( Eq
           , Ord -- ^ @since 0.6.5
           , Read
           , Show
           , Data
           , Generic  -- ^ @since 0.5.8
           , Generic1 -- ^ @since 0.5.8
           , Lift -- ^ @since 0.6.6
           )
#else
  deriving (Eq, Ord, Read, Show)
#endif

-- | This type synonym exists primarily for historical
-- reasons.
type Forest a = [Tree a]

-- | @since 0.5.9
instance Eq1 Tree where
  liftEq eq = leq
    where
      leq (Node a fr) (Node a' fr') = eq a a' && liftEq leq fr fr'

-- | @since 0.5.9
instance Ord1 Tree where
  liftCompare cmp = lcomp
    where
      lcomp (Node a fr) (Node a' fr') = cmp a a' <> liftCompare lcomp fr fr'

-- | @since 0.5.9
instance Show1 Tree where
  liftShowsPrec shw shwl p (Node a fr) = showParen (p > 10) $
        showString "Node {rootLabel = " . shw 0 a . showString ", " .
          showString "subForest = " . liftShowList shw shwl fr .
          showString "}"

-- | @since 0.5.9
instance Read1 Tree where
  liftReadsPrec rd rdl p = readParen (p > 10) $
    \s -> do
      ("Node", s1) <- lex s
      ("{", s2) <- lex s1
      ("rootLabel", s3) <- lex s2
      ("=", s4) <- lex s3
      (a, s5) <- rd 0 s4
      (",", s6) <- lex s5
      ("subForest", s7) <- lex s6
      ("=", s8) <- lex s7
      (fr, s9) <- liftReadList rd rdl s8
      ("}", s10) <- lex s9
      pure (Node a fr, s10)

instance Functor Tree where
    fmap = fmapTree
    x <$ Node _ ts = Node x (map (x <$) ts)

fmapTree :: (a -> b) -> Tree a -> Tree b
fmapTree f (Node x ts) = Node (f x) (map (fmapTree f) ts)

#ifdef __GLASGOW_HASKELL__
{-# NOINLINE [1] fmapTree #-}
{-# RULES
"fmapTree/coerce" fmapTree coerce = coerce
 #-}
#endif

instance Applicative Tree where
    pure x = Node x []
    Node f tfs <*> tx@(Node x txs) =
        Node (f x) (map (f <$>) txs ++ map (<*> tx) tfs)
    liftA2 f (Node x txs) ty@(Node y tys) =
        Node (f x y) (map (f x <$>) tys ++ map (\tx -> liftA2 f tx ty) txs)
    Node x txs <* ty@(Node _ tys) =
        Node x (map (x <$) tys ++ map (<* ty) txs)
    Node _ txs *> ty@(Node y tys) =
        Node y (tys ++ map (*> ty) txs)

instance Monad Tree where
    Node x ts >>= f = case f x of
        Node x' ts' -> Node x' (ts' ++ map (>>= f) ts)

-- | @since 0.5.11
instance MonadFix Tree where
  mfix = mfixTree

mfixTree :: (a -> Tree a) -> Tree a
mfixTree f
  | Node a children <- fix (f . rootLabel)
  = Node a (zipWith (\i _ -> mfixTree ((!! i) . subForest . f))
                    [0..] children)

-- | Traverses in pre-order.
instance Traversable Tree where
  traverse f = go
    where go (Node x ts) = liftA2 Node (f x) (traverse go ts)
  {-# INLINE traverse #-}

-- | Folds in pre-order.

-- See Note [Implemented Foldable Tree functions]
instance Foldable Tree where
    fold = foldMap id
    {-# INLINABLE fold #-}

    foldMap = foldMapDefault
    {-# INLINE foldMap #-}

    foldr f z = \t -> go t z  -- Use a lambda to allow inlining with two arguments
      where
        go (Node x ts) = f x . foldr (\t k -> go t . k) id ts
        -- This is equivalent to the following simpler definition, but has been found to optimize
        -- better in benchmarks:
        -- go (Node x ts) z' = f x (foldr go z' ts)
    {-# INLINE foldr #-}

    foldl' f = go
      where go !z (Node x ts) = foldl' go (f z x) ts
    {-# INLINE foldl' #-}

    foldr1 = foldrMap1 id

    foldl1 = foldlMap1 id

    null _ = False
    {-# INLINE null #-}

    elem = any . (==)
    {-# INLINABLE elem #-}

    maximum = foldlMap1' id max
    {-# INLINABLE maximum #-}

    minimum = foldlMap1' id min
    {-# INLINABLE minimum #-}

    sum = foldlMap1' id (+)
    {-# INLINABLE sum #-}

    product = foldlMap1' id (*)
    {-# INLINABLE product #-}

#if MIN_VERSION_base(4,18,0)
-- | Folds in pre-order.
--
-- @since 0.6.7

-- See Note [Implemented Foldable1 Tree functions]
instance Foldable1.Foldable1 Tree where
  foldMap1 f = go
    where
      -- We'd like to write
      --
      -- go (Node x (t : ts)) = f x <> Foldable1.foldMap1 go (t :| ts)
      --
      -- but foldMap1 for NonEmpty isn't very good, so we don't. See
      -- https://github.com/haskell/containers/pull/921#issuecomment-1410398618
      go (Node x []) = f x
      go (Node x (t : ts)) =
        f x <> Foldable1.foldrMap1 go (\t' z -> go t' <> z) (t :| ts)
  {-# INLINE foldMap1 #-}

  foldMap1' f = foldlMap1' f (\z x -> z <> f x)
  {-# INLINE foldMap1' #-}

  toNonEmpty (Node x ts) = x :| concatMap toList ts

  maximum = Foldable.maximum
  {-# INLINABLE maximum #-}

  minimum = Foldable.minimum
  {-# INLINABLE minimum #-}

  foldrMap1 = foldrMap1

  foldlMap1' = foldlMap1'

  foldlMap1 = foldlMap1
#endif

foldrMap1 :: (a -> b) -> (a -> b -> b) -> Tree a -> b
foldrMap1 f g = go
  where
    go (Node x [])     = f x
    go (Node x (t:ts)) = g x (foldrMap1NE go (\t' z -> foldr g z t') t ts)
{-# INLINE foldrMap1 #-}

-- This is foldrMap1 for Data.List.NonEmpty, but is not available before
-- base 4.18.
foldrMap1NE :: (a -> b) -> (a -> b -> b) -> a -> [a] -> b
foldrMap1NE f g = go
  where
    go x []      = f x
    go x (x':xs) = g x (go x' xs)
{-# INLINE foldrMap1NE #-}

foldlMap1' :: (a -> b) -> (b -> a -> b) -> Tree a -> b
foldlMap1' f g =  -- Use a lambda to allow inlining with two arguments
  \(Node x ts) -> foldl' (foldl' g) (f x) ts
{-# INLINE foldlMap1' #-}

foldlMap1 :: (a -> b) -> (b -> a -> b) -> Tree a -> b
foldlMap1 f g =  -- Use a lambda to allow inlining with two arguments
  \(Node x ts) -> foldl (foldl g) (f x) ts
{-# INLINE foldlMap1 #-}

instance NFData a => NFData (Tree a) where
    rnf (Node x ts) = rnf x `seq` rnf ts

-- | @since 0.8
instance NFData1 Tree where
    liftRnf rnfx = go
      where
      go (Node x ts) = rnfx x `seq` liftRnf go ts

-- | @since 0.5.10.1
instance MonadZip Tree where
  mzipWith f (Node a as) (Node b bs)
    = Node (f a b) (mzipWith (mzipWith f) as bs)

  munzip (Node (a, b) ts) = (Node a as, Node b bs)
    where (as, bs) = munzip (map munzip ts)

-- | 2-dimensional ASCII drawing of a tree.
--
-- ==== __Examples__
--
-- > putStr $ drawTree $ fmap show (Node 1 [Node 2 [], Node 3 []])
--
-- @
-- 1
-- |
-- +- 2
-- |
-- `- 3
-- @
--
drawTree :: Tree String -> String
drawTree  = unlines . draw

-- | 2-dimensional ASCII drawing of a forest.
--
-- ==== __Examples__
--
-- > putStr $ drawForest $ map (fmap show) [(Node 1 [Node 2 [], Node 3 []]), (Node 10 [Node 20 []])]
--
-- @
-- 1
-- |
-- +- 2
-- |
-- `- 3
--
-- 10
-- |
-- `- 20
-- @
--
drawForest :: [Tree String] -> String
drawForest  = unlines . map drawTree

draw :: Tree String -> [String]
draw (Node x ts0) = lines x ++ drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "`- " "   " (draw t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)

-- | Returns the elements of a tree in pre-order.
--
-- @flatten == Data.Foldable.'toList'@
--
-- @
--
--   a
--  / \\    => [a,b,c]
-- b   c
-- @
--
-- ==== __Examples__
--
-- > flatten (Node 1 [Node 2 [], Node 3 []]) == [1,2,3]
flatten :: Tree a -> [a]
flatten = toList

-- | Returns the list of nodes at each level of the tree.
--
-- @
--
--   a
--  / \\    => [[a], [b,c]]
-- b   c
-- @
--
-- ==== __Examples__
--
-- > levels (Node 1 [Node 2 [], Node 3 []]) == [[1],[2,3]]
--
levels :: Tree a -> [[a]]
levels t =
    map (map rootLabel) $
        takeWhile (not . null) $
        iterate (concatMap subForest) [t]

-- | Fold a tree into a "summary" value.
--
-- For each node in the tree, apply @f@ to the @rootLabel@ and the result
-- of applying @f@ to each @subForest@.
--
-- This is also known as the catamorphism on trees.
--
-- ==== __Examples__
--
-- Sum the values in a tree:
--
-- > foldTree (\x xs -> sum (x:xs)) (Node 1 [Node 2 [], Node 3 []]) == 6
--
-- Find the maximum value in the tree:
--
-- > foldTree (\x xs -> maximum (x:xs)) (Node 1 [Node 2 [], Node 3 []]) == 3
--
-- Count the number of leaves in the tree:
--
-- > foldTree (\_ xs -> if null xs then 1 else sum xs) (Node 1 [Node 2 [], Node 3 []]) == 2
--
-- Find depth of the tree; i.e. the number of branches from the root of the tree to the furthest leaf:
--
-- > foldTree (\_ xs -> if null xs then 0 else 1 + maximum xs) (Node 1 [Node 2 [], Node 3 []]) == 1
--
-- You can even implement traverse using foldTree:
--
-- > traverse' f = foldTree (\x xs -> liftA2 Node (f x) (sequenceA xs))
--
--
-- @since 0.5.8
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go where
    go (Node x ts) = f x (map go ts)

-- | Build a (possibly infinite) tree from a seed value.
--
-- @unfoldTree f b@ constructs a tree by starting with the tree
-- @Node { rootLabel=b, subForest=[] }@ and repeatedly applying @f@ to each
-- 'rootLabel' value in the tree's leaves to generate its 'subForest'.
--
-- For a monadic version, see 'unfoldTreeM' (depth-first) and
-- 'unfoldTreeM_BF' (breadth-first).
--
-- ==== __Examples__
--
-- Construct the tree of @Integer@s where each node has two children:
-- @left = 2*x@ and @right = 2*x + 1@, where @x@ is the 'rootLabel' of the node.
-- Stop when the values exceed 7.
--
-- > let buildNode x = if 2*x + 1 > 7 then (x, []) else (x, [2*x, 2*x+1])
-- > putStr $ drawTree $ fmap show $ unfoldTree buildNode 1
--
-- @
--
-- 1
-- |
-- +- 2
-- |  |
-- |  +- 4
-- |  |
-- |  `- 5
-- |
-- `- 3
--    |
--    +- 6
--    |
--    `- 7
-- @
--
unfoldTree :: (b -> (a, [b])) -> b -> Tree a
unfoldTree f b = let (a, bs) = f b in Node a (unfoldForest f bs)

-- | Build a (possibly infinite) forest from a list of seed values.
--
-- @unfoldForest f seeds@ invokes 'unfoldTree' on each seed value.
--
-- For a monadic version, see 'unfoldForestM' (depth-first) and
-- 'unfoldForestM_BF' (breadth-first).
--
unfoldForest :: (b -> (a, [b])) -> [b] -> [Tree a]
unfoldForest f = map (unfoldTree f)

-- | Monadic tree builder, in depth-first order.
unfoldTreeM :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfoldTreeM f b = do
    (a, bs) <- f b
    ts <- unfoldForestM f bs
    return (Node a ts)

-- | Monadic forest builder, in depth-first order.
unfoldForestM :: Monad m => (b -> m (a, [b])) -> [b] -> m ([Tree a])
unfoldForestM f = Prelude.mapM (unfoldTreeM f)

-- | Monadic tree builder, in breadth-first order.
--
-- See 'unfoldTree' for more info.
--
-- Implemented using an algorithm adapted from
-- /Breadth-First Numbering: Lessons from a Small Exercise in Algorithm Design/,
-- by Chris Okasaki, /ICFP'00/.
unfoldTreeM_BF :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfoldTreeM_BF f b = liftM getElement $ unfoldForestQ f (singleton b)
  where
    getElement xs = case viewl xs of
        x :< _ -> x
        EmptyL -> error "unfoldTreeM_BF"

-- | Monadic forest builder, in breadth-first order.
--
-- See 'unfoldForest' for more info.
--
-- Implemented using an algorithm adapted from
-- /Breadth-First Numbering: Lessons from a Small Exercise in Algorithm Design/,
-- by Chris Okasaki, /ICFP'00/.
unfoldForestM_BF :: Monad m => (b -> m (a, [b])) -> [b] -> m ([Tree a])
unfoldForestM_BF f = liftM toList . unfoldForestQ f . fromList

-- Takes a sequence (queue) of seeds and produces a sequence (reversed queue) of
-- trees of the same length.
unfoldForestQ :: Monad m => (b -> m (a, [b])) -> Seq b -> m (Seq (Tree a))
unfoldForestQ f aQ = case viewl aQ of
    EmptyL -> return empty
    a :< aQ' -> do
        (b, as) <- f a
        tQ <- unfoldForestQ f (Prelude.foldl (|>) aQ' as)
        let (tQ', ts) = splitOnto [] as tQ
        return (Node b ts <| tQ')
  where
    splitOnto :: [a'] -> [b'] -> Seq a' -> (Seq a', [a'])
    splitOnto as [] q = (q, as)
    splitOnto as (_:bs) q = case viewr q of
        q' :> a -> splitOnto (a:as) bs q'
        EmptyR -> error "unfoldForestQ"

-- | \(O(n)\). The leaves of the tree in left-to-right order.
--
-- A leaf is a node with no children.
--
-- ==== __Examples__
--
-- >>> :{
-- leaves $
--   Node 1
--     [ Node 2
--         [ Node 4 []
--         , Node 5 []
--         ]
--     , Node 3
--         [ Node 6 []
--         ]
--     ]
-- :}
-- [4,5,6]
-- >>> leaves (Node "root" [])
-- ["root"]
--
-- @since 0.8
leaves :: Tree a -> [a]
#ifdef __GLASGOW_HASKELL__
leaves t = GHC.Exts.build $ \cons nil ->
  let go (Node x []) z = cons x z
      go (Node _ ts) z = foldr go z ts
  in go t nil
{-# INLINE leaves #-} -- Inline for list fusion
#else
leaves t =
  let go (Node x []) z = x:z
      go (Node _ ts) z = foldr go z ts
  in go t []
#endif

-- | \(O(n)\). The edges of the tree as parent-child pairs in pre-order.
--
-- A tree with \(n\) nodes has \(n-1\) edges.
--
-- ==== __Examples__
--
-- >>> :{
-- edges $
--   Node 1
--     [ Node 2
--         [ Node 4 []
--         , Node 5 []
--         ]
--     , Node 3
--         [ Node 6 []
--         ]
--     ]
-- :}
-- [(1,2),(2,4),(2,5),(1,3),(3,6)]
-- >>> edges (Node "root" [])
-- []
--
-- @since 0.8
edges :: Tree a -> [(a, a)]
#ifdef __GLASGOW_HASKELL__
edges (Node x0 ts0) = GHC.Exts.build $ \cons nil ->
  let go p = foldr (\(Node x ts) z -> cons (p, x) (go x z ts))
  in go x0 nil ts0
{-# INLINE edges #-} -- Inline for list fusion
#else
edges (Node x0 ts0) =
  let go p = foldr (\(Node x ts) z -> (p, x) : go x z ts)
  in go x0 [] ts0
#endif

-- | \(O(n)\). Labels on the paths from each node to the root.
--
-- ==== __Examples__
--
-- >>> :{
-- pathsToRoot $
--   Node 1
--     [ Node 2 []
--     , Node 3 []
--     ]
-- :}
-- Node {rootLabel = 1 :| [], subForest = [Node {rootLabel = 2 :| [1], subForest = []},Node {rootLabel = 3 :| [1], subForest = []}]}
-- >>> pathsToRoot (Node "root" [])
-- Node {rootLabel = "root" :| [], subForest = []}
--
-- @since 0.8
pathsToRoot :: Tree a -> Tree (NonEmpty a)
pathsToRoot = go []
  where
    go ps (Node x ts) = Node (x :| ps) (map (go (x:ps)) ts)

-- | Labels on the paths from the root to each node.
--
-- If the path orientation is not important, consider using 'pathsToRoot'
-- instead because it is more efficient.
--
-- ==== __Examples__
--
-- >>> :{
-- pathsFromRoot $
--   Node 1
--     [ Node 2 []
--     , Node 3 []
--     ]
-- :}
-- Node {rootLabel = 1 :| [], subForest = [Node {rootLabel = 1 :| [2], subForest = []},Node {rootLabel = 1 :| [3], subForest = []}]}
-- >>> pathsFromRoot (Node "root" [])
-- Node {rootLabel = "root" :| [], subForest = []}
--
-- @since 0.8

-- See Note [pathsFromRoot implementation]
pathsFromRoot :: Tree a -> Tree (NonEmpty a)
pathsFromRoot (Node x0 ts0) = Node (x0 :| []) (map (go (singletonBQ x0)) ts0)
  where
    go !q (Node x ts) = Node (toNonEmptyBQ q') (map (go q') ts)
      where
        !q' = snocBQ q x

-- An implementation of Chris Okasaki's banker's queue.
-- Invariant: length front >= length rear
data BQ a = BQ
  a -- head
  {-# UNPACK #-} !Word -- length front + length rear
  [a] -- front
  ![a] -- rear (reversed)

singletonBQ :: a -> BQ a
singletonBQ x = BQ x 0 [] []

snocBQ :: BQ a -> a -> BQ a
snocBQ (BQ x0 n f r) x
  | doReverse = BQ x0 (n+1) (f ++ reverse (x:r)) []
  | otherwise = BQ x0 (n+1) f (x:r)
  where
    doReverse = (n+2) .&. (n+1) == 0
    -- We reverse whenever the length of r would exceed that of f.
    -- This happens every time n+2 is a power of 2.

toNonEmptyBQ :: BQ a -> NonEmpty a
toNonEmptyBQ (BQ x0 _ f r) = case r of
  [] -> x0 :| f -- optimization, no need to rebuild f
  _ -> x0 :| (f ++ reverse r)

-- | A newtype over 'Tree' that folds and traverses in post-order.
--
-- @since 0.8
newtype PostOrder a = PostOrder { unPostOrder :: Tree a }
#ifdef __GLASGOW_HASKELL__
  deriving (Eq, Ord, Read, Show, Data, Generic, Generic1, Lift)
#else
  deriving (Eq, Ord, Read, Show)
#endif

instance Functor PostOrder where
#ifdef __GLASGOW_HASKELL__
  fmap = (coerce :: ((a -> b) -> Tree a -> Tree b)
                 -> (a -> b) -> PostOrder a -> PostOrder b)
         fmapTree
  (<$) = (coerce :: (b -> Tree a -> Tree b)
                 -> b -> PostOrder a -> PostOrder b)
         (<$)
#else
  fmap f = PostOrder . fmapTree f . unPostOrder
  (<$) x = PostOrder . (x <$) . unPostOrder
#endif

-- See Note [Implemented Foldable Tree functions]
instance Foldable PostOrder where
    fold = foldMap id
    {-# INLINABLE fold #-}

    foldMap = foldMapDefault
    {-# INLINE foldMap #-}

    foldr f z0 = \(PostOrder t) -> go t z0  -- Use a lambda to inline with two arguments
      where
        go (Node x ts) z = foldr go (f x z) ts
    {-# INLINE foldr #-}

    foldl' f z0 = \(PostOrder t) -> go z0 t  -- Use a lambda to inline with two arguments
      where
        go !z (Node x ts) =
          let !z' = foldl' go z ts
          in f z' x
    {-# INLINE foldl' #-}

    foldr1 = foldrMap1PostOrder id

    foldl1 = foldlMap1PostOrder id

    null _ = False
    {-# INLINE null #-}

    elem = any . (==)
    {-# INLINABLE elem #-}

    maximum = foldlMap1'PostOrder id max
    {-# INLINABLE maximum #-}

    minimum = foldlMap1'PostOrder id min
    {-# INLINABLE minimum #-}

    sum = foldlMap1'PostOrder id (+)
    {-# INLINABLE sum #-}

    product = foldlMap1'PostOrder id (*)
    {-# INLINABLE product #-}

instance Traversable PostOrder where
  traverse f = \(PostOrder t) -> PostOrder <$> go t
    where
      go (Node x ts) = liftA2 (flip Node) (traverse go ts) (f x)
  {-# INLINE traverse #-}

#if MIN_VERSION_base(4,18,0)
-- See Note [Implemented Foldable1 Tree functions]
instance Foldable1.Foldable1 PostOrder where
  foldMap1 f = \(PostOrder t) -> go t  -- Use a lambda to inline with one argument
    where
      go (Node x []) = f x
      go (Node x (t:ts)) =
        Foldable1.foldrMap1 go (\t' z' -> go t' <> z') (t :| ts) <> f x
  {-# INLINE foldMap1 #-}

  foldMap1' f = foldlMap1'PostOrder f (\z x -> z <> f x)
  {-# INLINE foldMap1' #-}

  toNonEmpty (PostOrder t0) = go t0 []
    where
      go (Node x []) z = x :| z
      go (Node x (t:ts)) z =
        go t (foldr (\t' z' -> foldr (:) z' (PostOrder t')) (x:z) ts)

  maximum = Foldable.maximum
  {-# INLINABLE maximum #-}

  minimum = Foldable.minimum
  {-# INLINABLE minimum #-}

  foldrMap1 = foldrMap1PostOrder

  foldlMap1' = foldlMap1'PostOrder

  foldlMap1 = foldlMap1PostOrder
#endif

foldrMap1PostOrder :: (a -> b) -> (a -> b -> b) -> PostOrder a -> b
foldrMap1PostOrder f g = \(PostOrder (Node x ts)) ->
  foldr (\t z -> foldr g z (PostOrder t)) (f x) ts
{-# INLINE foldrMap1PostOrder #-}

foldlMap1PostOrder :: (a -> b) -> (b -> a -> b) -> PostOrder a -> b
foldlMap1PostOrder f g = \(PostOrder t) -> go t
  where
    go (Node x []) = f x
    go (Node x (t:ts)) =
      g (foldl (\z t' -> foldl g z (PostOrder t')) (go t) ts) x
{-# INLINE foldlMap1PostOrder #-}

foldlMap1'PostOrder :: (a -> b) -> (b -> a -> b) -> PostOrder a -> b
foldlMap1'PostOrder f g = \(PostOrder t) -> go t
  where
    go (Node x []) = f x
    go (Node x (t:ts)) =
      let !z' = foldl' (\z t' -> foldl' g z (PostOrder t')) (go t) ts
      in g z' x
{-# INLINE foldlMap1'PostOrder #-}

--------------------------------------------------------------------------------

-- Note [Implemented Foldable Tree functions]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Implemented:
--
-- foldMap, foldr, foldl': Basic functions.
-- fold, elem: Implemented same as the default definition, but INLINABLE to
-- allow specialization.
-- foldr1, foldl1, null, maximum, minimum: Implemented more efficiently than
-- defaults since trees are non-empty.
-- sum, product: Implemented as strict left folds. Defaults use the lazy foldMap
-- before base 4.15.1.
--
-- Not implemented:
--
-- foldMap', toList, length: Defaults perform well.
-- foldr', foldl: Unlikely to be used.

-- Note [Implemented Foldable1 Tree functions]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Implemented:
--
-- foldMap, foldrMap1, foldlMap1': Basic functions
-- foldMap1': Implemented same as the default definition, but INLINABLE to
-- allow specialization.
-- toNonEmpty, foldlMap1: Implemented more efficiently than default.
-- maximum, minimum: Uses Foldable's implementation.
--
-- Not implemented:
--
-- fold1, head: Defaults perform well.
-- foldrMap1': Unlikely to be used.

-- Note [pathsFromRoot implementation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We use Okasaki's banker's queue for pathsFromRoot because it has some
-- desirable properties when the result is consumed lazily.
--
-- 1. Fully evaluating a node's NonEmpty takes O(d) time, where d is
--    the depth of the node. This is optimal.
-- 2. The elements in the NonEmpty are yielded lazily. Note that the worst case
--    time to yield an element is not O(1), i.e. it is only amortized O(1).
--    More than O(1) work is done when the next element requires forcing (++)
--    suspensions or reversing a rear list. For example, yielding the head has
--    to force O(log d) (++) and so takes O(log d) time.
-- 3. It builds up some beneficial sharing. It is not possible to share the
--    results since the lists have different ends, but we can share some
--    intermediate structures. Consider m sibling nodes at depth d. The front
--    list is shared between them in (front ++ rear1), (front ++ rear2), ...
--    (front + rearm). Forcing a prefix of front in one list can take arbitrary
--    amounts of time per element (total bounded by O(d)), but once it is
--    forced, front is memoized and doing the same for any of the siblings will
--    take O(1) per element.
--
-- Alternatives:
--
-- * Implement it like pathsToRoot and reverse the NonEmptys. This does satisfy
--   point 1 above. On 2 there's a trade-off, it costs a full O(d) to access the
--   head and O(1) per element after that. On 3 it compares poorly because there
--   is no sharing. Accessing the heads of m siblings will take O(dm) compared
--   to the current O(d + m).
-- * Use Okasaki's real-time queues. This would guarantee O(1) per element, but
--   has worse constant-factor overall and does not seem worth the trouble.
--
-- GHC base also uses a banker's queue for Data.List.inits. inits is similar
-- in nature to pathsFromRoot since a list is a tree where each node has one or
-- zero children.
