{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif
#if __GLASGOW_HASKELL__ >= 703
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
-- Stability   :  experimental
-- Portability :  portable
--
-- Multi-way trees (/aka/ rose trees) and forests.
--
-----------------------------------------------------------------------------

module Data.Tree(
    Tree(..), Forest,
    -- * Two-dimensional drawing
    drawTree, drawForest, drawTreeU, drawForestU,
    -- * Extraction
    flatten, levels, foldTree,
    -- * Building trees
    unfoldTree, unfoldForest,
    unfoldTreeM, unfoldForestM,
    unfoldTreeM_BF, unfoldForestM_BF,
    ) where

#if MIN_VERSION_base(4,8,0)
import Data.Foldable (toList)
#else
import Control.Applicative (Applicative(..), (<$>))
import Data.Foldable (Foldable(foldMap), toList)
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable(traverse))
#endif

import Control.Monad (liftM)
import Data.Sequence (Seq, empty, singleton, (<|), (|>), fromList,
            ViewL(..), ViewR(..), viewl, viewr)
import Data.Typeable
import Control.DeepSeq (NFData(rnf))

#ifdef __GLASGOW_HASKELL__
import Data.Data (Data)
#endif
#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic, Generic1)
#elif __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif

#if MIN_VERSION_base(4,8,0)
import Data.Coerce
#endif

-- | Multi-way trees, also known as /rose trees/.
data Tree a = Node {
        rootLabel :: a,         -- ^ label value
        subForest :: Forest a   -- ^ zero or more child trees
    }
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 706
  deriving (Eq, Read, Show, Data, Generic, Generic1)
#elif __GLASGOW_HASKELL__ >= 702
  deriving (Eq, Read, Show, Data, Generic)
#else
  deriving (Eq, Read, Show, Data)
#endif
#else
  deriving (Eq, Read, Show)
#endif
type Forest a = [Tree a]

INSTANCE_TYPEABLE1(Tree)

instance Functor Tree where
    fmap = fmapTree

fmapTree :: (a -> b) -> Tree a -> Tree b
fmapTree f (Node x ts) = Node (f x) (map (fmapTree f) ts)
#if MIN_VERSION_base(4,8,0)
-- Safe coercions were introduced in 4.7.0, but I am not sure if they played
-- well enough with RULES to do what we want.
{-# NOINLINE [1] fmapTree #-}
{-# RULES
"fmapTree/coerce" fmapTree coerce = coerce
 #-}
#endif

instance Applicative Tree where
    pure x = Node x []
    Node f tfs <*> tx@(Node x txs) =
        Node (f x) (map (f <$>) txs ++ map (<*> tx) tfs)

instance Monad Tree where
    return = pure
    Node x ts >>= f = Node x' (ts' ++ map (>>= f) ts)
      where Node x' ts' = f x

instance Traversable Tree where
    traverse f (Node x ts) = Node <$> f x <*> traverse (traverse f) ts

instance Foldable Tree where
    foldMap f (Node x ts) = f x `mappend` foldMap (foldMap f) ts

#if MIN_VERSION_base(4,8,0)
    null _ = False
    {-# INLINE null #-}
    toList = flatten
    {-# INLINE toList #-}
#endif

instance NFData a => NFData (Tree a) where
    rnf (Node x ts) = rnf x `seq` rnf ts

-- | Neat 2-dimensional drawing of a tree.
drawTree :: Tree String -> String
drawTree  = unlines . draw

-- | Neat 2-dimensional drawing of a forest.
drawForest :: Forest String -> String
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

-- | Neat 2-dimensional drawing of a tree. Unicode paths
drawTreeU :: Tree String -> String
drawTreeU  = unlines . drawU

-- | Neat 2-dimensional drawing of a forest.  Unicode paths
drawForestU :: Forest String -> String
drawForestU  = unlines . map drawTreeU

drawU :: Tree String -> [String]
drawU (Node x ts0) = x : drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        shift "\x2514\x2500" "  " (draw t)
    drawSubTrees (t:ts) =
        shift "\x251c\x2500" "\x2502 " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)
-- | The elements of a tree in pre-order.
flatten :: Tree a -> [a]
flatten t = squish t []
  where squish (Node x ts) xs = x:Prelude.foldr squish xs ts

-- | Lists of nodes at each level of the tree.
levels :: Tree a -> [[a]]
levels t =
    map (map rootLabel) $
        takeWhile (not . null) $
        iterate (concatMap subForest) [t]

-- | Catamorphism on trees.
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go where
    go (Node x ts) = f x (map go ts)

-- | Build a tree from a seed value
unfoldTree :: (b -> (a, [b])) -> b -> Tree a
unfoldTree f b = let (a, bs) = f b in Node a (unfoldForest f bs)

-- | Build a forest from a list of seed values
unfoldForest :: (b -> (a, [b])) -> [b] -> Forest a
unfoldForest f = map (unfoldTree f)

-- | Monadic tree builder, in depth-first order
unfoldTreeM :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfoldTreeM f b = do
    (a, bs) <- f b
    ts <- unfoldForestM f bs
    return (Node a ts)

-- | Monadic forest builder, in depth-first order
unfoldForestM :: Monad m => (b -> m (a, [b])) -> [b] -> m (Forest a)
unfoldForestM f = Prelude.mapM (unfoldTreeM f)

-- | Monadic tree builder, in breadth-first order,
-- using an algorithm adapted from
-- /Breadth-First Numbering: Lessons from a Small Exercise in Algorithm Design/,
-- by Chris Okasaki, /ICFP'00/.
unfoldTreeM_BF :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfoldTreeM_BF f b = liftM getElement $ unfoldForestQ f (singleton b)
  where
    getElement xs = case viewl xs of
        x :< _ -> x
        EmptyL -> error "unfoldTreeM_BF"

-- | Monadic forest builder, in breadth-first order,
-- using an algorithm adapted from
-- /Breadth-First Numbering: Lessons from a Small Exercise in Algorithm Design/,
-- by Chris Okasaki, /ICFP'00/.
unfoldForestM_BF :: Monad m => (b -> m (a, [b])) -> [b] -> m (Forest a)
unfoldForestM_BF f = liftM toList . unfoldForestQ f . fromList

-- takes a sequence (queue) of seeds
-- produces a sequence (reversed queue) of trees of the same length
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
