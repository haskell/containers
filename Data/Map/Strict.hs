{-# LANGUAGE CPP #-}
#if !defined(TESTING) && __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map.Strict
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of ordered maps from keys to values
-- (dictionaries).
--
-- API of this module is strict in both the keys and the values.
-- If you need value-lazy maps, use 'Data.Map.Lazy' instead.
-- The 'Map' type is shared between the lazy and strict modules,
-- meaning that the same 'Map' value can be passed to functions in
-- both modules (although that is rarely needed).
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import qualified Data.Map.Strict as Map
--
-- The implementation of 'Map' is based on /size balanced/ binary trees (or
-- trees of /bounded balance/) as described by:
--
--    * Stephen Adams, \"/Efficient sets: a balancing act/\",
--     Journal of Functional Programming 3(4):553-562, October 1993,
--     <http://www.swiss.ai.mit.edu/~adams/BB/>.
--
--    * J. Nievergelt and E.M. Reingold,
--      \"/Binary search trees of bounded balance/\",
--      SIAM journal of computing 2(1), March 1973.
--
-- Note that the implementation is /left-biased/ -- the elements of a
-- first argument are always preferred to the second, for example in
-- 'union' or 'insert'.
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation (<http://en.wikipedia.org/wiki/Big_O_notation>).
--
-- Be aware that the 'Functor', 'Traversable' and 'Data' instances
-- are the same as for the 'Data.Map.Lazy' module, so if they are used
-- on strict maps, the resulting maps will be lazy.
-----------------------------------------------------------------------------

-- It is crucial to the performance that the functions specialize on the Ord
-- type when possible. GHC 7.0 and higher does this by itself when it sees th
-- unfolding of a function -- that is why all public functions are marked
-- INLINABLE (that exposes the unfolding).
--
-- For other compilers and GHC pre 7.0, we mark some of the functions INLINE.
-- We mark the functions that just navigate down the tree (lookup, insert,
-- delete and similar). That navigation code gets inlined and thus specialized
-- when possible. There is a price to pay -- code growth. The code INLINED is
-- therefore only the tree navigation, all the real work (rebalancing) is not
-- INLINED by using a NOINLINE.
--
-- All methods that can be INLINE are not recursive -- a 'go' function doing
-- the real work is provided.

module Data.Map.Strict
    (
    -- * Strictness properties
    -- $strictness

    -- * Map type
#if !defined(TESTING)
    Map              -- instance Eq,Show,Read
#else
    Map(..)          -- instance Eq,Show,Read
#endif

    -- * Operators
    , (!), (\\)

    -- * Query
    , null
    , size
    , member
    , notMember
    , lookup
    , findWithDefault

    -- * Construction
    , empty
    , singleton

    -- ** Insertion
    , insert
    , insertWith
    , insertWithKey
    , insertLookupWithKey

    -- ** Delete\/Update
    , delete
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter

    -- * Combine

    -- ** Union
    , union
    , unionWith
    , unionWithKey
    , unions
    , unionsWith

    -- ** Difference
    , difference
    , differenceWith
    , differenceWithKey

    -- ** Intersection
    , intersection
    , intersectionWith
    , intersectionWithKey

    -- * Traversal
    -- ** Map
    , map
    , mapWithKey
    , mapAccum
    , mapAccumWithKey
    , mapAccumRWithKey
    , mapKeys
    , mapKeysWith
    , mapKeysMonotonic

    -- * Folds
    , foldr
    , foldl
    , foldrWithKey
    , foldlWithKey
    -- ** Strict folds
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'

    -- * Conversion
    , elems
    , keys
    , keysSet
    , assocs

    -- ** Lists
    , toList
    , fromList
    , fromListWith
    , fromListWithKey

    -- ** Ordered lists
    , toAscList
    , toDescList
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromDistinctAscList

    -- * Filter
    , filter
    , filterWithKey
    , partition
    , partitionWithKey

    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey

    , split
    , splitLookup

    -- * Submap
    , isSubmapOf, isSubmapOfBy
    , isProperSubmapOf, isProperSubmapOfBy

    -- * Indexed
    , lookupIndex
    , findIndex
    , elemAt
    , updateAt
    , deleteAt

    -- * Min\/Max
    , findMin
    , findMax
    , deleteMin
    , deleteMax
    , deleteFindMin
    , deleteFindMax
    , updateMin
    , updateMax
    , updateMinWithKey
    , updateMaxWithKey
    , minView
    , maxView
    , minViewWithKey
    , maxViewWithKey

    -- * Debugging
    , showTree
    , showTreeWith
    , valid

#if defined(TESTING)
    -- * Internals
    , bin
    , balanced
    , join
    , merge
#endif
    ) where

import Prelude hiding (lookup,map,filter,foldr,foldl,null)
import qualified Data.List as List

import Data.Map.Base hiding
    ( findWithDefault
    , singleton
    , insert
    , insertWith
    , insertWithKey
    , insertLookupWithKey
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter
    , unionWith
    , unionWithKey
    , unionsWith
    , differenceWith
    , differenceWithKey
    , intersectionWith
    , intersectionWithKey
    , map
    , mapWithKey
    , mapAccum
    , mapAccumWithKey
    , mapAccumRWithKey
    , mapKeys
    , mapKeysWith
    , mapKeysMonotonic
    , fromList
    , fromListWith
    , fromListWithKey
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromDistinctAscList
    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey
    , updateAt
    , updateMin
    , updateMax
    , updateMinWithKey
    , updateMaxWithKey
    )
import Data.StrictPair

-- Use macros to define strictness of functions.  STRICT_x_OF_y
-- denotes an y-ary function strict in the x-th parameter. Similarly
-- STRICT_x_y_OF_z denotes an z-ary function strict in the x-th and
-- y-th parameter.  We do not use BangPatterns, because they are not
-- in any standard and we want the compilers to be compiled by as many
-- compilers as possible.
#define STRICT_2_OF_3(fn) fn _ arg _ | arg `seq` False = undefined
#define STRICT_1_2_OF_3(fn) fn arg1 arg2 _ | arg1 `seq` arg2 `seq` False = undefined
#define STRICT_2_3_OF_4(fn) fn _ arg1 arg2 _ | arg1 `seq` arg2 `seq` False = undefined

-- $strictness
--
-- This module satisfies the following strictness properties:
--
-- 1. Key and value arguments are evaluated to WHNF;
--
-- 2. Keys and values are evaluated to WHNF before they are stored in
--    the map.
--
-- Here are some examples that illustrate the first property:
--
-- > insertWith (\ new old -> old) k undefined m  ==  undefined
-- > delete undefined m  ==  undefined
--
-- Here are some examples that illustrate the second property:
--
-- > map (\ v -> undefined) m  ==  undefined      -- m is not empty
-- > mapKeys (\ k -> undefined) m  ==  undefined  -- m is not empty

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'

findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault def k m = def `seq` case lookup k m of
    Nothing -> def
    Just x  -> x
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE findWithDefault #-}
#else
{-# INLINE findWithDefault #-}
#endif

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}

-- | /O(1)/. A map with a single element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1

singleton :: k -> a -> Map k a
singleton k x = x `seq` Bin 1 k x Tip Tip
{-# INLINE singleton #-}

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}
-- | /O(log n)/. Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
--
-- > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
-- > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
-- > insert 5 'x' empty                         == singleton 5 'x'

insert :: Ord k => k -> a -> Map k a -> Map k a
insert = go
  where
    STRICT_1_2_OF_3(go)
    go kx x Tip = singleton kx x
    go kx x (Bin sz ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go kx x l) r
            GT -> balanceR ky y l (go kx x r)
            EQ -> Bin sz kx x l r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insert #-}
#else
{-# INLINE insert #-}
#endif

-- | /O(log n)/. Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f new_value old_value)@.
--
-- > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
-- > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f = insertWithKey (\_ x' y' -> f x' y')
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWith #-}
#else
{-# INLINE insertWith #-}
#endif

-- | /O(log n)/. Insert with a function, combining key, new value and old value.
-- @'insertWithKey' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key,f key new_value old_value)@.
-- Note that the key passed to f is the same key passed to 'insertWithKey'.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
-- > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"

insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey = go
  where
    STRICT_2_3_OF_4(go)
    go _ kx x Tip = singleton kx x
    go f kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go f kx x l) r
            GT -> balanceR ky y l (go f kx x r)
            EQ -> let x' = f kx x y
                  in x' `seq` Bin sy kx x' l r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWithKey #-}
#else
{-# INLINE insertWithKey #-}
#endif

-- | /O(log n)/. Combines insert operation with old value retrieval.
-- The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
-- > insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
-- > insertLookupWithKey f 5 "xxx" empty                         == (Nothing,  singleton 5 "xxx")
--
-- This is how to define @insertLookup@ using @insertLookupWithKey@:
--
-- > let insertLookup kx x t = insertLookupWithKey (\_ a _ -> a) kx x t
-- > insertLookup 5 "x" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "x")])
-- > insertLookup 7 "x" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "x")])

insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a
                    -> (Maybe a, Map k a)
insertLookupWithKey = go
  where
    STRICT_2_3_OF_4(go)
    go _ kx x Tip = Nothing `strictPair` singleton kx x
    go f kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> let (found, l') = go f kx x l
                  in found `strictPair` balanceL ky y l' r
            GT -> let (found, r') = go f kx x r
                  in found `strictPair` balanceR ky y l r'
            EQ -> let x' = f kx x y
                  in x' `seq` (Just y `strictPair` Bin sy kx x' l r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertLookupWithKey #-}
#else
{-# INLINE insertLookupWithKey #-}
#endif

{--------------------------------------------------------------------
  Deletion
  [delete] is the inlined version of [deleteWith (\k x -> Nothing)]
--------------------------------------------------------------------}

-- | /O(log n)/. Update a value at a specific key with the result of the provided function.
-- When the key is not
-- a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty

adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust f = adjustWithKey (\_ x -> f x)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE adjust #-}
#else
{-# INLINE adjust #-}
#endif

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty

adjustWithKey :: Ord k => (k -> a -> a) -> k -> Map k a -> Map k a
adjustWithKey f = updateWithKey (\k' x' -> Just (f k' x'))
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE adjustWithKey #-}
#else
{-# INLINE adjustWithKey #-}
#endif

-- | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f = updateWithKey (\_ x -> f x)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE update #-}
#else
{-# INLINE update #-}
#endif

-- | /O(log n)/. The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
updateWithKey = go
  where
    STRICT_2_OF_3(go)
    go _ _ Tip = Tip
    go f k(Bin sx kx x l r) =
        case compare k kx of
           LT -> balanceR kx x (go f k l) r
           GT -> balanceL kx x l (go f k r)
           EQ -> case f kx x of
                   Just x' -> x' `seq` Bin sx kx x' l r
                   Nothing -> glue l r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE updateWithKey #-}
#else
{-# INLINE updateWithKey #-}
#endif

-- | /O(log n)/. Lookup and update. See also 'updateWithKey'.
-- The function returns changed value, if it is updated.
-- Returns the original key value if the map entry is deleted.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "5:new a", fromList [(3, "b"), (5, "5:new a")])
-- > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")

updateLookupWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a,Map k a)
updateLookupWithKey = go
 where
   STRICT_2_OF_3(go)
   go _ _ Tip = (Nothing,Tip)
   go f k (Bin sx kx x l r) =
          case compare k kx of
               LT -> let (found,l') = go f k l
                     in found `strictPair` balanceR kx x l' r
               GT -> let (found,r') = go f k r
                     in found `strictPair` balanceL kx x l r'
               EQ -> case f kx x of
                       Just x' -> x' `seq` (Just x' `strictPair` Bin sx kx x' l r)
                       Nothing -> (Just x,glue l r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE updateLookupWithKey #-}
#else
{-# INLINE updateLookupWithKey #-}
#endif

-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
--
-- > let f _ = Nothing
-- > alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > alter f 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- >
-- > let f _ = Just "c"
-- > alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "c")]
-- > alter f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "c")]

alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter = go
  where
    STRICT_2_OF_3(go)
    go f k Tip = case f Nothing of
               Nothing -> Tip
               Just x  -> singleton k x

    go f k (Bin sx kx x l r) = case compare k kx of
               LT -> balance kx x (go f k l) r
               GT -> balance kx x l (go f k r)
               EQ -> case f (Just x) of
                       Just x' -> x' `seq` Bin sx kx x' l r
                       Nothing -> glue l r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE alter #-}
#else
{-# INLINE alter #-}
#endif

{--------------------------------------------------------------------
  Indexing
--------------------------------------------------------------------}

-- | /O(log n)/. Update the element at /index/. Calls 'error' when an
-- invalid index is used.
--
-- > updateAt (\ _ _ -> Just "x") 0    (fromList [(5,"a"), (3,"b")]) == fromList [(3, "x"), (5, "a")]
-- > updateAt (\ _ _ -> Just "x") 1    (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "x")]
-- > updateAt (\ _ _ -> Just "x") 2    (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\ _ _ -> Just "x") (-1) (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\_ _  -> Nothing)  0    (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
-- > updateAt (\_ _  -> Nothing)  1    (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > updateAt (\_ _  -> Nothing)  2    (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\_ _  -> Nothing)  (-1) (fromList [(5,"a"), (3,"b")])    Error: index out of range

updateAt :: (k -> a -> Maybe a) -> Int -> Map k a -> Map k a
updateAt f i t = i `seq`
  case t of
    Tip -> error "Map.updateAt: index out of range"
    Bin sx kx x l r -> case compare i sizeL of
      LT -> balanceR kx x (updateAt f i l) r
      GT -> balanceL kx x l (updateAt f (i-sizeL-1) r)
      EQ -> case f kx x of
              Just x' -> x' `seq` Bin sx kx x' l r
              Nothing -> glue l r
      where
        sizeL = size l

{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}

-- | /O(log n)/. Update the value at the minimal key.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMin :: (a -> Maybe a) -> Map k a -> Map k a
updateMin f m
  = updateMinWithKey (\_ x -> f x) m

-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMax :: (a -> Maybe a) -> Map k a -> Map k a
updateMax f m
  = updateMaxWithKey (\_ x -> f x) m


-- | /O(log n)/. Update the value at the minimal key.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMinWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMinWithKey _ Tip                 = Tip
updateMinWithKey f (Bin sx kx x Tip r) = case f kx x of
                                           Nothing -> r
                                           Just x' -> x' `seq` Bin sx kx x' Tip r
updateMinWithKey f (Bin _ kx x l r)    = balanceR kx x (updateMinWithKey f l) r

-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMaxWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMaxWithKey _ Tip                 = Tip
updateMaxWithKey f (Bin sx kx x l Tip) = case f kx x of
                                           Nothing -> l
                                           Just x' -> x' `seq` Bin sx kx x' l Tip
updateMaxWithKey f (Bin _ kx x l r)    = balanceL kx x l (updateMaxWithKey f r)

{--------------------------------------------------------------------
  Union.
--------------------------------------------------------------------}

-- | The union of a list of maps, with a combining operation:
--   (@'unionsWith' f == 'Prelude.foldl' ('unionWith' f) 'empty'@).
--
-- > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]

unionsWith :: Ord k => (a->a->a) -> [Map k a] -> Map k a
unionsWith f ts
  = foldlStrict (unionWith f) empty ts
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE unionsWith #-}
#endif

{--------------------------------------------------------------------
  Union with a combining function
--------------------------------------------------------------------}
-- | /O(n+m)/. Union with a combining function. The implementation uses the efficient /hedge-union/ algorithm.
--
-- > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]

unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith f m1 m2
  = unionWithKey (\_ x y -> f x y) m1 m2
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE unionWith #-}
#endif

-- | /O(n+m)/.
-- Union with a combining function. The implementation uses the efficient /hedge-union/ algorithm.
-- Hedge-union is more efficient on (bigset \``union`\` smallset).
--
-- > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]

unionWithKey :: Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey _ Tip t2  = t2
unionWithKey _ t1 Tip  = t1
unionWithKey f t1 t2 = hedgeUnionWithKey f NothingS NothingS t1 t2
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE unionWithKey #-}
#endif

hedgeUnionWithKey :: Ord a
                  => (a -> b -> b -> b)
                  -> MaybeS a -> MaybeS a
                  -> Map a b -> Map a b
                  -> Map a b
hedgeUnionWithKey _ _     _     t1 Tip
  = t1
hedgeUnionWithKey _ blo bhi Tip (Bin _ kx x l r)
  = join kx x (filterGt blo l) (filterLt bhi r)
hedgeUnionWithKey f blo bhi (Bin _ kx x l r) t2
  = newx `seq` join kx newx (hedgeUnionWithKey f blo bmi l lt)
                            (hedgeUnionWithKey f bmi bhi r gt)
  where
    bmi        = JustS kx
    lt         = trim blo bmi t2
    (found,gt) = trimLookupLo kx bhi t2
    newx       = case found of
                   Nothing -> x
                   Just (_,y) -> f kx x y
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE hedgeUnionWithKey #-}
#endif

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}

-- | /O(n+m)/. Difference with a combining function.
-- When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
--
-- > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- > differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
-- >     == singleton 3 "b:B"

differenceWith :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWith f m1 m2
  = differenceWithKey (\_ x y -> f x y) m1 m2
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE differenceWith #-}
#endif

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
--
-- > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- > differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
-- >     == singleton 3 "3:b|B"

differenceWithKey :: Ord k => (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWithKey _ Tip _   = Tip
differenceWithKey _ t1 Tip  = t1
differenceWithKey f t1 t2   = hedgeDiffWithKey f NothingS NothingS t1 t2
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE differenceWithKey #-}
#endif

hedgeDiffWithKey :: Ord a
                 => (a -> b -> c -> Maybe b)
                 -> MaybeS a -> MaybeS a
                 -> Map a b -> Map a c
                 -> Map a b
hedgeDiffWithKey _ _     _     Tip _
  = Tip
hedgeDiffWithKey _ blo bhi (Bin _ kx x l r) Tip
  = join kx x (filterGt blo l) (filterLt bhi r)
hedgeDiffWithKey f blo bhi t (Bin _ kx x l r)
  = case found of
      Nothing -> merge tl tr
      Just (ky,y) ->
          case f ky y x of
            Nothing -> merge tl tr
            Just z  -> z `seq` join ky z tl tr
  where
    bmi        = JustS kx
    lt         = trim blo bmi t
    (found,gt) = trimLookupLo kx bhi t
    tl         = hedgeDiffWithKey f blo bmi lt l
    tr         = hedgeDiffWithKey f bmi bhi gt r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE hedgeDiffWithKey #-}
#endif

{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}

-- | /O(n+m)/. Intersection with a combining function.
--
-- > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"

intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith f m1 m2
  = intersectionWithKey (\_ x y -> f x y) m1 m2
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE intersectionWith #-}
#endif

-- | /O(n+m)/. Intersection with a combining function.
-- Intersection is more efficient on (bigset \``intersection`\` smallset).
--
-- > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- > intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "5:a|A"


intersectionWithKey :: Ord k => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWithKey _ Tip _ = Tip
intersectionWithKey _ _ Tip = Tip
intersectionWithKey f t1@(Bin s1 k1 x1 l1 r1) t2@(Bin s2 k2 x2 l2 r2) =
   if s1 >= s2 then
      let (lt,found,gt) = splitLookupWithKey k2 t1
          tl            = intersectionWithKey f lt l2
          tr            = intersectionWithKey f gt r2
      in case found of
      Just (k,x) -> join k (f k x x2) tl tr
      Nothing -> merge tl tr
   else let (lt,found,gt) = splitLookup k1 t2
            tl            = intersectionWithKey f l1 lt
            tr            = intersectionWithKey f r1 gt
      in case found of
      Just x -> let x' = f k1 x1 x in x' `seq` join k1 x' tl tr
      Nothing -> merge tl tr
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE intersectionWithKey #-}
#endif

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}

-- | /O(n)/. Map values and collect the 'Just' results.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"

mapMaybe :: (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f = mapMaybeWithKey (\_ x -> f x)

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"

mapMaybeWithKey :: (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey _ Tip = Tip
mapMaybeWithKey f (Bin _ kx x l r) = case f kx x of
  Just y  -> y `seq` join kx y (mapMaybeWithKey f l) (mapMaybeWithKey f r)
  Nothing -> merge (mapMaybeWithKey f l) (mapMaybeWithKey f r)

-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- >
-- > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])

mapEither :: (a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEither f m
  = mapEitherWithKey (\_ x -> f x) m

-- | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results.
--
-- > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
-- > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
-- >
-- > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])

mapEitherWithKey :: (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEitherWithKey _ Tip = (Tip, Tip)
mapEitherWithKey f (Bin _ kx x l r) = case f kx x of
  Left y  -> y `seq` (join kx y l1 r1 `strictPair` merge l2 r2)
  Right z -> z `seq` (merge l1 r1 `strictPair` join kx z l2 r2)
 where
    (l1,l2) = mapEitherWithKey f l
    (r1,r2) = mapEitherWithKey f r

{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}
-- | /O(n)/. Map a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]

map :: (a -> b) -> Map k a -> Map k b
map f = mapWithKey (\_ x -> f x)

-- | /O(n)/. Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey _ Tip = Tip
mapWithKey f (Bin sx kx x l r) = let x' = f kx x
                                 in x' `seq` Bin sx kx x' (mapWithKey f l) (mapWithKey f r)

-- | /O(n)/. The function 'mapAccum' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])

mapAccum :: (a -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccum f a m
  = mapAccumWithKey (\a' _ x' -> f a' x') a m

-- | /O(n)/. The function 'mapAccumWithKey' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])

mapAccumWithKey :: (a -> k -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccumWithKey f a t
  = mapAccumL f a t

-- | /O(n)/. The function 'mapAccumL' threads an accumulating
-- argument through the map in ascending order of keys.
mapAccumL :: (a -> k -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccumL _ a Tip               = (a,Tip)
mapAccumL f a (Bin sx kx x l r) =
  let (a1,l') = mapAccumL f a l
      (a2,x') = f a1 kx x
      (a3,r') = mapAccumL f a2 r
  in x' `seq` (a3,Bin sx kx x' l' r')

-- | /O(n)/. The function 'mapAccumR' threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (a -> k -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccumRWithKey _ a Tip = (a,Tip)
mapAccumRWithKey f a (Bin sx kx x l r) =
  let (a1,r') = mapAccumRWithKey f a r
      (a2,x') = f a1 kx x
      (a3,l') = mapAccumRWithKey f a2 l
  in x' `seq` (a3,Bin sx kx x' l' r')

-- | /O(n*log n)/.
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the greatest of
-- the original keys is retained.
--
-- > mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        == fromList [(4, "b"), (6, "a")]
-- > mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "c"
-- > mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "c"

mapKeys :: Ord k2 => (k1->k2) -> Map k1 a -> Map k2 a
mapKeys = mapKeysWith (\x _ -> x)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE mapKeys #-}
#endif

-- | /O(n*log n)/.
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
--
-- > mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "cdab"
-- > mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "cdab"

mapKeysWith :: Ord k2 => (a -> a -> a) -> (k1->k2) -> Map k1 a -> Map k2 a
mapKeysWith c f = fromListWith c . List.map fFirst . toList
    where fFirst (x,y) = (f x, y)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE mapKeysWith #-}
#endif


-- | /O(n)/.
-- @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapKeysMonotonic f s == mapKeys f s
-- >     where ls = keys s
--
-- This means that @f@ maps distinct original keys to distinct resulting keys.
-- This function has better performance than 'mapKeys'.
--
-- > mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) == fromList [(6, "b"), (10, "a")]
-- > valid (mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")])) == True
-- > valid (mapKeysMonotonic (\ _ -> 1)     (fromList [(5,"a"), (3,"b")])) == False

mapKeysMonotonic :: (k1->k2) -> Map k1 a -> Map k2 a
mapKeysMonotonic _ Tip = Tip
mapKeysMonotonic f (Bin sz k x l r) =
    let k' = f k
    in k' `seq` Bin sz k' x (mapKeysMonotonic f l) (mapKeysMonotonic f r)

{--------------------------------------------------------------------
  Lists
  use [foldlStrict] to reduce demand on the control-stack
--------------------------------------------------------------------}
-- | /O(n*log n)/. Build a map from a list of key\/value pairs. See also 'fromAscList'.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
--
-- > fromList [] == empty
-- > fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
-- > fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]

fromList :: Ord k => [(k,a)] -> Map k a
fromList xs
  = foldlStrict ins empty xs
  where
    ins t (k,x) = insert k x t
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromList #-}
#endif

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
-- > fromListWith (++) [] == empty

fromListWith :: Ord k => (a -> a -> a) -> [(k,a)] -> Map k a
fromListWith f xs
  = fromListWithKey (\_ x y -> f x y) xs
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromListWith #-}
#endif

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWithKey'.
--
-- > let f k a1 a2 = (show k) ++ a1 ++ a2
-- > fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "3ab"), (5, "5a5ba")]
-- > fromListWithKey f [] == empty

fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k,a)] -> Map k a
fromListWithKey f xs
  = foldlStrict ins empty xs
  where
    ins t (k,x) = insertWithKey f k x t
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromListWithKey #-}
#endif

{--------------------------------------------------------------------
  Building trees from ascending/descending lists can be done in linear time.

  Note that if [xs] is ascending that:
    fromAscList xs       == fromList xs
    fromAscListWith f xs == fromListWith f xs
--------------------------------------------------------------------}
-- | /O(n)/. Build a map from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
-- > valid (fromAscList [(3,"b"), (5,"a"), (5,"b")]) == True
-- > valid (fromAscList [(5,"a"), (3,"b"), (5,"b")]) == False

fromAscList :: Eq k => [(k,a)] -> Map k a
fromAscList xs
  = fromAscListWithKey (\_ x _ -> x) xs
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromAscList #-}
#endif

-- | /O(n)/. Build a map from an ascending list in linear time with a combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]
-- > valid (fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")]) == True
-- > valid (fromAscListWith (++) [(5,"a"), (3,"b"), (5,"b")]) == False

fromAscListWith :: Eq k => (a -> a -> a) -> [(k,a)] -> Map k a
fromAscListWith f xs
  = fromAscListWithKey (\_ x y -> f x y) xs
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromAscListWith #-}
#endif

-- | /O(n)/. Build a map from an ascending list in linear time with a
-- combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > let f k a1 a2 = (show k) ++ ":" ++ a1 ++ a2
-- > fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")] == fromList [(3, "b"), (5, "5:b5:ba")]
-- > valid (fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")]) == True
-- > valid (fromAscListWithKey f [(5,"a"), (3,"b"), (5,"b"), (5,"b")]) == False

fromAscListWithKey :: Eq k => (k -> a -> a -> a) -> [(k,a)] -> Map k a
fromAscListWithKey f xs
  = fromDistinctAscList (combineEq f xs)
  where
  -- [combineEq f xs] combines equal elements with function [f] in an ordered list [xs]
  combineEq _ xs'
    = case xs' of
        []     -> []
        [x]    -> [x]
        (x:xx) -> combineEq' x xx

  combineEq' z [] = [z]
  combineEq' z@(kz,zz) (x@(kx,xx):xs')
    | kx==kz    = let yy = f kx xx zz in yy `seq` combineEq' (kx,yy) xs'
    | otherwise = z:combineEq' x xs'
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromAscListWithKey #-}
#endif

-- | /O(n)/. Build a map from an ascending list of distinct elements in linear time.
-- /The precondition is not checked./
--
-- > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]
-- > valid (fromDistinctAscList [(3,"b"), (5,"a")])          == True
-- > valid (fromDistinctAscList [(3,"b"), (5,"a"), (5,"b")]) == False

fromDistinctAscList :: [(k,a)] -> Map k a
fromDistinctAscList xs
  = create const (length xs) xs
  where
    -- 1) use continuations so that we use heap space instead of stack space.
    -- 2) special case for n==5 to create bushier trees.
    create c 0 xs' = c Tip xs'
    create c 5 xs' = case xs' of
                       ((k1,x1):(k2,x2):(k3,x3):(k4,x4):(k5,x5):xx)
                            -> x1 `seq` x2 `seq` x3 `seq` x4 `seq` x5 `seq`
                               c (bin k4 x4 (bin k2 x2 (singleton k1 x1) (singleton k3 x3))
                                  (singleton k5 x5)) xx
                       _ -> error "fromDistinctAscList create"
    create c n xs' = seq nr $ create (createR nr c) nl xs'
      where nl = n `div` 2
            nr = n - nl - 1

    createR n c l ((k,x):ys) = x `seq` create (createB l k x c) n ys
    createR _ _ _ []         = error "fromDistinctAscList createR []"
    createB l k x c r zs     = x `seq` c (bin k x l r) zs
