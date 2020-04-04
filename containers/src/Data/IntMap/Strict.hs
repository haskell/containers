{-# LANGUAGE CPP, BangPatterns #-}

#include "containers.h"

#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
#if USE_REWRITE_RULES
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMap.Strict
-- Copyright   :  Documentation & Interface (c) Daan Leijen 2002
--                Documentation (c) Andriy Palamarchuk 2008
--                Documentation & Implementation (c) Jonathan "gereeter" S. 2020
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
--
-- = Finite Int Maps (strict interface)
--
-- The @'IntMap' v@ type represents a finite map (sometimes called a dictionary)
-- from key of type @Int@ to values of type @v@.
--
-- Each function in this module is careful to force values before installing
-- them in an 'IntMap'. This is usually more efficient when laziness is not
-- necessary. When laziness /is/ required, use the functions in
-- "Data.IntMap.Lazy".
--
-- In particular, the functions in this module obey the following law:
--
--  - If all values stored in all maps in the arguments are in WHNF, then all
--    values stored in all maps in the results will be in WHNF once those maps
--    are evaluated.
--
-- For a walkthrough of the most commonly used functions see the
-- <https://haskell-containers.readthedocs.io/en/latest/map.html maps introduction>.
--
-- This module is intended to be imported qualified, to avoid name clashes with
-- Prelude functions:
--
-- > import Data.IntMap.Strict (IntMap)
-- > import qualified Data.IntMap.Strict as IntMap
--
-- Note that the implementation is generally /left-biased/. Functions that take
-- two maps as arguments and combine them, such as `union` and `intersection`,
-- prefer the values in the first argument to those in the second.
--
--
-- == Detailed performance information
--
-- The amortized running time is given for each operation, with /n/ referring to
-- the number of entries in the map and /W/ referring to the number of bits in
-- an 'Int' (32 or 64).
--
-- Benchmarks comparing "Data.IntMap.Strict" with other dictionary
-- implementations can be found at https://github.com/haskell-perf/dictionaries.
--
--
-- == Warning
--
-- The 'IntMap' type is shared between the lazy and strict modules, meaning that
-- the same 'IntMap' value can be passed to functions in both modules. This
-- means that the 'Functor', 'Traversable' and 'Data.Data.Data' instances are
-- the same as for the "Data.IntMap.Lazy" module, so if they are used the
-- resulting map may contain suspended values (thunks).
--
--
-- == Implementation
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation <http://en.wikipedia.org/wiki/Big_O_notation>.
-- Many operations have a worst-case complexity of /O(min(n,W))/.
-- This means that the operation can become linear in the number of
-- elements with a maximum of /W/ -- the number of bits in an 'Int'
-- (32 or 64).
--
-- Be aware that the 'Functor', 'Traversable' and 'Data' instances
-- are the same as for the "Data.IntMap.Lazy" module, so if they are used
-- on strict maps, the resulting maps will be lazy.
-----------------------------------------------------------------------------

module Data.IntMap.Strict (
    -- * Map type
      IntMap, Key

    -- * Construction
    , empty
    , singleton
    , fromSet

    -- ** From Unordered Lists
    , fromList
    , fromListWith
    , fromListWithKey

    -- ** From Ascending Lists
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromDistinctAscList

    -- * Insertion
    , insert
    , insertWith
    , insertWithKey
    , insertLookupWithKey

    -- * Deletion\/Update
    , delete
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter
    , alterF

    -- * Query
    -- ** Lookup
    , lookup
    , (!?)
    , (!)
    , findWithDefault
    , member
    , notMember
    , lookupLT
    , lookupGT
    , lookupLE
    , lookupGE

    -- ** Size
    , null
    , size

    -- * Combine
    -- ** Union
    , union
    , unionWith
    , unionWithKey
    , unions
    , unionsWith

    -- ** Difference
    , difference
    , (\\)
    , differenceWith
    , differenceWithKey

    -- ** Intersection
    , intersection
    , intersectionWith
    , intersectionWithKey

    -- ** Disjoint
    , disjoint

    -- ** Deprecated, unsafe general combining function
    , mergeWithKey

    -- * Traversal
    -- ** Map
    , map
    , mapWithKey
    , traverseWithKey
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
    , foldMapWithKey

    -- ** Strict folds
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'

    -- * Conversion
    , elems
    , keys
    , assocs
    , keysSet

    -- ** Lists
    , toList

    -- ** Ordered Lists
    , toAscList
    , toDescList

    -- * Filter
    , filter
    , filterWithKey
    , restrictKeys
    , withoutKeys
    , partition
    , partitionWithKey
    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey
    , split
    , splitLookup
    , splitRoot

    -- * Submap
    , isSubmapOf
    , isSubmapOfBy
    , isProperSubmapOf
    , isProperSubmapOfBy

    -- * Min\/Max
    , lookupMin
    , lookupMax
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

#if defined(__GLASGOW_HASKELL__)
    -- * Debugging
    , showTree
    , showTreeWith
#endif
) where

import Data.IntMap.Internal
#if USE_REWRITE_RULES
import qualified Data.IntMap.Lazy as L
#endif
import qualified Data.IntMap.Merge.Internal as Merge (runWhenMissingAll)
import qualified Data.IntMap.Merge.Strict as Merge (merge, mapMissing, mapMaybeMissing, traverseMissing, zipWithMaybeMatched)
#if defined(__GLASGOW_HASKELL__)
import Data.IntMap.Internal.DeprecatedDebug
#endif

#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity (runIdentity)
#else
import Data.IntMap.Merge.Internal (runIdentity)
import Control.Applicative (Applicative(..), (<$>))
#endif

import Utils.Containers.Internal.StrictPair (StrictPair(..), toPair)

import qualified Data.Foldable (Foldable, foldl')
import qualified Data.List (foldl', map)
import qualified Data.IntSet (IntSet, toList)

import Prelude hiding (foldr, foldl, lookup, null, map, filter, min, max)

(#!), (#) :: (a -> b) -> a -> b
(#!) = ($!)
(#) = ($)

wheval :: a -> ()
wheval !_ = ()

-- | /O(1)/. A map of one element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1
singleton :: Key -> a -> IntMap a
singleton !k !v = IntMap (NonEmpty (Bound k) v Tip)

-- | /O(min(n,W))/. Insert a new key\/value pair in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value, i.e. 'insert' is equivalent to
-- @'insertWith' 'const'@
--
-- > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
-- > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
-- > insert 5 'x' empty                         == singleton 5 'x'
insert :: Key -> a -> IntMap a -> IntMap a
insert = insertWithEval wheval const

-- | /O(min(n,W))/. Insert with a combining function.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert @f new_value old_value@.
--
-- > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
-- > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"
insertWith :: (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWith = insertWithEval wheval

-- | /O(min(n,W))/. Insert with a combining function.
-- @'insertWithKey' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert @f key new_value old_value@.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
-- > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"
insertWithKey :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWithKey f k = insertWith (f k) k

-- | /O(min(n,W))/. The expression (@'insertLookupWithKey' f k x map@)
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
insertLookupWithKey :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> (Maybe a, IntMap a)
insertLookupWithKey combine !k v = toPair . start
  where
    start (IntMap Empty) = Nothing :*: IntMap (NonEmpty (Bound k) #! v # Tip)
    start (IntMap (NonEmpty min minV root)) = case compareMinBound k min of
        InBound -> let mv :*: root' = goL (xor k min) min root
                    in mv :*: IntMap (NonEmpty min minV root')
        OutOfBound -> Nothing :*: IntMap (NonEmpty (Bound k) #! v # insertMinL (xor k min) min minV root)
        Matched -> Just minV :*: IntMap (NonEmpty (Bound k) #! combine k v minV # root)

    goL !_        _    Tip = Nothing :*: (Bin (Bound k) #! v # Tip # Tip)
    goL !xorCache min (Bin max maxV l r) = case compareMaxBound k max of
        InBound | xorCache < xorCacheMax -> let mv :*: l' = goL xorCache min l
                                             in mv :*: Bin max maxV l' r
                | otherwise              -> let mv :*: r' = goR xorCacheMax max r
                                             in mv :*: Bin max maxV l r'
        OutOfBound | xor (boundKey max) min < xorCacheMax -> Nothing :*: (Bin (Bound k) #! v # Bin max maxV l r # Tip)
                   | otherwise -> Nothing :*: (Bin (Bound k) #! v # l # insertMaxR xorCacheMax max maxV r)
        Matched -> Just maxV :*: (Bin max #! combine k v maxV # l # r)
      where xorCacheMax = xor k max

    goR !_        _    Tip = Nothing :*: (Bin (Bound k) #! v # Tip # Tip)
    goR !xorCache max (Bin min minV l r) = case compareMinBound k min of
        InBound | xorCache < xorCacheMin -> let mv :*: r' = goR xorCache max r
                                             in mv :*: Bin min minV l r'
                | otherwise              -> let mv :*: l' = goL xorCacheMin min l
                                             in mv :*: Bin min minV l' r
        OutOfBound | xor (boundKey min) max < xorCacheMin -> Nothing :*: (Bin (Bound k) #! v # Tip # Bin min minV l r)
                   | otherwise -> Nothing :*: (Bin (Bound k) #! v # insertMinL xorCacheMin min minV l # r)
        Matched -> Just minV :*: (Bin min #! combine k v minV # l # r)
      where xorCacheMin = xor k min

-- | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty
adjust :: (a -> a) -> Key -> IntMap a -> IntMap a
adjust f !k = start
  where
    start (IntMap Empty) = IntMap Empty
    start m@(IntMap (NonEmpty min minV node)) = case compareMinBound k min of
        InBound -> IntMap (NonEmpty min minV (goL (xor k min) node))
        OutOfBound -> m
        Matched -> IntMap (NonEmpty min #! f minV # node)

    goL !_           Tip = Tip
    goL !xorCache n@(Bin max maxV l r) = case compareMaxBound k max of
        InBound | xorCache < xorCacheMax -> Bin max maxV (goL xorCache l) r
                | otherwise              -> Bin max maxV l (goR xorCacheMax r)
        OutOfBound -> n
        Matched -> Bin max #! f maxV # l # r
      where xorCacheMax = xor k max

    goR !_           Tip = Tip
    goR !xorCache n@(Bin min minV l r) = case compareMinBound k min of
        InBound | xorCache < xorCacheMin -> Bin min minV l (goR xorCache r)
                | otherwise              -> Bin min minV (goL xorCacheMin l) r
        OutOfBound -> n
        Matched -> Bin min #! f minV # l # r
      where xorCacheMin = xor k min

-- | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty
adjustWithKey :: (Key -> a -> a) -> Key -> IntMap a -> IntMap a
adjustWithKey f k = adjust (f k) k

-- | /O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
update :: (a -> Maybe a) -> Key -> IntMap a -> IntMap a
update f !k = start
  where
    start (IntMap Empty) = IntMap Empty
    start m@(IntMap (NonEmpty min minV root)) = case compareMinBound k min of
        InBound -> IntMap (NonEmpty min minV (goL (xor k min) root))
        OutOfBound -> m
        Matched -> case f minV of
            Nothing -> IntMap (nodeToMapL root)
            Just !minV' -> IntMap (NonEmpty min minV' root)

    goL !_           Tip = Tip
    goL !xorCache n@(Bin max maxV l r) = case compareMaxBound k max of
        InBound | xorCache < xorCacheMax -> Bin max maxV (goL xorCache l) r
                | otherwise              -> Bin max maxV l (goR xorCacheMax r)
        OutOfBound -> n
        Matched -> case f maxV of
            Nothing -> extractBinL l r
            Just !maxV' -> Bin max maxV' l r
      where xorCacheMax = xor k max

    goR !_           Tip = Tip
    goR !xorCache n@(Bin min minV l r) = case compareMinBound k min of
        InBound | xorCache < xorCacheMin -> Bin min minV l (goR xorCache r)
                | otherwise              -> Bin min minV (goL xorCacheMin l) r
        OutOfBound -> n
        Matched -> case f minV of
            Nothing -> extractBinR l r
            Just !minV' -> Bin min minV' l r
      where xorCacheMin = xor k min

-- | /O(min(n,W))/. The expression (@'updateWithKey' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f k x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateWithKey :: (Key -> a -> Maybe a) -> Key -> IntMap a -> IntMap a
updateWithKey f k = update (f k) k

-- | /O(min(n,W))/. Lookup and update.
-- The function returns original value, if it is updated.
-- This is different behavior than 'Data.Map.updateLookupWithKey'.
-- Returns the original key value if the map entry is deleted.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:new a")])
-- > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")
updateLookupWithKey :: (Key -> a -> Maybe a) -> Key -> IntMap a -> (Maybe a, IntMap a)
updateLookupWithKey = start
  where
    start _ !_ (IntMap Empty) = (Nothing, IntMap Empty)
    start f !k m@(IntMap (NonEmpty min minV root)) = case compareMinBound k min of
        InBound -> let mv :*: root' = goL f k (xor k min) root
                    in (mv, IntMap (NonEmpty min minV root'))
        OutOfBound -> (Nothing, m)
        Matched -> (Just minV, case f k minV of
            Nothing -> IntMap (nodeToMapL root)
            Just !minV' -> IntMap (NonEmpty min minV' root))

    goL _ !_ !_           Tip = Nothing :*: Tip
    goL f !k !xorCache n@(Bin max maxV l r) = case compareMaxBound k max of
        InBound | xorCache < xorCacheMax -> let mv :*: l' = goL f k xorCache l
                                             in mv :*: Bin max maxV l' r
                | otherwise              -> let mv :*: r' = goR f k xorCacheMax r
                                             in mv :*: Bin max maxV l r'
        OutOfBound -> Nothing :*: n
        Matched -> Just maxV :*: case f k maxV of
            Nothing -> extractBinL l r
            Just !maxV' -> Bin max maxV' l r
      where xorCacheMax = xor k max

    goR _ !_ !_           Tip = Nothing :*: Tip
    goR f !k !xorCache n@(Bin min minV l r) = case compareMinBound k min of
        InBound | xorCache < xorCacheMin -> let mv :*: r' = goR f k xorCache r
                                             in mv :*: Bin min minV l r'
                | otherwise              -> let mv :*: l' = goL f k xorCacheMin l
                                             in mv :*: Bin min minV l' r
        OutOfBound -> Nothing :*: n
        Matched -> Just minV :*: case f k minV of
            Nothing -> extractBinR l r
            Just !minV' -> Bin min minV' l r
      where xorCacheMin = xor k min

-- | /O(min(n,W))/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in an 'IntMap'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: (Maybe a -> Maybe a) -> Key -> IntMap a -> IntMap a
alter f k m = case lookup k m of
    Nothing -> case f Nothing of
        Nothing -> m
        Just v -> insert k v m
    Just v -> case f (Just v) of
        Nothing -> delete k m
        Just v' -> insert k v' m

-- | /O(log n)/. The expression (@'alterF' f k map@) alters the value @x@ at
-- @k@, or absence thereof.  'alterF' can be used to inspect, insert, delete,
-- or update a value in an 'IntMap'.  In short : @'lookup' k '<$>' 'alterF' f k m = f
-- ('lookup' k m)@.
--
-- Example:
--
-- @
-- interactiveAlter :: Int -> IntMap String -> IO (IntMap String)
-- interactiveAlter k m = alterF f k m where
--   f Nothing = do
--      putStrLn $ show k ++
--          " was not found in the map. Would you like to add it?"
--      getUserResponse1 :: IO (Maybe String)
--   f (Just old) = do
--      putStrLn $ "The key is currently bound to " ++ show old ++
--          ". Would you like to change or delete it?"
--      getUserResponse2 :: IO (Maybe String)
-- @
--
-- 'alterF' is the most general operation for working with an individual
-- key that may or may not be in a given map.
--
-- Note: 'alterF' is a flipped version of the 'at' combinator from
-- 'Control.Lens.At'.
--
-- @since 0.5.8
alterF :: Functor f => (Maybe a -> f (Maybe a)) -> Key -> IntMap a -> f (IntMap a)
alterF f k m = case lookup k m of
    Nothing -> fmap (\ret -> case ret of
        Nothing -> m
        Just v -> insert k v m) (f Nothing)
    Just v -> fmap (\ret -> case ret of
        Nothing -> delete k m
        Just v' -> insert k v' m) (f (Just v))

-- | /O(n+m)/. The union with a combining function.
--
-- > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]
unionWith :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWith f = unionWithKey (const f)

-- | /O(n+m)/. The union with a combining function.
--
-- > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]
{-# INLINE unionWithKey #-}
unionWithKey :: (Key -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWithKey combine = unionWithUKey (\k a b -> combine (box k) a b)

-- | /O(n+m)/. The union with a combining function taking an unboxed key.
-- Identical in functionality to 'unionWithKey'.
unionWithUKey :: (UKey -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWithUKey = start
  where
    start _ (IntMap Empty) m2 = m2
    start _ m1 (IntMap Empty) = m1
    start combine (IntMap (NonEmpty min1 minV1 root1)) (IntMap (NonEmpty min2 minV2 root2))
        | min1 < min2 = IntMap (NonEmpty min1 minV1 (goL2 combine minV2 min1 root1 min2 root2))
        | min1 > min2 = IntMap (NonEmpty min2 minV2 (goL1 combine minV1 min1 root1 min2 root2))
        | otherwise = IntMap (NonEmpty min1 #! combine (boundUKey min1) minV1 minV2 # goLFused combine min1 root1 root2) -- we choose min1 arbitrarily, as min1 == min2

    goL1 _       minV1 !min1 Tip !_    Tip = Bin (minToMax min1) minV1 Tip Tip
    goL1 _       minV1 !min1 !n1 !min2 Tip = insertMinL (xor (boundKey min1) min2) min1 minV1 n1
    goL1 _       minV1 !min1 !n1 !min2 n2@(Bin max2 _ _ _) | boundsDisjoint min1 max2 = unionDisjointL minV1 min2 n2 min1 n1
    goL1 combine minV1 !min1 Tip !min2 !n2 = goInsertL1 combine (boundKey min1) minV1 (xor (boundKey min1) min2) min2 n2
    goL1 combine minV1 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
         LT | xor (boundKey min1) min2 < xor (boundKey min1) max2 -> Bin max2 maxV2 (goL1 combine minV1 min1 n1 min2 l2) r2 -- we choose min1 arbitrarily - we just need something from tree 1
            | max1 > max2 -> Bin max1 maxV1 l2 (goR2 combine maxV2 max1 (Bin min1 minV1 l1 r1) max2 r2)
            | max1 < max2 -> Bin max2 maxV2 l2 (goR1 combine maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2)
            | otherwise -> Bin max1 #! combine (boundUKey max1) maxV1 maxV2 # l2 # goRFused combine max1 (Bin min1 minV1 l1 r1) r2 -- we choose max1 arbitrarily, as max1 == max2
         EQ | max1 > max2 -> Bin max1 maxV1 (goL1 combine minV1 min1 l1 min2 l2) (goR2 combine maxV2 max1 r1 max2 r2)
            | max1 < max2 -> Bin max2 maxV2 (goL1 combine minV1 min1 l1 min2 l2) (goR1 combine maxV1 max1 r1 max2 r2)
            | otherwise -> Bin max1 #! combine (boundUKey max1) maxV1 maxV2 # goL1 combine minV1 min1 l1 min2 l2 # goRFused combine max1 r1 r2 -- we choose max1 arbitrarily, as max1 == max2
         GT -> Bin max1 maxV1 (goL1 combine minV1 min1 l1 min2 n2) r1

    goL2 _       minV2 !_    Tip !min2 Tip = Bin (minToMax min2) minV2 Tip Tip
    goL2 _       minV2 !min1 Tip !min2 !n2 = insertMinL (xor (boundKey min2) min1) min2 minV2 n2
    goL2 _       minV2 !min1 n1@(Bin max1 _ _ _) !min2 !n2 | boundsDisjoint min2 max1 = unionDisjointL minV2 min1 n1 min2 n2
    goL2 combine minV2 !min1 !n1 !min2 Tip = goInsertL2 combine (boundKey min2) minV2 (xor (boundKey min2) min1) min1 n1
    goL2 combine minV2 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
         GT | xor (boundKey min2) min1 < xor (boundKey min2) max1 -> Bin max1 maxV1 (goL2 combine minV2 min1 l1 min2 n2) r1 -- we choose min2 arbitrarily - we just need something from tree 2
            | max1 > max2 -> Bin max1 maxV1 l1 (goR2 combine maxV2 max1 r1 max2 (Bin min2 minV2 l2 r2))
            | max1 < max2 -> Bin max2 maxV2 l1 (goR1 combine maxV1 max1 r1 max2 (Bin min2 minV2 l2 r2))
            | otherwise -> Bin max1 #! combine (boundUKey max1) maxV1 maxV2 # l1 # goRFused combine max1 r1 (Bin min2 minV2 l2 r2) -- we choose max1 arbitrarily, as max1 == max2
         EQ | max1 > max2 -> Bin max1 maxV1 (goL2 combine minV2 min1 l1 min2 l2) (goR2 combine maxV2 max1 r1 max2 r2)
            | max1 < max2 -> Bin max2 maxV2 (goL2 combine minV2 min1 l1 min2 l2) (goR1 combine maxV1 max1 r1 max2 r2)
            | otherwise -> Bin max1 #! combine (boundUKey max1) maxV1 maxV2 # goL2 combine minV2 min1 l1 min2 l2 # goRFused combine max1 r1 r2 -- we choose max1 arbitrarily, as max1 == max2
         LT -> Bin max2 maxV2 (goL2 combine minV2 min1 n1 min2 l2) r2

    -- 'goLFused' is called instead of 'goL' if the minimums of the two trees are the same
    -- Note that because of this property, the trees cannot be disjoint, so we can skip most of the checks in 'goL'
    goLFused _ !_ Tip n2 = n2
    goLFused _ !_ n1 Tip = n1
    goLFused combine min n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min max1) (xorBounds min max2) of
        LT -> Bin max2 maxV2 (goLFused combine min n1 l2) r2
        EQ | max1 > max2 -> Bin max1 maxV1 (goLFused combine min l1 l2) (goR2 combine maxV2 max1 r1 max2 r2)
           | max1 < max2 -> Bin max2 maxV2 (goLFused combine min l1 l2) (goR1 combine maxV1 max1 r1 max2 r2)
           | otherwise -> Bin max1 #! combine (boundUKey max1) maxV1 maxV2 # goLFused combine min l1 l2 # goRFused combine max1 r1 r2 -- we choose max1 arbitrarily, as max1 == max2
        GT -> Bin max1 maxV1 (goLFused combine min l1 n2) r1

    goR1 _       maxV1 !max1 Tip !_    Tip = Bin (maxToMin max1) maxV1 Tip Tip
    goR1 _       maxV1 !max1 !n1 !max2 Tip = insertMaxR (xor (boundKey max1) max2) max1 maxV1 n1
    goR1 _       maxV1 !max1 !n1 !max2 n2@(Bin min2 _ _ _) | boundsDisjoint min2 max1 = unionDisjointR maxV1 max1 n1 max2 n2
    goR1 combine maxV1 !max1 Tip !max2 !n2 = goInsertR1 combine (boundKey max1) maxV1 (xor (boundKey max1) max2) max2 n2
    goR1 combine maxV1 !max1 n1@(Bin min1 minV1 l1 r1) !max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
         LT | xor (boundKey max1) min2 > xor (boundKey max1) max2 -> Bin min2 minV2 l2 (goR1 combine maxV1 max1 n1 max2 r2) -- we choose max1 arbitrarily - we just need something from tree 1
            | min1 < min2 -> Bin min1 minV1 (goL2 combine minV2 min1 (Bin max1 maxV1 l1 r1) min2 l2) r2
            | min1 > min2 -> Bin min2 minV2 (goL1 combine minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2) r2
            | otherwise -> Bin min1 #! combine (boundUKey min1) minV1 minV2 # goLFused combine min1 (Bin max1 maxV1 l1 r1) l2 # r2 -- we choose min1 arbitrarily, as min1 == min2
         EQ | min1 < min2 -> Bin min1 minV1 (goL2 combine minV2 min1 l1 min2 l2) (goR1 combine maxV1 max1 r1 max2 r2)
            | min1 > min2 -> Bin min2 minV2 (goL1 combine minV1 min1 l1 min2 l2) (goR1 combine maxV1 max1 r1 max2 r2)
            | otherwise -> Bin min1 #! combine (boundUKey min1) minV1 minV2 # goLFused combine min1 l1 l2 # goR1 combine maxV1 max1 r1 max2 r2 -- we choose min1 arbitrarily, as min1 == min2
         GT -> Bin min1 minV1 l1 (goR1 combine maxV1 max1 r1 max2 n2)

    goR2 _       maxV2 !_    Tip !max2 Tip = Bin (maxToMin max2) maxV2 Tip Tip
    goR2 _       maxV2 !max1 Tip !max2 !n2 = insertMaxR (xor (boundKey max2) max1) max2 maxV2 n2
    goR2 _       maxV2 !max1 n1@(Bin min1 _ _ _) !max2 !n2 | boundsDisjoint min1 max2 = unionDisjointR maxV2 max2 n2 max1 n1
    goR2 combine maxV2 !max1 !n1 !max2 Tip = goInsertR2 combine (boundKey max2) maxV2 (xor (boundKey max2) max1) max1 n1
    goR2 combine maxV2 !max1 n1@(Bin min1 minV1 l1 r1) !max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
         GT | xor (boundKey max2) min1 > xor (boundKey max2) max1 -> Bin min1 minV1 l1 (goR2 combine maxV2 max1 r1 max2 n2) -- we choose max2 arbitrarily - we just need something from tree 2
            | min1 < min2 -> Bin min1 minV1 (goL2 combine minV2 min1 l1 min2 (Bin max2 maxV2 l2 r2)) r1
            | min1 > min2 -> Bin min2 minV2 (goL1 combine minV1 min1 l1 min2 (Bin max2 maxV2 l2 r2)) r1
            | otherwise -> Bin min1 #! combine (boundUKey min1) minV1 minV2 # goLFused combine min1 l1 (Bin max2 maxV2 l2 r2) # r1 -- we choose min1 arbitrarily, as min1 == min2
         EQ | min1 < min2 -> Bin min1 minV1 (goL2 combine minV2 min1 l1 min2 l2) (goR2 combine maxV2 max1 r1 max2 r2)
            | min1 > min2 -> Bin min2 minV2 (goL1 combine minV1 min1 l1 min2 l2) (goR2 combine maxV2 max1 r1 max2 r2)
            | otherwise -> Bin min1 #! combine (boundUKey min1) minV1 minV2 # goLFused combine min1 l1 l2 # goR2 combine maxV2 max1 r1 max2 r2 -- we choose min1 arbitrarily, as min1 == min2
         LT -> Bin min2 minV2 l2 (goR2 combine maxV2 max1 n1 max2 r2)

    -- 'goRFused' is called instead of 'goR' if the minimums of the two trees are the same
    -- Note that because of this property, the trees cannot be disjoint, so we can skip most of the checks in 'goR'
    goRFused _ !_ Tip n2 = n2
    goRFused _ !_ n1 Tip = n1
    goRFused combine max n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max) (xorBounds min2 max) of
        LT -> Bin min2 minV2 l2 (goRFused combine max n1 r2)
        EQ | min1 < min2 -> Bin min1 minV1 (goL2 combine minV2 min1 l1 min2 l2) (goRFused combine max r1 r2)
           | min1 > min2 -> Bin min2 minV2 (goL1 combine minV1 min1 l1 min2 l2) (goRFused combine max r1 r2)
           | otherwise -> Bin min1 #! combine (boundUKey min1) minV1 minV2 # goLFused combine min1 l1 l2 # goRFused combine max r1 r2 -- we choose min1 arbitrarily, as min1 == min2
        GT -> Bin min1 minV1 l1 (goRFused combine max r1 n2)

    goInsertL1 _       k v !_        _    Tip = Bin (Bound k) #! v # Tip # Tip
    goInsertL1 combine k v !xorCache min (Bin max maxV l r) = case compareMaxBound k max of
        InBound | xorCache < xorCacheMax -> Bin max maxV (goInsertL1 combine k v xorCache min l) r
                | otherwise              -> Bin max maxV l (goInsertR1 combine k v xorCacheMax max r)
        OutOfBound | xor (boundKey max) min < xorCacheMax -> Bin (Bound k) v (Bin max maxV l r) Tip
                   | otherwise -> Bin (Bound k) v l (insertMaxR xorCacheMax max maxV r)
        Matched -> Bin max #! combine (unbox k) v maxV # l # r
      where xorCacheMax = xor k max

    goInsertR1 _       k v !_        _    Tip = Bin (Bound k) v Tip Tip
    goInsertR1 combine k v !xorCache max (Bin min minV l r) = case compareMinBound k min of
        InBound | xorCache < xorCacheMin -> Bin min minV l (goInsertR1 combine k v xorCache max r)
                | otherwise              -> Bin min minV (goInsertL1 combine k v xorCacheMin min l) r
        OutOfBound | xor (boundKey min) max < xorCacheMin -> Bin (Bound k) v Tip (Bin min minV l r)
                   | otherwise -> Bin (Bound k) v (insertMinL xorCacheMin min minV l) r
        Matched -> Bin min #! combine (unbox k) v minV # l # r
      where xorCacheMin = xor k min

    goInsertL2 _       k v !_        _    Tip = Bin (Bound k) v Tip Tip
    goInsertL2 combine k v !xorCache min (Bin max maxV l r) = case compareMaxBound k max of
        InBound | xorCache < xorCacheMax -> Bin max maxV (goInsertL2 combine k v xorCache min l) r
                | otherwise              -> Bin max maxV l (goInsertR2 combine k v xorCacheMax max r)
        OutOfBound | xor (boundKey max) min < xorCacheMax -> Bin (Bound k) v (Bin max maxV l r) Tip
                   | otherwise -> Bin (Bound k) v l (insertMaxR xorCacheMax max maxV r)
        Matched -> Bin max #! combine (unbox k) maxV v # l # r
      where xorCacheMax = xor k max

    goInsertR2 _       k v !_        _    Tip = Bin (Bound k) v Tip Tip
    goInsertR2 combine k v !xorCache max (Bin min minV l r) = case compareMinBound k min of
        InBound | xorCache < xorCacheMin -> Bin min minV l (goInsertR2 combine k v xorCache max r)
                | otherwise              -> Bin min minV (goInsertL2 combine k v xorCacheMin min l) r
        OutOfBound | xor (boundKey min) max < xorCacheMin -> Bin (Bound k) v Tip (Bin min minV l r)
                   | otherwise -> Bin (Bound k) v (insertMinL xorCacheMin min minV l) r
        Matched -> Bin min #! combine (unbox k) minV v # l # r
      where xorCacheMin = xor k min

-- | The union of a list of maps, with a combining operation.
--
-- > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]
unionsWith :: Data.Foldable.Foldable f => (a -> a -> a) -> f (IntMap a) -> IntMap a
unionsWith f = Data.Foldable.foldl' (unionWith f) empty

-- | /O(n+m)/. Difference with a combining function.
--
-- > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- > differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
-- >     == singleton 3 "b:B"
differenceWith :: (a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
differenceWith f = differenceWithKey (const f)

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference).
-- If it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- > differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
-- >     == singleton 3 "3:b|B"
{-# INLINE differenceWithKey #-}
differenceWithKey :: (Key -> a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
differenceWithKey combine = differenceWithUKey (\k a b -> combine (box k) a b)

-- | /O(n+m)/. The difference with a combining function taking an unboxed
-- key. Identical in functionality to 'differenceWithKey'.
differenceWithUKey :: (UKey -> a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
differenceWithUKey = start
  where
    start _ (IntMap Empty) !_ = IntMap Empty
    start _ !m (IntMap Empty) = m
    start combine (IntMap (NonEmpty min1 minV1 root1)) (IntMap (NonEmpty min2 minV2 root2))
        | min1 < min2 = IntMap (NonEmpty min1 minV1 (goL2 combine minV2 min1 root1 min2 root2))
        | min1 > min2 = IntMap (goL1 combine minV1 min1 root1 min2 root2)
        | otherwise = case combine (boundUKey min1) minV1 minV2 of
            Nothing -> IntMap (goLFused combine min1 root1 root2)
            Just !minV1' -> IntMap (NonEmpty min1 minV1' (goLFusedKeep combine min1 root1 root2))

    goL1 combine minV1 !min1 Tip !min2 !n2 = goLookupL combine (boundKey min1) minV1 (xor (boundKey min1) min2) n2
    goL1 _       minV1 !min1 !n1 !_    Tip = NonEmpty min1 minV1 n1
    goL1 _       minV1 !min1 n1@(Bin _ _ _ _) !_ (Bin max2 _ _ _) | boundsDisjoint min1 max2 = NonEmpty min1 minV1 n1
    goL1 combine minV1 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT | xor (boundKey min1) min2 < xor (boundKey min1) max2 -> goL1 combine minV1 min1 n1 min2 l2 -- min1 is arbitrary here - we just need something from tree 1
           | max1 > max2 -> r2lMap $ NonEmpty max1 maxV1 (goR2 combine maxV2 max1 (Bin min1 minV1 l1 r1) max2 r2)
           | max1 < max2 -> r2lMap $ goR1 combine maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2
           | otherwise -> case combine (boundUKey max1) maxV1 maxV2 of
                Nothing -> r2lMap $ goRFused combine max1 (Bin min1 minV1 l1 r1) r2
                Just !maxV1' -> r2lMap $ NonEmpty max1 maxV1' (goRFusedKeep combine max1 (Bin min1 minV1 l1 r1) r2)
        EQ | max1 > max2 -> binL (goL1 combine minV1 min1 l1 min2 l2) (NonEmpty max1 maxV1 (goR2 combine maxV2 max1 r1 max2 r2))
           | max1 < max2 -> binL (goL1 combine minV1 min1 l1 min2 l2) (goR1 combine maxV1 max1 r1 max2 r2)
           | otherwise -> case combine (boundUKey max1) maxV1 maxV2 of
                Nothing -> binL (goL1 combine minV1 min1 l1 min2 l2) (goRFused combine max1 r1 r2)
                Just !maxV1' -> binL (goL1 combine minV1 min1 l1 min2 l2) (NonEmpty max1 maxV1' (goRFusedKeep combine max1 r1 r2))
        GT -> binL (goL1 combine minV1 min1 l1 min2 n2) (NonEmpty max1 maxV1 r1)

    goL2 _       _     !_    Tip !_   !_  = Tip
    goL2 combine minV2 !min1 !n1 !min2 Tip = goDeleteL combine (boundKey min2) minV2 (xor (boundKey min2) min1) n1
    goL2 _       _     !_    n1@(Bin max1 _ _ _) !min2 (Bin _ _ _ _) | boundsDisjoint min2 max1 = n1
    goL2 combine minV2 !min1 n1@(Bin max1 maxV1 l1 r1) !min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT -> goL2 combine minV2 min1 n1 min2 l2
        EQ | max1 > max2 -> Bin max1 maxV1 (goL2 combine minV2 min1 l1 min2 l2) (goR2 combine maxV2 max1 r1 max2 r2)
           | max1 < max2 -> case goR1 combine maxV1 max1 r1 max2 r2 of
                Empty -> goL2 combine minV2 min1 l1 min2 l2
                NonEmpty max' maxV' r' -> Bin max' maxV' (goL2 combine minV2 min1 l1 min2 l2) r'
           | otherwise -> case combine (boundUKey max1) maxV1 maxV2 of
                Nothing -> case goRFused combine max1 r1 r2 of
                    Empty -> goL2 combine minV2 min1 l1 min2 l2
                    NonEmpty max' maxV' r' -> Bin max' maxV' (goL2 combine minV2 min1 l1 min2 l2) r'
                Just !maxV1' -> Bin max1 maxV1' (goL2 combine minV2 min1 l1 min2 l2) (goRFusedKeep combine max1 r1 r2)
        GT | xor (boundKey min2) min1 < xor (boundKey min2) max1 -> Bin max1 maxV1 (goL2 combine minV2 min1 l1 min2 n2) r1 -- min2 is arbitrary here - we just need something from tree 2
           | max1 > max2 -> Bin max1 maxV1 l1 (goR2 combine maxV2 max1 r1 max2 (Bin min2 minV2 l2 r2))
           | max1 < max2 -> case goR1 combine maxV1 max1 r1 max2 (Bin min2 minV2 l2 r2) of
                Empty -> l1
                NonEmpty max' maxV' r' -> Bin max' maxV' l1 r'
           | otherwise -> case combine (boundUKey max1) maxV1 maxV2 of
                Nothing -> case goRFused combine max1 r1 (Bin min2 minV2 l2 r2) of
                    Empty -> l1
                    NonEmpty max' maxV' r' -> Bin max' maxV' l1 r'
                Just !maxV1' -> Bin max1 maxV1' l1 (goRFusedKeep combine max1 r1 (Bin min2 minV2 l2 r2))

    goLFused _ !_ Tip !_ = Empty
    goLFused _ !_ (Bin max1 maxV1 l1 r1) Tip = case deleteMinL max1 maxV1 l1 r1 of
        NE min' minV' n' -> NonEmpty min' minV' n'
    goLFused combine !min n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min max1) (xorBounds min max2) of
        LT -> goLFused combine min n1 l2
        EQ | max1 > max2 -> binL (goLFused combine min l1 l2) (NonEmpty max1 maxV1 (goR2 combine maxV2 max1 r1 max2 r2))
           | max1 < max2 -> binL (goLFused combine min l1 l2) (goR1 combine maxV1 max1 r1 max2 r2)
           | otherwise -> case combine (boundUKey max1) maxV1 maxV2 of
                Nothing -> binL (goLFused combine min l1 l2) (goRFused combine max1 r1 r2) -- we choose max1 arbitrarily, as max1 == max2
                Just !maxV1' -> binL (goLFused combine min l1 l2) (NonEmpty max1 maxV1' (goRFusedKeep combine max1 r1 r2))
        GT -> binL (goLFused combine min l1 n2) (NonEmpty max1 maxV1 r1)

    goLFusedKeep _ !_ !n1 Tip = n1
    goLFusedKeep _ !_ Tip !_ = Tip
    goLFusedKeep combine !min n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min max1) (xorBounds min max2) of
        LT -> goLFusedKeep combine min n1 l2
        EQ | max1 > max2 -> Bin max1 maxV1 (goLFusedKeep combine min l1 l2) (goR2 combine maxV2 max1 r1 max2 r2)
           | max1 < max2 -> case goR1 combine maxV1 max1 r1 max2 r2 of
                Empty -> goLFusedKeep combine min l1 l2
                NonEmpty max' maxV' r' -> Bin max' maxV' (goLFusedKeep combine min l1 l2) r'
           | otherwise -> case combine (boundUKey max1) maxV1 maxV2 of
                Nothing -> case goRFused combine max1 r1 r2 of -- we choose max1 arbitrarily, as max1 == max2
                    Empty -> goLFusedKeep combine min l1 l2
                    NonEmpty max' maxV' r' -> Bin max' maxV' (goLFusedKeep combine min l1 l2) r'
                Just !maxV1' -> Bin max1 maxV1' (goLFusedKeep combine min l1 l2) (goRFusedKeep combine max1 r1 r2)
        GT -> Bin max1 maxV1 (goLFusedKeep combine min l1 n2) r1

    goR1 combine maxV1 !max1 Tip !max2 !n2 = goLookupR combine (boundKey max1) maxV1 (xor (boundKey max1) max2) n2
    goR1 _       maxV1 !max1 !n1 !_    Tip = NonEmpty max1 maxV1 n1
    goR1 _       maxV1 !max1 n1@(Bin _ _ _ _) !_ (Bin min2 _ _ _) | boundsDisjoint min2 max1 = NonEmpty max1 maxV1 n1
    goR1 combine maxV1 !max1 n1@(Bin min1 minV1 l1 r1) !max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT | xor (boundKey max1) min2 > xor (boundKey max1) max2 -> goR1 combine maxV1 max1 n1 max2 r2 -- max1 is arbitrary here - we just need something from tree 1
           | min1 < min2 -> l2rMap $ NonEmpty min1 minV1 (goL2 combine minV2 min1 (Bin max1 maxV1 l1 r1) min2 l2)
           | min1 > min2 -> l2rMap $ goL1 combine minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2
           | otherwise -> case combine (boundUKey min1) minV1 minV2 of
                Nothing -> l2rMap $ goLFused combine min1 (Bin max1 maxV1 l1 r1) l2
                Just !minV1' -> l2rMap $ NonEmpty min1 minV1' (goLFusedKeep combine min1 (Bin max1 maxV1 l1 r1) l2)
        EQ | min1 < min2 -> binR (NonEmpty min1 minV1 (goL2 combine minV2 min1 l1 min2 l2)) (goR1 combine maxV1 max1 r1 max2 r2)
           | min1 > min2 -> binR (goL1 combine minV1 min1 l1 min2 l2) (goR1 combine maxV1 max1 r1 max2 r2)
           | otherwise -> case combine (boundUKey min1) minV1 minV2 of
                Nothing -> binR (goLFused combine min1 l1 l2) (goR1 combine maxV1 max1 r1 max2 r2)
                Just !minV1' -> binR (NonEmpty min1 minV1' (goLFusedKeep combine min1 l1 l2)) (goR1 combine maxV1 max1 r1 max2 r2)
        GT -> binR (NonEmpty min1 minV1 l1) (goR1 combine maxV1 max1 r1 max2 n2)

    goR2 _       _     !_    Tip !_    !_  = Tip
    goR2 combine maxV2 !max1 !n1 !max2 Tip = goDeleteR combine (boundKey max2) maxV2 (xor (boundKey max2) max1) n1
    goR2 _       _     !_    n1@(Bin min1 _ _ _)       !max2 (Bin _ _ _ _) | boundsDisjoint min1 max2 = n1
    goR2 combine maxV2 !max1 n1@(Bin min1 minV1 l1 r1) !max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT -> goR2 combine maxV2 max1 n1 max2 r2
        EQ | min1 < min2 -> Bin min1 minV1 (goL2 combine minV2 min1 l1 min2 l2) (goR2 combine maxV2 max1 r1 max2 r2)
           | min1 > min2 -> case goL1 combine minV1 min1 l1 min2 l2 of
                Empty -> goR2 combine maxV2 max1 r1 max2 r2
                NonEmpty min' minV' l' -> Bin min' minV' l' (goR2 combine maxV2 max1 r1 max2 r2)
           | otherwise -> case combine (boundUKey min1) minV1 minV2 of
                Nothing -> case goLFused combine min1 l1 l2 of
                    Empty -> goR2 combine maxV2 max1 r1 max2 r2
                    NonEmpty min' minV' l' -> Bin min' minV' l' (goR2 combine maxV2 max1 r1 max2 r2)
                Just !minV1' -> Bin min1 minV1' (goLFusedKeep combine min1 l1 l2) (goR2 combine maxV2 max1 r1 max2 r2)
        GT | xor (boundKey max2) min1 > xor (boundKey max2) max1 -> Bin min1 minV1 l1 (goR2 combine maxV2 max1 r1 max2 n2) -- max2 is arbitrary here - we just need something from tree 2
           | min1 < min2 -> Bin min1 minV1 (goL2 combine minV2 min1 l1 min2 (Bin max2 maxV2 l2 r2)) r1
           | min1 > min2 -> case goL1 combine minV1 min1 l1 min2 (Bin max2 maxV2 l2 r2) of
                Empty -> r1
                NonEmpty min' minV' l' -> Bin min' minV' l' r1
           | otherwise -> case combine (boundUKey min1) minV1 minV2 of
                Nothing -> case goLFused combine min1 l1 (Bin max2 maxV2 l2 r2) of
                    Empty -> r1
                    NonEmpty min' minV' l' -> Bin min' minV' l' r1
                Just !minV1' -> Bin min1 minV1' (goLFusedKeep combine min1 l1 (Bin max2 maxV2 l2 r2)) r1

    goRFused _ !_ Tip !_ = Empty
    goRFused _ !_ (Bin min1 minV1 l1 r1) Tip = case deleteMaxR min1 minV1 l1 r1 of
        NE max' maxV' n' -> NonEmpty max' maxV' n'
    goRFused combine !max n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max) (xorBounds min2 max) of
        LT -> goRFused combine max n1 r2
        EQ | min1 < min2 -> binR (NonEmpty min1 minV1 (goL2 combine minV2 min1 l1 min2 l2)) (goRFused combine max r1 r2)
           | min1 > min2 -> binR (goL1 combine minV1 min1 l1 min2 l2) (goRFused combine max r1 r2)
           | otherwise -> case combine (boundUKey min1) minV1 minV2 of
                Nothing -> binR (goLFused combine min1 l1 l2) (goRFused combine max r1 r2) -- we choose min1 arbitrarily, as min1 == min2
                Just !minV1' -> binR (NonEmpty min1 minV1' (goLFusedKeep combine min1 l1 l2)) (goRFused combine max r1 r2)
        GT -> binR (NonEmpty min1 minV1 l1) (goRFused combine max r1 n2)

    goRFusedKeep _ !_ !n1 Tip = n1
    goRFusedKeep _ !_ Tip !_ = Tip
    goRFusedKeep combine !max n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max) (xorBounds min2 max) of
        LT -> goRFusedKeep combine max n1 r2
        EQ | min1 < min2 -> Bin min1 minV1 (goL2 combine minV2 min1 l1 min2 l2) (goRFusedKeep combine max r1 r2)
           | min1 > min2 -> case goL1 combine minV1 min1 l1 min2 l2 of
                Empty -> goRFusedKeep combine max r1 r2
                NonEmpty min' minV' l' -> Bin min' minV' l' (goRFusedKeep combine max r1 r2)
           | otherwise -> case combine (boundUKey min1) minV1 minV2 of -- we choose min1 arbitrarily, as min1 == min2
                Nothing -> case goLFused combine min1 l1 l2 of
                    Empty -> goRFusedKeep combine max r1 r2
                    NonEmpty min' minV' l' -> Bin min' minV' l' (goRFusedKeep combine max r1 r2)
                Just !minV1' -> Bin min1 minV1' (goLFusedKeep combine min1 l1 l2) (goRFusedKeep combine max r1 r2)
        GT -> Bin min1 minV1 l1 (goRFusedKeep combine max r1 n2)

    goLookupL _       !k v !_ Tip = NonEmpty (Bound k) v Tip
    goLookupL combine !k v !xorCache (Bin max maxV l r) = case compareMaxBound k max of
        InBound | xorCache < xorCacheMax -> goLookupL combine k v xorCache l
                | otherwise              -> goLookupR combine k v xorCacheMax r
        OutOfBound -> NonEmpty (Bound k) v Tip
        Matched -> case combine (unbox k) v maxV of
            Nothing -> Empty
            Just !v' -> NonEmpty (Bound k) v' Tip
      where xorCacheMax = xor k max

    goLookupR _       !k v !_ Tip = NonEmpty (Bound k) v Tip
    goLookupR combine !k v !xorCache (Bin min minV l r) = case compareMinBound k min of
        InBound | xorCache < xorCacheMin -> goLookupR combine k v xorCache r
                | otherwise              -> goLookupL combine k v xorCacheMin l
        OutOfBound -> NonEmpty (Bound k) v Tip
        Matched -> case combine (unbox k) v minV of
            Nothing -> Empty
            Just !v' -> NonEmpty (Bound k) v' Tip
      where xorCacheMin = xor k min

    goDeleteL _ !_ _ !_ Tip = Tip
    goDeleteL combine !k v !xorCache n@(Bin max maxV l r) = case compareMaxBound k max of
        InBound | xorCache < xorCacheMax -> Bin max maxV (goDeleteL combine k v xorCache l) r
                | otherwise              -> Bin max maxV l (goDeleteR combine k v xorCacheMax r)
        OutOfBound -> n
        Matched -> case combine (unbox k) maxV v of
            Nothing -> extractBinL l r
            Just !maxV' -> Bin max maxV' l r
      where xorCacheMax = xor k max

    goDeleteR _ !_ _ !_ Tip = Tip
    goDeleteR combine !k v !xorCache n@(Bin min minV l r) = case compareMinBound k min of
        InBound | xorCache < xorCacheMin -> Bin min minV l (goDeleteR combine k v xorCache r)
                | otherwise              -> Bin min minV (goDeleteL combine k v xorCacheMin l) r
        OutOfBound -> n
        Matched -> case combine (unbox k) minV v of
            Nothing -> extractBinR l r
            Just !minV' -> Bin min minV' l r
      where xorCacheMin = xor k min

-- | /O(n+m)/. The intersection with a combining function.
--
-- > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"
intersectionWith :: (a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWith f = intersectionWithKey (const f)

-- | /O(n+m)/. The intersection with a combining function.
--
-- > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- > intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "5:a|A"
{-# INLINE intersectionWithKey #-}
intersectionWithKey :: (Key -> a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWithKey combine = intersectionWithUKey (\k a b -> combine (box k) a b)

-- | /O(n+m)/. The intersection with a combining function taking an unboxed
-- key. Identical in functionality to 'intersectionWithKey'.
intersectionWithUKey :: (UKey -> a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWithUKey = start
  where
    start _ (IntMap Empty) !_ = IntMap Empty
    start _ !_ (IntMap Empty) = IntMap Empty
    start combine (IntMap (NonEmpty min1 minV1 root1)) (IntMap (NonEmpty min2 minV2 root2))
        | min1 < min2 = IntMap (goL2 combine minV2 min1 root1 min2 root2)
        | min1 > min2 = IntMap (goL1 combine minV1 min1 root1 min2 root2)
        | otherwise = IntMap (NonEmpty min1 #! combine (boundUKey min1) minV1 minV2 # goLFused combine min1 root1 root2) -- we choose min1 arbitrarily, as min1 == min2

    -- TODO: This scheme might produce lots of unnecessary l2r and r2l calls. This should be rectified.

    goL1 _       _     !_   !_  !_   Tip = Empty
    goL1 combine minV1 min1 Tip min2 n2  = goLookupL1 combine (boundKey min1) minV1 (xor (boundKey min1) min2) n2
    goL1 _       _ min1 (Bin _ _ _ _) _ (Bin max2 _ _ _) | boundsDisjoint min1 max2 = Empty
    goL1 combine minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT | xor (boundKey min1) min2 < xor (boundKey min1) max2 -> goL1 combine minV1 min1 n1 min2 l2 -- min1 is arbitrary here - we just need something from tree 1
           | max1 > max2 -> r2lMap $ goR2 combine maxV2 max1 (Bin min1 minV1 l1 r1) max2 r2
           | max1 < max2 -> r2lMap $ goR1 combine maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2
           | otherwise -> r2lMap $ NonEmpty max1 #! combine (boundUKey max1) maxV1 maxV2 # goRFused combine max1 (Bin min1 minV1 l1 r1) r2
        EQ | max1 > max2 -> binL (goL1 combine minV1 min1 l1 min2 l2) (goR2 combine maxV2 max1 r1 max2 r2)
           | max1 < max2 -> binL (goL1 combine minV1 min1 l1 min2 l2) (goR1 combine maxV1 max1 r1 max2 r2)
           | otherwise -> case goL1 combine minV1 min1 l1 min2 l2 of
                Empty -> r2lMap (NonEmpty max1 #! combine (boundUKey max1) maxV1 maxV2 # goRFused combine max1 r1 r2)
                NonEmpty min' minV' l' -> NonEmpty min' minV' (Bin max1 #! combine (boundUKey max1) maxV1 maxV2 # l' # goRFused combine max1 r1 r2)
        GT -> goL1 combine minV1 min1 l1 min2 n2

    goL2 _       _     !_   Tip !_   !_  = Empty
    goL2 combine minV2 min1 n1  min2 Tip = goLookupL2 combine (boundKey min2) minV2 (xor (boundKey min2) min1) n1
    goL2 _       _ _ (Bin max1 _ _ _) min2 (Bin _ _ _ _) | boundsDisjoint min2 max1 = Empty
    goL2 combine minV2 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT -> goL2 combine minV2 min1 n1 min2 l2
        EQ | max1 > max2 -> binL (goL2 combine minV2 min1 l1 min2 l2) (goR2 combine maxV2 max1 r1 max2 r2)
           | max1 < max2 -> binL (goL2 combine minV2 min1 l1 min2 l2) (goR1 combine maxV1 max1 r1 max2 r2)
           | otherwise -> case goL2 combine minV2 min1 l1 min2 l2 of
                Empty -> r2lMap (NonEmpty max1 #! combine (boundUKey max1) maxV1 maxV2 # goRFused combine max1 r1 r2)
                NonEmpty min' minV' l' -> NonEmpty min' minV' (Bin max1 #! combine (boundUKey max1) maxV1 maxV2 # l' # goRFused combine max1 r1 r2)
        GT | xor (boundKey min2) min1 < xor (boundKey min2) max1 -> goL2 combine minV2 min1 l1 min2 n2 -- min2 is arbitrary here - we just need something from tree 2
           | max1 > max2 -> r2lMap $ goR2 combine maxV2 max1 r1 max2 (Bin min2 minV2 l2 r2)
           | max1 < max2 -> r2lMap $ goR1 combine maxV1 max1 r1 max2 (Bin min2 minV2 l2 r2)
           | otherwise -> r2lMap $ NonEmpty max1 #! combine (boundUKey max1) maxV1 maxV2 # goRFused combine max1 r1 (Bin min2 minV2 l2 r2)

    goLFused _ !_ Tip !_ = Tip
    goLFused _ !_ !_ Tip = Tip
    goLFused combine !min n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xorBounds min max1) (xorBounds min max2) of
        LT -> goLFused combine min n1 l2
        EQ | max1 > max2 -> case goR2 combine maxV2 max1 r1 max2 r2 of
                Empty -> goLFused combine min l1 l2
                NonEmpty max' maxV' r' -> Bin max' maxV' (goLFused combine min l1 l2) r'
           | max1 < max2 -> case goR1 combine maxV1 max1 r1 max2 r2 of
                Empty -> goLFused combine min l1 l2
                NonEmpty max' maxV' r' -> Bin max' maxV' (goLFused combine min l1 l2) r'
           | otherwise -> Bin max1 #! combine (boundUKey max1) maxV1 maxV2 # goLFused combine min l1 l2 # goRFused combine max1 r1 r2 -- we choose max1 arbitrarily, as max1 == max2
        GT -> goLFused combine min l1 n2

    goR1 _       _     !_   !_  !_   Tip = Empty
    goR1 combine maxV1 max1 Tip max2 n2  = goLookupR1 combine (boundKey max1) maxV1 (xor (boundKey max1) max2) n2
    goR1 _       _ max1 (Bin _ _ _ _) _ (Bin min2 _ _ _) | boundsDisjoint min2 max1 = Empty
    goR1 combine maxV1 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT | xor (boundKey max1) min2 > xor (boundKey max1) max2 -> goR1 combine maxV1 max1 n1 max2 r2 -- max1 is arbitrary here - we just need something from tree 1
           | min1 < min2 -> l2rMap $ goL2 combine minV2 min1 (Bin max1 maxV1 l1 r1) min2 l2
           | min1 > min2 -> l2rMap $ goL1 combine minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2
           | otherwise -> l2rMap $ NonEmpty min1 #! combine (boundUKey min1) minV1 minV2 # goLFused combine min1 (Bin max1 maxV1 l1 r1) l2
        EQ | min1 < min2 -> binR (goL2 combine minV2 min1 l1 min2 l2) (goR1 combine maxV1 max1 r1 max2 r2)
           | min1 > min2 -> binR (goL1 combine minV1 min1 l1 min2 l2) (goR1 combine maxV1 max1 r1 max2 r2)
           | otherwise -> case goR1 combine maxV1 max1 r1 max2 r2 of
                Empty -> l2rMap (NonEmpty min1 #! combine (boundUKey min1) minV1 minV2 # goLFused combine min1 l1 l2)
                NonEmpty max' maxV' r' -> NonEmpty max' maxV' (Bin min1 #! combine (boundUKey min1) minV1 minV2 # goLFused combine min1 l1 l2 # r')
        GT -> goR1 combine maxV1 max1 r1 max2 n2

    goR2 _       _     !_   Tip !_   !_  = Empty
    goR2 combine maxV2 max1 n1  max2 Tip = goLookupR2 combine (boundKey max2) maxV2 (xor (boundKey max2) max1) n1
    goR2 _       _ _ (Bin min1 _ _ _) max2 (Bin _ _ _ _) | boundsDisjoint min1 max2 = Empty
    goR2 combine maxV2 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max1) (xorBounds min2 max2) of
        LT -> goR2 combine maxV2 max1 n1 max2 r2
        EQ | min1 < min2 -> binR (goL2 combine minV2 min1 l1 min2 l2) (goR2 combine maxV2 max1 r1 max2 r2)
           | min1 > min2 -> binR (goL1 combine minV1 min1 l1 min2 l2) (goR2 combine maxV2 max1 r1 max2 r2)
           | otherwise -> case goR2 combine maxV2 max1 r1 max2 r2 of
                Empty -> l2rMap (NonEmpty min1 #! combine (boundUKey min1) minV1 minV2 # goLFused combine min1 l1 l2)
                NonEmpty max' maxV' r' -> NonEmpty max' maxV' (Bin min1 #! combine (boundUKey min1) minV1 minV2 # goLFused combine min1 l1 l2 # r')
        GT | xor (boundKey max2) min1 > xor (boundKey max2) max1 -> goR2 combine maxV2 max1 r1 max2 n2 -- max2 is arbitrary here - we just need something from tree 2
           | min1 < min2 -> l2rMap $ goL2 combine minV2 min1 l1 min2 (Bin max2 maxV2 l2 r2)
           | min1 > min2 -> l2rMap $ goL1 combine minV1 min1 l1 min2 (Bin max2 maxV2 l2 r2)
           | otherwise -> l2rMap $ NonEmpty min1 #! combine (boundUKey min1) minV1 minV2 # goLFused combine min1 l1 (Bin max2 maxV2 l2 r2)

    goRFused _ !_ Tip !_ = Tip
    goRFused _ !_ !_ Tip = Tip
    goRFused combine !max n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 minV2 l2 r2) = case compareMSB (xorBounds min1 max) (xorBounds min2 max) of
        LT -> goRFused combine max n1 r2
        EQ | min1 < min2 -> case goL2 combine minV2 min1 l1 min2 l2 of
                Empty -> goRFused combine max r1 r2
                NonEmpty min' minV' l' -> Bin min' minV' l' (goRFused combine max r1 r2)
           | min1 > min2 -> case goL1 combine minV1 min1 l1 min2 l2 of
                Empty -> goRFused combine max r1 r2
                NonEmpty min' minV' l' -> Bin min' minV' l' (goRFused combine max r1 r2)
           | otherwise -> Bin min1 #! combine (boundUKey min1) minV1 minV2 # goLFused combine min1 l1 l2 # goRFused combine max r1 r2 -- we choose max1 arbitrarily, as max1 == max2
        GT -> goRFused combine max r1 n2

    goLookupL1 _ !_ _ !_ Tip = Empty
    goLookupL1 combine k v !xorCache (Bin max maxV l r) = case compareMaxBound k max of
        InBound | xorCache < xorCacheMax -> goLookupL1 combine k v xorCache l
                | otherwise              -> goLookupR1 combine k v xorCacheMax r
        OutOfBound -> Empty
        Matched -> NonEmpty (Bound k) #! combine (unbox k) v maxV # Tip
      where xorCacheMax = xor k max

    goLookupR1 _ !_ _ !_ Tip = Empty
    goLookupR1 combine k v !xorCache (Bin min minV l r) = case compareMinBound k min of
        InBound | xorCache < xorCacheMin -> goLookupR1 combine k v xorCache r
                | otherwise              -> goLookupL1 combine k v xorCacheMin l
        OutOfBound -> Empty
        Matched -> NonEmpty (Bound k) #! combine (unbox k) v minV # Tip
      where xorCacheMin = xor k min

    goLookupL2 _ !_ _ !_ Tip = Empty
    goLookupL2 combine k v !xorCache (Bin max maxV l r) = case compareMaxBound k max of
        InBound | xorCache < xorCacheMax -> goLookupL2 combine k v xorCache l
                | otherwise              -> goLookupR2 combine k v xorCacheMax r
        OutOfBound -> Empty
        Matched -> NonEmpty (Bound k) #! combine (unbox k) maxV v # Tip
      where xorCacheMax = xor k max

    goLookupR2 _ !_ _ !_ Tip = Empty
    goLookupR2 combine k v !xorCache (Bin min minV l r) = case compareMinBound k min of
        InBound | xorCache < xorCacheMin -> goLookupR2 combine k v xorCache r
                | otherwise              -> goLookupL2 combine k v xorCacheMin l
        OutOfBound -> Empty
        Matched -> NonEmpty (Bound k) #! combine (unbox k) minV v # Tip
      where xorCacheMin = xor k min

-- | /O(n+m)/. An unsafe general combining function.
--
-- WARNING: This function can produce corrupt maps and its results
-- may depend on the internal structures of its inputs. Users should
-- prefer 'merge' or 'mergeA'. This function is also significantly slower
-- than 'merge'.
--
-- When 'mergeWithKey' is given three arguments, it is inlined to the call
-- site. You should therefore use 'mergeWithKey' only to define custom
-- combining functions. For example, you could define 'unionWithKey',
-- 'differenceWithKey' and 'intersectionWithKey' as
--
-- > myUnionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) id id m1 m2
-- > myDifferenceWithKey f m1 m2 = mergeWithKey f id (const empty) m1 m2
-- > myIntersectionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) (const empty) (const empty) m1 m2
--
-- When calling @'mergeWithKey' combine only1 only2@, a function combining two
-- 'IntMap's is created, such that
--
-- * if a key is present in both maps, it is passed with both corresponding
--   values to the @combine@ function. Depending on the result, the key is either
--   present in the result with specified value, or is left out;
--
-- * a nonempty subtree present only in the first map is passed to @only1@ and
--   the output is added to the result;
--
-- * a nonempty subtree present only in the second map is passed to @only2@ and
--   the output is added to the result.
--
-- The @only1@ and @only2@ methods /must return a map with a subset (possibly empty) of the keys of the given map/.
-- The values can be modified arbitrarily. Most common variants of @only1@ and
-- @only2@ are 'id' and @'const' 'empty'@, but for example @'map' f@,
-- @'filterWithKey' f@, or @'mapMaybeWithKey' f@ could be used for any @f@.
mergeWithKey :: (Key -> a -> b -> Maybe c) -> (IntMap a -> IntMap c) -> (IntMap b -> IntMap c) -> IntMap a -> IntMap b -> IntMap c
mergeWithKey matched miss1 miss2 = Merge.merge (Merge.mapMaybeMissing (single miss1)) (Merge.mapMaybeMissing (single miss2)) (Merge.zipWithMaybeMatched matched) where
    single miss k v = case miss (IntMap (NonEmpty (Bound k) v Tip)) of
        IntMap Empty -> Nothing
        IntMap (NonEmpty _ v' _) -> Just v'

-- | /O(n)/. Map a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]
map :: (a -> b) -> IntMap a -> IntMap b
map f (IntMap m) = IntMap (mapStrict_ f m)

-- | /O(n)/. Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]
mapWithKey :: (Key -> a -> b) -> IntMap a -> IntMap b
mapWithKey f = runIdentity . Merge.runWhenMissingAll (Merge.mapMissing f)

#if USE_REWRITE_RULES
-- Pay close attention to strictness here. We need to force the
-- intermediate result for @'map' f . 'map' g@, and we need to refrain
-- from forcing it for @'map' f . 'L.map' g@, etc.
--
-- TODO Consider writing RULES for things like @'L.map' f ('map' g m)@. We'd
-- need a new function for this, and we'd have to pay attention to simplifier
-- phases. Somthing like
--
-- > lsmap :: (b -> c) -> (a -> b) -> Node t a -> Node t c
-- > lsmap _ _ Tip = Tip
-- > lsmap f g (Bin bound value l r) = let !gvalue = g value
-- >                                    in Bin bound (f gvalue) (lsmap f g l) (lsmap f g r)
{-# NOINLINE[1] map #-}
{-# NOINLINE[1] mapWithKey #-}
{-# RULES
"map/map" forall f g m . map f (map g m) = map (\v -> f $! g v) m
"map/mapL" forall f g m . map f (mapLazy g m) = map (f . g) m
"mapWithKey/map" forall f g m . mapWithKey f (map g m) = mapWithKey (\k v -> f k $! g v) m
"mapWithKey/mapL" forall f g m . mapWithKey f (mapLazy g m) = mapWithKey (\k -> f k . g) m
"map/mapWithKey" forall f g m . map f (mapWithKey g m) = mapWithKey (\k v -> f $! g k v) m
"map/mapWithKeyL" forall f g m . map f (L.mapWithKey g m) = mapWithKey (\k -> f . g k) m
"mapWithKey/mapWithKey" forall f g m . mapWithKey f (mapWithKey g m) = mapWithKey (\k v -> f k $! g k v) m
"mapWithKey/mapWithKeyL" forall f g m . mapWithKey f (L.mapWithKey g m) = mapWithKey (\k -> f k . g k) m
  #-}
#endif

-- | /O(n)/.
-- @'traverseWithKey' f s == 'fromList' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
--
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a'), (5, 'e')]) == Just (fromList [(1, 'b'), (5, 'f')])
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')])           == Nothing
{-# INLINE traverseWithKey #-}
traverseWithKey :: Applicative f => (Key -> a -> f b) -> IntMap a -> f (IntMap b)
traverseWithKey f = Merge.runWhenMissingAll (Merge.traverseMissing f)

-- | /O(n)/. The function @'mapAccum'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])
mapAccum :: (a -> b -> (a, c)) -> a -> IntMap b -> (a, IntMap c)
mapAccum f = mapAccumWithKey (\a _ x -> f a x)

-- | /O(n)/. The function @'mapAccumWithKey'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])
mapAccumWithKey :: (a -> Key -> b -> (a, c)) -> a -> IntMap b -> (a, IntMap c)
mapAccumWithKey f = start
  where
    start a (IntMap Empty) = (a, IntMap Empty)
    start a (IntMap (NonEmpty min minV root)) =
        let (a', !minV') = f a (boundKey min) minV
            (a'', root') = goL root a'
        in  (a'', IntMap (NonEmpty min minV' root'))

    goL  Tip a = (a, Tip)
    goL (Bin max maxV l r) a =
        let (a',    l') = goL l a
            (a'',   r') = goR r a'
            (a''', !maxV') = f a'' (boundKey max) maxV
        in  (a''', Bin max maxV' l' r')

    goR  Tip a = (a, Tip)
    goR (Bin min minV l r) a =
        let (a',   !minV') = f a (boundKey min) minV
            (a'',   l') = goL l a'
            (a''',  r') = goR r a''
        in  (a''', Bin min minV' l' r')

-- | /O(n)/. The function @'mapAccumRWithKey'@ threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (a -> Key -> b -> (a, c)) -> a -> IntMap b -> (a, IntMap c)
mapAccumRWithKey f = start
  where
    start a (IntMap Empty) = (a, IntMap Empty)
    start a (IntMap (NonEmpty min minV root)) =
        let (a',   root') = goL root a
            (a'', !minV') = f a' (boundKey min) minV
        in  (a'', IntMap (NonEmpty min minV' root'))

    goL  Tip a = (a, Tip)
    goL (Bin max maxV l r) a =
        let (a',  !maxV') = f a (boundKey max) maxV
            (a'',  r') = goR r a'
            (a''', l') = goL l a''
        in  (a''', Bin max maxV' l' r')

    goR  Tip a = (a, Tip)
    goR (Bin min minV l r) a =
        let (a',    r') = goR r a
            (a'',   l') = goL l a'
            (a''', !minV') = f a'' (boundKey min) minV
        in  (a''', Bin min minV' l' r')

-- | /O(n*min(n,W))/.
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the greatest of the
-- original keys is retained.
--
-- > mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        == fromList [(4, "b"), (6, "a")]
-- > mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "c"
-- > mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "c"
mapKeys :: (Key -> Key) -> IntMap a -> IntMap a
mapKeys f = foldlWithKey' (\m k a -> insert (f k) a m) empty

-- | /O(n*min(n,W))/.
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
--
-- > mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "cdab"
-- > mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "cdab"
mapKeysWith :: (a -> a -> a) -> (Key -> Key) -> IntMap a -> IntMap a
mapKeysWith combine f = foldlWithKey' (\m k a -> insertWith combine (f k) a m) empty

-- | /O(n*min(n,W))/.
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
-- This function has slightly better performance than 'mapKeys'.
--
-- > mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) == fromList [(6, "b"), (10, "a")]
mapKeysMonotonic :: (Key -> Key) -> IntMap a -> IntMap a
mapKeysMonotonic = mapKeys

-- | /O(n)/. Build a map from a set of keys and a function which for each key
-- computes its value.
--
-- > fromSet (\k -> replicate k 'a') (Data.IntSet.fromList [3, 5]) == fromList [(5,"aaaaa"), (3,"aaa")]
-- > fromSet undefined Data.IntSet.empty == empty
fromSet :: (Key -> a) -> Data.IntSet.IntSet -> IntMap a
fromSet f = fromDistinctAscList . Data.List.map (\k -> (k, f k)) . Data.IntSet.toList

-- | /O(n*min(n,W))/. Create a map from a list of key\/value pairs.
fromList :: [(Key, a)] -> IntMap a
fromList = Data.List.foldl' (\t (k, a) -> insert k a t) empty

-- | /O(n*min(n,W))/. Create a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")] == fromList [(3, "ab"), (5, "cba")]
-- > fromListWith (++) [] == empty
fromListWith :: (a -> a -> a) -> [(Key, a)] -> IntMap a
fromListWith f = Data.List.foldl' (\t (k, a) -> insertWith f k a t) empty

-- | /O(n*min(n,W))/. Build a map from a list of key\/value pairs with a combining function. See also fromAscListWithKey'.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")] == fromList [(3, "3:a|b"), (5, "5:c|5:b|a")]
-- > fromListWithKey f [] == empty
fromListWithKey :: (Key -> a -> a -> a) -> [(Key, a)] -> IntMap a
fromListWithKey f = Data.List.foldl' (\t (k, a) -> insertWithKey f k a t) empty

-- | /O(n)/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order.
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
fromAscList :: [(Key, a)] -> IntMap a
fromAscList = start where
    start [] = IntMap Empty
    start ((min, minV) : rest) = IntMap (go min minV rest StackBase)

    -- The strictness here is tricky, mimicking old behavior. Only the last value
    -- of a given key (the one being inserted into the map) is evaluated strictly.
    go !k v [] !stk = completeBuildStack (Bound k) v Tip stk
    go !k v ((next, nextV) : rest) !stk
        | next == k = nextV `seq` go k nextV rest stk
        | otherwise = v `seq` go next nextV rest (pushBuildStack (xor k (Bound next)) k v Tip stk)

-- | /O(n)/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order.
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
fromAscListWith :: (a -> a -> a) -> [(Key, a)] -> IntMap a
fromAscListWith f = fromAscListWithKey (const f)

-- | /O(n)/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order, with a combining function on equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "5:b|a")]
fromAscListWithKey :: (Key -> a -> a -> a) -> [(Key, a)] -> IntMap a
fromAscListWithKey f = start where
    start [] = IntMap Empty
    start ((min, minV) : rest) = IntMap (go min minV rest StackBase)

    go !k v [] !stk = completeBuildStack (Bound k) v Tip stk
    go !k v ((next, nextV) : rest) !stk
        | next == k = let !v' = f k nextV v in go k v' rest stk
        | otherwise = v `seq` go next nextV rest (pushBuildStack (xor k (Bound next)) k v Tip stk)

-- | /O(n)/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order and all distinct.
-- /The precondition (input list is strictly ascending) is not checked./
--
-- > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]
fromDistinctAscList :: [(Key, a)] -> IntMap a
fromDistinctAscList = start where
    start [] = IntMap Empty
    start ((min, !minV) : rest) = IntMap (go min minV rest StackBase)

    go !k !v [] !stk = completeBuildStack (Bound k) v Tip stk
    go !k !v ((next, !nextV) : rest) !stk = go next nextV rest (pushBuildStack (xor k (Bound next)) k v Tip stk)

-- | /O(n)/. Map values and collect the 'Just' results.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"
mapMaybe :: (a -> Maybe b) -> IntMap a -> IntMap b
mapMaybe f = mapMaybeWithKey (const f)

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"
{-# INLINE mapMaybeWithKey #-}
mapMaybeWithKey :: (Key -> a -> Maybe b) -> IntMap a -> IntMap b
mapMaybeWithKey f = runIdentity . Merge.runWhenMissingAll (Merge.mapMaybeMissing f)

-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- >
-- > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
mapEither :: (a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
mapEither f = mapEitherWithKey (const f)

-- | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results.
--
-- > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
-- > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
-- >
-- > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])
{-# INLINE mapEitherWithKey #-}
mapEitherWithKey :: (Key -> a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
mapEitherWithKey f = mapEitherWithUKey (\k a -> f (box k) a)

-- | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results with
-- a mapping function taking unboxed keys. Identical in functionality to
-- 'mapEitherWithKey'.
mapEitherWithUKey :: (UKey -> a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
mapEitherWithUKey func = start
  where
    start (IntMap Empty) = (IntMap Empty, IntMap Empty)
    start (IntMap (NonEmpty min minV root)) = case func (boundUKey min) minV of
        Left !v  -> let t :*: f = goTrueL root
                    in (IntMap (NonEmpty min v t), IntMap f)
        Right !v -> let t :*: f = goFalseL root
                    in (IntMap t, IntMap (NonEmpty min v f))

    goTrueL Tip = Tip :*: Empty
    goTrueL (Bin max maxV l r) = case func (boundUKey max) maxV of
        Left !v  -> let tl :*: fl = goTrueL l
                        tr :*: fr = goTrueR r
                    in Bin max v tl tr :*: binL fl fr
        Right !v -> let tl :*: fl = goTrueL l
                        tr :*: fr = goFalseR r
                    in binNodeMapL tl tr :*: binL fl (NonEmpty max v fr)

    goTrueR Tip = Tip :*: Empty
    goTrueR (Bin min minV l r) = case func (boundUKey min) minV of
        Left !v  -> let tl :*: fl = goTrueL l
                        tr :*: fr = goTrueR r
                    in Bin min v tl tr :*: binR fl fr
        Right !v -> let tl :*: fl = goFalseL l
                        tr :*: fr = goTrueR r
                    in binMapNodeR tl tr :*: binR (NonEmpty min v fl) fr

    goFalseL Tip = Empty :*: Tip
    goFalseL (Bin max maxV l r) = case func (boundUKey max) maxV of
        Left !v  -> let tl :*: fl = goFalseL l
                        tr :*: fr = goTrueR r
                    in binL tl (NonEmpty max v tr) :*: binNodeMapL fl fr
        Right !v -> let tl :*: fl = goFalseL l
                        tr :*: fr = goFalseR r
                    in binL tl tr :*: Bin max v fl fr

    goFalseR Tip = Empty :*: Tip
    goFalseR (Bin min minV l r) = case func (boundUKey min) minV of
        Left !v  -> let tl :*: fl = goTrueL l
                        tr :*: fr = goFalseR r
                    in binR (NonEmpty min v tl) tr :*: binMapNodeR fl fr
        Right !v -> let tl :*: fl = goFalseL l
                        tr :*: fr = goFalseR r
                    in binR tl tr :*: Bin min v fl fr

-- | /O(min(n,W))/. Update the value at the minimal key.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateMin :: (a -> Maybe a) -> IntMap a -> IntMap a
updateMin f = updateMinWithKey (const f)

-- | /O(min(n,W))/. Update the value at the maximal key.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
updateMax :: (a -> Maybe a) -> IntMap a -> IntMap a
updateMax f = updateMaxWithKey (const f)

-- | /O(min(n,W))/. Update the value at the minimal key.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateMinWithKey :: (Key -> a -> Maybe a) -> IntMap a -> IntMap a
updateMinWithKey _ (IntMap Empty) = IntMap Empty
updateMinWithKey f (IntMap (NonEmpty min minV root)) = IntMap $ case f (boundKey min) minV of
    Nothing -> nodeToMapL root
    Just !minV' -> NonEmpty min minV' root

-- | /O(min(n,W))/. Update the value at the maximal key.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
updateMaxWithKey :: (Key -> a -> Maybe a) -> IntMap a -> IntMap a
updateMaxWithKey _ (IntMap Empty) = IntMap Empty
updateMaxWithKey f (IntMap (NonEmpty min minV Tip)) = IntMap $ case f (boundKey min) minV of
    Nothing -> Empty
    Just !minV' -> NonEmpty min minV' Tip
updateMaxWithKey f (IntMap (NonEmpty min minV (Bin max maxV l r))) = IntMap . NonEmpty min minV $ case f (boundKey max) maxV of
    Nothing -> extractBinL l r
    Just !maxV' -> Bin max maxV' l r
