{-# LANGUAGE NoBangPatterns #-}
#if !defined(TESTING) && __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of maps from keys to values (dictionaries).
--
-- Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.Map (Map)
-- >  import qualified Data.Map as Map
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
-- the Big-O notation <http://en.wikipedia.org/wiki/Big_O_notation>.
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

module Data.Map (
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
            , insertWith'
            , insertWithKey
            , insertWithKey'
            , insertLookupWithKey
            , insertLookupWithKey'

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
            -- ** Legacy folds
            , fold
            , foldWithKey

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
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Monoid (Monoid(..))
import Control.Applicative (Applicative(..), (<$>))
import Data.Traversable (Traversable(traverse))
import qualified Data.Foldable as Foldable
import Data.Typeable
import Control.DeepSeq (NFData(rnf))

#if __GLASGOW_HASKELL__
import Text.Read
import Data.Data
#endif

-- Use macros to define strictness of functions.
-- STRICT_x_OF_y denotes an y-ary function strict in the x-th parameter.
-- We do not use BangPatterns, because they are not in any standard and we
-- want the compilers to be compiled by as many compilers as possible.
#define STRICT_1_OF_2(fn) fn arg _ | arg `seq` False = undefined
#define STRICT_1_OF_3(fn) fn arg _ _ | arg `seq` False = undefined
#define STRICT_2_OF_3(fn) fn _ arg _ | arg `seq` False = undefined
#define STRICT_2_OF_4(fn) fn _ arg _ _ | arg `seq` False = undefined

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 !,\\ --

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'

(!) :: Ord k => Map k a -> k -> a
m ! k    = find k m
{-# INLINE (!) #-}

-- | Same as 'difference'.
(\\) :: Ord k => Map k a -> Map k b -> Map k a
m1 \\ m2 = difference m1 m2
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE (\\) #-}
#endif

{--------------------------------------------------------------------
  Size balanced trees.
--------------------------------------------------------------------}
-- | A Map from keys @k@ to values @a@. 
data Map k a  = Tip 
              | Bin {-# UNPACK #-} !Size !k a !(Map k a) !(Map k a) 

type Size     = Int

instance (Ord k) => Monoid (Map k v) where
    mempty  = empty
    mappend = union
    mconcat = unions

#if __GLASGOW_HASKELL__

{--------------------------------------------------------------------
  A Data instance  
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We omit reflection services for the sake of data abstraction.

instance (Data k, Data a, Ord k) => Data (Map k a) where
  gfoldl f z m   = z fromList `f` toList m
  toConstr _     = error "toConstr"
  gunfold _ _    = error "gunfold"
  dataTypeOf _   = mkNoRepType "Data.Map.Map"
  dataCast2 f    = gcast2 f

#endif

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the map empty?
--
-- > Data.Map.null (empty)           == True
-- > Data.Map.null (singleton 1 'a') == False

null :: Map k a -> Bool
null Tip      = True
null (Bin {}) = False
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE null #-}
#endif

-- | /O(1)/. The number of elements in the map.
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3

size :: Map k a -> Int
size Tip              = 0
size (Bin sz _ _ _ _) = sz
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE size #-}
#endif


-- | /O(log n)/. Lookup the value at a key in the map.
--
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' if the key isn't in the map.
--
-- An example of using @lookup@:
--
-- > import Prelude hiding (lookup)
-- > import Data.Map
-- >
-- > employeeDept = fromList([("John","Sales"), ("Bob","IT")])
-- > deptCountry = fromList([("IT","USA"), ("Sales","France")])
-- > countryCurrency = fromList([("USA", "Dollar"), ("France", "Euro")])
-- >
-- > employeeCurrency :: String -> Maybe String
-- > employeeCurrency name = do
-- >     dept <- lookup name employeeDept
-- >     country <- lookup dept deptCountry
-- >     lookup country countryCurrency
-- >
-- > main = do
-- >     putStrLn $ "John's currency: " ++ (show (employeeCurrency "John"))
-- >     putStrLn $ "Pete's currency: " ++ (show (employeeCurrency "Pete"))
--
-- The output of this program:
--
-- >   John's currency: Just "Euro"
-- >   Pete's currency: Nothing

lookup :: Ord k => k -> Map k a -> Maybe a
lookup = go
  where
    STRICT_1_OF_2(go)
    go _ Tip = Nothing
    go k (Bin _ kx x l r) =
        case compare k kx of
            LT -> go k l
            GT -> go k r
            EQ -> Just x
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE lookup #-}
#else
{-# INLINE lookup #-}
#endif

lookupAssoc :: Ord k => k -> Map k a -> Maybe (k,a)
lookupAssoc = go
  where
    STRICT_1_OF_2(go)
    go _ Tip = Nothing
    go k (Bin _ kx x l r) =
        case compare k kx of
            LT -> go k l
            GT -> go k r
            EQ -> Just (kx,x)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINEABLE lookupAssoc #-}
#else
{-# INLINE lookupAssoc #-}
#endif

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False

member :: Ord k => k -> Map k a -> Bool
member k m = case lookup k m of
    Nothing -> False
    Just _  -> True
#if __GLASGOW_HASKELL__ >= 700
{-# INLINEABLE member #-}
#else
{-# INLINE member #-}
#endif

-- | /O(log n)/. Is the key not a member of the map? See also 'member'.
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True

notMember :: Ord k => k -> Map k a -> Bool
notMember k m = not $ member k m
{-# INLINE notMember #-}

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
-- Consider using 'lookup' when elements may not be present.
find :: Ord k => k -> Map k a -> a
find k m = case lookup k m of
    Nothing -> error "Map.find: element not in the map"
    Just x  -> x
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE find #-}
#else
{-# INLINE find #-}
#endif

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'

findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault def k m = case lookup k m of
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
-- | /O(1)/. The empty map.
--
-- > empty      == fromList []
-- > size empty == 0

empty :: Map k a
empty = Tip

-- | /O(1)/. A map with a single element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1

singleton :: k -> a -> Map k a
singleton k x = Bin 1 k x Tip Tip

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
    STRICT_1_OF_3(go)
    go kx x Tip = singleton kx x
    go kx x (Bin sz ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go kx x l) r
            GT -> balanceR ky y l (go kx x r)
            EQ -> Bin sz kx x l r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINEABLE insert #-}
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
{-# INLINE insertWith #-}

-- | Same as 'insertWith', but the combining function is applied strictly.
-- This is often the most desirable behavior.
--
-- For example, to update a counter:
--
-- > insertWith' (+) k 1 m
--
insertWith' :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith' f = insertWithKey' (\_ x' y' -> f x' y')
{-# INLINE insertWith' #-}

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
    STRICT_2_OF_4(go)
    go _ kx x Tip = singleton kx x
    go f kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go f kx x l) r
            GT -> balanceR ky y l (go f kx x r)
            EQ -> Bin sy kx (f kx x y) l r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINEABLE insertWithKey #-}
#else
{-# INLINE insertWithKey #-}
#endif

-- | Same as 'insertWithKey', but the combining function is applied strictly.
insertWithKey' :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey' = go
  where
    STRICT_2_OF_4(go)
    go _ kx x Tip = x `seq` singleton kx x
    go f kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go f kx x l) r
            GT -> balanceR ky y l (go f kx x r)
            EQ -> let x' = f kx x y in x' `seq` (Bin sy kx x' l r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINEABLE insertWithKey' #-}
#else
{-# INLINE insertWithKey' #-}
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
    STRICT_2_OF_4(go)
    go _ kx x Tip = (Nothing, singleton kx x)
    go f kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> let (found, l') = go f kx x l
                  in (found, balanceL ky y l' r)
            GT -> let (found, r') = go f kx x r
                  in (found, balanceR ky y l r')
            EQ -> (Just y, Bin sy kx (f kx x y) l r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINEABLE insertLookupWithKey #-}
#else
{-# INLINE insertLookupWithKey #-}
#endif

-- | /O(log n)/. A strict version of 'insertLookupWithKey'.
insertLookupWithKey' :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a
                     -> (Maybe a, Map k a)
insertLookupWithKey' = go
  where
    STRICT_2_OF_4(go)
    go _ kx x Tip = x `seq` (Nothing, singleton kx x)
    go f kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> let (found, l') = go f kx x l
                  in (found, balanceL ky y l' r)
            GT -> let (found, r') = go f kx x r
                  in (found, balanceR ky y l r')
            EQ -> let x' = f kx x y in x' `seq` (Just y, Bin sy kx x' l r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINEABLE insertLookupWithKey' #-}
#else
{-# INLINE insertLookupWithKey' #-}
#endif

{--------------------------------------------------------------------
  Deletion
  [delete] is the inlined version of [deleteWith (\k x -> Nothing)]
--------------------------------------------------------------------}
-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
--
-- > delete 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > delete 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > delete 5 empty                         == empty

delete :: Ord k => k -> Map k a -> Map k a
delete = go
  where
    STRICT_1_OF_2(go)
    go _ Tip = Tip
    go k (Bin _ kx x l r) =
        case compare k kx of
            LT -> balanceR kx x (go k l) r
            GT -> balanceL kx x l (go k r)
            EQ -> glue l r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINEABLE delete #-}
#else
{-# INLINE delete #-}
#endif

-- | /O(log n)/. Update a value at a specific key with the result of the provided function.
-- When the key is not
-- a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty

adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust f = adjustWithKey (\_ x -> f x)
{-# INLINE adjust #-}

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty

adjustWithKey :: Ord k => (k -> a -> a) -> k -> Map k a -> Map k a
adjustWithKey f = updateWithKey (\k' x' -> Just (f k' x'))
{-# INLINE adjustWithKey #-}

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
{-# INLINE update #-}

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
                   Just x' -> Bin sx kx x' l r
                   Nothing -> glue l r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINEABLE updateWithKey #-}
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
               LT -> let (found,l') = go f k l in (found,balanceR kx x l' r)
               GT -> let (found,r') = go f k r in (found,balanceL kx x l r') 
               EQ -> case f kx x of
                       Just x' -> (Just x',Bin sx kx x' l r)
                       Nothing -> (Just x,glue l r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINEABLE updateLookupWithKey #-}
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
                       Just x' -> Bin sx kx x' l r
                       Nothing -> glue l r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINEABLE alter #-}
#else
{-# INLINE alter #-}
#endif

{--------------------------------------------------------------------
  Indexing
--------------------------------------------------------------------}
-- | /O(log n)/. Return the /index/ of a key. The index is a number from
-- /0/ up to, but not including, the 'size' of the map. Calls 'error' when
-- the key is not a 'member' of the map.
--
-- > findIndex 2 (fromList [(5,"a"), (3,"b")])    Error: element is not in the map
-- > findIndex 3 (fromList [(5,"a"), (3,"b")]) == 0
-- > findIndex 5 (fromList [(5,"a"), (3,"b")]) == 1
-- > findIndex 6 (fromList [(5,"a"), (3,"b")])    Error: element is not in the map

findIndex :: Ord k => k -> Map k a -> Int
findIndex k t
  = case lookupIndex k t of
      Nothing  -> error "Map.findIndex: element is not in the map"
      Just idx -> idx
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE findIndex #-}
#endif

-- | /O(log n)/. Lookup the /index/ of a key. The index is a number from
-- /0/ up to, but not including, the 'size' of the map.
--
-- > isJust (lookupIndex 2 (fromList [(5,"a"), (3,"b")]))   == False
-- > fromJust (lookupIndex 3 (fromList [(5,"a"), (3,"b")])) == 0
-- > fromJust (lookupIndex 5 (fromList [(5,"a"), (3,"b")])) == 1
-- > isJust (lookupIndex 6 (fromList [(5,"a"), (3,"b")]))   == False

lookupIndex :: Ord k => k -> Map k a -> Maybe Int
lookupIndex k = lkp k 0
  where
    STRICT_1_OF_3(lkp)
    STRICT_2_OF_3(lkp)
    lkp _   _    Tip  = Nothing
    lkp key idx (Bin _ kx _ l r)
      = case compare key kx of
          LT -> lkp key idx l
          GT -> lkp key (idx + size l + 1) r
          EQ -> let idx' = idx + size l in idx' `seq` Just idx'
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE lookupIndex #-}
#endif

-- | /O(log n)/. Retrieve an element by /index/. Calls 'error' when an
-- invalid index is used.
--
-- > elemAt 0 (fromList [(5,"a"), (3,"b")]) == (3,"b")
-- > elemAt 1 (fromList [(5,"a"), (3,"b")]) == (5, "a")
-- > elemAt 2 (fromList [(5,"a"), (3,"b")])    Error: index out of range

elemAt :: Int -> Map k a -> (k,a)
STRICT_1_OF_2(elemAt)
elemAt _ Tip = error "Map.elemAt: index out of range"
elemAt i (Bin _ kx x l r)
  = case compare i sizeL of
      LT -> elemAt i l
      GT -> elemAt (i-sizeL-1) r
      EQ -> (kx,x)
  where
    sizeL = size l
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE elemAt #-}
#endif

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
              Just x' -> Bin sx kx x' l r
              Nothing -> glue l r
      where
        sizeL = size l
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE updateAt #-}
#endif

-- | /O(log n)/. Delete the element at /index/.
-- Defined as (@'deleteAt' i map = 'updateAt' (\k x -> 'Nothing') i map@).
--
-- > deleteAt 0  (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
-- > deleteAt 1  (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > deleteAt 2 (fromList [(5,"a"), (3,"b")])     Error: index out of range
-- > deleteAt (-1) (fromList [(5,"a"), (3,"b")])  Error: index out of range

deleteAt :: Int -> Map k a -> Map k a
deleteAt i m
  = updateAt (\_ _ -> Nothing) i m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE deleteAt #-}
#endif


{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}
-- | /O(log n)/. The minimal key of the map. Calls 'error' if the map is empty.
--
-- > findMin (fromList [(5,"a"), (3,"b")]) == (3,"b")
-- > findMin empty                            Error: empty map has no minimal element

findMin :: Map k a -> (k,a)
findMin (Bin _ kx x Tip _)  = (kx,x)
findMin (Bin _ _  _ l _)    = findMin l
findMin Tip                 = error "Map.findMin: empty map has no minimal element"
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE findMin #-}
#endif

-- | /O(log n)/. The maximal key of the map. Calls 'error' if the map is empty.
--
-- > findMax (fromList [(5,"a"), (3,"b")]) == (5,"a")
-- > findMax empty                            Error: empty map has no maximal element

findMax :: Map k a -> (k,a)
findMax (Bin _ kx x _ Tip)  = (kx,x)
findMax (Bin _ _  _ _ r)    = findMax r
findMax Tip                 = error "Map.findMax: empty map has no maximal element"
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE findMax #-}
#endif

-- | /O(log n)/. Delete the minimal key. Returns an empty map if the map is empty.
--
-- > deleteMin (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(5,"a"), (7,"c")]
-- > deleteMin empty == empty

deleteMin :: Map k a -> Map k a
deleteMin (Bin _ _  _ Tip r)  = r
deleteMin (Bin _ kx x l r)    = balanceR kx x (deleteMin l) r
deleteMin Tip                 = Tip
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE deleteMin #-}
#endif

-- | /O(log n)/. Delete the maximal key. Returns an empty map if the map is empty.
--
-- > deleteMax (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(3,"b"), (5,"a")]
-- > deleteMax empty == empty

deleteMax :: Map k a -> Map k a
deleteMax (Bin _ _  _ l Tip)  = l
deleteMax (Bin _ kx x l r)    = balanceL kx x l (deleteMax r)
deleteMax Tip                 = Tip
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE deleteMax #-}
#endif

-- | /O(log n)/. Update the value at the minimal key.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMin :: (a -> Maybe a) -> Map k a -> Map k a
updateMin f m
  = updateMinWithKey (\_ x -> f x) m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE updateMin #-}
#endif

-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMax :: (a -> Maybe a) -> Map k a -> Map k a
updateMax f m
  = updateMaxWithKey (\_ x -> f x) m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE updateMax #-}
#endif


-- | /O(log n)/. Update the value at the minimal key.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMinWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMinWithKey _ Tip                 = Tip
updateMinWithKey f (Bin sx kx x Tip r) = case f kx x of
                                           Nothing -> r
                                           Just x' -> Bin sx kx x' Tip r
updateMinWithKey f (Bin _ kx x l r)    = balanceR kx x (updateMinWithKey f l) r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE updateMinWithKey #-}
#endif

-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMaxWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMaxWithKey _ Tip                 = Tip
updateMaxWithKey f (Bin sx kx x l Tip) = case f kx x of
                                           Nothing -> l
                                           Just x' -> Bin sx kx x' l Tip
updateMaxWithKey f (Bin _ kx x l r)    = balanceL kx x l (updateMaxWithKey f r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE updateMaxWithKey #-}
#endif

-- | /O(log n)/. Retrieves the minimal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
-- > minViewWithKey empty == Nothing

minViewWithKey :: Map k a -> Maybe ((k,a), Map k a)
minViewWithKey Tip = Nothing
minViewWithKey x   = Just (deleteFindMin x)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE minViewWithKey #-}
#endif

-- | /O(log n)/. Retrieves the maximal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
-- > maxViewWithKey empty == Nothing

maxViewWithKey :: Map k a -> Maybe ((k,a), Map k a)
maxViewWithKey Tip = Nothing
maxViewWithKey x   = Just (deleteFindMax x)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE maxViewWithKey #-}
#endif

-- | /O(log n)/. Retrieves the value associated with minimal key of the
-- map, and the map stripped of that element, or 'Nothing' if passed an
-- empty map.
--
-- > minView (fromList [(5,"a"), (3,"b")]) == Just ("b", singleton 5 "a")
-- > minView empty == Nothing

minView :: Map k a -> Maybe (a, Map k a)
minView Tip = Nothing
minView x   = Just (first snd $ deleteFindMin x)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE minView #-}
#endif

-- | /O(log n)/. Retrieves the value associated with maximal key of the
-- map, and the map stripped of that element, or 'Nothing' if passed an
--
-- > maxView (fromList [(5,"a"), (3,"b")]) == Just ("a", singleton 3 "b")
-- > maxView empty == Nothing

maxView :: Map k a -> Maybe (a, Map k a)
maxView Tip = Nothing
maxView x   = Just (first snd $ deleteFindMax x)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE maxView #-}
#endif

-- Update the 1st component of a tuple (special case of Control.Arrow.first)
first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

{--------------------------------------------------------------------
  Union. 
--------------------------------------------------------------------}
-- | The union of a list of maps:
--   (@'unions' == 'Prelude.foldl' 'union' 'empty'@).
--
-- > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "b"), (5, "a"), (7, "C")]
-- > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
-- >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]

unions :: Ord k => [Map k a] -> Map k a
unions ts
  = foldlStrict union empty ts
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE unions #-}
#endif

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

-- | /O(n+m)/.
-- The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@. 
-- It prefers @t1@ when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
-- The implementation uses the efficient /hedge-union/ algorithm.
-- Hedge-union is more efficient on (bigset \``union`\` smallset).
--
-- > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]

union :: Ord k => Map k a -> Map k a -> Map k a
union Tip t2  = t2
union t1 Tip  = t1
union (Bin _ k x Tip Tip) t = insert k x t
union t (Bin _ k x Tip Tip) = insertWith (\_ y->y) k x t
union t1 t2 = hedgeUnionL NothingS NothingS t1 t2
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE union #-}
#endif

-- left-biased hedge union
hedgeUnionL :: Ord a
            => MaybeS a -> MaybeS a -> Map a b -> Map a b
            -> Map a b
hedgeUnionL _     _     t1 Tip
  = t1
hedgeUnionL blo bhi Tip (Bin _ kx x l r)
  = join kx x (filterGt blo l) (filterLt bhi r)
hedgeUnionL blo bhi (Bin _ kx x l r) t2
  = join kx x (hedgeUnionL blo bmi l (trim blo bmi t2))
              (hedgeUnionL bmi bhi r (trim bmi bhi t2))
  where
    bmi = JustS kx
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE hedgeUnionL #-}
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
{-# INLINE unionWith #-}

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
  = join kx newx (hedgeUnionWithKey f blo bmi l lt)
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
-- | /O(n+m)/. Difference of two maps. 
-- Return elements of the first map not existing in the second map.
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
--
-- > difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 3 "b"

difference :: Ord k => Map k a -> Map k b -> Map k a
difference Tip _   = Tip
difference t1 Tip  = t1
difference t1 t2   = hedgeDiff NothingS NothingS t1 t2
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE difference #-}
#endif

hedgeDiff :: Ord a
          => MaybeS a -> MaybeS a -> Map a b -> Map a c
          -> Map a b
hedgeDiff _     _     Tip _
  = Tip
hedgeDiff blo bhi (Bin _ kx x l r) Tip
  = join kx x (filterGt blo l) (filterLt bhi r)
hedgeDiff blo bhi t (Bin _ kx _ l r)
  = merge (hedgeDiff blo bmi (trim blo bmi t) l)
          (hedgeDiff bmi bhi (trim bmi bhi t) r)
  where
    bmi = JustS kx
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE hedgeDiff #-}
#endif

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
{-# INLINE differenceWith #-}

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
            Just z  -> join ky z tl tr
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
-- | /O(n+m)/. Intersection of two maps.
-- Return data in the first map for the keys existing in both maps.
-- (@'intersection' m1 m2 == 'intersectionWith' 'const' m1 m2@).
--
-- > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"

intersection :: Ord k => Map k a -> Map k b -> Map k a
intersection m1 m2
  = intersectionWithKey (\_ x _ -> x) m1 m2
{-# INLINE intersection #-}

-- | /O(n+m)/. Intersection with a combining function.
--
-- > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"

intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith f m1 m2
  = intersectionWithKey (\_ x y -> f x y) m1 m2
{-# INLINE intersectionWith #-}

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
      Just x -> join k1 (f k1 x1 x) tl tr
      Nothing -> merge tl tr
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE intersectionWithKey #-}
#endif



{--------------------------------------------------------------------
  Submap
--------------------------------------------------------------------}
-- | /O(n+m)/.
-- This function is defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
--
isSubmapOf :: (Ord k,Eq a) => Map k a -> Map k a -> Bool
isSubmapOf m1 m2 = isSubmapOfBy (==) m1 m2
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE isSubmapOf #-}
#endif

{- | /O(n+m)/.
 The expression (@'isSubmapOfBy' f t1 t2@) returns 'True' if
 all keys in @t1@ are in tree @t2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following 
 expressions are all 'True':
 
 > isSubmapOfBy (==) (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (<=) (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (==) (fromList [('a',1),('b',2)]) (fromList [('a',1),('b',2)])

 But the following are all 'False':
 
 > isSubmapOfBy (==) (fromList [('a',2)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (<)  (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (==) (fromList [('a',1),('b',2)]) (fromList [('a',1)])
 

-}
isSubmapOfBy :: Ord k => (a->b->Bool) -> Map k a -> Map k b -> Bool
isSubmapOfBy f t1 t2
  = (size t1 <= size t2) && (submap' f t1 t2)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE isSubmapOfBy #-}
#endif

submap' :: Ord a => (b -> c -> Bool) -> Map a b -> Map a c -> Bool
submap' _ Tip _ = True
submap' _ _ Tip = False
submap' f (Bin _ kx x l r) t
  = case found of
      Nothing -> False
      Just y  -> f x y && submap' f l lt && submap' f r gt
  where
    (lt,found,gt) = splitLookup kx t
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE submap' #-}
#endif

-- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal). 
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: (Ord k,Eq a) => Map k a -> Map k a -> Bool
isProperSubmapOf m1 m2
  = isProperSubmapOfBy (==) m1 m2
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE isProperSubmapOf #-}
#endif

{- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
 The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
 @m1@ and @m2@ are not equal,
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following 
 expressions are all 'True':
 
  > isProperSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])

 But the following are all 'False':
 
  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
  > isProperSubmapOfBy (<)  (fromList [(1,1)])       (fromList [(1,1),(2,2)])
  
 
-}
isProperSubmapOfBy :: Ord k => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
isProperSubmapOfBy f t1 t2
  = (size t1 < size t2) && (submap' f t1 t2)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE isProperSubmapOfBy #-}
#endif

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}
-- | /O(n)/. Filter all values that satisfy the predicate.
--
-- > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > filter (> "x") (fromList [(5,"a"), (3,"b")]) == empty
-- > filter (< "a") (fromList [(5,"a"), (3,"b")]) == empty

filter :: Ord k => (a -> Bool) -> Map k a -> Map k a
filter p m
  = filterWithKey (\_ x -> p x) m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE filter #-}
#endif

-- | /O(n)/. Filter all keys\/values that satisfy the predicate.
--
-- > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

filterWithKey :: Ord k => (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey _ Tip = Tip
filterWithKey p (Bin _ kx x l r)
  | p kx x    = join kx x (filterWithKey p l) (filterWithKey p r)
  | otherwise = merge (filterWithKey p l) (filterWithKey p r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE filterWithKey #-}
#endif

-- | /O(n)/. Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partition :: Ord k => (a -> Bool) -> Map k a -> (Map k a,Map k a)
partition p m
  = partitionWithKey (\_ x -> p x) m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE partition #-}
#endif

-- | /O(n)/. Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
-- > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partitionWithKey :: Ord k => (k -> a -> Bool) -> Map k a -> (Map k a,Map k a)
partitionWithKey _ Tip = (Tip,Tip)
partitionWithKey p (Bin _ kx x l r)
  | p kx x    = (join kx x l1 r1,merge l2 r2)
  | otherwise = (merge l1 r1,join kx x l2 r2)
  where
    (l1,l2) = partitionWithKey p l
    (r1,r2) = partitionWithKey p r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE partitionWithKey #-}
#endif

-- | /O(n)/. Map values and collect the 'Just' results.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"

mapMaybe :: Ord k => (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f = mapMaybeWithKey (\_ x -> f x)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE mapMaybe #-}
#endif

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"

mapMaybeWithKey :: Ord k => (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey _ Tip = Tip
mapMaybeWithKey f (Bin _ kx x l r) = case f kx x of
  Just y  -> join kx y (mapMaybeWithKey f l) (mapMaybeWithKey f r)
  Nothing -> merge (mapMaybeWithKey f l) (mapMaybeWithKey f r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE mapMaybeWithKey #-}
#endif

-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- >
-- > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])

mapEither :: Ord k => (a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEither f m
  = mapEitherWithKey (\_ x -> f x) m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE mapEither #-}
#endif

-- | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results.
--
-- > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
-- > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
-- >
-- > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])

mapEitherWithKey :: Ord k =>
  (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEitherWithKey _ Tip = (Tip, Tip)
mapEitherWithKey f (Bin _ kx x l r) = case f kx x of
  Left y  -> (join kx y l1 r1, merge l2 r2)
  Right z -> (merge l1 r1, join kx z l2 r2)
 where
    (l1,l2) = mapEitherWithKey f l
    (r1,r2) = mapEitherWithKey f r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE mapEitherWithKey #-}
#endif

{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}
-- | /O(n)/. Map a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]

map :: (a -> b) -> Map k a -> Map k b
map f = mapWithKey (\_ x -> f x)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE map #-}
#endif

-- | /O(n)/. Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey _ Tip = Tip
mapWithKey f (Bin sx kx x l r) = Bin sx kx (f kx x) (mapWithKey f l) (mapWithKey f r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE mapWithKey #-}
#endif

-- | /O(n)/. The function 'mapAccum' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])

mapAccum :: (a -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccum f a m
  = mapAccumWithKey (\a' _ x' -> f a' x') a m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE mapAccum #-}
#endif

-- | /O(n)/. The function 'mapAccumWithKey' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])

mapAccumWithKey :: (a -> k -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccumWithKey f a t
  = mapAccumL f a t
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE mapAccumWithKey #-}
#endif

-- | /O(n)/. The function 'mapAccumL' threads an accumulating
-- argument through the map in ascending order of keys.
mapAccumL :: (a -> k -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccumL _ a Tip               = (a,Tip)
mapAccumL f a (Bin sx kx x l r) =
  let (a1,l') = mapAccumL f a l
      (a2,x') = f a1 kx x
      (a3,r') = mapAccumL f a2 r
  in (a3,Bin sx kx x' l' r')
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE mapAccumL #-}
#endif

-- | /O(n)/. The function 'mapAccumR' threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (a -> k -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccumRWithKey _ a Tip = (a,Tip)
mapAccumRWithKey f a (Bin sx kx x l r) =
  let (a1,r') = mapAccumRWithKey f a r
      (a2,x') = f a1 kx x
      (a3,l') = mapAccumRWithKey f a2 l
  in (a3,Bin sx kx x' l' r')
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE mapAccumRWithKey #-}
#endif

-- | /O(n*log n)/.
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
-- 
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the smallest of
-- these keys is retained.
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
    Bin sz (f k) x (mapKeysMonotonic f l) (mapKeysMonotonic f r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE mapKeysMonotonic #-}
#endif

{--------------------------------------------------------------------
  Folds  
--------------------------------------------------------------------}

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator. This function is an equivalent of 'foldr' and is present
-- for compatibility only.
--
-- /Please note that fold will be deprecated in the future and removed./
fold :: (a -> b -> b) -> b -> Map k a -> b
fold = foldr
{-# INLINE fold #-}

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
--
-- For example,
--
-- > elems map = foldr (:) [] map
--
-- > let f a len = len + (length a)
-- > foldr f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldr :: (a -> b -> b) -> b -> Map k a -> b
foldr f = go
  where
    go z Tip             = z
    go z (Bin _ _ x l r) = go (f x (go z r)) l
{-# INLINE foldr #-}

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> Map k a -> b
foldr' f = go
  where
    STRICT_1_OF_2(go)
    go z Tip             = z
    go z (Bin _ _ x l r) = go (f x (go z r)) l
{-# INLINE foldr' #-}

-- | /O(n)/. Fold the values in the map using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
--
-- For example,
--
-- > elems = reverse . foldl (flip (:)) []
--
-- > let f len a = len + (length a)
-- > foldl f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldl :: (a -> b -> a) -> a -> Map k b -> a
foldl f = go
  where
    go z Tip             = z
    go z (Bin _ _ x l r) = go (f (go z l) x) r
{-# INLINE foldl #-}

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> b -> a) -> a -> Map k b -> a
foldl' f = go
  where
    STRICT_1_OF_2(go)
    go z Tip             = z
    go z (Bin _ _ x l r) = go (f (go z l) x) r
{-# INLINE foldl' #-}

-- | /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator. This function is an equivalent of 'foldrWithKey' and is present
-- for compatibility only.
--
-- /Please note that foldWithKey will be deprecated in the future and removed./
foldWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldWithKey = foldrWithKey
{-# INLINE foldWithKey #-}

-- | /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
--
-- For example,
--
-- > keys map = foldrWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldrWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"
foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey f = go
  where
    go z Tip             = z
    go z (Bin _ kx x l r) = go (f kx x (go z r)) l
{-# INLINE foldrWithKey #-}

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldrWithKey' :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey' f = go
  where
    STRICT_1_OF_2(go)
    go z Tip              = z
    go z (Bin _ kx x l r) = go (f kx x (go z r)) l
{-# INLINE foldrWithKey' #-}

-- | /O(n)/. Fold the keys and values in the map using the given left-associative
-- binary operator, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
--
-- For example,
--
-- > keys = reverse . foldlWithKey (\ks k x -> k:ks) []
--
-- > let f result k a = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldlWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (3:b)(5:a)"
foldlWithKey :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlWithKey f = go
  where
    go z Tip              = z
    go z (Bin _ kx x l r) = go (f (go z l) kx x) r
{-# INLINE foldlWithKey #-}

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlWithKey' :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlWithKey' f = go
  where
    STRICT_1_OF_2(go)
    go z Tip              = z
    go z (Bin _ kx x l r) = go (f (go z l) kx x) r
{-# INLINE foldlWithKey' #-}

{--------------------------------------------------------------------
  List variations 
--------------------------------------------------------------------}
-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
--
-- > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
-- > elems empty == []

elems :: Map k a -> [a]
elems m
  = [x | (_,x) <- assocs m]
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE elems #-}
#endif

-- | /O(n)/. Return all keys of the map in ascending order.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []

keys  :: Map k a -> [k]
keys m
  = [k | (k,_) <- assocs m]
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE keys  #-}
#endif

-- | /O(n)/. The set of all keys of the map.
--
-- > keysSet (fromList [(5,"a"), (3,"b")]) == Data.Set.fromList [3,5]
-- > keysSet empty == Data.Set.empty

keysSet :: Map k a -> Set.Set k
keysSet m = Set.fromDistinctAscList (keys m)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE keysSet #-}
#endif

-- | /O(n)/. Return all key\/value pairs in the map in ascending key order.
--
-- > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > assocs empty == []

assocs :: Map k a -> [(k,a)]
assocs m
  = toList m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE assocs #-}
#endif

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

-- | /O(n)/. Convert to a list of key\/value pairs.
--
-- > toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > toList empty == []

toList :: Map k a -> [(k,a)]
toList t      = toAscList t
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE toList #-}
#endif

-- | /O(n)/. Convert to an ascending list.
--
-- > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]

toAscList :: Map k a -> [(k,a)]
toAscList t   = foldrWithKey (\k x xs -> (k,x):xs) [] t
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE toAscList #-}
#endif

-- | /O(n)/. Convert to a descending list.
toDescList :: Map k a -> [(k,a)]
toDescList t  = foldlWithKey (\xs k x -> (k,x):xs) [] t
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE toDescList #-}
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
    | kx==kz    = let yy = f kx xx zz in combineEq' (kx,yy) xs'
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
  = build const (length xs) xs
  where
    -- 1) use continuations so that we use heap space instead of stack space.
    -- 2) special case for n==5 to build bushier trees. 
    build c 0 xs'  = c Tip xs'
    build c 5 xs'  = case xs' of
                       ((k1,x1):(k2,x2):(k3,x3):(k4,x4):(k5,x5):xx) 
                            -> c (bin k4 x4 (bin k2 x2 (singleton k1 x1) (singleton k3 x3)) (singleton k5 x5)) xx
                       _ -> error "fromDistinctAscList build"
    build c n xs'  = seq nr $ build (buildR nr c) nl xs'
                   where
                     nl = n `div` 2
                     nr = n - nl - 1

    buildR n c l ((k,x):ys) = build (buildB l k x c) n ys
    buildR _ _ _ []         = error "fromDistinctAscList buildR []"
    buildB l k x c r zs     = c (bin k x l r) zs
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromDistinctAscList #-}
#endif


{--------------------------------------------------------------------
  Utility functions that return sub-ranges of the original
  tree. Some functions take a `Maybe value` as an argument to
  allow comparisons against infinite values. These are called `blow`
  (Nothing is -\infty) and `bhigh` (here Nothing is +\infty).
  We use MaybeS value, which is a Maybe strict in the Just case.

  [trim blow bhigh t]   A tree that is either empty or where [x > blow]
                        and [x < bhigh] for the value [x] of the root.
  [filterGt blow t]     A tree where for all values [k]. [k > blow]
  [filterLt bhigh t]    A tree where for all values [k]. [k < bhigh]

  [split k t]           Returns two trees [l] and [r] where all keys
                        in [l] are <[k] and all keys in [r] are >[k].
  [splitLookup k t]     Just like [split] but also returns whether [k]
                        was found in the tree.
--------------------------------------------------------------------}

data MaybeS a = NothingS | JustS !a

{--------------------------------------------------------------------
  [trim blo bhi t] trims away all subtrees that surely contain no
  values between the range [blo] to [bhi]. The returned tree is either
  empty or the key of the root is between @blo@ and @bhi@.
--------------------------------------------------------------------}
trim :: Ord k => MaybeS k -> MaybeS k -> Map k a -> Map k a
trim NothingS   NothingS   t = t
trim (JustS lk) NothingS   t = greater lk t where greater lo (Bin _ k _ _ r) | k <= lo = greater lo r
                                                  greater _  t' = t'
trim NothingS   (JustS hk) t = lesser hk t  where lesser  hi (Bin _ k _ l _) | k >= hi = lesser  hi l
                                                  lesser  _  t' = t'
trim (JustS lk) (JustS hk) t = middle lk hk t  where middle lo hi (Bin _ k _ _ r) | k <= lo = middle lo hi r
                                                     middle lo hi (Bin _ k _ l _) | k >= hi = middle lo hi l
                                                     middle _  _  t' = t'
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE trim #-}
#endif

trimLookupLo :: Ord k => k -> MaybeS k -> Map k a -> (Maybe (k,a), Map k a)
trimLookupLo _  _  Tip = (Nothing, Tip)
trimLookupLo lo hi t@(Bin _ kx x l r)
  = case compare lo kx of
      LT -> case compare' kx hi of
              LT -> (lookupAssoc lo t, t)
              _  -> trimLookupLo lo hi l
      GT -> trimLookupLo lo hi r
      EQ -> (Just (kx,x),trim (JustS lo) hi r)
  where compare' _    NothingS   = LT
        compare' kx' (JustS hi') = compare kx' hi'
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE trimLookupLo #-}
#endif


{--------------------------------------------------------------------
  [filterGt b t] filter all keys >[b] from tree [t]
  [filterLt b t] filter all keys <[b] from tree [t]
--------------------------------------------------------------------}
filterGt :: Ord k => MaybeS k -> Map k v -> Map k v
filterGt NothingS t = t
filterGt (JustS b) t = filter' b t
  where filter' _   Tip = Tip
        filter' b' (Bin _ kx x l r) =
          case compare b' kx of LT -> join kx x (filter' b' l) r
                                EQ -> r
                                GT -> filter' b' r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE filterGt #-}
#endif

filterLt :: Ord k => MaybeS k -> Map k v -> Map k v
filterLt NothingS t = t
filterLt (JustS b) t = filter' b t
  where filter' _   Tip = Tip
        filter' b' (Bin _ kx x l r) =
          case compare kx b' of LT -> join kx x l (filter' b' r)
                                EQ -> l
                                GT -> filter' b' l
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE filterLt #-}
#endif

{--------------------------------------------------------------------
  Split
--------------------------------------------------------------------}
-- | /O(log n)/. The expression (@'split' k map@) is a pair @(map1,map2)@ where
-- the keys in @map1@ are smaller than @k@ and the keys in @map2@ larger than @k@.
-- Any key equal to @k@ is found in neither @map1@ nor @map2@.
--
-- > split 2 (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3,"b"), (5,"a")])
-- > split 3 (fromList [(5,"a"), (3,"b")]) == (empty, singleton 5 "a")
-- > split 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > split 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", empty)
-- > split 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], empty)

split :: Ord k => k -> Map k a -> (Map k a,Map k a)
split k t = k `seq`
  case t of
    Tip            -> (Tip, Tip)
    Bin _ kx x l r -> case compare k kx of
      LT -> let (lt,gt) = split k l in (lt,join kx x gt r)
      GT -> let (lt,gt) = split k r in (join kx x l lt,gt)
      EQ -> (l,r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE split #-}
#endif

-- | /O(log n)/. The expression (@'splitLookup' k map@) splits a map just
-- like 'split' but also returns @'lookup' k map@.
--
-- > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (empty, Nothing, fromList [(3,"b"), (5,"a")])
-- > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (empty, Just "b", singleton 5 "a")
-- > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Nothing, singleton 5 "a")
-- > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", empty)
-- > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], Nothing, empty)

splitLookup :: Ord k => k -> Map k a -> (Map k a,Maybe a,Map k a)
splitLookup k t = k `seq`
  case t of
    Tip            -> (Tip,Nothing,Tip)
    Bin _ kx x l r -> case compare k kx of
      LT -> let (lt,z,gt) = splitLookup k l in (lt,z,join kx x gt r)
      GT -> let (lt,z,gt) = splitLookup k r in (join kx x l lt,z,gt)
      EQ -> (l,Just x,r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE splitLookup #-}
#endif

-- | /O(log n)/.
splitLookupWithKey :: Ord k => k -> Map k a -> (Map k a,Maybe (k,a),Map k a)
splitLookupWithKey k t = k `seq`
  case t of
    Tip            -> (Tip,Nothing,Tip)
    Bin _ kx x l r -> case compare k kx of
      LT -> let (lt,z,gt) = splitLookupWithKey k l in (lt,z,join kx x gt r)
      GT -> let (lt,z,gt) = splitLookupWithKey k r in (join kx x l lt,z,gt)
      EQ -> (l,Just (kx, x),r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE splitLookupWithKey #-}
#endif

{--------------------------------------------------------------------
  Utility functions that maintain the balance properties of the tree.
  All constructors assume that all values in [l] < [k] and all values
  in [r] > [k], and that [l] and [r] are valid trees.
  
  In order of sophistication:
    [Bin sz k x l r]  The type constructor.
    [bin k x l r]     Maintains the correct size, assumes that both [l]
                      and [r] are balanced with respect to each other.
    [balance k x l r] Restores the balance and size.
                      Assumes that the original tree was balanced and
                      that [l] or [r] has changed by at most one element.
    [join k x l r]    Restores balance and size. 

  Furthermore, we can construct a new tree from two trees. Both operations
  assume that all values in [l] < all values in [r] and that [l] and [r]
  are valid:
    [glue l r]        Glues [l] and [r] together. Assumes that [l] and
                      [r] are already balanced with respect to each other.
    [merge l r]       Merges two trees and restores balance.

  Note: in contrast to Adam's paper, we use (<=) comparisons instead
  of (<) comparisons in [join], [merge] and [balance]. 
  Quickcheck (on [difference]) showed that this was necessary in order 
  to maintain the invariants. It is quite unsatisfactory that I haven't 
  been able to find out why this is actually the case! Fortunately, it 
  doesn't hurt to be a bit more conservative.
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  Join 
--------------------------------------------------------------------}
join :: Ord k => k -> a -> Map k a -> Map k a -> Map k a
join kx x Tip r  = insertMin kx x r
join kx x l Tip  = insertMax kx x l
join kx x l@(Bin sizeL ky y ly ry) r@(Bin sizeR kz z lz rz)
  | delta*sizeL < sizeR  = balanceL kz z (join kx x l lz) rz
  | delta*sizeR < sizeL  = balanceR ky y ly (join kx x ry r)
  | otherwise            = bin kx x l r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE join #-}
#endif


-- insertMin and insertMax don't perform potentially expensive comparisons.
insertMax,insertMin :: k -> a -> Map k a -> Map k a 
insertMax kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balanceR ky y l (insertMax kx x r)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertMax #-}
#endif

insertMin kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balanceL ky y (insertMin kx x l) r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertMin #-}
#endif

{--------------------------------------------------------------------
  [merge l r]: merges two trees.
--------------------------------------------------------------------}
merge :: Map k a -> Map k a -> Map k a
merge Tip r   = r
merge l Tip   = l
merge l@(Bin sizeL kx x lx rx) r@(Bin sizeR ky y ly ry)
  | delta*sizeL < sizeR = balanceL ky y (merge l ly) ry
  | delta*sizeR < sizeL = balanceR kx x lx (merge rx r)
  | otherwise           = glue l r
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE merge #-}
#endif

{--------------------------------------------------------------------
  [glue l r]: glues two trees together.
  Assumes that [l] and [r] are already balanced with respect to each other.
--------------------------------------------------------------------}
glue :: Map k a -> Map k a -> Map k a
glue Tip r = r
glue l Tip = l
glue l r   
  | size l > size r = let ((km,m),l') = deleteFindMax l in balanceR km m l' r
  | otherwise       = let ((km,m),r') = deleteFindMin r in balanceL km m l r'
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE glue #-}
#endif


-- | /O(log n)/. Delete and find the minimal element.
--
-- > deleteFindMin (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((3,"b"), fromList[(5,"a"), (10,"c")]) 
-- > deleteFindMin                                            Error: can not return the minimal element of an empty map

deleteFindMin :: Map k a -> ((k,a),Map k a)
deleteFindMin t 
  = case t of
      Bin _ k x Tip r -> ((k,x),r)
      Bin _ k x l r   -> let (km,l') = deleteFindMin l in (km,balanceR k x l' r)
      Tip             -> (error "Map.deleteFindMin: can not return the minimal element of an empty map", Tip)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE deleteFindMin #-}
#endif

-- | /O(log n)/. Delete and find the maximal element.
--
-- > deleteFindMax (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((10,"c"), fromList [(3,"b"), (5,"a")])
-- > deleteFindMax empty                                      Error: can not return the maximal element of an empty map

deleteFindMax :: Map k a -> ((k,a),Map k a)
deleteFindMax t
  = case t of
      Bin _ k x l Tip -> ((k,x),l)
      Bin _ k x l r   -> let (km,r') = deleteFindMax r in (km,balanceL k x l r')
      Tip             -> (error "Map.deleteFindMax: can not return the maximal element of an empty map", Tip)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE deleteFindMax #-}
#endif


{--------------------------------------------------------------------
  [balance l x r] balances two trees with value x.
  The sizes of the trees should balance after decreasing the
  size of one of them. (a rotation).

  [delta] is the maximal relative difference between the sizes of
          two trees, it corresponds with the [w] in Adams' paper.
  [ratio] is the ratio between an outer and inner sibling of the
          heavier subtree in an unbalanced setting. It determines
          whether a double or single rotation should be performed
          to restore balance. It is corresponds with the inverse
          of $\alpha$ in Adam's article.

  Note that according to the Adam's paper:
  - [delta] should be larger than 4.646 with a [ratio] of 2.
  - [delta] should be larger than 3.745 with a [ratio] of 1.534.

  But the Adam's paper is erroneous:
  - It can be proved that for delta=2 and delta>=5 there does
    not exist any ratio that would work.
  - Delta=4.5 and ratio=2 does not work.

  That leaves two reasonable variants, delta=3 and delta=4,
  both with ratio=2.

  - A lower [delta] leads to a more 'perfectly' balanced tree.
  - A higher [delta] performs less rebalancing.

  In the benchmarks, delta=3 is faster on insert operations,
  and delta=4 has slightly better deletes. As the insert speedup
  is larger, we currently use delta=3.

--------------------------------------------------------------------}
delta,ratio :: Int
delta = 3
ratio = 2

-- The balance function is equivalent to the following:
--
--   balance :: k -> a -> Map k a -> Map k a -> Map k a
--   balance k x l r
--     | sizeL + sizeR <= 1    = Bin sizeX k x l r
--     | sizeR > delta*sizeL   = rotateL k x l r
--     | sizeL > delta*sizeR   = rotateR k x l r
--     | otherwise             = Bin sizeX k x l r
--     where
--       sizeL = size l
--       sizeR = size r
--       sizeX = sizeL + sizeR + 1
--
--   rotateL :: a -> b -> Map a b -> Map a b -> Map a b
--   rotateL k x l r@(Bin _ _ _ ly ry) | size ly < ratio*size ry = singleL k x l r
--                                     | otherwise               = doubleL k x l r
--
--   rotateR :: a -> b -> Map a b -> Map a b -> Map a b
--   rotateR k x l@(Bin _ _ _ ly ry) r | size ry < ratio*size ly = singleR k x l r
--                                     | otherwise               = doubleR k x l r
--
--   singleL, singleR :: a -> b -> Map a b -> Map a b -> Map a b
--   singleL k1 x1 t1 (Bin _ k2 x2 t2 t3)  = bin k2 x2 (bin k1 x1 t1 t2) t3
--   singleR k1 x1 (Bin _ k2 x2 t1 t2) t3  = bin k2 x2 t1 (bin k1 x1 t2 t3)
--
--   doubleL, doubleR :: a -> b -> Map a b -> Map a b -> Map a b
--   doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
--   doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
--
-- It is only written in such a way that every node is pattern-matched only once.

balance :: k -> a -> Map k a -> Map k a -> Map k a
balance k x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x Tip r
           (Bin _ rk rx Tip rr@(Bin _ _ _ _ _)) -> Bin 3 rk rx (Bin 1 k x Tip Tip) rr
           (Bin _ rk rx (Bin _ rlk rlx _ _) Tip) -> Bin 3 rlk rlx (Bin 1 k x Tip Tip) (Bin 1 rk rx Tip Tip)
           (Bin rs rk rx rl@(Bin rls rlk rlx rll rlr) rr@(Bin rrs _ _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rk rx (Bin (1+rls) k x Tip rl) rr
             | otherwise -> Bin (1+rs) rlk rlx (Bin (1+size rll) k x Tip rll) (Bin (1+rrs+size rlr) rk rx rlr rr)

  (Bin ls lk lx ll lr) -> case r of
           Tip -> case (ll, lr) of
                    (Tip, Tip) -> Bin 2 k x l Tip
                    (Tip, (Bin _ lrk lrx _ _)) -> Bin 3 lrk lrx (Bin 1 lk lx Tip Tip) (Bin 1 k x Tip Tip)
                    ((Bin _ _ _ _ _), Tip) -> Bin 3 lk lx ll (Bin 1 k x Tip Tip)
                    ((Bin lls _ _ _ _), (Bin lrs lrk lrx lrl lrr))
                      | lrs < ratio*lls -> Bin (1+ls) lk lx ll (Bin (1+lrs) k x lr Tip)
                      | otherwise -> Bin (1+ls) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+size lrr) k x lrr Tip)
           (Bin rs rk rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (Bin rls rlk rlx rll rlr, Bin rrs _ _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rk rx (Bin (1+ls+rls) k x l rl) rr
                     | otherwise -> Bin (1+ls+rs) rlk rlx (Bin (1+ls+size rll) k x l rll) (Bin (1+rrs+size rlr) rk rx rlr rr)
                   (_, _) -> error "Failure in Data.Map.balance"
              | ls > delta*rs  -> case (ll, lr) of
                   (Bin lls _ _ _ _, Bin lrs lrk lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lk lx ll (Bin (1+rs+lrs) k x lr r)
                     | otherwise -> Bin (1+ls+rs) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+rs+size lrr) k x lrr r)
                   (_, _) -> error "Failure in Data.Map.balance"
              | otherwise -> Bin (1+ls+rs) k x l r
{-# NOINLINE balance #-}

-- Functions balanceL and balanceR are specialised versions of balance.
-- balanceL only checks whether the left subtree is too big,
-- balanceR only checks whether the right subtree is too big.

-- balanceL is called when left subtree might have been inserted to or when
-- right subtree might have been deleted from.
balanceL :: k -> a -> Map k a -> Map k a -> Map k a
balanceL k x l r = case r of
  Tip -> case l of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x l Tip
           (Bin _ lk lx Tip (Bin _ lrk lrx _ _)) -> Bin 3 lrk lrx (Bin 1 lk lx Tip Tip) (Bin 1 k x Tip Tip)
           (Bin _ lk lx ll@(Bin _ _ _ _ _) Tip) -> Bin 3 lk lx ll (Bin 1 k x Tip Tip)
           (Bin ls lk lx ll@(Bin lls _ _ _ _) lr@(Bin lrs lrk lrx lrl lrr))
             | lrs < ratio*lls -> Bin (1+ls) lk lx ll (Bin (1+lrs) k x lr Tip)
             | otherwise -> Bin (1+ls) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+size lrr) k x lrr Tip)

  (Bin rs _ _ _ _) -> case l of
           Tip -> Bin (1+rs) k x Tip r

           (Bin ls lk lx ll lr)
              | ls > delta*rs  -> case (ll, lr) of
                   (Bin lls _ _ _ _, Bin lrs lrk lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lk lx ll (Bin (1+rs+lrs) k x lr r)
                     | otherwise -> Bin (1+ls+rs) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+rs+size lrr) k x lrr r)
                   (_, _) -> error "Failure in Data.Map.balanceL"
              | otherwise -> Bin (1+ls+rs) k x l r
{-# NOINLINE balanceL #-}

-- balanceR is called when right subtree might have been inserted to or when
-- left subtree might have been deleted from.
balanceR :: k -> a -> Map k a -> Map k a -> Map k a
balanceR k x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x Tip r
           (Bin _ rk rx Tip rr@(Bin _ _ _ _ _)) -> Bin 3 rk rx (Bin 1 k x Tip Tip) rr
           (Bin _ rk rx (Bin _ rlk rlx _ _) Tip) -> Bin 3 rlk rlx (Bin 1 k x Tip Tip) (Bin 1 rk rx Tip Tip)
           (Bin rs rk rx rl@(Bin rls rlk rlx rll rlr) rr@(Bin rrs _ _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rk rx (Bin (1+rls) k x Tip rl) rr
             | otherwise -> Bin (1+rs) rlk rlx (Bin (1+size rll) k x Tip rll) (Bin (1+rrs+size rlr) rk rx rlr rr)

  (Bin ls _ _ _ _) -> case r of
           Tip -> Bin (1+ls) k x l Tip

           (Bin rs rk rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (Bin rls rlk rlx rll rlr, Bin rrs _ _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rk rx (Bin (1+ls+rls) k x l rl) rr
                     | otherwise -> Bin (1+ls+rs) rlk rlx (Bin (1+ls+size rll) k x l rll) (Bin (1+rrs+size rlr) rk rx rlr rr)
                   (_, _) -> error "Failure in Data.Map.balanceR"
              | otherwise -> Bin (1+ls+rs) k x l r
{-# NOINLINE balanceR #-}


{--------------------------------------------------------------------
  The bin constructor maintains the size of the tree
--------------------------------------------------------------------}
bin :: k -> a -> Map k a -> Map k a -> Map k a
bin k x l r
  = Bin (size l + size r + 1) k x l r
{-# INLINE bin #-}


{--------------------------------------------------------------------
  Eq converts the tree to a list. In a lazy setting, this 
  actually seems one of the faster methods to compare two trees 
  and it is certainly the simplest :-)
--------------------------------------------------------------------}
instance (Eq k,Eq a) => Eq (Map k a) where
  t1 == t2  = (size t1 == size t2) && (toAscList t1 == toAscList t2)

{--------------------------------------------------------------------
  Ord 
--------------------------------------------------------------------}

instance (Ord k, Ord v) => Ord (Map k v) where
    compare m1 m2 = compare (toAscList m1) (toAscList m2)

{--------------------------------------------------------------------
  Functor
--------------------------------------------------------------------}
instance Functor (Map k) where
  fmap f m  = map f m

instance Traversable (Map k) where
  traverse _ Tip = pure Tip
  traverse f (Bin s k v l r)
    = flip (Bin s k) <$> traverse f l <*> f v <*> traverse f r

instance Foldable.Foldable (Map k) where
  fold Tip = mempty
  fold (Bin _ _ v l r) = Foldable.fold l `mappend` v `mappend` Foldable.fold r
  foldr = foldr
  foldl = foldl
  foldMap _ Tip = mempty
  foldMap f (Bin _ _ v l r) = Foldable.foldMap f l `mappend` f v `mappend` Foldable.foldMap f r

instance (NFData k, NFData a) => NFData (Map k a) where
    rnf Tip = ()
    rnf (Bin _ kx x l r) = rnf kx `seq` rnf x `seq` rnf l `seq` rnf r

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance (Ord k, Read k, Read e) => Read (Map k e) where
#ifdef __GLASGOW_HASKELL__
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)

  readListPrec = readListPrecDefault
#else
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    return (fromList xs,t)
#endif

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance (Show k, Show a) => Show (Map k a) where
  showsPrec d m  = showParen (d > 10) $
    showString "fromList " . shows (toList m)

-- | /O(n)/. Show the tree that implements the map. The tree is shown
-- in a compressed, hanging format. See 'showTreeWith'.
showTree :: (Show k,Show a) => Map k a -> String
showTree m
  = showTreeWith showElem True False m
  where
    showElem k x  = show k ++ ":=" ++ show x


{- | /O(n)/. The expression (@'showTreeWith' showelem hang wide map@) shows
 the tree that implements the map. Elements are shown using the @showElem@ function. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.

>  Map> let t = fromDistinctAscList [(x,()) | x <- [1..5]]
>  Map> putStrLn $ showTreeWith (\k x -> show (k,x)) True False t
>  (4,())
>  +--(2,())
>  |  +--(1,())
>  |  +--(3,())
>  +--(5,())
>
>  Map> putStrLn $ showTreeWith (\k x -> show (k,x)) True True t
>  (4,())
>  |
>  +--(2,())
>  |  |
>  |  +--(1,())
>  |  |
>  |  +--(3,())
>  |
>  +--(5,())
>
>  Map> putStrLn $ showTreeWith (\k x -> show (k,x)) False True t
>  +--(5,())
>  |
>  (4,())
>  |
>  |  +--(3,())
>  |  |
>  +--(2,())
>     |
>     +--(1,())

-}
showTreeWith :: (k -> a -> String) -> Bool -> Bool -> Map k a -> String
showTreeWith showelem hang wide t
  | hang      = (showsTreeHang showelem wide [] t) ""
  | otherwise = (showsTree showelem wide [] [] t) ""

showsTree :: (k -> a -> String) -> Bool -> [String] -> [String] -> Map k a -> ShowS
showsTree showelem wide lbars rbars t
  = case t of
      Tip -> showsBars lbars . showString "|\n"
      Bin _ kx x Tip Tip
          -> showsBars lbars . showString (showelem kx x) . showString "\n" 
      Bin _ kx x l r
          -> showsTree showelem wide (withBar rbars) (withEmpty rbars) r .
             showWide wide rbars .
             showsBars lbars . showString (showelem kx x) . showString "\n" .
             showWide wide lbars .
             showsTree showelem wide (withEmpty lbars) (withBar lbars) l

showsTreeHang :: (k -> a -> String) -> Bool -> [String] -> Map k a -> ShowS
showsTreeHang showelem wide bars t
  = case t of
      Tip -> showsBars bars . showString "|\n" 
      Bin _ kx x Tip Tip
          -> showsBars bars . showString (showelem kx x) . showString "\n" 
      Bin _ kx x l r
          -> showsBars bars . showString (showelem kx x) . showString "\n" . 
             showWide wide bars .
             showsTreeHang showelem wide (withBar bars) l .
             showWide wide bars .
             showsTreeHang showelem wide (withEmpty bars) r

showWide :: Bool -> [String] -> String -> String
showWide wide bars 
  | wide      = showString (concat (reverse bars)) . showString "|\n" 
  | otherwise = id

showsBars :: [String] -> ShowS
showsBars bars
  = case bars of
      [] -> id
      _  -> showString (concat (reverse (tail bars))) . showString node

node :: String
node           = "+--"

withBar, withEmpty :: [String] -> [String]
withBar bars   = "|  ":bars
withEmpty bars = "   ":bars

{--------------------------------------------------------------------
  Typeable
--------------------------------------------------------------------}

#include "Typeable.h"
INSTANCE_TYPEABLE2(Map,mapTc,"Map")

{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}
-- | /O(n)/. Test if the internal map structure is valid.
--
-- > valid (fromAscList [(3,"b"), (5,"a")]) == True
-- > valid (fromAscList [(5,"a"), (3,"b")]) == False

valid :: Ord k => Map k a -> Bool
valid t
  = balanced t && ordered t && validsize t

ordered :: Ord a => Map a b -> Bool
ordered t
  = bounded (const True) (const True) t
  where
    bounded lo hi t'
      = case t' of
          Tip              -> True
          Bin _ kx _ l r  -> (lo kx) && (hi kx) && bounded lo (<kx) l && bounded (>kx) hi r

-- | Exported only for "Debug.QuickCheck"
balanced :: Map k a -> Bool
balanced t
  = case t of
      Tip            -> True
      Bin _ _ _ l r  -> (size l + size r <= 1 || (size l <= delta*size r && size r <= delta*size l)) &&
                        balanced l && balanced r

validsize :: Map a b -> Bool
validsize t
  = (realsize t == Just (size t))
  where
    realsize t'
      = case t' of
          Tip            -> Just 0
          Bin sz _ _ l r -> case (realsize l,realsize r) of
                            (Just n,Just m)  | n+m+1 == sz  -> Just sz
                            _                               -> Nothing

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}
foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f = go
  where
    go z []     = z
    go z (x:xs) = let z' = f z x in z' `seq` go z' xs
{-# INLINE foldlStrict #-}
