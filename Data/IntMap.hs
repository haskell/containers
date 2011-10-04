{-# LANGUAGE NoBangPatterns, ScopedTypeVariables #-}
#if !defined(TESTING) && __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMap
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of maps from integer keys to values.
--
-- Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.IntMap (IntMap)
-- >  import qualified Data.IntMap as IntMap
--
-- The implementation is based on /big-endian patricia trees/.  This data
-- structure performs especially well on binary operations like 'union'
-- and 'intersection'.  However, my benchmarks show that it is also
-- (much) faster on insertions and deletions when compared to a generic
-- size-balanced map implementation (see "Data.Map").
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--      Workshop on ML, September 1998, pages 77-86,
--      <http://citeseer.ist.psu.edu/okasaki98fast.html>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve
--      Information Coded In Alphanumeric/\", Journal of the ACM, 15(4),
--      October 1968, pages 514-534.
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation <http://en.wikipedia.org/wiki/Big_O_notation>.
-- Many operations have a worst-case complexity of /O(min(n,W))/.
-- This means that the operation can become linear in the number of
-- elements with a maximum of /W/ -- the number of bits in an 'Int'
-- (32 or 64).
-----------------------------------------------------------------------------

-- It is essential that the bit fiddling functions like mask, zero, branchMask
-- etc are inlined. If they do not, the memory allocation skyrockets. The GHC
-- usually gets it right, but it is disastrous if it does not. Therefore we
-- explicitly mark these functions INLINE.

module Data.IntMap (
            -- * Map type
#if !defined(TESTING)
              IntMap, Key          -- instance Eq,Show
#else
              IntMap(..), Key          -- instance Eq,Show
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
            ) where

import Prelude hiding (lookup,map,filter,foldr,foldl,null)
import Data.Bits 
import qualified Data.IntSet as IntSet
import Data.Monoid (Monoid(..))
import Data.Maybe (fromMaybe)
import Data.Typeable
import qualified Data.Foldable as Foldable
import Data.Traversable (Traversable(traverse))
import Control.Applicative (Applicative(pure,(<*>)),(<$>))
import Control.Monad ( liftM )
import Control.DeepSeq (NFData(rnf))
{-
-- just for testing
import qualified Prelude
import Test.QuickCheck 
import List (nub,sort)
import qualified List
-}  

#if __GLASGOW_HASKELL__
import Text.Read
import Data.Data (Data(..), mkNoRepType)
#endif

#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts ( Word(..), Int(..), shiftRL# )
#elif __GLASGOW_HASKELL__
import Word
import GlaExts ( Word(..), Int(..), shiftRL# )
#else
import Data.Word
#endif

-- Use macros to define strictness of functions.
-- STRICT_x_OF_y denotes an y-ary function strict in the x-th parameter.
-- We do not use BangPatterns, because they are not in any standard and we
-- want the compilers to be compiled by as many compilers as possible.
#define STRICT_1_OF_2(fn) fn arg _ | arg `seq` False = undefined

infixl 9 \\{-This comment teaches CPP correct behaviour -}

-- A "Nat" is a natural machine word (an unsigned Int)
type Nat = Word

natFromInt :: Key -> Nat
natFromInt = fromIntegral
{-# INLINE natFromInt #-}

intFromNat :: Nat -> Key
intFromNat = fromIntegral
{-# INLINE intFromNat #-}

shiftRL :: Nat -> Key -> Nat
#if __GLASGOW_HASKELL__
{--------------------------------------------------------------------
  GHC: use unboxing to get @shiftRL@ inlined.
--------------------------------------------------------------------}
shiftRL (W# x) (I# i)
  = W# (shiftRL# x i)
#else
shiftRL x i   = shiftR x i
{-# INLINE shiftRL #-}
#endif

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}

-- | /O(min(n,W))/. Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'

(!) :: IntMap a -> Key -> a
m ! k    = find k m

-- | Same as 'difference'.
(\\) :: IntMap a -> IntMap b -> IntMap a
m1 \\ m2 = difference m1 m2

{--------------------------------------------------------------------
  Types  
--------------------------------------------------------------------}

-- The order of constructors of IntMap matters when considering performance.
-- Currently in GHC 7.0, when type has 3 constructors, they are matched from
-- the first to the last -- the best performance is achieved when the
-- constructors are ordered by frequency.
-- On GHC 7.0, reordering constructors from Nil | Tip | Bin to Bin | Tip | Nil
-- improves the containers_benchmark by 9.5% on x86 and by 8% on x86_64.

-- | A map of integers to values @a@.
data IntMap a = Bin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask !(IntMap a) !(IntMap a)
              | Tip {-# UNPACK #-} !Key a
              | Nil

type Prefix = Int
type Mask   = Int
type Key    = Int

instance Monoid (IntMap a) where
    mempty  = empty
    mappend = union
    mconcat = unions

instance Foldable.Foldable IntMap where
  fold Nil = mempty
  fold (Tip _ v) = v
  fold (Bin _ _ l r) = Foldable.fold l `mappend` Foldable.fold r
  foldr = foldr
  foldl = foldl
  foldMap _ Nil = mempty
  foldMap f (Tip _k v) = f v
  foldMap f (Bin _ _ l r) = Foldable.foldMap f l `mappend` Foldable.foldMap f r

instance Traversable IntMap where
    traverse _ Nil = pure Nil
    traverse f (Tip k v) = Tip k <$> f v
    traverse f (Bin p m l r) = Bin p m <$> traverse f l <*> traverse f r

instance NFData a => NFData (IntMap a) where
    rnf Nil = ()
    rnf (Tip _ v) = rnf v
    rnf (Bin _ _ l r) = rnf l `seq` rnf r

#if __GLASGOW_HASKELL__

{--------------------------------------------------------------------
  A Data instance  
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We omit reflection services for the sake of data abstraction.

instance Data a => Data (IntMap a) where
  gfoldl f z im = z fromList `f` (toList im)
  toConstr _    = error "toConstr"
  gunfold _ _   = error "gunfold"
  dataTypeOf _  = mkNoRepType "Data.IntMap.IntMap"
  dataCast1 f   = gcast1 f

#endif

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the map empty?
--
-- > Data.IntMap.null (empty)           == True
-- > Data.IntMap.null (singleton 1 'a') == False

null :: IntMap a -> Bool
null Nil = True
null _   = False

-- | /O(n)/. Number of elements in the map.
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3
size :: IntMap a -> Int
size t
  = case t of
      Bin _ _ l r -> size l + size r
      Tip _ _ -> 1
      Nil     -> 0

-- | /O(min(n,W))/. Is the key a member of the map?
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False

member :: Key -> IntMap a -> Bool
member k m
  = case lookup k m of
      Nothing -> False
      Just _  -> True

-- | /O(log n)/. Is the key not a member of the map?
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True

notMember :: Key -> IntMap a -> Bool
notMember k m = not $ member k m

-- The 'go' function in the lookup causes 10% speedup, but also an increased
-- memory allocation. It does not cause speedup with other methods like insert
-- and delete, so it is present only in lookup.

-- | /O(min(n,W))/. Lookup the value at a key in the map. See also 'Data.Map.lookup'.
lookup :: Key -> IntMap a -> Maybe a
lookup k = k `seq` go
  where
    go (Bin _ m l r)
      | zero k m  = go l
      | otherwise = go r
    go (Tip kx x)
      | k == kx   = Just x
      | otherwise = Nothing
    go Nil      = Nothing


find :: Key -> IntMap a -> a
find k m
  = case lookup k m of
      Nothing -> error ("IntMap.find: key " ++ show k ++ " is not an element of the map")
      Just x  -> x

-- | /O(min(n,W))/. The expression @('findWithDefault' def k map)@
-- returns the value at key @k@ or returns @def@ when the key is not an
-- element of the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'

findWithDefault :: a -> Key -> IntMap a -> a
findWithDefault def k m
  = case lookup k m of
      Nothing -> def
      Just x  -> x

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty map.
--
-- > empty      == fromList []
-- > size empty == 0

empty :: IntMap a
empty
  = Nil

-- | /O(1)/. A map of one element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1

singleton :: Key -> a -> IntMap a
singleton k x
  = Tip k x

{--------------------------------------------------------------------
  Insert
--------------------------------------------------------------------}
-- | /O(min(n,W))/. Insert a new key\/value pair in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value, i.e. 'insert' is equivalent to
-- @'insertWith' 'const'@.
--
-- > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
-- > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
-- > insert 5 'x' empty                         == singleton 5 'x'

insert :: Key -> a -> IntMap a -> IntMap a
insert k x t = k `seq`
  case t of
    Bin p m l r
      | nomatch k p m -> join k (Tip k x) p t
      | zero k m      -> Bin p m (insert k x l) r
      | otherwise     -> Bin p m l (insert k x r)
    Tip ky _
      | k==ky         -> Tip k x
      | otherwise     -> join k (Tip k x) ky t
    Nil -> Tip k x

-- right-biased insertion, used by 'union'
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
insertWith f k x t
  = insertWithKey (\_ x' y' -> f x' y') k x t

-- | Same as 'insertWith', but the combining function is applied strictly.
insertWith' :: (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWith' f k x t
  = insertWithKey' (\_ x' y' -> f x' y') k x t

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
insertWithKey f k x t = k `seq`
  case t of
    Bin p m l r
      | nomatch k p m -> join k (Tip k x) p t
      | zero k m      -> Bin p m (insertWithKey f k x l) r
      | otherwise     -> Bin p m l (insertWithKey f k x r)
    Tip ky y
      | k==ky         -> Tip k (f k x y)
      | otherwise     -> join k (Tip k x) ky t
    Nil -> Tip k x

-- | Same as 'insertWithKey', but the combining function is applied strictly.
insertWithKey' :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWithKey' f k x t = k `seq`
    case t of
      Bin p m l r
        | nomatch k p m -> join k (Tip k x) p t
        | zero k m      -> Bin p m (insertWithKey' f k x l) r
        | otherwise     -> Bin p m l (insertWithKey' f k x r)
      Tip ky y
        | k==ky         -> let x' = f k x y in seq x' (Tip k x')
        | otherwise     -> join k (Tip k x) ky t
      Nil -> Tip k x

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
insertLookupWithKey f k x t = k `seq`
  case t of
    Bin p m l r
      | nomatch k p m -> (Nothing,join k (Tip k x) p t)
      | zero k m      -> let (found,l') = insertLookupWithKey f k x l in (found,Bin p m l' r)
      | otherwise     -> let (found,r') = insertLookupWithKey f k x r in (found,Bin p m l r')
    Tip ky y
      | k==ky         -> (Just y,Tip k (f k x y))
      | otherwise     -> (Nothing,join k (Tip k x) ky t)
    Nil -> (Nothing,Tip k x)


{--------------------------------------------------------------------
  Deletion
  [delete] is the inlined version of [deleteWith (\k x -> Nothing)]
--------------------------------------------------------------------}
-- | /O(min(n,W))/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
--
-- > delete 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > delete 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > delete 5 empty                         == empty

delete :: Key -> IntMap a -> IntMap a
delete k t = k `seq`
  case t of
    Bin p m l r
      | nomatch k p m -> t
      | zero k m      -> bin p m (delete k l) r
      | otherwise     -> bin p m l (delete k r)
    Tip ky _
      | k==ky         -> Nil
      | otherwise     -> t
    Nil -> Nil

-- | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty

adjust ::  (a -> a) -> Key -> IntMap a -> IntMap a
adjust f k m
  = adjustWithKey (\_ x -> f x) k m

-- | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty

adjustWithKey ::  (Key -> a -> a) -> Key -> IntMap a -> IntMap a
adjustWithKey f
  = updateWithKey (\k' x -> Just (f k' x))

-- | /O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

update ::  (a -> Maybe a) -> Key -> IntMap a -> IntMap a
update f
  = updateWithKey (\_ x -> f x)

-- | /O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f k x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateWithKey ::  (Key -> a -> Maybe a) -> Key -> IntMap a -> IntMap a
updateWithKey f k t = k `seq`
  case t of
    Bin p m l r
      | nomatch k p m -> t
      | zero k m      -> bin p m (updateWithKey f k l) r
      | otherwise     -> bin p m l (updateWithKey f k r)
    Tip ky y
      | k==ky         -> case (f k y) of
                           Just y' -> Tip ky y'
                           Nothing -> Nil
      | otherwise     -> t
    Nil -> Nil

-- | /O(min(n,W))/. Lookup and update.
-- The function returns original value, if it is updated.
-- This is different behavior than 'Data.Map.updateLookupWithKey'.
-- Returns the original key value if the map entry is deleted.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:new a")])
-- > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")

updateLookupWithKey ::  (Key -> a -> Maybe a) -> Key -> IntMap a -> (Maybe a,IntMap a)
updateLookupWithKey f k t = k `seq`
  case t of
    Bin p m l r
      | nomatch k p m -> (Nothing,t)
      | zero k m      -> let (found,l') = updateLookupWithKey f k l in (found,bin p m l' r)
      | otherwise     -> let (found,r') = updateLookupWithKey f k r in (found,bin p m l r')
    Tip ky y
      | k==ky         -> case (f k y) of
                           Just y' -> (Just y,Tip ky y')
                           Nothing -> (Just y,Nil)
      | otherwise     -> (Nothing,t)
    Nil -> (Nothing,Nil)



-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in an 'IntMap'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: (Maybe a -> Maybe a) -> Key -> IntMap a -> IntMap a
alter f k t = k `seq`
  case t of
    Bin p m l r
      | nomatch k p m -> case f Nothing of
                           Nothing -> t
                           Just x -> join k (Tip k x) p t
      | zero k m      -> bin p m (alter f k l) r
      | otherwise     -> bin p m l (alter f k r)
    Tip ky y
      | k==ky         -> case f (Just y) of
                           Just x -> Tip ky x
                           Nothing -> Nil
      | otherwise     -> case f Nothing of
                           Just x -> join k (Tip k x) ky t
                           Nothing -> Tip ky y
    Nil               -> case f Nothing of
                           Just x -> Tip k x
                           Nothing -> Nil


{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
-- | The union of a list of maps.
--
-- > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "b"), (5, "a"), (7, "C")]
-- > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
-- >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]

unions :: [IntMap a] -> IntMap a
unions xs
  = foldlStrict union empty xs

-- | The union of a list of maps, with a combining operation.
--
-- > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]

unionsWith :: (a->a->a) -> [IntMap a] -> IntMap a
unionsWith f ts
  = foldlStrict (unionWith f) empty ts

-- | /O(n+m)/. The (left-biased) union of two maps.
-- It prefers the first map when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
--
-- > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]

union :: IntMap a -> IntMap a -> IntMap a
union t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = union1
  | shorter m2 m1  = union2
  | p1 == p2       = Bin p1 m1 (union l1 l2) (union r1 r2)
  | otherwise      = join p1 t1 p2 t2
  where
    union1  | nomatch p2 p1 m1  = join p1 t1 p2 t2
            | zero p2 m1        = Bin p1 m1 (union l1 t2) r1
            | otherwise         = Bin p1 m1 l1 (union r1 t2)

    union2  | nomatch p1 p2 m2  = join p1 t1 p2 t2
            | zero p1 m2        = Bin p2 m2 (union t1 l2) r2
            | otherwise         = Bin p2 m2 l2 (union t1 r2)

union (Tip k x) t = insert k x t
union t (Tip k x) = insertWith (\_ y -> y) k x t  -- right bias
union Nil t       = t
union t Nil       = t

-- | /O(n+m)/. The union with a combining function.
--
-- > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]

unionWith :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWith f m1 m2
  = unionWithKey (\_ x y -> f x y) m1 m2

-- | /O(n+m)/. The union with a combining function.
--
-- > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]

unionWithKey :: (Key -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWithKey f t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = union1
  | shorter m2 m1  = union2
  | p1 == p2       = Bin p1 m1 (unionWithKey f l1 l2) (unionWithKey f r1 r2)
  | otherwise      = join p1 t1 p2 t2
  where
    union1  | nomatch p2 p1 m1  = join p1 t1 p2 t2
            | zero p2 m1        = Bin p1 m1 (unionWithKey f l1 t2) r1
            | otherwise         = Bin p1 m1 l1 (unionWithKey f r1 t2)

    union2  | nomatch p1 p2 m2  = join p1 t1 p2 t2
            | zero p1 m2        = Bin p2 m2 (unionWithKey f t1 l2) r2
            | otherwise         = Bin p2 m2 l2 (unionWithKey f t1 r2)

unionWithKey f (Tip k x) t = insertWithKey f k x t
unionWithKey f t (Tip k x) = insertWithKey (\k' x' y' -> f k' y' x') k x t  -- right bias
unionWithKey _ Nil t  = t
unionWithKey _ t Nil  = t

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | /O(n+m)/. Difference between two maps (based on keys).
--
-- > difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 3 "b"

difference :: IntMap a -> IntMap b -> IntMap a
difference t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = difference1
  | shorter m2 m1  = difference2
  | p1 == p2       = bin p1 m1 (difference l1 l2) (difference r1 r2)
  | otherwise      = t1
  where
    difference1 | nomatch p2 p1 m1  = t1
                | zero p2 m1        = bin p1 m1 (difference l1 t2) r1
                | otherwise         = bin p1 m1 l1 (difference r1 t2)

    difference2 | nomatch p1 p2 m2  = t1
                | zero p1 m2        = difference t1 l2
                | otherwise         = difference t1 r2

difference t1@(Tip k _) t2
  | member k t2  = Nil
  | otherwise    = t1

difference Nil _       = Nil
difference t (Tip k _) = delete k t
difference t Nil       = t

-- | /O(n+m)/. Difference with a combining function.
--
-- > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- > differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
-- >     == singleton 3 "b:B"

differenceWith :: (a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
differenceWith f m1 m2
  = differenceWithKey (\_ x y -> f x y) m1 m2

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference).
-- If it returns (@'Just' y@), the element is updated with a new value @y@. 
--
-- > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- > differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
-- >     == singleton 3 "3:b|B"

differenceWithKey :: (Key -> a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
differenceWithKey f t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = difference1
  | shorter m2 m1  = difference2
  | p1 == p2       = bin p1 m1 (differenceWithKey f l1 l2) (differenceWithKey f r1 r2)
  | otherwise      = t1
  where
    difference1 | nomatch p2 p1 m1  = t1
                | zero p2 m1        = bin p1 m1 (differenceWithKey f l1 t2) r1
                | otherwise         = bin p1 m1 l1 (differenceWithKey f r1 t2)

    difference2 | nomatch p1 p2 m2  = t1
                | zero p1 m2        = differenceWithKey f t1 l2
                | otherwise         = differenceWithKey f t1 r2

differenceWithKey f t1@(Tip k x) t2 
  = case lookup k t2 of
      Just y  -> case f k x y of
                   Just y' -> Tip k y'
                   Nothing -> Nil
      Nothing -> t1

differenceWithKey _ Nil _       = Nil
differenceWithKey f t (Tip k y) = updateWithKey (\k' x -> f k' x y) k t
differenceWithKey _ t Nil       = t


{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O(n+m)/. The (left-biased) intersection of two maps (based on keys).
--
-- > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"

intersection :: IntMap a -> IntMap b -> IntMap a
intersection t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = intersection1
  | shorter m2 m1  = intersection2
  | p1 == p2       = bin p1 m1 (intersection l1 l2) (intersection r1 r2)
  | otherwise      = Nil
  where
    intersection1 | nomatch p2 p1 m1  = Nil
                  | zero p2 m1        = intersection l1 t2
                  | otherwise         = intersection r1 t2

    intersection2 | nomatch p1 p2 m2  = Nil
                  | zero p1 m2        = intersection t1 l2
                  | otherwise         = intersection t1 r2

intersection t1@(Tip k _) t2
  | member k t2  = t1
  | otherwise    = Nil
intersection t (Tip k _)
  = case lookup k t of
      Just y  -> Tip k y
      Nothing -> Nil
intersection Nil _ = Nil
intersection _ Nil = Nil

-- | /O(n+m)/. The intersection with a combining function.
--
-- > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"

intersectionWith :: (a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWith f m1 m2
  = intersectionWithKey (\_ x y -> f x y) m1 m2

-- | /O(n+m)/. The intersection with a combining function.
--
-- > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- > intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "5:a|A"

intersectionWithKey :: (Key -> a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWithKey f t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = intersection1
  | shorter m2 m1  = intersection2
  | p1 == p2       = bin p1 m1 (intersectionWithKey f l1 l2) (intersectionWithKey f r1 r2)
  | otherwise      = Nil
  where
    intersection1 | nomatch p2 p1 m1  = Nil
                  | zero p2 m1        = intersectionWithKey f l1 t2
                  | otherwise         = intersectionWithKey f r1 t2

    intersection2 | nomatch p1 p2 m2  = Nil
                  | zero p1 m2        = intersectionWithKey f t1 l2
                  | otherwise         = intersectionWithKey f t1 r2

intersectionWithKey f (Tip k x) t2
  = case lookup k t2 of
      Just y  -> Tip k (f k x y)
      Nothing -> Nil
intersectionWithKey f t1 (Tip k y) 
  = case lookup k t1 of
      Just x  -> Tip k (f k x y)
      Nothing -> Nil
intersectionWithKey _ Nil _ = Nil
intersectionWithKey _ _ Nil = Nil


{--------------------------------------------------------------------
  Min\/Max
--------------------------------------------------------------------}

-- | /O(log n)/. Update the value at the minimal key.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMinWithKey :: (Key -> a -> a) -> IntMap a -> IntMap a
updateMinWithKey f t
    = case t of
        Bin p m l r | m < 0 -> let t' = updateMinWithKeyUnsigned f r in Bin p m l t'
        Bin p m l r         -> let t' = updateMinWithKeyUnsigned f l in Bin p m t' r
        Tip k y -> Tip k (f k y)
        Nil -> error "maxView: empty map has no maximal element"

updateMinWithKeyUnsigned :: (Key -> a -> a) -> IntMap a -> IntMap a
updateMinWithKeyUnsigned f t
    = case t of
        Bin p m l r -> let t' = updateMinWithKeyUnsigned f l in Bin p m t' r
        Tip k y -> Tip k (f k y)
        Nil -> error "updateMinWithKeyUnsigned Nil"

-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMaxWithKey :: (Key -> a -> a) -> IntMap a -> IntMap a
updateMaxWithKey f t
    = case t of
        Bin p m l r | m < 0 -> let t' = updateMaxWithKeyUnsigned f l in Bin p m t' r
        Bin p m l r         -> let t' = updateMaxWithKeyUnsigned f r in Bin p m l t'
        Tip k y -> Tip k (f k y)
        Nil -> error "maxView: empty map has no maximal element"

updateMaxWithKeyUnsigned :: (Key -> a -> a) -> IntMap a -> IntMap a
updateMaxWithKeyUnsigned f t
    = case t of
        Bin p m l r -> let t' = updateMaxWithKeyUnsigned f r in Bin p m l t'
        Tip k y -> Tip k (f k y)
        Nil -> error "updateMaxWithKeyUnsigned Nil"


-- | /O(log n)/. Retrieves the maximal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
-- > maxViewWithKey empty == Nothing

maxViewWithKey :: IntMap a -> Maybe ((Key, a), IntMap a)
maxViewWithKey t
    = case t of
        Bin p m l r | m < 0 -> let (result, t') = maxViewUnsigned l in Just (result, bin p m t' r)
        Bin p m l r         -> let (result, t') = maxViewUnsigned r in Just (result, bin p m l t')
        Tip k y -> Just ((k,y), Nil)
        Nil -> Nothing

maxViewUnsigned :: IntMap a -> ((Key, a), IntMap a)
maxViewUnsigned t
    = case t of
        Bin p m l r -> let (result,t') = maxViewUnsigned r in (result,bin p m l t')
        Tip k y -> ((k,y), Nil)
        Nil -> error "maxViewUnsigned Nil"

-- | /O(log n)/. Retrieves the minimal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
-- > minViewWithKey empty == Nothing

minViewWithKey :: IntMap a -> Maybe ((Key, a), IntMap a)
minViewWithKey t
    = case t of
        Bin p m l r | m < 0 -> let (result, t') = minViewUnsigned r in Just (result, bin p m l t')
        Bin p m l r         -> let (result, t') = minViewUnsigned l in Just (result, bin p m t' r)
        Tip k y -> Just ((k,y),Nil)
        Nil -> Nothing

minViewUnsigned :: IntMap a -> ((Key, a), IntMap a)
minViewUnsigned t
    = case t of
        Bin p m l r -> let (result,t') = minViewUnsigned l in (result,bin p m t' r)
        Tip k y -> ((k,y),Nil)
        Nil -> error "minViewUnsigned Nil"


-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMax :: (a -> a) -> IntMap a -> IntMap a
updateMax f = updateMaxWithKey (const f)

-- | /O(log n)/. Update the value at the minimal key.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMin :: (a -> a) -> IntMap a -> IntMap a
updateMin f = updateMinWithKey (const f)

-- Similar to the Arrow instance.
first :: (a -> c) -> (a, b) -> (c, b)
first f (x,y) = (f x,y)

-- | /O(log n)/. Retrieves the maximal key of the map, and the map
-- stripped of that element, or 'Nothing' if passed an empty map.
maxView :: IntMap a -> Maybe (a, IntMap a)
maxView t = liftM (first snd) (maxViewWithKey t)

-- | /O(log n)/. Retrieves the minimal key of the map, and the map
-- stripped of that element, or 'Nothing' if passed an empty map.
minView :: IntMap a -> Maybe (a, IntMap a)
minView t = liftM (first snd) (minViewWithKey t)

-- | /O(log n)/. Delete and find the maximal element.
deleteFindMax :: IntMap a -> (a, IntMap a)
deleteFindMax = fromMaybe (error "deleteFindMax: empty map has no maximal element") . maxView

-- | /O(log n)/. Delete and find the minimal element.
deleteFindMin :: IntMap a -> (a, IntMap a)
deleteFindMin = fromMaybe (error "deleteFindMin: empty map has no minimal element") . minView

-- | /O(log n)/. The minimal key of the map.
findMin :: IntMap a -> (Key, a)
findMin Nil = error $ "findMin: empty map has no minimal element"
findMin (Tip k v) = (k,v)
findMin (Bin _ m l r)
  |   m < 0   = go r
  | otherwise = go l
    where go (Tip k v)      = (k,v)
          go (Bin _ _ l' _) = go l'
          go Nil            = error "findMax Nil"

-- | /O(log n)/. The maximal key of the map.
findMax :: IntMap a -> (Key, a)
findMax Nil = error $ "findMax: empty map has no maximal element"
findMax (Tip k v) = (k,v)
findMax (Bin _ m l r)
  |   m < 0   = go l
  | otherwise = go r
    where go (Tip k v)      = (k,v)
          go (Bin _ _ _ r') = go r'
          go Nil            = error "findMax Nil"

-- | /O(log n)/. Delete the minimal key. An error is thrown if the IntMap is already empty.
-- Note, this is not the same behavior Map.
deleteMin :: IntMap a -> IntMap a
deleteMin = maybe (error "deleteMin: empty map has no minimal element") snd . minView

-- | /O(log n)/. Delete the maximal key. An error is thrown if the IntMap is already empty.
-- Note, this is not the same behavior Map.
deleteMax :: IntMap a -> IntMap a
deleteMax = maybe (error "deleteMax: empty map has no maximal element") snd . maxView


{--------------------------------------------------------------------
  Submap
--------------------------------------------------------------------}
-- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal). 
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: Eq a => IntMap a -> IntMap a -> Bool
isProperSubmapOf m1 m2
  = isProperSubmapOfBy (==) m1 m2

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
isProperSubmapOfBy :: (a -> b -> Bool) -> IntMap a -> IntMap b -> Bool
isProperSubmapOfBy predicate t1 t2
  = case submapCmp predicate t1 t2 of
      LT -> True
      _  -> False

submapCmp :: (a -> b -> Bool) -> IntMap a -> IntMap b -> Ordering
submapCmp predicate t1@(Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  | shorter m1 m2  = GT
  | shorter m2 m1  = submapCmpLt
  | p1 == p2       = submapCmpEq
  | otherwise      = GT  -- disjoint
  where
    submapCmpLt | nomatch p1 p2 m2  = GT
                | zero p1 m2        = submapCmp predicate t1 l2
                | otherwise         = submapCmp predicate t1 r2
    submapCmpEq = case (submapCmp predicate l1 l2, submapCmp predicate r1 r2) of
                    (GT,_ ) -> GT
                    (_ ,GT) -> GT
                    (EQ,EQ) -> EQ
                    _       -> LT

submapCmp _         (Bin _ _ _ _) _  = GT
submapCmp predicate (Tip kx x) (Tip ky y)
  | (kx == ky) && predicate x y = EQ
  | otherwise                   = GT  -- disjoint
submapCmp predicate (Tip k x) t
  = case lookup k t of
     Just y | predicate x y -> LT
     _                      -> GT -- disjoint
submapCmp _    Nil Nil = EQ
submapCmp _    Nil _   = LT

-- | /O(n+m)/. Is this a submap?
-- Defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
isSubmapOf :: Eq a => IntMap a -> IntMap a -> Bool
isSubmapOf m1 m2
  = isSubmapOfBy (==) m1 m2

{- | /O(n+m)/.
 The expression (@'isSubmapOfBy' f m1 m2@) returns 'True' if
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following 
 expressions are all 'True':
 
  > isSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])

 But the following are all 'False':
 
  > isSubmapOfBy (==) (fromList [(1,2)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (<) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
-}
isSubmapOfBy :: (a -> b -> Bool) -> IntMap a -> IntMap b -> Bool
isSubmapOfBy predicate t1@(Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  | shorter m1 m2  = False
  | shorter m2 m1  = match p1 p2 m2 && (if zero p1 m2 then isSubmapOfBy predicate t1 l2
                                                      else isSubmapOfBy predicate t1 r2)                     
  | otherwise      = (p1==p2) && isSubmapOfBy predicate l1 l2 && isSubmapOfBy predicate r1 r2
isSubmapOfBy _         (Bin _ _ _ _) _ = False
isSubmapOfBy predicate (Tip k x) t     = case lookup k t of
                                         Just y  -> predicate x y
                                         Nothing -> False
isSubmapOfBy _         Nil _           = True

{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}
-- | /O(n)/. Map a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]

map :: (a -> b) -> IntMap a -> IntMap b
map f = mapWithKey (\_ x -> f x)

-- | /O(n)/. Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]

mapWithKey :: (Key -> a -> b) -> IntMap a -> IntMap b
mapWithKey f t  
  = case t of
      Bin p m l r -> Bin p m (mapWithKey f l) (mapWithKey f r)
      Tip k x     -> Tip k (f k x)
      Nil         -> Nil

-- | /O(n)/. The function @'mapAccum'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])

mapAccum :: (a -> b -> (a,c)) -> a -> IntMap b -> (a,IntMap c)
mapAccum f = mapAccumWithKey (\a' _ x -> f a' x)

-- | /O(n)/. The function @'mapAccumWithKey'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])

mapAccumWithKey :: (a -> Key -> b -> (a,c)) -> a -> IntMap b -> (a,IntMap c)
mapAccumWithKey f a t
  = mapAccumL f a t

-- | /O(n)/. The function @'mapAccumL'@ threads an accumulating
-- argument through the map in ascending order of keys.
mapAccumL :: (a -> Key -> b -> (a,c)) -> a -> IntMap b -> (a,IntMap c)
mapAccumL f a t
  = case t of
      Bin p m l r -> let (a1,l') = mapAccumL f a l
                         (a2,r') = mapAccumL f a1 r
                     in (a2,Bin p m l' r')
      Tip k x     -> let (a',x') = f a k x in (a',Tip k x')
      Nil         -> (a,Nil)

-- | /O(n)/. The function @'mapAccumR'@ threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (a -> Key -> b -> (a,c)) -> a -> IntMap b -> (a,IntMap c)
mapAccumRWithKey f a t
  = case t of
      Bin p m l r -> let (a1,r') = mapAccumRWithKey f a r
                         (a2,l') = mapAccumRWithKey f a1 l
                     in (a2,Bin p m l' r')
      Tip k x     -> let (a',x') = f a k x in (a',Tip k x')
      Nil         -> (a,Nil)

{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
-- | /O(n)/. Filter all values that satisfy some predicate.
--
-- > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > filter (> "x") (fromList [(5,"a"), (3,"b")]) == empty
-- > filter (< "a") (fromList [(5,"a"), (3,"b")]) == empty

filter :: (a -> Bool) -> IntMap a -> IntMap a
filter p m
  = filterWithKey (\_ x -> p x) m

-- | /O(n)/. Filter all keys\/values that satisfy some predicate.
--
-- > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

filterWithKey :: (Key -> a -> Bool) -> IntMap a -> IntMap a
filterWithKey predicate t
  = case t of
      Bin p m l r 
        -> bin p m (filterWithKey predicate l) (filterWithKey predicate r)
      Tip k x 
        | predicate k x -> t
        | otherwise     -> Nil
      Nil -> Nil

-- | /O(n)/. Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partition :: (a -> Bool) -> IntMap a -> (IntMap a,IntMap a)
partition p m
  = partitionWithKey (\_ x -> p x) m

-- | /O(n)/. Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
-- > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partitionWithKey :: (Key -> a -> Bool) -> IntMap a -> (IntMap a,IntMap a)
partitionWithKey predicate t
  = case t of
      Bin p m l r 
        -> let (l1,l2) = partitionWithKey predicate l
               (r1,r2) = partitionWithKey predicate r
           in (bin p m l1 r1, bin p m l2 r2)
      Tip k x 
        | predicate k x -> (t,Nil)
        | otherwise     -> (Nil,t)
      Nil -> (Nil,Nil)

-- | /O(n)/. Map values and collect the 'Just' results.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"

mapMaybe :: (a -> Maybe b) -> IntMap a -> IntMap b
mapMaybe f = mapMaybeWithKey (\_ x -> f x)

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"

mapMaybeWithKey :: (Key -> a -> Maybe b) -> IntMap a -> IntMap b
mapMaybeWithKey f (Bin p m l r)
  = bin p m (mapMaybeWithKey f l) (mapMaybeWithKey f r)
mapMaybeWithKey f (Tip k x) = case f k x of
  Just y  -> Tip k y
  Nothing -> Nil
mapMaybeWithKey _ Nil = Nil

-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- >
-- > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])

mapEither :: (a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
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

mapEitherWithKey :: (Key -> a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
mapEitherWithKey f (Bin p m l r)
  = (bin p m l1 r1, bin p m l2 r2)
  where
    (l1,l2) = mapEitherWithKey f l
    (r1,r2) = mapEitherWithKey f r
mapEitherWithKey f (Tip k x) = case f k x of
  Left y  -> (Tip k y, Nil)
  Right z -> (Nil, Tip k z)
mapEitherWithKey _ Nil = (Nil, Nil)

-- | /O(log n)/. The expression (@'split' k map@) is a pair @(map1,map2)@
-- where all keys in @map1@ are lower than @k@ and all keys in
-- @map2@ larger than @k@. Any key equal to @k@ is found in neither @map1@ nor @map2@.
--
-- > split 2 (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3,"b"), (5,"a")])
-- > split 3 (fromList [(5,"a"), (3,"b")]) == (empty, singleton 5 "a")
-- > split 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > split 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", empty)
-- > split 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], empty)

split :: Key -> IntMap a -> (IntMap a,IntMap a)
split k t
  = case t of
      Bin _ m l r
          | m < 0 -> (if k >= 0 -- handle negative numbers.
                      then let (lt,gt) = split' k l in (union r lt, gt)
                      else let (lt,gt) = split' k r in (lt, union gt l))
          | otherwise   -> split' k t
      Tip ky _
        | k>ky      -> (t,Nil)
        | k<ky      -> (Nil,t)
        | otherwise -> (Nil,Nil)
      Nil -> (Nil,Nil)

split' :: Key -> IntMap a -> (IntMap a,IntMap a)
split' k t
  = case t of
      Bin p m l r
        | nomatch k p m -> if k>p then (t,Nil) else (Nil,t)
        | zero k m  -> let (lt,gt) = split k l in (lt,union gt r)
        | otherwise -> let (lt,gt) = split k r in (union l lt,gt)
      Tip ky _
        | k>ky      -> (t,Nil)
        | k<ky      -> (Nil,t)
        | otherwise -> (Nil,Nil)
      Nil -> (Nil,Nil)

-- | /O(log n)/. Performs a 'split' but also returns whether the pivot
-- key was found in the original map.
--
-- > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (empty, Nothing, fromList [(3,"b"), (5,"a")])
-- > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (empty, Just "b", singleton 5 "a")
-- > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Nothing, singleton 5 "a")
-- > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", empty)
-- > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], Nothing, empty)

splitLookup :: Key -> IntMap a -> (IntMap a,Maybe a,IntMap a)
splitLookup k t
  = case t of
      Bin _ m l r
          | m < 0 -> (if k >= 0 -- handle negative numbers.
                      then let (lt,found,gt) = splitLookup' k l in (union r lt,found, gt)
                      else let (lt,found,gt) = splitLookup' k r in (lt,found, union gt l))
          | otherwise   -> splitLookup' k t
      Tip ky y 
        | k>ky      -> (t,Nothing,Nil)
        | k<ky      -> (Nil,Nothing,t)
        | otherwise -> (Nil,Just y,Nil)
      Nil -> (Nil,Nothing,Nil)

splitLookup' :: Key -> IntMap a -> (IntMap a,Maybe a,IntMap a)
splitLookup' k t
  = case t of
      Bin p m l r
        | nomatch k p m -> if k>p then (t,Nothing,Nil) else (Nil,Nothing,t)
        | zero k m  -> let (lt,found,gt) = splitLookup k l in (lt,found,union gt r)
        | otherwise -> let (lt,found,gt) = splitLookup k r in (union l lt,found,gt)
      Tip ky y 
        | k>ky      -> (t,Nothing,Nil)
        | k<ky      -> (Nil,Nothing,t)
        | otherwise -> (Nil,Just y,Nil)
      Nil -> (Nil,Nothing,Nil)

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator. This function is an equivalent of 'foldr' and is present
-- for compatibility only.
--
-- /Please note that fold will be deprecated in the future and removed./
fold :: (a -> b -> b) -> b -> IntMap a -> b
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
foldr :: (a -> b -> b) -> b -> IntMap a -> b
foldr f z t =
  case t of Bin 0 m l r | m < 0 -> go (go z l) r -- put negative numbers before
            _                   -> go z t
  where
    go z' Nil           = z'
    go z' (Tip _ x)     = f x z'
    go z' (Bin _ _ l r) = go (go z' r) l
{-# INLINE foldr #-}

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> IntMap a -> b
foldr' f z t =
  case t of Bin 0 m l r | m < 0 -> go (go z l) r -- put negative numbers before
            _                   -> go z t
  where
    STRICT_1_OF_2(go)
    go z' Nil           = z'
    go z' (Tip _ x)     = f x z'
    go z' (Bin _ _ l r) = go (go z' r) l
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
foldl :: (a -> b -> a) -> a -> IntMap b -> a
foldl f z t =
  case t of Bin 0 m l r | m < 0 -> go (go z r) l -- put negative numbers before
            _                   -> go z t
  where
    go z' Nil           = z'
    go z' (Tip _ x)     = f z' x
    go z' (Bin _ _ l r) = go (go z' l) r
{-# INLINE foldl #-}

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> b -> a) -> a -> IntMap b -> a
foldl' f z t =
  case t of Bin 0 m l r | m < 0 -> go (go z r) l -- put negative numbers before
            _                   -> go z t
  where
    STRICT_1_OF_2(go)
    go z' Nil           = z'
    go z' (Tip _ x)     = f z' x
    go z' (Bin _ _ l r) = go (go z' l) r
{-# INLINE foldl' #-}

-- | /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator. This function is an equivalent of 'foldrWithKey' and is present
-- for compatibility only.
--
-- /Please note that foldWithKey will be deprecated in the future and removed./
foldWithKey :: (Int -> a -> b -> b) -> b -> IntMap a -> b
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
foldrWithKey :: (Int -> a -> b -> b) -> b -> IntMap a -> b
foldrWithKey f z t =
  case t of Bin 0 m l r | m < 0 -> go (go z l) r -- put negative numbers before
            _                   -> go z t
  where
    go z' Nil           = z'
    go z' (Tip kx x)    = f kx x z'
    go z' (Bin _ _ l r) = go (go z' r) l
{-# INLINE foldrWithKey #-}

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldrWithKey' :: (Int -> a -> b -> b) -> b -> IntMap a -> b
foldrWithKey' f z t =
  case t of Bin 0 m l r | m < 0 -> go (go z l) r -- put negative numbers before
            _                   -> go z t
  where
    STRICT_1_OF_2(go)
    go z' Nil           = z'
    go z' (Tip kx x)    = f kx x z'
    go z' (Bin _ _ l r) = go (go z' r) l
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
foldlWithKey :: (a -> Int -> b -> a) -> a -> IntMap b -> a
foldlWithKey f z t =
  case t of Bin 0 m l r | m < 0 -> go (go z r) l -- put negative numbers before
            _                   -> go z t
  where
    go z' Nil           = z'
    go z' (Tip kx x)    = f z' kx x
    go z' (Bin _ _ l r) = go (go z' l) r
{-# INLINE foldlWithKey #-}

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlWithKey' :: (a -> Int -> b -> a) -> a -> IntMap b -> a
foldlWithKey' f z t =
  case t of Bin 0 m l r | m < 0 -> go (go z r) l -- put negative numbers before
            _                   -> go z t
  where
    STRICT_1_OF_2(go)
    go z' Nil           = z'
    go z' (Tip kx x)    = f z' kx x
    go z' (Bin _ _ l r) = go (go z' l) r
{-# INLINE foldlWithKey' #-}

{--------------------------------------------------------------------
  List variations 
--------------------------------------------------------------------}
-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
--
-- > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
-- > elems empty == []

elems :: IntMap a -> [a]
elems
  = foldr (:) []

-- | /O(n)/. Return all keys of the map in ascending order.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []

keys  :: IntMap a -> [Key]
keys
  = foldrWithKey (\k _ ks -> k:ks) []

-- | /O(n*min(n,W))/. The set of all keys of the map.
--
-- > keysSet (fromList [(5,"a"), (3,"b")]) == Data.IntSet.fromList [3,5]
-- > keysSet empty == Data.IntSet.empty

keysSet :: IntMap a -> IntSet.IntSet
keysSet m = IntSet.fromDistinctAscList (keys m)


-- | /O(n)/. Return all key\/value pairs in the map in ascending key order.
--
-- > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > assocs empty == []

assocs :: IntMap a -> [(Key,a)]
assocs m
  = toList m


{--------------------------------------------------------------------
  Lists 
--------------------------------------------------------------------}
-- | /O(n)/. Convert the map to a list of key\/value pairs.
--
-- > toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > toList empty == []

toList :: IntMap a -> [(Key,a)]
toList
  = foldrWithKey (\k x xs -> (k,x):xs) []

-- | /O(n)/. Convert the map to a list of key\/value pairs where the
-- keys are in ascending order.
--
-- > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]

toAscList :: IntMap a -> [(Key,a)]
toAscList t   
  = -- NOTE: the following algorithm only works for big-endian trees
    let (pos,neg) = span (\(k,_) -> k >=0) (foldrWithKey (\k x xs -> (k,x):xs) [] t) in neg ++ pos

-- | /O(n*min(n,W))/. Create a map from a list of key\/value pairs.
--
-- > fromList [] == empty
-- > fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
-- > fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]

fromList :: [(Key,a)] -> IntMap a
fromList xs
  = foldlStrict ins empty xs
  where
    ins t (k,x)  = insert k x t

-- | /O(n*min(n,W))/. Create a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")] == fromList [(3, "ab"), (5, "cba")]
-- > fromListWith (++) [] == empty

fromListWith :: (a -> a -> a) -> [(Key,a)] -> IntMap a 
fromListWith f xs
  = fromListWithKey (\_ x y -> f x y) xs

-- | /O(n*min(n,W))/. Build a map from a list of key\/value pairs with a combining function. See also fromAscListWithKey'.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")] == fromList [(3, "3:a|b"), (5, "5:c|5:b|a")]
-- > fromListWithKey f [] == empty

fromListWithKey :: (Key -> a -> a -> a) -> [(Key,a)] -> IntMap a 
fromListWithKey f xs 
  = foldlStrict ins empty xs
  where
    ins t (k,x) = insertWithKey f k x t

-- | /O(n)/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order.
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]

fromAscList :: [(Key,a)] -> IntMap a
fromAscList xs
  = fromAscListWithKey (\_ x _ -> x) xs

-- | /O(n)/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order, with a combining function on equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]

fromAscListWith :: (a -> a -> a) -> [(Key,a)] -> IntMap a
fromAscListWith f xs
  = fromAscListWithKey (\_ x y -> f x y) xs

-- | /O(n)/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order, with a combining function on equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "5:b|a")]

fromAscListWithKey :: (Key -> a -> a -> a) -> [(Key,a)] -> IntMap a
fromAscListWithKey _ []         = Nil
fromAscListWithKey f (x0 : xs0) = fromDistinctAscList (combineEq x0 xs0)
  where
    -- [combineEq f xs] combines equal elements with function [f] in an ordered list [xs]
    combineEq z [] = [z]
    combineEq z@(kz,zz) (x@(kx,xx):xs)
      | kx==kz    = let yy = f kx xx zz in combineEq (kx,yy) xs
      | otherwise = z:combineEq x xs

-- | /O(n)/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order and all distinct.
-- /The precondition (input list is strictly ascending) is not checked./
--
-- > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]

#ifdef __GLASGOW_HASKELL__
fromDistinctAscList :: forall a. [(Key,a)] -> IntMap a
#else
fromDistinctAscList ::           [(Key,a)] -> IntMap a
#endif
fromDistinctAscList []         = Nil
fromDistinctAscList (z0 : zs0) = work z0 zs0 Nada
  where
    work (kx,vx) []            stk = finish kx (Tip kx vx) stk
    work (kx,vx) (z@(kz,_):zs) stk = reduce z zs (branchMask kx kz) kx (Tip kx vx) stk

#ifdef __GLASGOW_HASKELL__
    reduce :: (Key,a) -> [(Key,a)] -> Mask -> Prefix -> IntMap a -> Stack a -> IntMap a
#endif
    reduce z zs _ px tx Nada = work z zs (Push px tx Nada)
    reduce z zs m px tx stk@(Push py ty stk') =
        let mxy = branchMask px py
            pxy = mask px mxy
        in  if shorter m mxy
                 then reduce z zs m pxy (Bin pxy mxy ty tx) stk'
                 else work z zs (Push px tx stk)

    finish _  t  Nada = t
    finish px tx (Push py ty stk) = finish p (join py ty px tx) stk
        where m = branchMask px py
              p = mask px m

data Stack a = Push {-# UNPACK #-} !Prefix !(IntMap a) !(Stack a) | Nada


{--------------------------------------------------------------------
  Eq 
--------------------------------------------------------------------}
instance Eq a => Eq (IntMap a) where
  t1 == t2  = equal t1 t2
  t1 /= t2  = nequal t1 t2

equal :: Eq a => IntMap a -> IntMap a -> Bool
equal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2) 
equal (Tip kx x) (Tip ky y)
  = (kx == ky) && (x==y)
equal Nil Nil = True
equal _   _   = False

nequal :: Eq a => IntMap a -> IntMap a -> Bool
nequal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 /= m2) || (p1 /= p2) || (nequal l1 l2) || (nequal r1 r2) 
nequal (Tip kx x) (Tip ky y)
  = (kx /= ky) || (x/=y)
nequal Nil Nil = False
nequal _   _   = True

{--------------------------------------------------------------------
  Ord 
--------------------------------------------------------------------}

instance Ord a => Ord (IntMap a) where
    compare m1 m2 = compare (toList m1) (toList m2)

{--------------------------------------------------------------------
  Functor 
--------------------------------------------------------------------}

instance Functor IntMap where
    fmap = map

{--------------------------------------------------------------------
  Show 
--------------------------------------------------------------------}

instance Show a => Show (IntMap a) where
  showsPrec d m   = showParen (d > 10) $
    showString "fromList " . shows (toList m)

{-
XXX unused code

showMap :: (Show a) => [(Key,a)] -> ShowS
showMap []     
  = showString "{}" 
showMap (x:xs) 
  = showChar '{' . showElem x . showTail xs
  where
    showTail []     = showChar '}'
    showTail (x':xs') = showChar ',' . showElem x' . showTail xs'
    
    showElem (k,v)  = shows k . showString ":=" . shows v
-}

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance (Read e) => Read (IntMap e) where
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
  Typeable
--------------------------------------------------------------------}

#include "Typeable.h"
INSTANCE_TYPEABLE1(IntMap,intMapTc,"IntMap")

{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}
-- | /O(n)/. Show the tree that implements the map. The tree is shown
-- in a compressed, hanging format.
showTree :: Show a => IntMap a -> String
showTree s
  = showTreeWith True False s


{- | /O(n)/. The expression (@'showTreeWith' hang wide map@) shows
 the tree that implements the map. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.
-}
showTreeWith :: Show a => Bool -> Bool -> IntMap a -> String
showTreeWith hang wide t
  | hang      = (showsTreeHang wide [] t) ""
  | otherwise = (showsTree wide [] [] t) ""

showsTree :: Show a => Bool -> [String] -> [String] -> IntMap a -> ShowS
showsTree wide lbars rbars t
  = case t of
      Bin p m l r
          -> showsTree wide (withBar rbars) (withEmpty rbars) r .
             showWide wide rbars .
             showsBars lbars . showString (showBin p m) . showString "\n" .
             showWide wide lbars .
             showsTree wide (withEmpty lbars) (withBar lbars) l
      Tip k x
          -> showsBars lbars . showString " " . shows k . showString ":=" . shows x . showString "\n" 
      Nil -> showsBars lbars . showString "|\n"

showsTreeHang :: Show a => Bool -> [String] -> IntMap a -> ShowS
showsTreeHang wide bars t
  = case t of
      Bin p m l r
          -> showsBars bars . showString (showBin p m) . showString "\n" . 
             showWide wide bars .
             showsTreeHang wide (withBar bars) l .
             showWide wide bars .
             showsTreeHang wide (withEmpty bars) r
      Tip k x
          -> showsBars bars . showString " " . shows k . showString ":=" . shows x . showString "\n" 
      Nil -> showsBars bars . showString "|\n" 

showBin :: Prefix -> Mask -> String
showBin _ _
  = "*" -- ++ show (p,m)

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
  Helpers
--------------------------------------------------------------------}
{--------------------------------------------------------------------
  Join
--------------------------------------------------------------------}
join :: Prefix -> IntMap a -> Prefix -> IntMap a -> IntMap a
join p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m
{-# INLINE join #-}

{--------------------------------------------------------------------
  @bin@ assures that we never have empty trees within a tree.
--------------------------------------------------------------------}
bin :: Prefix -> Mask -> IntMap a -> IntMap a -> IntMap a
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r   = Bin p m l r
{-# INLINE bin #-}

  
{--------------------------------------------------------------------
  Endian independent bit twiddling
--------------------------------------------------------------------}
zero :: Key -> Mask -> Bool
zero i m
  = (natFromInt i) .&. (natFromInt m) == 0
{-# INLINE zero #-}

nomatch,match :: Key -> Prefix -> Mask -> Bool
nomatch i p m
  = (mask i m) /= p
{-# INLINE nomatch #-}

match i p m
  = (mask i m) == p
{-# INLINE match #-}

mask :: Key -> Mask -> Prefix
mask i m
  = maskW (natFromInt i) (natFromInt m)
{-# INLINE mask #-}


{--------------------------------------------------------------------
  Big endian operations  
--------------------------------------------------------------------}
maskW :: Nat -> Nat -> Prefix
maskW i m
  = intFromNat (i .&. (complement (m-1) `xor` m))
{-# INLINE maskW #-}

shorter :: Mask -> Mask -> Bool
shorter m1 m2
  = (natFromInt m1) > (natFromInt m2)
{-# INLINE shorter #-}

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  = intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))
{-# INLINE branchMask #-}

{----------------------------------------------------------------------
  Finding the highest bit (mask) in a word [x] can be done efficiently in
  three ways:
  * convert to a floating point value and the mantissa tells us the 
    [log2(x)] that corresponds with the highest bit position. The mantissa 
    is retrieved either via the standard C function [frexp] or by some bit 
    twiddling on IEEE compatible numbers (float). Note that one needs to 
    use at least [double] precision for an accurate mantissa of 32 bit 
    numbers.
  * use bit twiddling, a logarithmic sequence of bitwise or's and shifts (bit).
  * use processor specific assembler instruction (asm).

  The most portable way would be [bit], but is it efficient enough?
  I have measured the cycle counts of the different methods on an AMD 
  Athlon-XP 1800 (~ Pentium III 1.8Ghz) using the RDTSC instruction:

  highestBitMask: method  cycles
                  --------------
                   frexp   200
                   float    33
                   bit      11
                   asm      12

  highestBit:     method  cycles
                  --------------
                   frexp   195
                   float    33
                   bit      11
                   asm      11

  Wow, the bit twiddling is on today's RISC like machines even faster
  than a single CISC instruction (BSR)!
----------------------------------------------------------------------}

{----------------------------------------------------------------------
  [highestBitMask] returns a word where only the highest bit is set.
  It is found by first setting all bits in lower positions than the 
  highest bit and than taking an exclusive or with the original value.
  Allthough the function may look expensive, GHC compiles this into
  excellent C code that subsequently compiled into highly efficient
  machine code. The algorithm is derived from Jorg Arndt's FXT library.
----------------------------------------------------------------------}
highestBitMask :: Nat -> Nat
highestBitMask x0
  = case (x0 .|. shiftRL x0 1) of
     x1 -> case (x1 .|. shiftRL x1 2) of
      x2 -> case (x2 .|. shiftRL x2 4) of
       x3 -> case (x3 .|. shiftRL x3 8) of
        x4 -> case (x4 .|. shiftRL x4 16) of
         x5 -> case (x5 .|. shiftRL x5 32) of   -- for 64 bit platforms
          x6 -> (x6 `xor` (shiftRL x6 1))
{-# INLINE highestBitMask #-}


{--------------------------------------------------------------------
  Utilities 
--------------------------------------------------------------------}

foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f = go
  where
    go z []     = z
    go z (x:xs) = let z' = f z x in z' `seq` go z' xs
{-# INLINE foldlStrict #-}
