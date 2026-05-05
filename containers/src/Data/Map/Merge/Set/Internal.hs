-- |
-- = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.
--
-- = Description
--
-- This module defines common constructs used by both "Data.Map.Merge.Set.Lazy"
-- and "Data.Map.Merge.Set.Strict".
--
-- @since FIXME
--
module Data.Map.Merge.Set.Internal
  ( WhenMatched(..)
  , SimpleWhenMatched
  , filterMatched
  , filterAMatched

  , WhenMissingSet(..)
  , SimpleWhenMissingSet
  , dropMissingSet

  , merge
  , mergeA

  , runWhenMatched
  , runWhenMissingSet
  ) where

import Control.Applicative (liftA3)
import Data.Functor.Identity (Identity(..))
import Data.Set (Set)
import qualified Data.Set.Internal as S
import Data.Map (Map)
import qualified Data.Map.Internal as M

-- | A tactic for dealing with keys present in both the set and the map in
-- 'merge' or 'mergeA'.
--
-- A tactic of type @WhenMatched f k a b@ is an abstract representation of
-- a function of type @k -> a -> f (Maybe b)@.
--
-- @since FIXME
newtype WhenMatched f k a b = WhenMatched
  { matchedKey :: k -> a -> f (Maybe b)
  }

-- | Run @WhenMatched@.
--
-- @since FIXME
runWhenMatched :: WhenMatched f k a b -> k -> a -> f (Maybe b)
runWhenMatched = matchedKey

-- | A tactic for dealing with keys present in both the set and the map in
-- 'merge'.
--
-- A tactic of type @SimpleWhenMatched k a b@ is an abstract representation of
-- a function of type @k -> a -> Maybe b@.
--
-- @since FIXME
type SimpleWhenMatched = WhenMatched Identity

-- | When a key is found in both the map and the set, apply a function to the
-- key and the value in the map and keep the value in the merged map if the
-- result is @True@.
--
-- @since FIXME
filterMatched :: Applicative f => (k -> a -> Bool) -> WhenMatched f k a a
filterMatched f =
  WhenMatched (\k x -> if f k x then pure (Just x) else pure Nothing)
{-# INLINE filterMatched #-}

-- | When a key is found in both the map and the set, apply a function to the
-- key and the value in the map and keep the value in the merged map if the
-- result of the action is @True@.
--
-- @since FIXME
filterAMatched :: Functor f => (k -> a -> f Bool) -> WhenMatched f k a a
filterAMatched f =
  WhenMatched (\k x -> (\b -> if b then Just x else Nothing) <$> f k x)
{-# INLINE filterAMatched #-}

-- | A tactic for dealing with keys present in the set but not in the map in
-- 'merge' or 'mergeA'.
--
-- A tactic of type @WhenMissingSet f k a@ is an abstract representation of
-- a function of type @k -> f (Maybe a)@.
--
-- @since FIXME
data WhenMissingSet f k a = WhenMissingSet
  { missingSubtree :: Set k -> f (Map k a)
  , missingKey :: k -> f (Maybe a)
  }

-- | Run @WhenMissingSet@.
--
-- @since FIXME
runWhenMissingSet :: WhenMissingSet f k a -> k -> f (Maybe a)
runWhenMissingSet = missingKey

-- | A tactic for dealing with keys present in the set but not in the map in
-- 'merge'.
--
-- A tactic of type @SimpleWhenMissingSet k a@ is an abstract representation of
-- a function of type @k -> Maybe a@.
--
-- @since FIXME
type SimpleWhenMissingSet = WhenMissingSet Identity

-- | Drop keys that are present in the set but missing from the map.
--
-- @since FIXME
dropMissingSet :: Applicative f => WhenMissingSet f k a
dropMissingSet = WhenMissingSet
  { missingSubtree = \_ -> pure M.empty
  , missingKey = \_ -> pure Nothing
  }
{-# INLINE dropMissingSet #-}

-- | Merge a map and a set into a map.
--
-- @since FIXME
merge
  :: Ord k
  => M.SimpleWhenMissing k a b -- ^ What to do with keys in @m1@ but not @s2@
  -> SimpleWhenMissingSet k b -- ^ What to do with keys in @s2@ but not @m1@
  -> SimpleWhenMatched k a b -- ^ What to do with keys in both @m1@ and @s2@
  -> Map k a -- ^ Map @m1@
  -> Set k -- ^ Set @s2@
  -> Map k b
merge miss1 miss2 match = \t1 t2 -> runIdentity (mergeA miss1 miss2 match t1 t2)
{-# INLINE merge #-}

-- | Merge a map and a set into a map. Applicative version of 'merge'.
--
-- @since FIXME
mergeA
  :: (Applicative f, Ord k)
  => M.WhenMissing f k a b -- ^ What to do with keys in @m1@ but not @s2@
  -> WhenMissingSet f k b -- ^ What to do with keys in @s2@ but not @m1@
  -> WhenMatched f k a b -- ^ What to do with keys in both @m1@ and @s2@
  -> Map k a -- ^ Map @m1@
  -> Set k -- ^ Set @s2@
  -> f (Map k b)
mergeA
  M.WhenMissing{M.missingSubtree = g1t, M.missingKey = g1k}
  WhenMissingSet{missingSubtree = g2t}
  WhenMatched{matchedKey = f} = go
  where
    go t1 S.Tip = g1t t1
    go M.Tip t2 = g2t t2
    go (M.Bin _ k1 x1 l1 r1) t2 = case S.splitMember k1 t2 of
      (l2, found, r2) ->
        liftA3
          (\l' mx' r' -> maybe M.link2 (M.link k1) mx' l' r')
          (go l1 l2)
          (if found then f k1 x1 else g1k k1 x1)
          (go r1 r2)
{-# INLINE mergeA #-}
