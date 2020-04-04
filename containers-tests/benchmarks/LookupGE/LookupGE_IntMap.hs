{-# LANGUAGE CPP #-}
module LookupGE_IntMap where

import Prelude hiding (null)
import Data.IntMap.Internal

lookupGE1 :: Key -> IntMap a -> Maybe (Key,a)
lookupGE1 k m =
    case splitLookup k m of
        (_,Just v,_)  -> Just (k,v)
        (_,Nothing,r) -> findMinMaybe r


lookupGE2 = lookupGE

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | /O(log n)/. The minimal key of the map.
findMinMaybe :: IntMap a -> Maybe (Key, a)
findMinMaybe m
  | null m = Nothing
  | otherwise = Just (findMin m)

#ifdef TESTING
-------------------------------------------------------------------------------
-- Properties:
-------------------------------------------------------------------------------

prop_lookupGE12 :: Int -> [Int] -> Bool
prop_lookupGE12 x xs = case fromList $ zip xs xs of m -> lookupGE1 x m == lookupGE2 x m
#endif

