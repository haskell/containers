module IntMapValidity (valid) where

import Data.IntMap.Internal
import Test.QuickCheck (Property, counterexample, property, (.&&.))
import Utils.Containers.Internal.BitUtil (bitcount)

{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}
-- | Returns true iff the internal structure of the IntMap is valid.
valid :: IntMap a -> Bool
valid = start
  where
    start (IntMap Empty) = True
    start (IntMap (NonEmpty min _ root)) = allKeys (> min) root && goL min root
    
    goL _    Tip = True
    goL min (Bin max _ l r) =
           allKeys (< max) l
        && allKeys (< max) r
        && allKeys (\k -> xor min k < xor k max) l
        && allKeys (\k -> xor min k > xor k max) r
        && goL min l
        && goR max r
        
    goR _    Tip = True
    goR max (Bin min _ l r) =
           allKeys (> min) l
        && allKeys (> min) r
        && allKeys (\k -> xor min k < xor k max) l
        && allKeys (\k -> xor min k > xor k max) r
        && goL min l
        && goR max r
        
    allKeys :: (Key -> Bool) -> Node t a -> Bool
    allKeys _ Tip = True
    allKeys p (Bin b _ l r) = p b && allKeys p l && allKeys p r
