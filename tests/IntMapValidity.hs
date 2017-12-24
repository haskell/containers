module IntMapValidity (valid) where

import Data.Bits (xor, (.&.))
import Data.IntMap.Internal
import Test.QuickCheck (Property, property, (.&&.))

{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}
-- | Returns true iff the internal structure of the IntMap is valid.
valid :: IntMap a -> Property
valid t
  = property (nilNeverChildOfBin t) .&&.
    property (commonPrefix t) .&&.
    property (maskRespected t)

-- Invariant: Nil is never found as a child of Bin.
nilNeverChildOfBin :: IntMap a  -> Bool
nilNeverChildOfBin t
  = case t of
      Nil -> True
      Tip _ _ -> True
      Bin _ _ l r -> noNilInSet l && noNilInSet r
  where
    noNilInSet t'
      = case t' of
          Nil -> False
          Tip _ _ -> True
          Bin _ _ l' r' -> noNilInSet l' && noNilInSet r'

-- Invariant: Prefix is the common high-order bits that all elements share to
--            the left of the Mask bit.
commonPrefix :: IntMap a -> Bool
commonPrefix t
  = case t of
      Nil -> True
      Tip _ _ -> True
      b@(Bin p _ _ _) -> all (sharedPrefix p) (keys b)
  where
    sharedPrefix :: Prefix -> Int -> Bool
    sharedPrefix p a = 0 == (p `xor` (p .&. a))

-- Invariant: In Bin prefix mask left right, left consists of the elements that
--            don't have the mask bit set; right is all the elements that do.
maskRespected :: IntMap a -> Bool
maskRespected t
  = case t of
      Nil -> True
      Tip _ _ -> True
      Bin _ binMask l r ->
        all (\x -> zero x binMask) (keys l)
        && all (\x -> not (zero x binMask)) (keys r)
