module IntMapValidity (valid) where

import Data.Bits (xor, (.&.))
import Data.List (intercalate, elemIndex)
import Data.IntMap.Internal
import Numeric (showHex)
import Test.Tasty.QuickCheck (Property, counterexample, property, (.&&.))

{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}
-- | Returns true iff the internal structure of the IntMap is valid.
valid :: IntMap a -> Property
valid t =
  counterexample "nilNeverChildOfBin" (nilNeverChildOfBin t) .&&.
  counterexample "prefixOk" (prefixOk t)

-- Invariant: Nil is never found as a child of Bin.
nilNeverChildOfBin :: IntMap a  -> Bool
nilNeverChildOfBin t =
  case t of
    Nil -> True
    Tip _ _ -> True
    Bin _ l r -> noNilInSet l && noNilInSet r
  where
    noNilInSet t' =
      case t' of
        Nil -> False
        Tip _ _ -> True
        Bin _ l' r' -> noNilInSet l' && noNilInSet r'

-- Invariants:
-- * All keys in a Bin start with the Bin's Prefix.
-- * All keys in the Bin's left child have the Prefix's mask bit unset.
-- * All keys in the Bin's right child have the Prefix's mask bit set.
prefixOk :: IntMap a -> Property
prefixOk t =
  case t of
    Nil -> property ()
    Tip _ _ -> property ()
    Bin p l r ->
      let px = unPrefix p
          m = px .&. (-px)
          keysl = keys l
          keysr = keys r
          debugStr = concat
            [ "px=" ++ showIntHex px
            , ", keysl=[" ++ intercalate "," (fmap showIntHex keysl) ++ "]"
            , ", keysr=[" ++ intercalate "," (fmap showIntHex keysr) ++ "]"
            ]
      in counterexample debugStr $
           counterexample "mask bit absent" (px /= 0) .&&.
           counterexample "prefix not shared" (all (`hasPrefix` p) (keysl ++ keysr)) .&&.
           counterexample "left child, mask found set" (all (\x -> x .&. m == 0) keysl) .&&.
           counterexample "right child, mask found unset" (all (\x -> x .&. m /= 0) keysr)

hasPrefix :: Int -> Prefix -> Bool
hasPrefix i p = (i `xor` px) .&. prefixMask == 0
  where
    px = unPrefix p
    prefixMask = px `xor` (-px)

showIntHex :: Int -> String
showIntHex x = "0x" ++ showHex (fromIntegral x :: Word) ""
