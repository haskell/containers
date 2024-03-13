module IntMapValidity (valid) where

import Data.Bits (xor, (.&.), (.|.), shiftR)
import Data.List (intercalate)
import Data.IntMap.Internal
import Numeric (showHex)
import Test.Tasty.QuickCheck (Property, counterexample, property, (.&&.), conjoin)
import Utils.Containers.Internal.BitUtil (bitcount, shiftRL)

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
-- * All keys in the Bin's left child start with the Bin's Prefix followed by a
--   zero bit.
-- * All keys in the Bin's right child start with the Bin's Prefix followed by a
--   one bit.
prefixOk :: IntMap a -> Property
prefixOk t =
  case t of
    Nil -> property ()
    Tip _ _ -> property ()
    Bin p l r ->
      let px = unPrefix p
          m = px .&. (-px)
          m' = fromIntegral (fromIntegral m `shiftRL` 1)
          -- pl = px but make the next bit 0 and shift the mask bit right
          pl = Prefix ((px `xor` m) .|. m')
          -- pr = px but keep the next bit 1 and shift the mask bit right
          pr = Prefix (px .|. m')
          keysl = keys l
          keysr = keys r
          debugStr = concat
            [ "px=" ++ showIntHex px
            , ", keysl=[" ++ intercalate "," (fmap showIntHex keysl) ++ "]"
            , ", keysr=[" ++ intercalate "," (fmap showIntHex keysr) ++ "]"
            ]
      in counterexample debugStr $
           all (match p) (keysl ++ keysr) &&
           all (match pl) keysl &&
           all (match pr) keysr

-- | Whether the @Int@ starts with the given @Prefix@.
match :: Prefix -> Int -> Bool
match p k = (k `xor` px) .&. prefixMask == 0
  where
    px = unPrefix p
    prefixMask = px `xor` (-px)

showIntHex :: Int -> String
showIntHex x = "0x" ++ showHex (fromIntegral x :: Word) ""
