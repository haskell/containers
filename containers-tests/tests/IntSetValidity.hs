{-# LANGUAGE CPP #-}
module IntSetValidity (valid) where

import Data.Bits (xor, (.&.))
import Data.IntSet.Internal.IntTreeCommons (Prefix(..), nomatch)
import Data.IntSet.Internal
import Data.List (intercalate)
import Numeric (showHex)
import Test.Tasty.QuickCheck (Property, counterexample, property, (.&&.))
import Utils.Containers.Internal.BitUtil (bitcount)

{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}
-- | Returns true iff the internal structure of the IntSet is valid.
valid :: IntSet -> Property
valid t =
  counterexample "nilNeverChildOfBin" (nilNeverChildOfBin t) .&&.
  counterexample "prefixOk" (prefixOk t) .&&.
  counterexample "tipsValid" (tipsValid t)

-- Invariant: Nil is never found as a child of Bin.
nilNeverChildOfBin :: IntSet -> Bool
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
-- * All keys in a Bin start with the Bin's shared prefix.
-- * All keys in the Bin's left child have the Prefix's mask bit unset.
-- * All keys in the Bin's right child have the Prefix's mask bit set.
prefixOk :: IntSet -> Property
prefixOk t =
  case t of
    Nil -> property ()
    Tip _ _ -> property ()
    Bin p l r ->
      let px = unPrefix p
          m = px .&. (-px)
          keysl = elems l
          keysr = elems r
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
hasPrefix i p = not (nomatch i p)

-- Invariant: The Prefix is zero for the last 5 (on 32 bit arches) or 6 bits
--            (on 64 bit arches). The values of the set represented by a tip
--            are the prefix plus the indices of the set bits in the bit map.
--
-- Note: Valid entries stored in tip omitted.
tipsValid :: IntSet -> Bool
tipsValid t =
  case t of
    Nil -> True
    tip@(Tip p b) -> validTipPrefix p
    Bin _ l r -> tipsValid l && tipsValid r

validTipPrefix :: Int -> Bool
#if WORD_SIZE_IN_BITS==32
-- Last 5 bits of the prefix must be zero for 32 bit arches.
validTipPrefix p = (0x0000001F .&. p) == 0
#else
-- Last 6 bits of the prefix must be zero for 64 bit arches.
validTipPrefix p = (0x000000000000003F .&. p) == 0
#endif

showIntHex :: Int -> String
showIntHex x = "0x" ++ showHex (fromIntegral x :: Word) ""
