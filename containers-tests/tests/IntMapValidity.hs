module IntMapValidity
  ( valid
  , hasPrefix
  , hasPrefixSimple
  ) where

import Data.Bits (finiteBitSize, testBit, xor, (.&.))
import Data.List (intercalate, elemIndex)
import Data.IntSet.Internal.IntTreeCommons (Prefix(..), nomatch)
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
  counterexample "prefixesOk" (prefixesOk t)

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
-- * All keys in a Bin start with the Bin's shared prefix.
-- * All keys in the Bin's left child have the Prefix's mask bit unset.
-- * All keys in the Bin's right child have the Prefix's mask bit set.
prefixesOk :: IntMap a -> Property
prefixesOk t = case t of
  Nil -> property ()
  Tip _ _ -> property ()
  Bin p l r -> currentOk .&&. prefixesOk l .&&. prefixesOk r
    where
      px = unPrefix p
      m = px .&. (-px)
      keysl = keys l
      keysr = keys r
      debugStr = concat
        [ "px=" ++ showIntHex px
        , ", keysl=[" ++ intercalate "," (fmap showIntHex keysl) ++ "]"
        , ", keysr=[" ++ intercalate "," (fmap showIntHex keysr) ++ "]"
        ]
      currentOk = counterexample debugStr $
        counterexample "mask bit absent" (px /= 0) .&&.
        counterexample "prefix not shared" (all (`hasPrefix` p) (keysl ++ keysr)) .&&.
        counterexample "left child, mask found set" (all (\x -> x .&. m == 0) keysl) .&&.
        counterexample "right child, mask found unset" (all (\x -> x .&. m /= 0) keysr)

hasPrefix :: Int -> Prefix -> Bool
hasPrefix i p = not (nomatch i p)

-- We test that hasPrefix behaves the same as hasPrefixSimple.
hasPrefixSimple :: Int -> Prefix -> Bool
hasPrefixSimple k p = case elemIndex True pbits of
  Nothing -> error "no mask bit" -- should not happen
  Just i -> drop (i+1) kbits == drop (i+1) pbits
  where
    kbits = toBits k
    pbits = toBits (unPrefix p)

    -- Bits from lowest to highest.
    toBits x = fmap (testBit x) [0 .. finiteBitSize (0 :: Int) - 1]

showIntHex :: Int -> String
showIntHex x = "0x" ++ showHex (fromIntegral x :: Word) ""
