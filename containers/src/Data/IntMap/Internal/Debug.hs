{-# LANGUAGE CPP, BangPatterns #-}

module Data.IntMap.Internal.Debug
  ( showTree
  , showTreeWith
  , valid
  , validWith
  ) where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
#if !MIN_VERSION_base(4,8,0)
import Data.Word (Word)
#endif

import Prelude hiding (min, max)
import Data.IntMap.Internal

-- | Show the tree that implements the map. The tree is shown in in-order,
-- ASCII format.
showTree :: Show a => IntMap a -> String
showTree = showTreeWith True False where

-- | The expression @'showTreeWith' inorder unicode map@ shows the tree that
-- implements the map. If @inorder@ is true, the tree is arranged so that keys
-- will appear in their natural order; otherwise, parents will always precede
-- their children. If @unicode@ is true, the tree will be drawn with Unicode
-- Box Drawing characters; otherwise, the tree will be drawn in ASCII art.
showTreeWith :: Show a => Bool -> Bool -> IntMap a -> String
showTreeWith inorder unicode = unlines . start where
    start (IntMap Empty) = []
    start (IntMap (NonEmpty min minV node)) = (show (boundKey min) ++ ":=" ++ show minV) : goL True True node

    goL root fromBefore Tip = [(if fromBefore then if root then ur else rtee else dr) ++ tip]
    goL root fromBefore (Bin max maxV l r)
        | inorder   = fmap (indent fromBefore) children ++ [line]
        | otherwise = [line] ++ fmap (indent (not root)) children
      where
        children = goL False (not inorder) l ++ goR (not inorder) r
        line = (if fromBefore then if root then ur else rtee else dr)
            ++ horiz ++ horiz ++ horiz
            ++ (if inorder then ul else dl)
            ++ " " ++ show (boundKey max) ++ ":=" ++ show maxV

    goR fromBefore Tip = [(if fromBefore then ur else rtee) ++ tip]
    goR fromBefore (Bin min minV l r) = [line] ++ fmap (indent (not fromBefore)) children
      where
        children = goL False True l ++ goR True r
        line = (if fromBefore then ur else rtee)
            ++ horiz ++ horiz ++ horiz
            ++ dl
            ++ " " ++ show (boundKey min) ++ ":=" ++ show minV

    indent lined line = prefix ++ "   " ++ line
      where
        prefix = if lined then vert else " "

    ul = if unicode then "┘" else "'"
    dl = if unicode then "┐" else "."
    ur = if unicode then "└" else "`"
    dr = if unicode then "┌" else ","
    rtee = if unicode then "├" else "+"
    vert = if unicode then "│" else "|"
    horiz = if unicode then "─" else "-"
    tip = if unicode then "╼" else "-*"

-- | /O(n)/. Test if the internal map structure is valid.
--
-- >>> valid (fromAscList [(3,"b"), (5,"a")])
-- True
--
-- >>> valid (fromAscList [(5,"a"), (3,"b")])
-- False
--
-- For information about the invariants that are checked, see 'IntMap_'.
valid :: IntMap a -> Bool
valid = validWith const (&&)

-- | /O(n)/. Test if the internal map structure is valid, returning extra
-- information about why the map is invalid if it is so. To return this info,
-- @'validWith' assert (.&&.)@ passes a description of everything it asserts
-- to @assert@ along with the assertion's trutheness, then combines the
-- results of these assertions with @.&&.@.
validWith :: (Bool -> String -> prop) -> (prop -> prop -> prop) -> IntMap a -> prop
validWith assert (.&&.) = start
  where
    start (IntMap Empty) = assert True "Empty maps are always valid."
    start (IntMap (NonEmpty _ _ Tip)) = assert True "Singleton maps are always valid."
    start (IntMap (NonEmpty min _ (Bin max _ l r))) =
             assertInMinBound (boundKey max) min
        .&&. goL min max l
        .&&. goR min max r

    -- When recursing, we already know that @innerMax < max@, so checking in
    -- the subtree that keys are less than @innerMax@ also shows that they
    -- are less than @max@. Similarly, we can replace @min@ with @innerMin@ in
    -- 'goR'.
    goL !_ !_ Tip = assert True "Leaf nodes are always valid."
    goL !min !max (Bin innerMax _ l r) =
             assertInMinBound (boundKey innerMax) min
        .&&. assertInMaxBound (boundKey innerMax) max
        .&&. assert (xor (boundKey innerMax) min < xor (boundKey innerMax) max)
                    (trieError min max (boundKey innerMax) True)
        .&&. goL min innerMax l
        .&&. goR min innerMax r

    goR !_ !_ Tip = assert True "Leaf nodes are always valid."
    goR !min !max (Bin innerMin _ l r) =
             assertInMinBound (boundKey innerMin) min
        .&&. assertInMaxBound (boundKey innerMin) max
        .&&. assert (xor (boundKey innerMin) min > xor (boundKey innerMin) max)
                    (trieError min max (boundKey innerMin) False)
        .&&. goL innerMin max l
        .&&. goR innerMin max r

    assertInMinBound k min = assert (inMinBound k min) ("Ordering invariant: expected key " ++ show k ++ " > minimum bound " ++ show (boundKey min))
    assertInMaxBound k max = assert (inMaxBound k max) ("Ordering invariant: expected key " ++ show k ++ " < maximum bound " ++ show (boundKey max))

    showBinary k = showIntAtBase 2 intToDigit (fromIntegral k :: Word) ""

    trieError min max k isLeft = "Trie invariant: between " ++ show (boundKey min) ++ " and " ++ show (boundKey max)
                       ++ ", " ++ show k ++ " was expected to share more bits with " ++ show (if isLeft then boundKey min else boundKey max)
                       ++ " as it is on the " ++ (if isLeft then "left" else "right") ++ " branch:"
                       ++ "\n  min: " ++ replicate (binLength - length binMin) '0' ++ binMin
                       ++ "\n    k: " ++ replicate (binLength - length binK) '0' ++ binK
                       ++ "\n  max: " ++ replicate (binLength - length binMax) '0' ++ binMax
      where
        binMin = showBinary (boundKey min)
        binK = showBinary k
        binMax = showBinary (boundKey max)
        binLength = maximum [length binMin, length binK, length binMax]
