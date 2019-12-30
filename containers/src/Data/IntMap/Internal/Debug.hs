module Data.IntMap.Internal.Debug
  ( showTree
  , showTreeWith
  ) where

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
