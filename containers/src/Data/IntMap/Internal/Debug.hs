module Data.IntMap.Internal.Debug
  ( showTree
  , showTreeWith
  ) where

import Prelude hiding (min, max)
import Data.IntMap.Internal

-- | Show the tree that implements the map.
showTree :: Show a => IntMap a -> String
showTree = unlines . aux where
    aux (IntMap Empty) = []
    aux (IntMap (NonEmpty min minV node)) = (show min ++ " " ++ show minV) : auxNode False node
    auxNode :: Show a => Bool -> Node t a -> [String]
    auxNode _ Tip = ["+-."]
    auxNode lined (Bin bound val l r) = ["+--" ++ show bound ++ " " ++ show val, prefix : "  |"] ++ fmap indent (auxNode True l) ++ [prefix : "  |"] ++ fmap indent (auxNode False r)
      where
        prefix = if lined then '|' else ' '
        indent line = prefix : "  " ++ line

showTreeWith :: Show a => Bool -> Bool -> IntMap a -> String
showTreeWith _ _ = showTree
