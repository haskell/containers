{-# LANGUAGE CPP, FlexibleContexts, DataKinds, MonoLocalBinds #-}

#include "containers.h"

-- | This module simply holds disabled copies of functions from
-- Data.Map.Internal.Debug.
module Data.Map.Internal.DeprecatedShowTree where

import Data.Map.Internal (Map)
import Utils.Containers.Internal.TypeError

-- | This function has moved to 'Data.Map.Internal.Debug.showTree'.
showTree :: Whoops "showTree has moved to Data.Map.Internal.Debug.showTree."
         => Map k a -> String
showTree _ = undefined

-- | This function has moved to 'Data.Map.Internal.Debug.showTreeWith'.
showTreeWith ::
      Whoops "showTreeWith has moved to Data.Map.Internal.Debug.showTreeWith."
   => (k -> a -> String) -> Bool -> Bool -> Map k a -> String
showTreeWith _ _ _ _ = undefined
