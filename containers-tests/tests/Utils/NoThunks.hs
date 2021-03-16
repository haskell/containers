module Utils.NoThunks (whnfHasNoThunks) where

import Data.Maybe (isNothing)

import NoThunks.Class (NoThunks, noThunks)
import Test.QuickCheck (Property, ioProperty)

-- | Check that after evaluating the argument to weak head normal form there
-- are no thunks.
--
whnfHasNoThunks :: NoThunks a => a -> Property
whnfHasNoThunks a = ioProperty
                  . fmap isNothing
                  . noThunks []
                 $! a
