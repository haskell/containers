module Utils.NoThunks (whnfHasNoThunks) where

import NoThunks.Class (NoThunks, noThunks)
import Test.QuickCheck (Property, counterexample, ioProperty, property)

-- | Check that after evaluating the argument to weak head normal form there
-- are no thunks.
--
whnfHasNoThunks :: NoThunks a => a -> Property
whnfHasNoThunks a = ioProperty $
  maybe (property True) ((`counterexample` False) . show)
    <$> (noThunks [] $! a)
