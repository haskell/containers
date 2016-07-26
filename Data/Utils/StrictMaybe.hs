module Data.Utils.StrictMaybe (MaybeS (..), maybeS, toMaybe, toMaybeS) where
import Data.Foldable (Foldable (..))
import Data.Monoid (Monoid (..))

data MaybeS a = NothingS | JustS !a

instance Foldable MaybeS where
  foldMap _ NothingS = mempty
  foldMap f (JustS a) = f a

maybeS :: r -> (a -> r) -> MaybeS a -> r
maybeS n _ NothingS = n
maybeS _ j (JustS a) = j a

toMaybe :: MaybeS a -> Maybe a
toMaybe NothingS = Nothing
toMaybe (JustS a) = Just a

toMaybeS :: Maybe a -> MaybeS a
toMaybeS Nothing = NothingS
toMaybeS (Just a) = JustS a
