module Utils.MergeFunc
  ( WhenMatchedFunc(..)
  , WhenMissingFunc(..)
  ) where

import Test.QuickCheck
import Utils.Strictness (Func, Func2, Func3)

-- k: key, x: left map value, y: right map value, z: result map value,
-- a,b: fmaps over the result value. a and b are independent variables to allow
-- for coercions involving Bot. See prop_strictMerge in map-strictness.hs for
-- an example.
data WhenMatchedFunc k x y z a b
  = MaybeMatchedFunc (Func3 k x y (Maybe b))
  | FmapMaybeMatchedFunc (Func a b) (Func3 k x y (Maybe z))
  | MatchedFunc (Func3 k x y b)
  | FmapMatchedFunc (Func a b) (Func3 k x y z)
  deriving Show

instance
  ( CoArbitrary k, Function k
  , CoArbitrary x, Function x
  , CoArbitrary y, Function y
  , Arbitrary z
  , CoArbitrary a, Function a, Arbitrary a
  , Arbitrary b
  ) => Arbitrary (WhenMatchedFunc k x y z a b) where
  arbitrary = oneof
    [ MaybeMatchedFunc <$> arbitrary
    , FmapMaybeMatchedFunc <$> arbitrary <*> arbitrary
    , MatchedFunc <$> arbitrary
    , FmapMatchedFunc <$> arbitrary <*> arbitrary
    ]
  shrink wmf = case wmf of
    MaybeMatchedFunc fun -> MaybeMatchedFunc <$> shrink fun
    FmapMaybeMatchedFunc fun2 fun1 ->
      uncurry FmapMaybeMatchedFunc <$> shrink (fun2, fun1)
    MatchedFunc fun -> MatchedFunc <$> shrink fun
    FmapMatchedFunc fun2 fun1 ->
      uncurry FmapMatchedFunc <$> shrink (fun2, fun1)

-- k: key, x: map value, y: result map value, a,b: fmaps over the result value.
-- a and b are independent variables to allow for coercions involving Bot. See
-- prop_strictMerge in map-strictness.hs for an example.
data WhenMissingFunc k x y a b
  = MapMaybeMissingFunc (Func2 k x (Maybe b))
  | FmapMapMaybeMissingFunc (Func a b) (Func2 k x (Maybe y))
  | MapMissingFunc (Func2 k x b)
  | FmapMapMissingFunc (Func a b) (Func2 k x y)
  deriving Show

instance
  ( CoArbitrary k, Function k
  , CoArbitrary x, Function x
  , Arbitrary y
  , CoArbitrary a, Function a, Arbitrary a
  , Arbitrary b
  ) => Arbitrary (WhenMissingFunc k x y a b) where
  arbitrary = oneof
    [ MapMaybeMissingFunc <$> arbitrary
    , FmapMapMaybeMissingFunc <$> arbitrary <*> arbitrary
    , MapMissingFunc <$> arbitrary
    , FmapMapMissingFunc <$> arbitrary <*> arbitrary
    ]
  shrink wmf = case wmf of
    MapMaybeMissingFunc fun -> MapMaybeMissingFunc <$> shrink fun
    FmapMapMaybeMissingFunc fun2 fun1 ->
      uncurry FmapMapMaybeMissingFunc <$> shrink (fun2, fun1)
    MapMissingFunc fun -> MapMissingFunc <$> shrink fun
    FmapMapMissingFunc fun2 fun1 ->
      uncurry FmapMapMissingFunc <$> shrink (fun2, fun1)
