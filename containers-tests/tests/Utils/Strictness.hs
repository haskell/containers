module Utils.Strictness
  ( Bot(..)
  , Func(..)
  , applyFunc
  , Func2(..)
  , applyFunc2
  , Func3(..)
  , applyFunc3
  ) where

import Test.ChasingBottoms.IsBottom (isBottom)
import Test.QuickCheck

{--------------------------------------------------------------------
  Bottom stuff
--------------------------------------------------------------------}

-- | Arbitrary (Bot a) values may be bottom
newtype Bot a = Bot a

instance Show a => Show (Bot a) where
  show (Bot x) = if isBottom x then "<bottom>" else show x

instance Arbitrary a => Arbitrary (Bot a) where
  arbitrary = frequency
    [ (1, pure (error "<bottom>"))
    , (4, Bot <$> arbitrary)
    ]

{--------------------------------------------------------------------
  Lazy functions
--------------------------------------------------------------------}

-- | Function which may be lazy in its argument
data Func a b
  = FuncLazy b
  | FuncStrict (Fun a b)

instance (Show a, Show b) => Show (Func a b) where
  show (FuncLazy x) = "{_lazy->" ++ show x ++ "}"
  show (FuncStrict fun) = show fun

applyFunc :: Func a b -> a -> b
applyFunc fun x = case fun of
  FuncLazy y -> y
  FuncStrict f -> applyFun f x

instance (CoArbitrary a, Function a, Arbitrary b) => Arbitrary (Func a b) where
  arbitrary = frequency
    [ (1, FuncLazy <$> arbitrary)
    , (4, FuncStrict <$> arbitrary)
    ]

  shrink fun = case fun of
    FuncLazy x -> FuncLazy <$> shrink x
    FuncStrict f -> FuncStrict <$> shrink f

-- | Function which may be lazy in its arguments

-- Note: We have two separate cases here because we want to generate functions
-- of type `a -> b -> c` with all possible strictness configurations.
-- `Func a (Func b c)` is not enough for this, since it cannot generate
-- functions that are conditionally lazy in the first argument, for instance:
--
-- leftLazyOr :: Bool -> Bool -> Bool
-- leftLazyOr a b = if b then True else a

data Func2 a b c
  = F2A (Func a (Func b c))
  | F2B (Func b (Func a c))
  deriving Show

instance
  (CoArbitrary a, Function a, CoArbitrary b, Function b, Arbitrary c)
  => Arbitrary (Func2 a b c) where
  arbitrary = oneof [F2A <$> arbitrary, F2B <$> arbitrary]

  shrink fun2 = case fun2 of
    F2A fun -> F2A <$> shrink fun
    F2B fun -> F2B <$> shrink fun

applyFunc2 :: Func2 a b c -> a -> b -> c
applyFunc2 fun2 x y = case fun2 of
  F2A fun -> applyFunc (applyFunc fun x) y
  F2B fun -> applyFunc (applyFunc fun y) x

-- | Function which may be lazy in its arguments

-- See Note on Func2.
data Func3 a b c d
  = F3A (Func a (Func2 b c d))
  | F3B (Func b (Func2 a c d))
  | F3C (Func c (Func2 a b d))
  deriving Show

instance
  ( CoArbitrary a, Function a
  , CoArbitrary b, Function b
  , CoArbitrary c, Function c
  , Arbitrary d
  )
  => Arbitrary (Func3 a b c d) where
  arbitrary = oneof [F3A <$> arbitrary, F3B <$> arbitrary, F3C <$> arbitrary]

  shrink fun3 = case fun3 of
    F3A fun -> F3A <$> shrink fun
    F3B fun -> F3B <$> shrink fun
    F3C fun -> F3C <$> shrink fun

applyFunc3 :: Func3 a b c d -> a -> b -> c -> d
applyFunc3 fun3 x y z = case fun3 of
  F3A fun -> applyFunc2 (applyFunc fun x) y z
  F3B fun -> applyFunc2 (applyFunc fun y) x z
  F3C fun -> applyFunc2 (applyFunc fun z) x y
