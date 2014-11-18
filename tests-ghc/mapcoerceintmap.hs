{-# LANGUAGE MagicHash #-}

import GHC.Exts hiding (fromList)
import Unsafe.Coerce
import Data.IntMap.Lazy

newtype Age = Age Int

fooAge :: IntMap Int -> IntMap Age
fooAge = fmap Age
fooCoerce :: IntMap Int -> IntMap Age
fooCoerce = fmap coerce
fooUnsafeCoerce :: IntMap Int -> IntMap Age
fooUnsafeCoerce = fmap unsafeCoerce

same :: a -> b -> IO ()
same x y = case reallyUnsafePtrEquality# (unsafeCoerce x) y of
    1# -> putStrLn "yes"
    _  -> putStrLn "no"

main = do
    let l = fromList [(1,1),(2,2),(3,3)]
    same (fooAge l) l
    same (fooCoerce l) l
    same (fooUnsafeCoerce l) l
