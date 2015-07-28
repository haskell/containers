{-# LANGUAGE MagicHash #-}

import GHC.Exts (reallyUnsafePtrEquality#)
import Unsafe.Coerce
import Data.Coerce
import Data.Map

newtype Age = Age Int

fooAge :: Map Int Int -> Map Age Int
fooAge = mapKeysMonotonic Age
fooCoerce :: Map Int Int -> Map Age Int
fooCoerce = mapKeysMonotonic coerce
fooUnsafeCoerce :: Map Int Int -> Map Age Int
fooUnsafeCoerce = mapKeysMonotonic unsafeCoerce

barAge :: Map Int Int -> Map Int Age
barAge = Data.Map.map Age
barCoerce :: Map Int Int -> Map Int Age
barCoerce = Data.Map.map coerce
barUnsafeCoerce :: Map Int Int -> Map Int Age
barUnsafeCoerce = Data.Map.map unsafeCoerce

same :: a -> b -> IO ()
same x y = case reallyUnsafePtrEquality# (unsafeCoerce x) y of
    1# -> putStrLn "yes"
    _  -> putStrLn "no"

main = do
    let m  = fromList [(1,1), (2,2), (3,3)]
    same (fooAge m) m
    same (fooCoerce m) m
    same (fooUnsafeCoerce m) m
    same (barAge m) m
    same (barCoerce m) m
    same (barUnsafeCoerce m) m
