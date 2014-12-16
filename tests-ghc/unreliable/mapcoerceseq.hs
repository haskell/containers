{-# LANGUAGE MagicHash #-}

import GHC.Exts hiding (fromList)
import Unsafe.Coerce
import Data.Sequence

newtype Age = Age Int

fooAge :: Seq Int -> Seq Age
fooAge = fmap Age
fooCoerce :: Seq Int -> Seq Age
fooCoerce = fmap coerce
fooUnsafeCoerce :: Seq Int -> Seq Age
fooUnsafeCoerce = fmap unsafeCoerce

same :: a -> b -> IO ()
same x y = case reallyUnsafePtrEquality# (unsafeCoerce x) y of
    1# -> putStrLn "yes"
    _  -> putStrLn "no"

main = do
    let l = fromList [1,2,3]
    same (fooAge l) l
    same (fooCoerce l) l
    same (fooUnsafeCoerce l) l
