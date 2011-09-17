import qualified Data.IntSet as S
import qualified Data.DenseIntSet as DS
import System.Environment

main = do
    [what, sizeS, stepS] <- getArgs
    let list = [0,read stepS .. read stepS * read sizeS]
    if what `elem` ["r","R"] then  S.size ( S.fromList list) `seq` return ()
                             else DS.size (DS.fromList list) `seq` return ()
        
