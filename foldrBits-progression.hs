{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion
import Progression.Main
import Data.List (foldl')
import qualified Data.DenseIntSet as DS
import qualified Data.IntSet as S

instance NFData S.IntSet where
    rnf S.Nil = ()
    rnf (S.Tip a) = rnf a
    rnf (S.Bin p m l r) = rnf p `seq` rnf m `seq` rnf l `seq` rnf r

instance NFData DS.IntSet where
    rnf DS.Nil = ()
    rnf (DS.Tip a b) = rnf a `seq` rnf b
    rnf (DS.Bin p m l r) = rnf p `seq` rnf m `seq` rnf l `seq` rnf r

main = do
    let l = [2^n-1 | n<-[1..64]] ++ [2^n | n<-[1..64]]
        s = DS.fromList $ [0,2 ..2^20] ++ [0,100 ..2^20] 
    liftIO . evaluate $ rnf (l,s)
    defaultMain $ bgroup "" [
          bench "foldr" $ nf (map (DS.foldrBits 0 (+) 0)) l
        , bench "toList" $ nf (DS.toList) s
        ]


