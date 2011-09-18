{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Monad
import Criterion
import Criterion.Measurement
import Criterion.Environment
import Data.List (foldl')
import qualified Data.DenseIntSet as DS
import qualified Data.IntSet as S
import Control.Monad
import Text.Printf
import Statistics.Sample
import Data.Colour.Names
import Data.Colour
import Data.List
import Data.Maybe
import Data.Monoid

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Gtk
import Data.Accessor

instance NFData S.IntSet where
    rnf S.Nil = ()
    rnf (S.Tip a) = rnf a
    rnf (S.Bin p m l r) = rnf p `seq` rnf m `seq` rnf l `seq` rnf r

instance NFData DS.IntSet where
    rnf DS.Nil = ()
    rnf (DS.Tip a b) = rnf a `seq` rnf b
    rnf (DS.Bin p m l r) = rnf p `seq` rnf m `seq` rnf l `seq` rnf r

benchPair (s,ds) =
    ( bench "memberS" $ nf (member [0..1000]) s
    , bench "memberDS" $ nf (memberD [0..1000]) ds )

benches = [
    {-( "member"
    , \step size -> do
        let s = DS.fromAscList [0,step..2^size*step]
        let probe = 2^(pred size)
        liftIO . evaluate $ rnf (s,probe)
        return $ nf (DS.member probe) s
    , \step size -> do
        let s = S.fromAscList [0,step..2^size*step]
        let probe = 2^(pred size)
        liftIO . evaluate $ rnf (s,probe)
        return $ nf (S.member probe) s
    ),
    ( "insert"
    , \step size -> do
        let s = DS.fromAscList [0,step..2^size*step]
        let probe = 2^(pred size)
        liftIO . evaluate $ rnf (s,probe)
        return $ nf (DS.insert probe) s
    , \step size -> do
        let s = S.fromAscList [0,step..2^size*step]
        let probe = 2^(pred size)
        liftIO . evaluate $ rnf (s,probe)
        return $ nf (S.insert probe) s
    ),-}
    ( "toList"
    , \step size -> do
        let s = DS.fromAscList [0,step..2^size*step]
        liftIO . evaluate $ rnf s
        return $ nf (DS.toList) s
    , \step size -> do
        let s = S.fromAscList [0,step..2^size*step]
        liftIO . evaluate $ rnf s
        return $ nf (S.toList) s
    ){-,
    ( "intersection"
    , \step size -> do
        let s1 = DS.fromAscList [0,step..2^size*step]
            s2 = DS.fromAscList [2^(pred size)*step,2^(pred size)*step + step..2^(pred size)*step+2^size*step]
        liftIO . evaluate $ rnf (s1,s2)
        return $ nf (uncurry DS.intersection) (s1,s2)
    , \step size -> do
        let s1 = S.fromAscList [0,step..2^size*step]
            s2 = S.fromAscList [2^(pred size)*step,2^(pred size)*step + step..2^(pred size)*step+2^size*step]
        liftIO . evaluate $ rnf (s1,s2)
        return $ nf (uncurry S.intersection) (s1,s2)
    ),
    ( "union"
    , \step size -> do
        let s1 = DS.fromAscList [0,step..2^size*step]
            s2 = DS.fromAscList [2^(pred size)*step,2^(pred size)*step + step..2^(pred size)*step+2^size*step]
        liftIO . evaluate $ rnf (s1,s2)
        return $ nf (uncurry DS.union) (s1,s2)
    , \step size -> do
        let s1 = S.fromAscList [0,step..2^size*step]
            s2 = S.fromAscList [2^(pred size)*step,2^(pred size)*step + step..2^(pred size)*step+2^size*step]
        liftIO . evaluate $ rnf (s1,s2)
        return $ nf (uncurry S.union) (s1,s2)
    )-}
    ]

configurations = [
        (name,
        [
        (step, dense, [ (size, (if dense then denseBench else regularBench) step size) |
            size <- [10,13,16,19,22::Int]
        ]) |
        dense <- [False, True],
        step <- steps
        ])
    | (name,denseBench,regularBench) <- benches ]

colors = [ aliceblue, bisque, black, chocolate, darkgoldenrod, darkmagenta ]
steps = [1,4,16,64,100]

main = withConfig (defaultConfig { cfgSamples = Last (Just 100) } )$ do
    env <- measureEnvironment
    layouts <- forM configurations $ \(name, serieses) -> do 
        plots <- forM serieses $ \(step,dense,benches) -> do
            let series = printf "%s, %d" (if dense then "Dense" else "Regular") step
            values <- forM benches $ \(size,runBench) -> do 
                liftIO $ printf "Running %s, set size %d, step %d, variant %s: "
                    name size step (if dense then "Dense" else "Regular")
                bench <- runBench
                sample <- runBenchmark env bench
                let m = mean sample
                liftIO $ putStrLn (secs m)
                return (size,m)
            return $
                plot_lines_style ^= (
                    (if dense then dashedLine 1 [4,2] else solidLine 1) $
                    (defaultColorSeq !! fromJust (step `elemIndex` steps))
                ) $
                plot_lines_values ^= [values] $
                plot_lines_title ^= series $
                defaultPlotLines

        let layout = layout1_title ^= name $
                     layout1_plots ^= map (Right . toPlot) plots $
                     layout1_right_axis ^= relabelAxis secs defaultLayoutAxis $
                     layout1_bottom_axis ^= relabelAxis (("2^" ++ ) . show) defaultLayoutAxis $
                     defaultLayout1
        return layout

    let grid = aboveN (map (flip tspan (1,1)) layouts)

    liftIO $ renderableToPDFFile (toRenderable grid) 600 (300*length layouts) "comparison.pdf" 

relabelAxis func = 
    laxis_override ^: (
        (.) $ axis_labels ^: map (map (\(d,_) -> (d,func d)))
    )

member :: [Int] -> S.IntSet -> Int
member xs s = foldl' (\n x -> if S.member x s then n + 1 else n) 0 xs

memberD :: [Int] -> DS.IntSet -> Int
memberD xs s = foldl' (\n x -> if DS.member x s then n + 1 else n) 0 xs

ins :: [Int] -> S.IntSet -> S.IntSet
ins xs s0 = foldl' (\s a -> S.insert a s) s0 xs

del :: [Int] -> S.IntSet -> S.IntSet
del xs s0 = foldl' (\s k -> S.delete k s) s0 xs
