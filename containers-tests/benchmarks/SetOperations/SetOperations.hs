{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SetOperations (benchmark) where

import Gauge (bench, defaultMain, whnf)
import Data.List (partition, sortBy)
import Data.Ord (comparing)
import Data.Tuple as Tuple

-- | Benchmark a set operation for the given container.
-- Takes the following arguments:
-- * A way to construct the container
-- * Flag if we should benchmark the operations with reversed arguments.
-- * A list of operations.
benchmark :: forall container. (Show container, Eq container) => ([Int] -> container) -> Bool -> [(String, container -> container -> container)] -> IO ()
benchmark fromList swap methods = do

  defaultMain $ [ bench (method_str++"-"++input_str ++ "_" ++ data_sizes) $
                        whnf (method input1) input2

                | (method_str, method) <- methods
                , (input_str, data_sizes, (input1, input2)) <- sortBenchs (base_inputs ++ swapped_input)
                ]

  where
    -- Sort benchmark inputs by (data variant, data sizes)
    sortBenchs = sortBy (comparing (\(name,size,_) -> (name,size)))

    -- Data size descriptions, also used in the benchmark names.
    -- They are used to describe how large the input data is, but NOT the data itself.
    -- So for example nn_swap /= nn since the data size for both arguments is the same
    -- but the actual data is different.
    n, s, t :: Int
    n = 100000
    s {-small-} = n `div` 10
    t {-tiny-} = round $ sqrt $ fromIntegral n

    base_inputs :: [(String,String,(container,container))]
    base_inputs = [ ("disj", "nn", disj_nn), ("disj","ns", disj_ns), ("disj","nt", disj_nt)
                                            , ("common","nn", common_nn), ("common","ns", common_ns), ("common","nt", common_nt)
                                            , ("mix","nn", mix_nn), ("mix","ns", mix_ns), ("mix","nt", mix_nt)
                                            , ("block","nn", block_nn), ("block","ns", block_ns)
                                            ]

    -- Input with set arguments swapped.
    swapped_input
      | swap = map swap_input base_inputs
      | otherwise = []

    -- Reverse arguments
    swap_input (name, data_sizes, input_data) =
        (name, reverse data_sizes ++ "_swap", Tuple.swap input_data)

    -- Data variants
    all_n = fromList [1..n]

    !disj_nn = seqPair $ (all_n, fromList [n+1..n+n])
    !disj_ns = seqPair $ (all_n, fromList [n+1..n+s])
    !disj_nt = seqPair $ (all_n, fromList [n+1..n+t])
    !common_nn = seqPair $ (all_n, fromList [2,4..n])
    !common_ns = seqPair $ (all_n, fromList [0,1+n`div`s..n])
    !common_nt = seqPair $ (all_n, fromList [0,1+n`div`t..n])
    !mix_nn = seqPair $ fromLists $ partition ((/= 0) . (`mod` 2)) [1..n+n]
    !mix_ns = seqPair $ fromLists $ partition ((/= 0) . (`mod` (1 + n`div`s))) [1..s+n]
    !mix_nt = seqPair $ fromLists $ partition ((/= 0) . (`mod` (1 + n`div`t))) [1..t+n]
    !block_nn = seqPair $ fromLists $ partition ((>= t) . (`mod` (t * 2))) [1..n+n]
    !block_ns = seqPair $ fromLists $ partition ((>= t) . (`mod` (t * (1 + n`div`s)))) [1..s+n]

    fromLists (xs, ys) = (fromList xs, fromList ys)
    seqPair pair@(xs, ys) = xs `seq` ys `seq` pair
