module Main (main) where

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, whnf)
import Criterion.Types ()

import Bench.Int qualified
import Bench.Vector qualified

--------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain
    [ Bench.Int.benchmark
    , Bench.Vector.benchmark
    ]
