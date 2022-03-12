module Main (main) where

import Criterion.Main (defaultMain)

import Bench.Int qualified
import Bench.Vector qualified

--------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain
    [ Bench.Int.benchmark
    , Bench.Vector.benchmark
    ]
