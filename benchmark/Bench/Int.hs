{-# LANGUAGE MagicHash #-}

module Bench.Int where

import Criterion.Main (Benchmark, bench, bgroup, whnf)

import Data.Bits
import Foreign.Storable
import GHC.Types

import Data.Int.Unlifted

--------------------------------------------------------------------------------

benchmark :: Benchmark
benchmark =
  bgroup
    "Data.Int.Unlifted"
    [ bench "maxInt:    slow" $ whnf slow'maxInt (123 :: Int)
    , bench "maxInt:    fast" $ whnf fast'maxInt (123 :: Int)
    , --
      bench "posInt:    slow" $ whnf slow'posInt (123 :: Int)
    , bench "posInt:    fast" $ whnf fast'posInt (123 :: Int)
    , --
      bench "ceilBytes: slow" $ whnf slow'ceilBytesToWord (123 :: Int)
    , bench "ceilBytes: fast" $ whnf fast'ceilBytesToWord (123 :: Int)
    ]

slow'maxInt :: Int -> Int
slow'maxInt x = max 100 x

fast'maxInt :: Int -> Int
fast'maxInt (I# x) = I# (maxInt# 100# x)

slow'posInt :: Int -> Int
slow'posInt x = max 0 x

fast'posInt :: Int -> Int
fast'posInt (I# x) = I# (posInt# x)

slow'ceilBytesToWord :: Int -> Int
slow'ceilBytesToWord x =
  let size = sizeOf x
   in (x + size - 1) .&. negate size

fast'ceilBytesToWord :: Int -> Int
fast'ceilBytesToWord (I# x) = I# (ceilBytesToWord# x)
