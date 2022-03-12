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
      bench "minInt:    slow" $ whnf slow'minInt (123 :: Int)
    , bench "minInt:    fast" $ whnf fast'minInt (123 :: Int)
    , --
      bench "posInt:    slow" $ whnf slow'posInt (123 :: Int)
    , bench "posInt:    fast" $ whnf fast'posInt (123 :: Int)
    , --
      bench "negInt:    slow" $ whnf slow'negInt (123 :: Int)
    , bench "negInt:    fast" $ whnf fast'negInt (123 :: Int)
    , --
      bench "ceilBytes: slow" $ whnf slow'ceilBytesToWord (123 :: Int)
    , bench "ceilBytes: fast" $ whnf fast'ceilBytesToWord (123 :: Int)
    ]

-- -----------------------------------------------------------------------------
--
-- 'maxInt'
--

slow'maxInt :: Int -> Int
slow'maxInt x = max 100 x

fast'maxInt :: Int -> Int
fast'maxInt (I# x) = I# (maxInt# 100# x)

-- -----------------------------------------------------------------------------
--
-- 'minInt'
--

slow'minInt :: Int -> Int
slow'minInt x = max -100 x

fast'minInt :: Int -> Int
fast'minInt (I# x) = I# (minInt# -100# x)

-- -----------------------------------------------------------------------------
--
-- 'posInt'
--

slow'posInt :: Int -> Int
slow'posInt x = max 0 x

fast'posInt :: Int -> Int
fast'posInt (I# x) = I# (posInt# x)

-- -----------------------------------------------------------------------------
--
-- 'negInt'
--

slow'negInt :: Int -> Int
slow'negInt x = max 0 x

fast'negInt :: Int -> Int
fast'negInt (I# x) = I# (negInt# x)

-- -----------------------------------------------------------------------------
--
-- 'ceilBytesToWord'
--

slow'ceilBytesToWord :: Int -> Int
slow'ceilBytesToWord x =
  let size = sizeOf x
   in (x + size - 1) .&. negate size

fast'ceilBytesToWord :: Int -> Int
fast'ceilBytesToWord (I# x) = I# (ceilBytesToWord# x)
