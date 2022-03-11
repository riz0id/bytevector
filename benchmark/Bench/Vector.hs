module Bench.Vector where

import Criterion.Main (Benchmark, bench, bgroup, whnf, whnfIO)

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Word
import Foreign.Storable
import GHC.Types

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString

import Data.ByteVector
import Data.ByteVector.Mutable qualified as Mutable

-- -----------------------------------------------------------------------------

benchmark :: Benchmark
benchmark =
  bgroup
    "Data.Vector"
    [ bench "pushback: bytevector" $ whnfIO (pushback'bytevector 10000)
    , bench "pushback: bytestring" $ whnf pushback'bytestring 10000
    ]

pushback'bytevector :: Int -> IO ByteVector
pushback'bytevector n = do
  vec <- Mutable.empty
  replicateM_ n do
    Mutable.pushback (10 :: Word8) vec
  freeze vec

pushback'bytestring :: Int -> ByteString
pushback'bytestring n = go n ByteString.empty
  where
    go 0 bs = bs
    go i bs = go (i - 1) (ByteString.snoc bs 10)
