
module Test.Data.ByteVector.Gen
  ( -- * Generators
    list'word8,
    list'word16,
    list'word32,
  )
where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Data.Word

--------------------------------------------------------------------------------

list'word8 :: MonadGen m => m [Word8]
list'word8 = Gen.sized \s -> do
  let range = Range.linear 0 (fromIntegral s `max` 0)
  Gen.list range (Gen.word8 Range.linearBounded)

list'word16 :: MonadGen m => m [Word16]
list'word16 = Gen.sized \s -> do
  let range = Range.linear 0 (fromIntegral s `max` 0)
  Gen.list range (Gen.word16 Range.linearBounded)

list'word32 :: MonadGen m => m [Word32]
list'word32 = Gen.sized \s -> do
  let range = Range.linear 0 (fromIntegral s `max` 0)
  Gen.list range (Gen.word32 Range.linearBounded)
