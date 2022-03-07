
module Main (main) where

import Data.Word

import Data.ByteVector

--------------------------------------------------------------------------------

push'pair :: Word8 -> Word8 -> ByteVector -> ByteVector
push'pair w0 w1 = pushW8 w0 . pushW8 w1

main :: IO ()
main = do
  print (push'pair 10 20 empty)
  pure ()
