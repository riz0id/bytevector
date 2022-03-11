-- |

module Data.ByteVector
  ( -- * ByteVector
    ByteVector (ByteVector),
    freeze
  )
where

import Control.Monad.Primitive
import Data.Primitive.ByteArray

import Data.ByteVector.Mutable qualified as Mutable

-- -----------------------------------------------------------------------------

data ByteVector = ByteVector Int ByteArray

freeze :: PrimMonad m => Mutable.ByteVector (PrimState m) -> m ByteVector
freeze mvec = do
  len <- Mutable.sizeof mvec
  arr <- Mutable.toByteArray mvec
  pure (ByteVector len arr)
