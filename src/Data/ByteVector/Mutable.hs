-- | TODO:
--
-- @since 1.0.0
module Data.ByteVector.Mutable
  ( -- * Mutable ByteVector
    ByteVector (ByteVector),
    sizeof,
    capacity,
    new,
    empty,
    resize,
    set,
    get,
    write,
    read,
    pushback,
    pack,
    toByteArray,
  )
where

import Control.Monad.Primitive
import Data.Primitive
import GHC.Exts
import GHC.Types
import GHC.Word

import Prelude hiding (read)

import Data.ByteVector.Mutable.Unlifted

--------------------------------------------------------------------------------

type ST# :: Type -> TYPE r -> Type
type ST# s a = State# s -> (# State# s, a #)

bytevector ::
  PrimMonad m =>
  ST# (PrimState m) (ByteVector# (PrimState m)) ->
  m (ByteVector (PrimState m))
bytevector k =
  primitive \st ->
    let !(# st', vec #) = k st
     in (# st', ByteVector vec #)

--------------------------------------------------------------------------------

-- | TODO:
--
-- @since 1.0.0
data ByteVector s = ByteVector (ByteVector# s)

-- | TODO:
--
-- @since 1.0.0
sizeof :: PrimMonad m => ByteVector (PrimState m) -> m Int
sizeof (ByteVector xs) =
  primitive \st ->
    let !(# st', s #) = sizeof# xs st
     in (# st', I# s #)

-- | TODO:
--
-- @since 1.0.0
capacity :: PrimMonad m => ByteVector (PrimState m) -> m Int
capacity (ByteVector xs) =
  primitive \st ->
    let !(# st', c #) = capacity# xs st
     in (# st', I# c #)

-- | TODO:
--
-- @since 1.0.0
empty :: PrimMonad m => m (ByteVector (PrimState m))
empty = bytevector empty#

-- | TODO:
--
-- @since 1.0.0
new :: PrimMonad m => Int -> m (ByteVector (PrimState m))
new (I# len) = bytevector (new# len)

-- | TODO:
--
-- @since 1.0.0
resize :: PrimMonad m => Int -> ByteVector (PrimState m) -> m ()
resize (I# len) (ByteVector xs) =
  primitive \st ->
    let st' = resize# len xs st
     in (# st', () #)

-- | TODO:
--
-- @since 1.0.0
set :: PrimMonad m => Int -> Word8 -> ByteVector (PrimState m) -> m ()
set = write

-- | TODO:
--
-- @since 1.0.0
get :: PrimMonad m => Int -> ByteVector (PrimState m) -> m Word8
get = read

-- | TODO:
--
-- @since 1.0.0
write :: (PrimMonad m, Prim a) => Int -> a -> ByteVector (PrimState m) -> m ()
write (I# i) x (ByteVector xs) =
  primitive \st ->
    let st' = set# i x xs st
     in (# st', () #)

-- | TODO:
--
-- @since 1.0.0
read :: (PrimMonad m, Prim a) => Int -> ByteVector (PrimState m) -> m a
read (I# i) (ByteVector xs) = primitive (get# i xs)

-- | TODO:
--
-- @since 1.0.0
pushback :: (PrimMonad m, Prim a) => a -> ByteVector (PrimState m) -> m ()
pushback x (ByteVector vec) = primitive \st -> (# pushback# x vec st, () #)

-- | TODO:
--
-- @since 1.0.0
pack :: PrimMonad m => [Word8] -> m (ByteVector (PrimState m))
pack ws = bytevector (pack# ws)

-- | TODO:
--
-- @since 1.0.0
-- unpack :: PrimMonad m => ByteVector (PrimState m) -> m [Word8]
-- unpack = undefined

-- | TODO:
--
-- @since 1.0.0
toByteArray :: PrimMonad m => ByteVector (PrimState m) -> m ByteArray
toByteArray (ByteVector vec#) =
  primitive \st ->
    let !(# st', arr# #) = toByteArray# vec# st
     in (# st', ByteArray arr# #)

-- toMutableByteArray :: PrimMonad m =>

-- -- | TODO:
-- --
-- -- @since 1.0.0
-- avaliable :: PrimMonad m => ByteVector (PrimState m) -> m Int
-- avaliable vec = do
--   capacity <- allocated vec
--   stored <- size vec
--   pure (max (capacity - stored) 0)

-- -- | TODO:
-- --
-- -- @since 1.0.0
-- compact :: PrimMonad m => ByteVector (PrimState m) -> m ()
-- compact (ByteVector var) = do
--   (len, mut) <- readMutVar var
--   mut' <- resizeMutableByteArray mut (ceilBytesToWord len)
--   writeMutVar var (len, mut')

-- -- | TODO:
-- --
-- -- @since 1.0.0
-- unpack :: PrimMonad m => ByteVector (PrimState m) -> m [Word8]
-- unpack vec = size vec >>= copy 0
--   where
--     copy idx len
--       | idx < len = liftA2 (:) (index idx vec) (copy (succ idx) len)
--       | otherwise = pure []
