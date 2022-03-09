{-# LANGUAGE MagicHash #-}

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
    pushback,
    pack,
  )
where

import Control.Monad.Primitive
import Data.Foldable (traverse_)
import Data.Primitive
import GHC.Exts

import Data.ByteVector.Mutable.Unlifted

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
empty =
  primitive \st ->
    let !(# st', vec #) = empty# st
     in (# st', ByteVector vec #)

-- | TODO:
--
-- @since 1.0.0
new :: PrimMonad m => Int -> m (ByteVector (PrimState m))
new (I# len) =
  primitive \st ->
    let !(# st', vec #) = new# len st
     in (# st', ByteVector vec #)

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
set :: (PrimMonad m, Prim a) => Int -> a -> ByteVector (PrimState m) -> m ()
set (I# i) x (ByteVector xs) =
  primitive \st ->
    let st' = set# i x xs st
     in (# st', () #)

-- | TODO:
--
-- @since 1.0.0
get :: (PrimMonad m, Prim a) => Int -> ByteVector (PrimState m) -> m a
get (I# i) (ByteVector xs) = primitive (get# i xs)

-- | TODO:
--
-- @since 1.0.0
pushback :: (PrimMonad m, Prim a) => a -> ByteVector (PrimState m) -> m ()
pushback x (ByteVector vec) = primitive \st -> (# pushback# x vec st, () #)

-- | TODO:
--
-- @since 1.0.0
pack :: (PrimMonad m, Prim a) => [a] -> m (ByteVector (PrimState m))
pack = \case
  [] -> empty
  x0 : xs0 -> do
    vec <- new (sizeOf x0 + quot (sizeOf x0) 2)
    traverse_ (`pushback` vec) (x0 : xs0)
    pure vec

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
