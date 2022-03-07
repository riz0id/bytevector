{-# LANGUAGE TypeFamilies #-}

-- | TODO:
--
-- @since 1.0.0
module Data.ByteVector
  ( -- * Byte Vectors
    ByteVector (ByteVector),

    -- ** Construction
    empty,
    newByteVector,

    -- ** Index,
    index,

    -- ** Index,
    writeW8,

    -- ** Query
    size,
    capacity,

    -- ** Packing
    packW,
    packW8,

    -- ** Unpacking
    unpackW8,

    -- ** Unpacking
    pushW8,

    -- ** Conversion
    toByteArray,
  )
where

import Data.Primitive.ByteArray
import GHC.Exts
import GHC.Word

import Data.ByteVector.Unboxed

--------------------------------------------------------------------------------

-- | TODO:
--
-- @since 1.0.0
data ByteVector = ByteVector ByteVector#

-- | @since 1.0.0
instance IsList ByteVector where
  type Item ByteVector = Word8

  toList = unpackW8
  {-# INLINE CONLIKE [1] Exts.toList #-}

  fromList = packW8
  {-# INLINE CONLIKE [1] fromList #-}

  fromListN _ = packW8 -- TODO: recheck
  {-# INLINE CONLIKE [1] fromListN #-}

-- | @since 1.0.0
instance Show ByteVector where
  show = show . unpackW8
  {-# INLINE show #-}

--------------------------------------------------------------------------------
-- Construction

-- | TODO:
--
-- @since 1.0.0
empty :: ByteVector
empty = case runRW# empty# of
  (# _, bs #) -> ByteVector bs

-- | TODO:
--
-- @since 1.0.0
newByteVector :: Int -> ByteVector
newByteVector (I# sz) =
  case runRW# (alloc# sz) of
    (# _, bs #) -> ByteVector bs

--------------------------------------------------------------------------------
-- Index

-- | TODO:
--
-- @since 1.0.0
index :: Int -> ByteVector -> Word8
index (I# i) (ByteVector bs) = W8# (readW8# bs i)

--------------------------------------------------------------------------------
-- Write

-- | TODO:
--
-- @since 1.0.0
writeW8 :: Int -> Word8 -> ByteVector -> ByteVector
writeW8 (I# i) (W8# x) (ByteVector bs) =
  case runRW# (writeW8# bs i x) of
    (# _, bs' #) -> ByteVector bs'

--------------------------------------------------------------------------------
-- Query

-- | TODO:
--
-- @since 1.0.0
size :: ByteVector -> Int
size (ByteVector bs) = I# (size# bs)

-- | TODO:
--
-- @since 1.0.0
capacity :: ByteVector -> Int
capacity (ByteVector bs) = I# (capacity# bs)

--------------------------------------------------------------------------------
-- Packing

-- | Pack a list of 'Word' into a 'ByteVector'.
--
-- @since 1.0.0
packW :: [Word] -> ByteVector
packW ws = case runRW# (packW# ws) of
  (# _, bytes #) -> ByteVector bytes

-- | Pack a list of 'Word8' into a 'ByteVector'.
--
-- @since 1.0.0
packW8 :: [Word8] -> ByteVector
packW8 ws = case runRW# (packW8# ws) of
  (# _, bytes #) -> ByteVector bytes

-- | Pack a list of 'Word8' into a 'ByteVector'.
--
-- @since 1.0.0
-- packW16 :: [Word16] -> ByteVector
-- packW16 ws = runST do
--   ST \st -> case U.packWord16# ws st of
--     (# st', bs #) -> (# st', ByteVector bs #)

--------------------------------------------------------------------------------
-- Unpacking

-- | TODO:
--
-- @since 1.0.0
unpackW8 :: ByteVector -> [Word8]
unpackW8 (ByteVector bs) =
  case runRW# (unpackW8# bs) of
    (# _, ws #) -> ws

--------------------------------------------------------------------------------
-- Push

pushW8 :: Word8 -> ByteVector -> ByteVector
pushW8 (W8# x) (ByteVector bs) =
  case runRW# (pushW8# bs x) of
    (# _, bytes #) -> ByteVector bytes

--------------------------------------------------------------------------------
-- Conversion

-- | TODO:
--
-- @since 1.0.0
toByteArray :: ByteVector -> ByteArray
toByteArray (ByteVector bs) = ByteArray (array# bs)
