{-# LANGUAGE MagicHash #-}

-- |
module Data.ByteVector.Mutable
  ( -- * Mutable ByteVector
    ByteVector (ByteVector),

    -- ** Construction
    empty,

    -- ** Read
    readW,
    readW8,
    readW16,
    readW32,
    readW64,

    -- ** Write
    writeW,

    -- ** Query
    size,
    capacity,

    -- ** Packing
    packW,
    packW8,
    -- packW16,
    -- packW32,
    -- packW64,

    -- ** Packing
    pushW8,

    -- ** Unpacking
    unpackW8,

    -- ** Conversion
    toByteArray,
  )
where

import Data.Primitive (ByteArray (ByteArray))
import GHC.ST
import GHC.Exts
import GHC.Word

import Data.ByteVector.Mutable.Unboxed

--------------------------------------------------------------------------------

data ByteVector s = ByteVector (ByteVector# s)

-- | TODO:
--
-- @since 1.0.0
capacity :: ByteVector s -> Int
capacity (ByteVector bs) = I# (capacity# bs)
{-# INLINE CONLIKE capacity #-}

-- | TODO:
--
-- @since 1.0.0
size :: ByteVector s -> Int
size (ByteVector bs) = I# (size# bs)
{-# INLINE CONLIKE size #-}

--------------------------------------------------------------------------------
-- Construction

-- | TODO:
--
-- @since 1.0.0
empty :: ST s (ByteVector s)
empty = ST \st -> case empty# st of
  (# st', bs #) -> (# st', ByteVector bs #)
{-# INLINE CONLIKE empty #-}

--------------------------------------------------------------------------------
-- Read

-- | TODO:
--
-- @since 1.0.0
readW :: Int -> ByteVector s -> ST s Word
readW (I# i) (ByteVector bs) =
  ST \st -> case readW# bs i st of
    (# st', x #) -> (# st', W# x #)

-- | TODO:
--
-- @since 1.0.0
readW8 :: Int -> ByteVector s -> ST s Word8
readW8 (I# i) (ByteVector bs) =
  ST \st -> case readW8# bs i st of
    (# st', x #) -> (# st', W8# x #)

-- | TODO:
--
-- @since 1.0.0
readW16 :: Int -> ByteVector s -> ST s Word16
readW16 (I# i) (ByteVector bs) =
  ST \st -> case readW16# bs i st of
    (# st', x #) -> (# st', W16# x #)

-- | TODO:
--
-- @since 1.0.0
readW32 :: Int -> ByteVector s -> ST s Word32
readW32 (I# i) (ByteVector bs) =
  ST \st -> case readW32# bs i st of
    (# st', x #) -> (# st', W32# x #)

-- | TODO:
--
-- @since 1.0.0
readW64 :: Int -> ByteVector s -> ST s Word64
readW64 (I# i) (ByteVector bs) =
  ST \st -> case readW64# bs i st of
    (# st', x #) -> (# st', W64# x #)

--------------------------------------------------------------------------------
-- Modification

-- | TODO:
--
-- @since 1.0.0
writeW :: ByteVector s -> Int -> Word -> ST s ()
writeW (ByteVector bs) (I# i) (W# x) =
  ST \s -> case writeW# bs i x s of
    st' -> (# st', () #)

--------------------------------------------------------------------------------
-- Packing

pushW8 :: ByteVector s -> Word8 -> ST s (ByteVector s)
pushW8 (ByteVector bs) (W8# x) =
  ST \st -> case pushW8# bs x st of
    (# st', bs' #) -> (# st', ByteVector bs' #)

--------------------------------------------------------------------------------
-- Packing

-- | TODO:
--
-- @since 1.0.0
packW :: [Word] -> ST s (ByteVector s)
packW ws = ST \st -> case packW# ws st of
  (# st', bs #) -> (# st', ByteVector bs #)

-- | TODO:
--
-- @since 1.0.0
packW8 :: [Word8] -> ST s (ByteVector s)
packW8 ws = ST \st -> case packW8# ws st of
  (# st', bs #) -> (# st', ByteVector bs #)

-- -- | TODO:
-- --
-- -- @since 1.0.0
-- packW16 :: [Word8] -> ST s (ByteVector s)
-- packW16 ws = ST \st -> case packW16# ws st of
--   (# st', bs #) -> (# st', ByteVector bs #)

-- -- | TODO:
-- --
-- -- @since 1.0.0
-- packW32 :: [Word] -> ST s (ByteVector s)
-- packW32 ws = ST \st -> case packW# ws st of
--   (# st', bs #) -> (# st', ByteVector bs #)

-- -- | TODO:
-- --
-- -- @since 1.0.0
-- packW64 :: [Word8] -> ST s (ByteVector s)
-- packW64 ws = ST \st -> case packW8# ws st of
--   (# st', bs #) -> (# st', ByteVector bs #)

--------------------------------------------------------------------------------
-- Unpacking

unpackW8 :: ByteVector s -> ST s [Word8]
unpackW8 (ByteVector bs) = ST (unpackW8# bs)

--------------------------------------------------------------------------------
-- Conversion

-- | TODO:
--
-- @since 1.0.0
toByteArray :: ByteVector s -> ST s ByteArray
toByteArray (ByteVector bs) =
  ST \st -> case toByteArray# bs st of
    (# st', array #) -> (# st' , ByteArray array #)
