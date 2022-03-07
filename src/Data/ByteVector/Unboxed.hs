{-# LANGUAGE NamedFieldPuns #-}

-- | TODO:
--
-- @since 1.0.0
module Data.ByteVector.Unboxed
  ( -- * Byte Vectors
    ByteVector# (ByteVector#, capacity#, size#, array#),

    -- ** Construction
    empty#,
    alloc#,

    -- ** Freeze & Thaw
    freeze#,

    -- ** Index

    -- ** Read
    readW#,
    readW8#,
    readW16#,
    readW32#,
    readW64#,

    -- ** Write
    writeW8#,

    -- ** Push
    pushW8#,

    -- ** Packing
    packW#,
    packW8#,

    -- ** Unpacking
    unpackW8#,

    -- ** Conversion
    toByteArray#,
  )
where

import Data.Word
import Debug.Trace qualified as Debug
import GHC.Prim
import GHC.Exts

import Data.ByteVector.Rep (ByteVectorRep)
import Data.ByteVector.Mutable.Unboxed qualified as Mutable

--------------------------------------------------------------------------------

-- | TODO:
--
-- @since 1.0.0
newtype ByteVector# :: TYPE ByteVectorRep where
  MkByteVector :: (# Int#, Int#, ByteArray# #) -> ByteVector#

{-# COMPLETE ByteVector# #-}

-- | TODO:
--
-- @since 1.0.0
pattern ByteVector# :: Int# -> Int# -> ByteArray# -> ByteVector#
pattern ByteVector# {capacity#, size#, array#} =
  MkByteVector (# capacity#, size#, array# #)

--------------------------------------------------------------------------------
-- Construction

-- | TODO:
--
-- @since 1.0.0
empty# :: State# s -> (# State# s, ByteVector# #)
empty# st = case Mutable.empty# st of
  (# st', mut #) -> freeze# mut st'

-- | TODO:
--
-- @since 1.0.0
alloc# :: Int# -> State# s -> (# State# s, ByteVector# #)
alloc# sz st = case Mutable.alloc# sz st of
  (# st', mut #) -> freeze# mut st'

--------------------------------------------------------------------------------
-- Freeze & Thaw

thaw# :: ByteVector# -> State# s -> (# State# s, Mutable.ByteVector# s #)
thaw# ByteVector# {capacity#, size#, array#} st =
  let !(# st0, dst #) = newByteArray# capacity# st
      st1 = copyByteArray# array# 0# dst 0# capacity# st0
   in (# st1, Mutable.ByteVector# capacity# size# dst #)

  -- case thawArray# array# 0# capacity# st of
  --   (# st', mut #) -> (# st', Mutable.ByteVector# capacity# size# mut #)

-- | TODO:
--
-- @since 1.0.0
freeze# :: Mutable.ByteVector# s -> State# s -> (# State# s, ByteVector# #)
freeze# mut st =
  case Mutable.toByteArray# mut st of
    (# st', array #) ->
      let cz = Mutable.capacity# mut
          sz = Mutable.size# mut
          !_ = Debug.trace (show (I# cz)) ()
       in (# st', ByteVector# cz sz array #)

--------------------------------------------------------------------------------
-- Read

-- | TODO:
--
-- @since 1.0.0
readW# :: ByteVector# -> Int# -> Word#
readW# ByteVector# {array#} = indexWordArray# array#
{-# INLINE CONLIKE [1] readW# #-}

-- | TODO:
--
-- @since 1.0.0
readW8# :: ByteVector# -> Int# -> Word8#
readW8# ByteVector# {array#} = indexWord8Array# array#
{-# INLINE CONLIKE [1] readW8# #-}

-- | TODO:
--
-- @since 1.0.0
readW16# :: ByteVector# -> Int# -> Word16#
readW16# ByteVector# {array#} = indexWord16Array# array#
{-# INLINE CONLIKE [1] readW16# #-}

-- | TODO:
--
-- @since 1.0.0
readW32# :: ByteVector# -> Int# -> Word32#
readW32# ByteVector# {array#} = indexWord32Array# array#
{-# INLINE CONLIKE [1] readW32# #-}

-- | TODO:
--
-- @since 1.0.0
readW64# :: ByteVector# -> Int# -> Word#
readW64# ByteVector# {array#} = indexWord64Array# array#
{-# INLINE CONLIKE [1] readW64# #-}

--------------------------------------------------------------------------------
-- Write

-- | TODO:
--
-- @since 1.0.0
-- writeW# :: ByteVector# -> Int# -> Word#
-- writeW# ByteVector# {array#} = indexWordArray# array#
-- {-# INLINE CONLIKE [1] writeW# #-}

-- | TODO:
--
-- @since 1.0.0
writeW8# :: ByteVector# -> Int# -> Word8# -> State# s -> (# State# s, ByteVector# #)
writeW8# bs i x st =
  let !(# st0, mut #) = thaw# bs st
      st1 = Mutable.writeW8# mut i x st0
   in freeze# mut st1
{-# INLINE CONLIKE [1] writeW8# #-}

--------------------------------------------------------------------------------
-- Conversion

pushW8# :: ByteVector# -> Word8# -> State# s -> (# State# s, ByteVector# #)
pushW8# bs x st =
  let !(# st0, mut0 #) = thaw# bs st
      !(# st1, mut1 #) = Mutable.pushW8# mut0 x st0
   in freeze# mut1 st1

--------------------------------------------------------------------------------
-- Packing

-- | TODO:
--
-- @since 1.0.0
packW# :: [Word] -> State# s -> (# State# s, ByteVector# #)
packW# ws st = case Mutable.packW# ws st of
  (# st', mut #) -> freeze# mut st'

-- | TODO:
--
-- @since 1.0.0
packW8# :: [Word8] -> State# s -> (# State# s, ByteVector# #)
packW8# ws st =
  case Mutable.packW8# ws st of
    (# st0, mut0 #) -> case unsafeFreezeByteArray# (Mutable.mutable# mut0) st0 of
      (# st1, array #) ->
        let cz = Mutable.capacity# mut0
            sz = Mutable.size# mut0
         in (# st1, ByteVector# cz sz array #)

--------------------------------------------------------------------------------
-- Unpacking

unpackW8# :: ByteVector# -> State# s -> (# State# s, [Word8] #)
unpackW8# bs st =
  let !(# st0, mut #) = thaw# bs st
      !(# st1, ws  #) = Mutable.unpackW8# mut st0
   in (# st1, ws #)

--------------------------------------------------------------------------------
-- Conversion

-- | TODO:
--
-- @since 1.0.0
toByteArray# :: ByteVector# -> ByteArray#
toByteArray# = array#
{-# INLINE CONLIKE [1] toByteArray# #-}
