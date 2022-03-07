{-# LANGUAGE NamedFieldPuns #-}

-- | TODO:
--
-- @since 1.0.0
module Data.ByteVector.Mutable.Unboxed
  ( -- * Unboxed Mutable ByteVector
    ByteVector# (ByteVector#, capacity#, size#, mutable#),

    -- ** Construction
    empty#,
    alloc#,
    grow#,

    -- ** Read
    readW#,
    readW8#,
    readW16#,
    readW32#,
    readW64#,
    readI#,

    -- ** Write
    writeW#,
    writeW8#,
    writeW16#,
    writeW32#,
    writeW64#,
    writeI#,

    -- ** Push
    pushW8#,

    -- ** Conversion
    toMutableByteArray#,
    toByteArray#,

    -- ** Packing
    packW#,
    packW8#,

    -- ** Packing
    unpackW8#,
  )
where

import Data.Kind (Type)
import GHC.Prim
import GHC.Exts
import GHC.Word

import Data.ByteVector.Rep
import Data.Int.Unboxed (ceilBytesToWord#, maxInt#)

--------------------------------------------------------------------------------

newtype Bool# :: TYPE 'IntRep where
  Bool# :: Int# -> Bool#

pattern True# :: Bool#
pattern True# = Bool# 1#
{-# INLINE CONLIKE [0] True# #-}

cmpInt# :: Int# -> Int# -> Bool#
cmpInt# a b = case a ==# b of
  1# -> True#
  bv -> Bool# bv
{-# INLINE CONLIKE [0] cmpInt# #-}

--------------------------------------------------------------------------------

-- | TODO:
--
-- @since 1.0.0
newtype ByteVector# :: Type -> TYPE ByteVectorRep where
  MkByteVector# :: (# Int#, Int#, MutableByteArray# s #) -> ByteVector# s

{-# COMPLETE ByteVector# #-}

-- | TODO:
--
-- @since 1.0.0
pattern ByteVector# :: Int# -> Int# -> MutableByteArray# s -> ByteVector# s
pattern ByteVector# {capacity#, size#, mutable#} =
  MkByteVector# (# capacity#, size#, mutable# #)

{-# INLINE CONLIKE [0] capacity# #-}
{-# INLINE CONLIKE [0] size#     #-}
{-# INLINE CONLIKE [0] mutable#  #-}

--------------------------------------------------------------------------------
-- Construction

-- | Constructs an empty 'ByteVector#' with an initial capacity of 1#.
--
-- @since 1.0.0
empty# :: State# s -> (# State# s, ByteVector# s #)
empty# st =
  let cz = ceilBytesToWord# 1#
   in case newByteArray# 0# st of
        (# st', mut #) -> (# st', ByteVector# cz 0# mut #)
{-# INLINE CONLIKE [1] empty# #-}

-- | Constructs an empty 'ByteVector#' with an initial byte capacity @n@. An
-- initial capacity of size 1# is used if the given capacity is less than 0#.
--
-- @since 1.0.0
alloc# :: Int# -> State# s -> (# State# s, ByteVector# s #)
alloc# bytes st =
  let sz = maxInt# 1# bytes
      cz = ceilBytesToWord# sz
   in case newByteArray# sz st of
        (# st', mut #) -> (# st', ByteVector# cz sz mut #)

-- | TODO:
--
-- @since 1.0.0
grow# :: State# s -> ByteVector# s -> (# State# s, ByteVector# s #)
grow# st ByteVector# {size#, capacity#, mutable#} =
  -- NOTE: The operation here computes @size# = size# * 1.5@ rounded up to be
  -- aligned with the platform word size.
  let cz = ceilBytesToWord# (capacity# +# uncheckedIShiftRA# capacity# 1#)
   in case resizeMutableByteArray# mutable# cz st of
        (# st', mut #) -> (# st', ByteVector# cz size# mut #)

--------------------------------------------------------------------------------
-- Push

pushW8# :: ByteVector# s -> Word8# -> State# s -> (# State# s, ByteVector# s #)
pushW8# bs@ByteVector# {capacity#, size#, mutable#} x st =
  let sz = 1# +# size#
   in case cmpInt# capacity# size# of
        True# -> case grow# st bs of
          (# st', bs' #) -> (# writeW8# bs' size# x st', bs' {size# = sz} #)
        _ -> (# writeW8# bs size# x st, ByteVector# capacity# sz mutable# #)

--------------------------------------------------------------------------------
-- Read

-- | TODO: make sure to note this index is unchecked
--
-- @since 1.0.0
readW# :: ByteVector# s -> Int# -> State# s -> (# State# s, Word# #)
readW# ByteVector# {mutable#} = readWordArray# mutable#
{-# INLINE CONLIKE [1] readW# #-}

-- | TODO: make sure to note this index is unchecked
--
-- @since 1.0.0
readW8# :: ByteVector# s -> Int# -> State# s -> (# State# s, Word8# #)
readW8# ByteVector# {mutable#} = readWord8Array# mutable#
{-# INLINE CONLIKE [1] readW8# #-}

-- | TODO: make sure to note this index is unchecked
--
-- @since 1.0.0
readW16# :: ByteVector# s -> Int# -> State# s -> (# State# s, Word16# #)
readW16# ByteVector# {mutable#} = readWord16Array# mutable#
{-# INLINE CONLIKE [1] readW16# #-}

-- | TODO: make sure to note this index is unchecked
--
-- @since 1.0.0
readW32# :: ByteVector# s -> Int# -> State# s -> (# State# s, Word32# #)
readW32# ByteVector# {mutable#} = readWord32Array# mutable#
{-# INLINE CONLIKE [1] readW32# #-}

-- | TODO: make sure to note this index is unchecked
--
-- @since 1.0.0
readW64# :: ByteVector# s -> Int# -> State# s -> (# State# s, Word# #)
readW64# ByteVector# {mutable#} = readWord64Array# mutable#
{-# INLINE CONLIKE [1] readW64# #-}

-- | TODO: make sure to note this index is unchecked
--
-- @since 1.0.0
readI# :: ByteVector# s -> Int# -> State# s -> (# State# s, Int# #)
readI# ByteVector# {mutable#} = readIntArray# mutable#
{-# INLINE CONLIKE [1] readI# #-}

--------------------------------------------------------------------------------
-- Write

-- | TODO: make sure to note this index is unchecked
--
-- @since 1.0.0
writeW# :: ByteVector# s -> Int# -> Word# -> State# s -> State# s
writeW# ByteVector# {mutable#} = writeWordArray# mutable#
{-# INLINE CONLIKE [1] writeW# #-}

-- | TODO: make sure to note this index is unchecked
--
-- @since 1.0.0
writeW8# :: ByteVector# s -> Int# -> Word8# -> State# s -> State# s
writeW8# ByteVector# {mutable#} = writeWord8Array# mutable#
{-# INLINE CONLIKE [1] writeW8# #-}

-- | TODO: make sure to note this index is unchecked
--
-- @since 1.0.0
writeW16# :: ByteVector# s -> Int# -> Word16# -> State# s -> State# s
writeW16# ByteVector# {mutable#} = writeWord16Array# mutable#
{-# INLINE CONLIKE [1] writeW16# #-}

-- | TODO: make sure to note this index is unchecked
--
-- @since 1.0.0
writeW32# :: ByteVector# s -> Int# -> Word32# -> State# s -> State# s
writeW32# ByteVector# {mutable#} = writeWord32Array# mutable#
{-# INLINE CONLIKE [1] writeW32# #-}

-- | TODO: make sure to note this index is unchecked
--
-- @since 1.0.0
writeW64# :: ByteVector# s -> Int# -> Word# -> State# s -> State# s
writeW64# ByteVector# {mutable#} = writeWord64Array# mutable#
{-# INLINE CONLIKE [1] writeW64# #-}

-- | TODO: make sure to note this index is unchecked
--
-- @since 1.0.0
writeI# :: ByteVector# s -> Int# -> Int# -> State# s -> State# s
writeI# ByteVector# {mutable#} = writeIntArray# mutable#
{-# INLINE CONLIKE [1] writeI# #-}

--------------------------------------------------------------------------------
-- Conversion

-- | TODO:
--
-- @since 1.0.0
toMutableByteArray# :: ByteVector# s -> MutableByteArray# s
toMutableByteArray# = mutable#
{-# INLINE CONLIKE [1] toMutableByteArray# #-}

-- | TODO:
--
-- @since 1.0.0
toByteArray# :: ByteVector# s -> State# s -> (# State# s, ByteArray# #)
toByteArray# (ByteVector# cz sz src) st =
  let !(# st', dst #) = newByteArray# cz st
      st'' = copyMutableByteArray# src 0# dst 0# sz st'
   in unsafeFreezeByteArray# dst st''

--------------------------------------------------------------------------------
-- Packing

-- | TODO: make sure to note this index is unchecked
--
-- @since 1.0.0
packW# :: [Word] -> State# s -> (# State# s, ByteVector# s #)
packW# ws0 st0 =
  -- FIXME: Replace 8# with platform-independent machine word size
  -- FIXME: fix capacity and size issues
  let size'words = case length ws0 of I# sz -> 8# *# sz
   in case newByteArray# size'words st0 of
    (# st, mut #) -> (# copy mut st 0# ws0, ByteVector# size'words size'words mut #)
  where
    copy :: MutableByteArray# s -> State# s -> Int# -> [Word] -> State# s
    copy mut st i = \case
      W# w : ws -> copy mut (writeWordArray# mut i w st) (i +# 1#) ws
      [] -> st

-- | TODO: make sure to note this index is unchecked
--
-- @since 1.0.0
packW8# :: [Word8] -> State# s -> (# State# s, ByteVector# s #)
packW8# ws0 st0 =
  -- FIXME: reimplement via alloc#
  let size'words = ceilBytesToWord# size'bytes
      size'bytes = case length ws0 of I# sz -> sz
   in case newByteArray# size'bytes st0 of
    (# st, mut #) ->
      let !st' = copy mut st 0# ws0
       in (# st', ByteVector# size'words size'bytes mut #)
  where
    copy :: MutableByteArray# s -> State# s -> Int# -> [Word8] -> State# s
    copy mut st i = \case
      W8# w : ws -> copy mut (writeWord8Array# mut i w st) (i +# 1#) ws
      [] -> st

--------------------------------------------------------------------------------
-- Packing

unpackW8# :: forall s. ByteVector# s -> State# s -> (# State# s, [Word8] #)
unpackW8# bs = run 0#
  where
    run :: Int# -> State# s -> (# State# s, [Word8] #)
    run i st = case cmpInt# (size# bs) i of
      True# -> (# st, [] #)
      _ -> let !(# st0, x  #) = readWord8Array# (mutable# bs) i st
               !(# st1, xs #) = run (1# +# i) st0
            in (# st1, W8# x : xs #)
