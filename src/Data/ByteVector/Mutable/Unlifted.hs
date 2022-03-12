-- | TODO:
--
-- @since 1.0.0
module Data.ByteVector.Mutable.Unlifted
  ( -- *
    ByteVector# (ByteVector#),
    sizeof#,
    capacity#,
    unreserved#,
    new#,
    empty#,
    resize#,
    set#,
    get#,
    pushback#,
    pack#,
    toMutableByteArray#,
    toByteArray#,
  )
where

import Data.Primitive
import GHC.Prim
import GHC.Types
import GHC.Word

import Data.Bool.Unlifted (Bool# (True#, False#), leInt#)
import Data.Int.Unlifted
import Data.ByteVector.Mutable.Struct (MutStruct#)
import Data.ByteVector.Mutable.Struct qualified as MutStruct

--------------------------------------------------------------------------------

unI# :: Int -> Int#
unI# (I# x) = x

-- | Fast integer scaling by 1.5
--
-- prop> (scale# x) = (x + quot x 2)
scale# :: Int# -> Int#
scale# x = x +# uncheckedIShiftRA# x 1#

--------------------------------------------------------------------------------

-- | TODO:
--
-- @since 1.0.0
newtype ByteVector# :: Type -> UnliftedType where
  ByteVector# :: MutStruct# s -> ByteVector# s

-- | TODO:
--
-- @since 1.0.0
sizeof# :: ByteVector# s -> State# s -> (# State# s, Int# #)
sizeof# (ByteVector# mut) = MutStruct.sizeof# mut

-- | TODO:
--
-- @since 1.0.0
capacity# :: ByteVector# s -> State# s -> (# State# s, Int# #)
capacity# (ByteVector# var) = MutStruct.capacity# var

-- | TODO:
--
-- @since 1.0.0
unreserved# :: ByteVector# s -> State# s -> (# State# s, Int# #)
unreserved# vec st0 =
  let !(# st1, cz #) = capacity# vec st0
      !(# st2, sz #) = sizeof# vec st1
   in (# st2, cz -# sz #)

-- | TODO:
--
-- @since 1.0.0
new# :: Int# -> State# s -> (# State# s, ByteVector# s #)
new# cap st =
  let !(# st0, arr #) = newByteArray# (ceilBytesToWord# cap) st
      !(# st1, mut #) = MutStruct.new# 0# arr st0
   in (# st1, ByteVector# mut #)

-- | TODO:
--
-- @since 1.0.0
empty# :: State# s -> (# State# s, ByteVector# s #)
empty# = case bytesInWord of I# s -> new# s

-- | TODO:
--
-- @since 1.0.0
resize# :: Int# -> ByteVector# s -> State# s -> State# s
resize# len (ByteVector# mut) st0 =
  let !(# st1, arr0 #) = MutStruct.readByteArray# mut st0
      !(# st2, arr1 #) = resizeMutableByteArray# arr0 (ceilBytesToWord# len) st1
   in MutStruct.writeByteArray# arr1 mut st2

-- | TODO:
--
-- @since 1.0.0
grow# :: Int# -> ByteVector# s -> State# s -> State# s
grow# bz vec st0 =
  let !(# st1, cz #) = capacity# vec st0
      !(# st2, sz #) = sizeof# vec st1
   in case cz `leInt#` bz +# sz of
        True# -> resize# (scale# (bz +# sz)) vec st2
        False# -> st2

-- | TODO:
--
-- @since 1.0.0
set# :: Prim a => Int# -> a -> ByteVector# s -> State# s -> State# s
set# i x (ByteVector# mut) st =
  let !(# st', arr #) = MutStruct.readByteArray# mut st
   in writeByteArray# arr i x st'

-- | TODO:
--
-- @since 1.0.0
get# :: Prim a => Int# -> ByteVector# s -> State# s -> (# State# s, a #)
get# i (ByteVector# mut) st =
  let !(# st', arr #) = MutStruct.readByteArray# mut st
   in readByteArray# arr i st'

-- | TODO:
--
-- @since 1.0.0
pushback# :: Prim a => a -> ByteVector# s -> State# s -> State# s
pushback# x vec@(ByteVector# mut) st0 =
  let !(# st1, len #) = sizeof# vec st0
      st2 = grow# (unI# (sizeOf x)) vec st1
      st3 = MutStruct.modifyArraySize# (1# +#) mut st2
   in set# len x vec st3

-- | TODO:
--
-- @since 1.0.0
pack# :: [Word8] -> State# s -> (# State# s, ByteVector# s #)
pack# [] st0 = empty# st0
pack# ws st0 =
  -- NOTE: ceilBytesToWord# and scale# do not commute. ceilBytesToWord# must be
  -- applied first to ensure @cz@ is always aligned to platform word size.
  let cz = ceilBytesToWord# (scale# sz)
      sz = unI# (length ws)
      !(# st1, arr #) = newByteArray# cz st0
      !(# st2, mut #) = MutStruct.new# sz arr (foldcopy# 0# ws arr st1)
   in (# st2, ByteVector# mut #)
  where
    foldcopy# _ [] _ st = st
    foldcopy# i (W8# x : xs) mut st =
      let st' = writeWord8Array# mut i x st
       in foldcopy# (1# +# i) xs mut st'

-- | TODO:
--
-- @since 1.0.0
-- FIXME: this should probably copy, doesn't make sense to expose the internals
toMutableByteArray# :: ByteVector# s -> State# s -> (# State# s, MutableByteArray# s #)
toMutableByteArray# (ByteVector# mut) = MutStruct.readByteArray# mut

toByteArray# :: ByteVector# s -> State# s -> (# State# s, ByteArray# #)
toByteArray# (ByteVector# mut) st0 =
  let !(# st1, len #) = MutStruct.sizeof# mut st0
      !(# st2, src #) = MutStruct.readByteArray# mut st1
      !(# st3, dst #) = newByteArray# len st2
   in case copyMutableByteArray# src 0# dst 0# len st3 of
        st4 -> unsafeFreezeByteArray# dst st4
