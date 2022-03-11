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

import Data.Bool.Unlifted
import Data.Int.Unlifted
import Data.ByteVector.Mutable.Struct (Struct (Struct), Struct# (Struct#))
import Data.ByteVector.Mutable.Struct qualified as Struct

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
  ByteVector# :: MutVar# s (Struct s) -> ByteVector# s

-- | TODO:
--
-- @since 1.0.0
sizeof# :: ByteVector# s -> State# s -> (# State# s, Int# #)
sizeof# (ByteVector# var) st =
   let !(# st', struct #) = readMutVar# var st
    in (# st', Struct.sizeof struct #)

-- | TODO:
--
-- @since 1.0.0
capacity# :: ByteVector# s -> State# s -> (# State# s, Int# #)
capacity# (ByteVector# var) st =
  let !(# st', struct #) = readMutVar# var st
   in Struct.capacity struct st'

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
new# len st =
  let !(# st0, arr #) = newByteArray# (ceilBytesToWord# len) st
      !(# st1, mut #) = newMutVar# (Struct (Struct# 0# arr)) st0
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
resize# len (ByteVector# var) st =
  let !(# st0, Struct (Struct# sz mut) #) = readMutVar# var st
      !(# st1, mut' #) = resizeMutableByteArray# mut (ceilBytesToWord# len) st0
   in writeMutVar# var (Struct (Struct# sz mut')) st1

-- | TODO:
--
-- @since 1.0.0
grow# :: Int# -> ByteVector# s -> State# s -> State# s
grow# bz vec st0 =
  let !(# st1, cz #) = capacity# vec st0
      !(# st2, sz #) = sizeof# vec st1
   in case cz `leqInt#` bz +# sz of
        True# -> resize# (scale# (bz +# sz)) vec st2
        False# -> st2

-- | TODO:
--
-- @since 1.0.0
set# :: Prim a => Int# -> a -> ByteVector# s -> State# s -> State# s
set# i x vec st =
  let !(# st', array #) = toMutableByteArray# vec st
   in writeByteArray# array i x st'

-- | TODO:
--
-- @since 1.0.0
get# :: Prim a => Int# -> ByteVector# s -> State# s -> (# State# s, a #)
get# i xs st =
  let !(# st', array #) = toMutableByteArray# xs st
   in readByteArray# array i st'

-- | TODO:
--
-- @since 1.0.0
pushback# :: Prim a => a -> ByteVector# s -> State# s -> State# s
pushback# x vec@(ByteVector# var) st0 =
  let st1 = grow# (unI# (sizeOf x)) vec st0
   in case readMutVar# var st1 of
        (# st2, Struct (Struct# len mut) #) ->
          let st3 = set# len x vec st2
           in writeMutVar# var (Struct (Struct# (1# +# len) mut)) st3

-- | TODO:
--
-- @since 1.0.0
pack# :: [Word8] -> State# s -> (# State# s, ByteVector# s #)
pack# [] st0 = empty# st0
pack# ws st0 =
  -- NOTE: ceilBytesToWord# and scale# do not commute. ceilBytesToWord# must be
  -- applied first to ensure @cz@ is always aligned to platform word size.
  let !(# st1, mut #) = newByteArray# cz st0
      cz = ceilBytesToWord# (scale# sz)
      sz = unI# (length ws)
   in case newMutVar# (Struct (Struct# sz mut)) (foldcopy# 0# ws mut st1) of
        (# st2, var #) -> (# st2, ByteVector# var #)
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
toMutableByteArray# (ByteVector# var) st =
  let !(# st', struct #) = readMutVar# var st
   in case Struct.toMutableByteArray struct of
        MutableByteArray array -> (# st', array #)

toByteArray# :: ByteVector# s -> State# s -> (# State# s, ByteArray# #)
toByteArray# (ByteVector# var) st =
  case readMutVar# var st of
    (# st0, Struct (Struct# len src#) #) ->
      let !(# st1, dst# #) = newByteArray# len st0
          st2 = copyMutableByteArray# src# 0# dst# 0# len st1
       in unsafeFreezeByteArray# dst# st2
