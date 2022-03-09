{-# LANGUAGE MagicHash #-}

-- | TODO:
--
-- @since 1.0.0
module Data.ByteVector.Mutable.Unlifted
  ( -- *
    ByteVector# (ByteVector#),
    struct#,
    sizeof#,
    capacity#,
    new#,
    empty#,
    resize#,
    set#,
    get#,
    pushback#,
    toMutableByteArray#,
  )
where

import Data.Primitive
import GHC.Exts (Int (I#))
import GHC.Prim
import GHC.Types (Type, UnliftedType)

import Data.Bool.Unlifted
import Data.Int.Unlifted
import Data.ByteVector.Mutable.Struct (Struct (Struct), Struct# (Struct#))
import Data.ByteVector.Mutable.Struct qualified as Struct

--------------------------------------------------------------------------------

-- | TODO:
--
-- @since 1.0.0
newtype ByteVector# :: Type -> UnliftedType where
  ByteVector# :: MutVar# s (Struct s) -> ByteVector# s

-- | TODO:
--
-- @since 1.0.0
struct# :: ByteVector# s -> State# s -> (# State# s, Struct s #)
struct# (ByteVector# mut) = readMutVar# mut

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
capacity# vec st =
  let !(# st', struct #) = struct# vec st
   in Struct.capacity struct st'

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
set# :: Prim a => Int# -> a -> ByteVector# s -> State# s -> State# s
set# i x vec st =
  let !(# st', array #) = toMutableByteArray# vec st
   in writeByteArray# array i x st'

-- | TODO:
--
-- @since 1.0.0
get# :: Prim a => Int# -> ByteVector# s -> State# s -> (# State# s, a #)
get# i xs st = case toMutableByteArray# xs st of
  (# st', array #) -> readByteArray# array i st'

-- | TODO:
--
-- @since 1.0.0
pushback# :: Prim a => a -> ByteVector# s -> State# s -> State# s
pushback# x xs@(ByteVector# var) st0 =
  let !(# st1, cz #) = capacity# xs st0
      !(# st2, sz #) = sizeof# xs st1
      !(I# bytes)    = sizeOf x
   in case leqInt# cz (sz +# bytes) of
        False# ->
          let !(# st3, Struct (Struct# len mut) #) = readMutVar# var st2
              st4 = set# len x xs st3
           in writeMutVar# var (Struct (Struct# (1# +# len) mut)) st4
        True# ->
          let st3 = resize# (cz +# quotInt# cz 2#) xs st2
              !(# st4, Struct (Struct# len mut) #) = readMutVar# var st3
              st5 = set# len x xs st4
           in writeMutVar# var (Struct (Struct# (1# +# len) mut)) st5

-- | TODO:
--
-- @since 1.0.0
toMutableByteArray# :: ByteVector# s -> State# s -> (# State# s, MutableByteArray# s #)
toMutableByteArray# (ByteVector# var) st =
  let !(# st', struct #) = readMutVar# var st
   in case Struct.toMutableByteArray struct of
        MutableByteArray array -> (# st', array #)

--------------------------------------------------------------------------------

-- unsafeSetSizeof# :: ByteVector# s -> State# s -> State# s
-- unsafeSetSizeof# (ByteVector# var) st =
--   let !(# st0, Struct (Struct# len mut) #) = readMutVar# var st
--    in writeMutVar# var (Struct (Struct# (1# +# len) mut)) st0
