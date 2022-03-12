{-# LANGUAGE MagicHash #-}

-- | TODO:
--
-- @since 1.0.0
module Data.ByteVector.Mutable.Struct
  ( -- * TODO:
    MutStruct# (MutStruct#),

    -- *
    new#,

    -- *
    sizeof#,
    capacity#,

    -- *
    readByteArray#,
    writeByteArray#,
    writeArraySize#,
    modifyArraySize#,
  )
where

import GHC.Prim
  ( MutableByteArray#,
    Int#,
    MutVar#,
    State#,
    getSizeofMutableByteArray#,
    newMutVar#,
    readMutVar#,
    writeMutVar#
  )
import GHC.Exts (UnliftedRep)
import GHC.Types (Type, TYPE)

--------------------------------------------------------------------------------

-- | TODO:
--
-- @since 1.0.0
newtype MutStruct# :: Type -> TYPE UnliftedRep where
  MutStruct# :: MutVar# s (Struct s) -> MutStruct# s

-- | TODO:
--
-- @since 1.0.0
data Struct s = Struct !Int# !(MutableByteArray# s)

-- | TODO:
--
-- @since 1.0.0
new# :: Int# -> MutableByteArray# s -> State# s -> (# State# s, MutStruct# s #)
new# len arr st =
  let !(# st', var #) = newMutVar# (Struct len arr) st
   in (# st', MutStruct# var #)

-- | TODO:
--
-- @since 1.0.0
sizeof# :: MutStruct# s -> State# s -> (# State# s, Int# #)
sizeof# (MutStruct# var) st =
  let !(# st', Struct len _ #) = readMutVar# var st
   in (# st', len #)

-- | TODO:
--
-- @since 1.0.0
capacity# :: MutStruct# s -> State# s -> (# State# s, Int# #)
capacity# (MutStruct# var) st =
  let !(# st', Struct _ vec #) = readMutVar# var st
   in getSizeofMutableByteArray# vec st'

-- | TODO:
--
-- @since 1.0.0
readByteArray# :: MutStruct# s -> State# s -> (# State# s, MutableByteArray# s #)
readByteArray# (MutStruct# var) st =
  let !(# st', Struct _ vec #) = readMutVar# var st
   in (# st', vec #)

-- | TODO:
--
-- @since 1.0.0
writeByteArray# :: MutableByteArray# s -> MutStruct# s -> State# s -> State# s
writeByteArray# arr (MutStruct# var) st =
  let !(# st', Struct len _ #) = readMutVar# var st
   in writeMutVar# var (Struct len arr) st'

-- | TODO:
--
-- @since 1.0.0
writeArraySize# :: Int# -> MutStruct# s -> State# s -> State# s
writeArraySize# len (MutStruct# var) st =
  let !(# st', Struct _ arr #) = readMutVar# var st
   in writeMutVar# var (Struct len arr) st'

-- | TODO:
--
-- @since 1.0.0
modifyArraySize# :: (Int# -> Int#) -> MutStruct# s -> State# s -> State# s
modifyArraySize# f mut st =
  let !(# st', len #) = sizeof# mut st
   in writeArraySize# (f len) mut st'
