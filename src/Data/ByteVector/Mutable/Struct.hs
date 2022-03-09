{-# LANGUAGE MagicHash #-}

-- | TODO:
--
-- @since 1.0.0
module Data.ByteVector.Mutable.Struct
  ( -- * TODO:
    Struct (Struct),
    sizeof,
    capacity,
    toMutableByteArray,

    -- * Unlifted
    Struct# (Struct#),
  )
where

import Data.Kind (Type)
import Data.Primitive
import GHC.Prim (State#, Int#)
import GHC.Exts (Int (I#))
import GHC.ST

import Data.ByteVector.Mutable.Struct.Unlifted

--------------------------------------------------------------------------------

-- | TODO:
--
-- @since 1.0.0
data Struct :: Type -> Type where
  Struct :: Struct# s -> Struct s

-- | TODO:
--
-- @since 1.0.0
sizeof :: Struct s -> Int#
sizeof (Struct s) = sizeof# s

-- | TODO:
--
-- @since 1.0.0
capacity :: Struct s -> State# s -> (# State# s, Int# #)
capacity (Struct struct) st =
  case capacity# struct st of
    (# st', c #) -> (# st', c #)

-- | TODO:
--
-- @since 1.0.0
toMutableByteArray :: Struct s -> MutableByteArray s
toMutableByteArray (Struct s) = MutableByteArray (mutable# s)
