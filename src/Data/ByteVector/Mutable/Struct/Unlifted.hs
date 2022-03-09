{-# LANGUAGE NamedFieldPuns #-}

-- | TODO:
--
-- @since 1.0.0
module Data.ByteVector.Mutable.Struct.Unlifted
  ( -- * TODO:
    Struct# (Struct#, sizeof#, mutable#),
    capacity#,
  )
where

import GHC.Prim
import GHC.Types (Type)

import Data.ByteVector.Mutable.Struct.Rep

--------------------------------------------------------------------------------

newtype Struct# :: Type -> StructType where
  MkStruct# :: (# Int#, MutableByteArray# s #) -> Struct# s

-- | TODO:
--
-- @since 1.0.0
pattern Struct# :: Int# -> MutableByteArray# s -> Struct# s
pattern Struct# {sizeof#, mutable#} = MkStruct# (# sizeof#, mutable# #)

{-# COMPLETE Struct# #-}

-- | TODO:
--
-- @since 1.0.0
capacity# :: Struct# s -> State# s -> (# State# s, Int# #)
capacity# Struct# {mutable#} = getSizeofMutableByteArray# mutable#
