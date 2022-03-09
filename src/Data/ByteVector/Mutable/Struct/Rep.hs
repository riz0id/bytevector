-- | TODO:
--
-- @since 1.0.0
module Data.ByteVector.Mutable.Struct.Rep
  ( -- * Struct Kind
    StructType,

    -- * Runtime Representation
    StructRep,
  )
where

import GHC.Types

--------------------------------------------------------------------------------

-- | TODO:
--
-- @since 1.0.0
type StructType :: Type
type StructType = TYPE StructRep

-- | TODO:
--
-- @since 1.0.0
type StructRep :: RuntimeRep
type StructRep = 'TupleRep '[ 'IntRep, UnliftedRep ]
