-- | TODO:
--
-- @since 1.0.0
module Data.ByteVector.Rep
  ( type ByteVectorRep,
  )
where

import GHC.Prim
import GHC.Exts

--------------------------------------------------------------------------------

-- | TODO:
--
-- @since 1.0.0
type ByteVectorRep :: RuntimeRep
type ByteVectorRep = 'TupleRep '[ 'IntRep, 'IntRep, UnliftedRep ]
