-- | TODO:
--
-- @since 1.0.0
module Data.Bool.Unlifted
  ( -- *
    Bool# (True#, False#),

    -- *
    leqInt#,
  )
where

import GHC.Prim
import GHC.Exts (Int (I#))
import GHC.Types (TYPE, RuntimeRep (IntRep))

--------------------------------------------------------------------------------

-- | TODO:
--
-- @since 1.0.0
newtype Bool# :: TYPE 'IntRep where
  B# :: Int# -> Bool#

{-# COMPLETE True#, False# #-}

-- | TODO:
--
-- @since 1.0.0
pattern True# :: Bool#
pattern True# = B# 1#

-- | TODO:
--
-- @since 1.0.0
pattern False# :: Bool#
pattern False# = B# 0#

-- | TODO:
--
-- @since 1.0.0
leqInt# :: Int# -> Int# -> Bool#
leqInt# a b =
  case a <=# b of
    1# -> True#
    _ -> False#
