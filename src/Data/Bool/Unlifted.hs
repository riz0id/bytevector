-- | TODO:
--
-- @since 1.0.0
module Data.Bool.Unlifted
  ( -- *
    Bool# (True#, False#),

    -- *
    leqInt#,
    ltInt#,
  )
where

import GHC.Prim
import GHC.Types (RuntimeRep (IntRep))

infix 4 `leqInt#`, `ltInt#`

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

-- | TODO:
--
-- @since 1.0.0
ltInt# :: Int# -> Int# -> Bool#
ltInt# a b =
  case a <# b of
    1# -> True#
    _ -> False#
