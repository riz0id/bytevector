{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

-- | Fast primitive operations on unlifted integers.
--
-- @since 1.0.0
module Data.Int.Unlifted
  ( -- * Byte
    ceilBytesToWord#,
    bytesToWords#,

    -- * Minimum & Maximum
    posInt#,
    maxInt#,
    negInt#,
    minInt#,
  )
where

import GHC.Exts

-- defines the `WORD_SIZE_IN_BITS` macro
#include "MachDeps.h"

--------------------------------------------------------------------------------

-- | TODO:
--
-- @since 1.0.0
ceilBytesToWord# :: Int# -> Int#
ceilBytesToWord# x = bytesToWords# x *# WORD_SIZE_IN_BITS#

-- | Converts a unit in number of bytes to number of words. The provided number
-- must be positive to yield a correct result.
--
-- === __Examples:__
--
-- >>> I# (bytesToWords 0) -- 64 bit and 32 bit
-- 0
--
-- >>> I# (bytesToWords 12) -- 64 bit
-- 2
--
-- >>> I# (bytesToWords 12) -- 32 bit
-- 3
--
-- @since 1.0.0
bytesToWords# :: Int# -> Int#
bytesToWords# n = quotInt# (n +# WORD_SIZE_IN_BITS# -# 1#) WORD_SIZE_IN_BITS#

--------------------------------------------------------------------------------
-- Minimum & Maximum

-- | Clamps negative integers to 0, leaving positive integers unchanged.
--
-- prop> I# (posInt# 128#)
-- 128
--
-- prop> I# (posInt# -128#)
-- 0
--
-- @since 1.0.0
posInt# :: Int# -> Int#
posInt# x = andI# x (shiftIWordR# (notI# x))

-- | Compute the maximum of two unboxed integers without branching. @maxInt a b@
-- is only defined if integers @a@, @b@, satisify the precondition:
--
-- prop> -2^29 <= (b - a) && (b - a) <= 2^29-1
--
-- @since 1.0.0
maxInt# :: Int# -> Int# -> Int#
maxInt# a b =
  -- See [NOTE: Compute minimum and maximum bitwise]
  let mask = shiftIWordR# (a -# b)
   in a +# (andI# (b -# a) mask)
{-# INLINEABLE [0] maxInt# #-}

{-# RULES
"maxInt# 0# b" forall b. maxInt# 0# b = posInt# b
"maxInt# a 0#" forall a. maxInt# a 0# = posInt# a
  #-}

-- | Clamps positive integers to 0, leaving negative integers unchanged.
--
-- prop> I# (negInt# 128#)
-- 0
--
-- prop> I# (negInt# -128#)
-- -128
--
-- @since 1.0.0
negInt# :: Int# -> Int#
negInt# x = andI# x (shiftIWordR# x)

-- | Compute the maximum of two unboxed integers without branching. For integers
-- @a@ and @b@, @minInt a b@ /must/ satisify the precondition:
--
-- prop> -2^29+1 <= (a - b) && (a - b) <= 2^29-1
--
-- @since 1.0.0
minInt# :: Int# -> Int# -> Int#
minInt# a b =
  -- See [NOTE: Compute minimum and maximum bitwise]
  let mask = shiftIWordR# (a -# b)
   in b -# (andI# (b -# a) mask)

--------------------------------------------------------------------------------
--
-- Internal
--

shiftIWordR# :: Int# -> Int#
shiftIWordR# x = uncheckedIShiftRA# x (WORD_SIZE_IN_BITS# -# 1#)

--------------------------------------------------------------------------------

-- [NOTE: Compute minimum and maximum bitwise]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A branchless implementation for computing minimum and maximum of two integers
-- is given by the following bitwise twiddles:
--
-- @
-- r = y + ((x - y) & ((x - y) >> (sizeof(int) * CHAR_BIT - 1))); // min(x, y)
-- r = x - ((x - y) & ((x - y) >> (sizeof(int) * CHAR_BIT - 1))); // max(x, y)
-- @
--
-- which are taken from:
-- <http://www.graphics.stanford.edu/~seander/bithacks.html#IntegerMinOrMax>
--
-- These operations depend on @(x - y)@ to overflow/underflow integer bounds in
-- order to be defined. If you're unsure if @(x - y)@ is always true, then best
-- to use compute minimum/maximum with a comparison.

-- [NOTE: Word size constant folding]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The functions in this module are dependent on the size of 'Int#'. The width
-- needed to store an 'Int#' in bits could be obtained by using
--
-- >>> 'Foreign.Storable.sizeOf' (0 :: Int)
--
-- but then constant folding is lost.
