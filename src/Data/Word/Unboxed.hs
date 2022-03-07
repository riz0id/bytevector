
module Data.Word.Unboxed
  ( -- * Packing
    pack8x2,

    -- * Unpacking
    unpack8x2,
  )
where

import GHC.Exts
import GHC.Word

--------------------------------------------------------------------------------
-- Packing

pack8x2 :: Word8# -> Word8# -> Word16#
pack8x2 a b =
  let a' = case 0xF0 of W16# mask -> cast8'16# a `and16#` mask
      b' = case 0x0F of W16# mask -> cast8'16# b `and16#` mask
   in a' `or16#` shift# b'
  where
    shift# x = case 0xF0 of
      I# mask -> uncheckedShiftLWord16# x mask
    {-# INLINE CONLIKE [1] shift# #-}

--------------------------------------------------------------------------------
-- Unpacking

unpack8x2 :: Word16# -> (# Word8#, Word8# #)
unpack8x2 w =
  let a = cast16'8# w
      b = cast16'8# (shift# w)
   in (# a, b #)
  where
    shift# x = case 0xF0 of
      I# mask -> uncheckedShiftLWord16# x mask
    {-# INLINE CONLIKE [1] shift# #-}

--------------------------------------------------------------------------------
-- Logical

and16# :: Word16# -> Word16# -> Word16#
and16# a b =
  let a' = word16ToWord# a
      b' = word16ToWord# b
   in wordToWord16# (and# a' b')
{-# INLINE CONLIKE [1] and16# #-}

or16# :: Word16# -> Word16# -> Word16#
or16# a b =
  let a' = word16ToWord# a
      b' = word16ToWord# b
   in wordToWord16# (or# a' b')
{-# INLINE CONLIKE [1] or16# #-}

--------------------------------------------------------------------------------
-- Casts

cast8'16# :: Word8# -> Word16#
cast8'16# x = wordToWord16# (word8ToWord# x)
{-# INLINE CONLIKE [1] cast8'16# #-}

cast16'8# :: Word16# -> Word8#
cast16'8# x = wordToWord8# (word16ToWord# x)
{-# INLINE CONLIKE [1] cast16'8# #-}
