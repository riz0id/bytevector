-- |
--
-- @since 1.0.0
module Test.Data.Int.Gen
  ( -- * Generators
    gen'linearBounded'maxInt#,
    gen'maxInt#,
    gen'linearBounded'minInt#,
    gen'minInt#,

    -- * Preconditions
    precond'maxInt#,
    precond'minInt#,
  )
where

import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import GHC.Exts

--------------------------------------------------------------------------------
-- Generators

-- | Integer pair generator that produces valid arguments to 'maxInt#' for the
-- full range of 'Int' dependent on Hedgehog's sizing factor.
gen'linearBounded'maxInt# :: MonadGen m => m (Int, Int)
gen'linearBounded'maxInt# =
  gen'maxInt#
    (Gen.int Range.linearBounded)
    (Gen.int Range.linearBounded)

-- | Generator combinator that produces valid arguments to 'maxInt#'.
gen'maxInt# :: MonadGen m => m Int -> m Int -> m (Int, Int)
gen'maxInt# genA genB = do
  lhs <- genA
  rhs <- genB
  if precond'maxInt# lhs rhs
    then pure (lhs, rhs)
    else Gen.discard

-- | Integer pair generator that produces valid arguments to 'minInt#' for the
-- full range of 'Int' dependent on Hedgehog's sizing factor.
gen'linearBounded'minInt# :: MonadGen m => m (Int, Int)
gen'linearBounded'minInt# =
  gen'minInt#
    (Gen.int Range.linearBounded)
    (Gen.int Range.linearBounded)

-- | Integer pair generator that produces valid arguments to 'minInt#'.
gen'minInt# :: MonadGen m => m Int -> m Int -> m (Int, Int)
gen'minInt# genA genB = do
  lhs <- genA
  rhs <- genB
  if precond'minInt# lhs rhs
    then pure (lhs, rhs)
    else Gen.discard

--------------------------------------------------------------------------------
-- Preconditions
--
-- A handful of the operations in 'Data.Int.Unboxed' require preconditions on
-- their inputs to produce defined results. This section contains preconditions
-- written as predicates used to generate valid arguments for testing these
-- functions. The naming convention for denoting a precondition is:
--
-- @
-- ( "precond'" name )
-- @
--
-- where "name" is the name of the function as it appears in the library.

-- | The precondition two 'Int' values must satisfy to be valid arguments of
-- 'maxInt#'.
--
-- prop> -2^29 <= (b - a) && (b - a) <= 2^29-1
precond'maxInt# :: Int -> Int -> Bool
precond'maxInt# a b =
  case b `subIntC` a of
    Left {} -> False
    Right {} -> True

-- | The precondition two 'Int' values must satisfy to be valid arguments of
-- 'minInt#'.
--
-- prop> -2^29+1 <= (a - b) && (a - b) <= 2^29-1
precond'minInt# :: Int -> Int -> Bool
precond'minInt# a b =
  case a `subIntC` b of
    Left {} -> False
    Right {} -> True

--------------------------------------------------------------------------------

-- | Subtracts two integers reporting overflow.
--
-- (1) 'Right' stores the subtracted result if no overflow or underflow occured.
--
-- (2) 'Left' stores the truncated result.
--
--   * Positive truncation indicates overflow.
--
--   * Negative truncation indicates underflow.
subIntC :: Int -> Int -> Either Int Int
subIntC (I# a) (I# b) = case subIntC# a b of
  (# ret, 0# #) -> Right (I# ret)
  (# trun, _ #) -> Left (I# trun)
