{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}

module Test.Data.Int.Unboxed (tests) where

import Hedgehog (Property, PropertyT, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import GHC.Exts qualified as Exts

-- local modules
import Test.Data.Int.Gen qualified as Gen

import Data.Int.Unboxed (maxInt#, minInt#)

--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Data.Int.Unboxed"
    [ tests'maxInt#
    , tests'minInt#
    ]

-- | Test the commutivity of an 'Int#' operation.
prop'commutative :: (Int -> Int -> Int) -> Int -> Int -> PropertyT IO ()
prop'commutative op a b = op a b === op b a

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- 'maxInt#' tests

tests'maxInt# :: TestTree
tests'maxInt# =
  testGroup
    "Data.Int.Unboxed.maxInt#"
    [ testProperty "maxInt# a maxBound == maxBound" maxBound'maxInt#
    , testProperty "coherence:   maxInt#" coherence'maxInt#
    , testProperty "commutivity: maxInt#" commutative'maxInt#
    , testProperty "idempotent:  maxInt#" idempotent'maxInt#
    ]

-- | TODO:
box'maxInt :: Int -> Int -> Int
box'maxInt (Exts.I# a) (Exts.I# b) = Exts.I# (maxInt# a b)

-- | prop> forall x, 0 <= x. maxInt# maxBound x == maxBound
maxBound'maxInt# :: Property
maxBound'maxInt# = property do
  a <- forAll (Gen.int $ Range.linear 0 maxBound)
  box'maxInt maxBound a === maxBound

-- | Tests that 'maxInt#' is idempotent.
idempotent'maxInt# :: Property
idempotent'maxInt# = property do
  a <- forAll (Gen.int Range.linearBounded)
  box'maxInt a a === a
  a === box'maxInt a a

-- | Tests 'maxInt#' agrees with 'max' for all pairs of integers satisfying the
-- precondition on 'maxInt#'.
coherence'maxInt# :: Property
coherence'maxInt# = property do
  (a, b) <- forAll Gen.gen'linearBounded'maxInt#
  box'maxInt a b === max a b

-- | Tests the commutivity of 'maxInt#'
commutative'maxInt# :: Property
commutative'maxInt# = property do
  (a, b) <- forAll Gen.gen'linearBounded'maxInt#
  prop'commutative box'maxInt a b

--------------------------------------------------------------------------------
-- 'maxInt#' tests

tests'minInt# :: TestTree
tests'minInt# =
  testGroup
    "Data.Int.Unboxed.minInt#"
    [ testProperty "minInt# a minBound == minBound" maxBound'minInt#
    , testProperty "coherence:   minInt#" coherence'minInt#
    , testProperty "commutivity: minInt#" commutative'minInt#
    , testProperty "idempotent:  minInt#" idempotent'minInt#
    ]

-- | TODO:
box'minInt :: Int -> Int -> Int
box'minInt (Exts.I# a) (Exts.I# b) = Exts.I# (minInt# a b)

-- | prop> forall x, x <= 0. minInt# x minBound == minBound
maxBound'minInt# :: Property
maxBound'minInt# = property do
  a <- forAll (Gen.int $ Range.linear minBound 0)
  box'minInt a minBound === minBound

-- | Tests that 'minInt#' is idempotent.
idempotent'minInt# :: Property
idempotent'minInt# = property do
  a <- forAll (Gen.int Range.linearBounded)
  box'minInt a a === a
  a === box'minInt a a

-- | Tests 'minInt#' agrees with 'min' for all pairs of integers satisfying the
-- precondition on 'minInt#'.
coherence'minInt# :: Property
coherence'minInt# = property do
  (a, b) <- forAll Gen.gen'linearBounded'minInt#
  box'minInt a b === min a b

-- | Tests the commutivity of 'minInt#'
commutative'minInt# :: Property
commutative'minInt# = property do
  (a, b) <- forAll Gen.gen'linearBounded'minInt#
  prop'commutative box'minInt a b
