{-# LANGUAGE ImportQualifiedPost #-}

module Test.Data.ByteVector.Conversion (tests) where

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

import Data.Functor.Identity (Identity (Identity))

-- Local Modules
import Test.Data.ByteVector.Gen qualified as Gen

import Data.ByteVector

--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Data.ByteVector: Conversion Tests"
    [ testProperty "round trip: [Word8]" trip'list'word8
    ]

--------------------------------------------------------------------------------

trip'list'word8 :: Property
trip'list'word8 = property do
  xs <- forAll Gen.list'word8
  tripping xs packW8 (Identity . unpackW8)
