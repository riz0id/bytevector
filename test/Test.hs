
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Data.ByteVector
import qualified Test.Data.Int.Unlifted

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup
    "bytevector tests"
    [ Test.Data.ByteVector.tests
    , Test.Data.Int.Unlifted.tests
    ]
