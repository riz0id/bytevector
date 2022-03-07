
module Test.Gen
  (
  )
where

import Hedgehog (MonadGen, Range)
import Hedgehog.Gen qualified as Gen

import GHC.Prim (Int#)
import GHC.Prim qualified as Prim
import GHC.Exts qualified as Exts

--------------------------------------------------------------------------------

-- int# :: MonadGen m => Range -> m Int#
-- int# range = fmap (\case (Exts.I# x) -> x) (Gen.int range)
