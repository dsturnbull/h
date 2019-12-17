module CPU.Instructions.NOPSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.NOP

import Control.Lens

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Test.Hspec

spec :: Spec
spec = describe "lsr" $
  it "imm" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    (cpu & nop & p) === (cpu & p)
