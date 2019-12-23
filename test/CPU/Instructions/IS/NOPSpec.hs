module CPU.Instructions.IS.NOPSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.IS.NOP

import Control.Lens

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Test.Hspec

spec :: Spec
spec = describe "lsr" $
  it "imm" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    (cpu & nop & p) === (cpu & p)
