module CPU.Instructions.IS.JMPSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.IS.JMP
import CPU.Instructions.IS.LDA
import CPU.Instructions.IS.STA

import Control.Lens
import Data.Bits

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "jmp" $ do
  it "abs" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 4 maxBound)
    cpu     <- genCPU (fromIntegral memSize)
    addr    <- forAll $ word16 (linear minBound maxBound)
    let cpu' = cpu & jmpAbs (fromIntegral addr)
    (cpu' & pc) === addr

  it "ind" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 4 maxBound)
    cpu     <- genCPU (fromIntegral memSize)
    ind     <- forAll $ word16 (linear minBound (memSize `div` 3))
    addr    <- forAll $ word16 (linear minBound (memSize `div` 3))
    let cpu' = cpu
             & ldaImm (fromIntegral (addr .&. 0x00ff)) & staAbs ind
             & ldaImm (fromIntegral (addr `shiftR` 8)) & staAbs (ind + 1)
             & jmpInd ind
    (cpu' & pc) === addr
