module CPU.Instructions.LDXSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.LDA
import CPU.Instructions.LDX
import CPU.Instructions.LDY
import CPU.Instructions.STA

import Control.Lens

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "ldx" $ do
  it "imm" $ requireProperty $ do
    cpu  <- forAll $ genCPU 0
    w    <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu & ldxImm w
    (cpu' & rX) === w
    (cpu' & p & zero) === (w == 0)

  it "zpg" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 1 256)
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word8 (linear minBound (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldaImm w & staZpg addr
             & ldxZpg addr
    (cpu' & rX) === w
    (cpu' & p & zero) === (w == 0)

  it "zpg, y" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 8 256)
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    y       <- forAll $ word8 (linear 1 2)
    addr    <- forAll $ word8 (linear (fromIntegral y) (fromIntegral memSize `div` 2))
    let cpu' = cpu
             & ldaImm w & staZpg (addr + y)
             & ldyImm y
             & ldxZpgY addr
    (cpu' & rX) === w
    (cpu' & p & zero) === (w == 0)

  it "abs" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 1 256)
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word16 (linear minBound (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldaImm w & staAbs (fromIntegral addr)
             & ldxAbs (fromIntegral addr)
    (cpu' & rX) === w
    (cpu' & p & zero) === (w == 0)

  it "abs, y" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 8 256)
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    y       <- forAll $ word8 (linear 1 2)
    addr    <- forAll $ word16 (linear (fromIntegral y) (fromIntegral memSize `div` 2))
    let cpu' = cpu
             & ldaImm w & staZpg (fromIntegral addr + y)
             & ldyImm y
             & ldxAbsY (fromIntegral addr)
    annotateShow cpu'
    (cpu' & rX) === w
    (cpu' & p & zero) === (w == 0)
