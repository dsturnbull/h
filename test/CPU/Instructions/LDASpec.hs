module CPU.Instructions.LDASpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.LDA
import CPU.Instructions.LDX
import CPU.Instructions.LDY
import CPU.Instructions.STA

import Control.Lens
import Data.Bits

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "lda" $ do
  it "imm" $ requireProperty $ do
    cpu  <- forAll $ genCPU 0
    w    <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu & ldaImm w
    (cpu' & rA) === w
    (cpu' & p & zero) === (w == 0)

  it "abs" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 1 256)
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word16 (linear minBound (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldaImm w & staAbs (fromIntegral addr)
             & ldaImm (w - 1)
             & ldaAbs (fromIntegral addr)
    (cpu' & rA) === w
    (cpu' & p & zero) === (w == 0)

  it "abs, x" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 2 256)
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    x       <- forAll $ word8 (linear 1 2)
    addr    <- forAll $ word16 (linear (fromIntegral x) (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldaImm  w & staAbs  (fromIntegral addr)
             & ldxImm  x
             & ldaAbsX (addr - fromIntegral x)
    (cpu' & rA) === w
    (cpu' & p & zero) === (w == 0)

  it "abs, y" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    y       <- forAll $ word8 (linear 1 2)
    addr    <- forAll $ word16 (linear (fromIntegral y) (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldaImm  w & staAbs  (fromIntegral addr)
             & ldyImm  y
             & ldaAbsY (addr - fromIntegral y)
    (cpu' & rA) === w
    (cpu' & p & zero) === (w == 0)

  it "x, ind" $ requireProperty $ do
    memSize <- forAll $ G.constant 280
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear 7 maxBound)
    x       <- forAll $ word8 (linear 1 2)
    ind     <- forAll $ word8 (linear (fromIntegral x) maxBound) -- allow space for x
    addr    <- forAll $ word16 (linear 256 (fromIntegral memSize - 1)) -- allow space for ind zeropage
    let cpu' = cpu
             & ldaImm  w                                & staAbs (fromIntegral addr)
             & ldaImm  (fromIntegral (addr .&. 0x00ff)) & staZpg ind
             & ldaImm  (fromIntegral (addr `shiftR` 8)) & staZpg (ind + 1)
             & ldxImm  x
             & ldaIndX (ind - x)
    (cpu' & rA) === w
    (cpu' & p & zero) === (w == 0)

  it "ind, y" $ requireProperty $ do
    memSize <- forAll $ G.constant 280
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear 7 maxBound)
    y       <- forAll $ word8 (linear 1 2)
    ind     <- forAll $ word8 (linear (fromIntegral y) maxBound) -- allow space for x
    addr    <- forAll $ word16 (linear 256 (fromIntegral memSize - fromIntegral y - 1)) -- allow space for ind zeropage
    let cpu' = cpu
             & ldaImm  w                                & staAbs (fromIntegral addr + fromIntegral y)
             & ldaImm  (fromIntegral (addr .&. 0x00ff)) & staZpg ind
             & ldaImm  (fromIntegral (addr `shiftR` 8)) & staZpg (ind + 1)
             & ldyImm  y
             & ldaIndY ind
    (cpu' & rA) === w
    (cpu' & p & zero) === (w == 0)
