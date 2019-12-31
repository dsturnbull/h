module CPU.Instructions.IS.LDYSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.IS.LDA
import CPU.Instructions.IS.LDX
import CPU.Instructions.IS.LDY
import CPU.Instructions.IS.STA

import Control.Lens

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

{-# ANN spec "HLint: ignore Reduce duplication" #-}

spec :: Spec
spec = describe "ldy" $ do
  it "imm" $ requireProperty $ do
    cpu  <- genCPU 0
    w    <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu & ldyImm w
    (cpu' & rY) === w
    (cpu' & p & zero) === (w == 0)

  it "zpg" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 1 256)
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word8 (linear minBound (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldaImm w & staZpg addr
             & ldyZpg addr
    (cpu' & rY) === w
    (cpu' & p & zero) === (w == 0)

  it "zpg, y" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 8 256)
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    x       <- forAll $ word8 (linear 1 2)
    addr    <- forAll $ word8 (linear (fromIntegral x) (fromIntegral memSize `div` 2))
    let cpu' = cpu
             & ldaImm w & staZpg (addr + x)
             & ldxImm x
             & ldyZpgX addr
    (cpu' & rY) === w
    (cpu' & p & zero) === (w == 0)

  it "abs" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 1 256)
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word16 (linear minBound (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldaImm w & staAbs (fromIntegral addr)
             & ldaImm (w - 1)
             & ldyAbs (fromIntegral addr)
    (cpu' & rY) === w
    (cpu' & p & zero) === (w == 0)

  it "abs, y" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 8 256)
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    x       <- forAll $ word8 (linear 1 2)
    addr    <- forAll $ word16 (linear (fromIntegral x) (fromIntegral memSize `div` 2))
    let cpu' = cpu
             & ldaImm w & staZpg (fromIntegral addr + x)
             & ldxImm x
             & ldyAbsX (fromIntegral addr)
    (cpu' & rY) === w
    (cpu' & p & zero) === (w == 0)
