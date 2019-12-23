{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Instructions.IS.SBCSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.IS.LDA
import CPU.Instructions.IS.LDX
import CPU.Instructions.IS.LDY
import CPU.Instructions.IS.SBC
import CPU.Instructions.IS.SE
import CPU.Instructions.IS.STA

import Control.Lens
import Data.Bits
import Data.Vector.Storable

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "sbc" $ do
  it "imm" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- genCPU memSize
    a       <- forAll $ word8 (linear minBound maxBound)
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm a
             & sec
             & sbcImm w
    (cpu' & rA) === (a - w)
    (cpu' & p & carry) === (a > w)
    (cpu' & p & zero)  === (a - w == 0)

  it "zeropage" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 2 256)
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear 9 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    addr    <- forAll $ word8 (linear minBound (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldaImm w' & staZpg addr
             & ldaImm w
             & sec
             & sbcZpg addr
    ((cpu' & mem) ! fromIntegral addr) === w'
    (cpu' & rA) === (w - w')
    (cpu' & p & carry) === (w > w')
    (cpu' & p & zero)  === (w - w' == 0)

  it "zeropage, x" $ requireProperty $ do
    memSize <- forAll $ G.constant 40
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound maxBound)
    x       <- forAll $ word8 (linear minBound 10)
    addr    <- forAll $ word8 (linear (x + 2) (x + 4))
    let cpu' = cpu
             & ldaImm w' & ldxImm x & staZpgX addr
             & ldaImm w
             & sec
             & sbcZpgX addr
    (cpu' & rA) === (w - w')
    (cpu' & p & carry) === (w > w')
    (cpu' & p & zero)  === (w - w' == 0)

  it "abs" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 2 256)
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word16 (linear minBound (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldaImm w' & staAbs (fromIntegral addr)
             & ldaImm w
             & sec
             & sbcAbs (fromIntegral addr)
    (cpu' & rA) === (w - w')
    (cpu' & p & carry) === (w > w')
    (cpu' & p & zero)  === (w - w' == 0)

  it "abs, x" $ requireProperty $ do
    memSize <- forAll $ G.constant 80
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word16 (linear minBound 40)
    x       <- forAll $ word8 (linear minBound 10)
    let cpu' = cpu
             & ldaImm w' & ldxImm x & staAbsX (fromIntegral addr)
             & ldaImm w
             & sec
             & sbcAbsX (fromIntegral addr)
    (cpu' & rA) === (w - w')
    (cpu' & p & carry) === (w > w')
    (cpu' & p & zero)  === (w - w' == 0)

  it "abs, y" $ requireProperty $ do
    memSize <- forAll $ G.constant 80
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word16 (linear minBound (fromIntegral memSize `div` 2))
    y       <- forAll $ word8 (linear 0 10)
    let cpu' = cpu
             & ldaImm w' & ldyImm y & staAbsY (fromIntegral addr)
             & ldaImm w
             & sec
             & sbcAbsY (fromIntegral addr)
    (cpu' & rA) === (w - w')
    (cpu' & p & carry) === (w > w')
    (cpu' & p & zero)  === (w - w' == 0)

  it "x, ind" $ requireProperty $ do
    memSize <- forAll $ G.constant 280
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound maxBound)
    x       <- forAll $ word8 (linear 1 2)
    ind     <- forAll $ word8 (linear minBound (maxBound - 2))
    addr    <- forAll $ word16 (linear 256 (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldaImm (fromIntegral (addr .&. 0x00ff)) & staZpg (ind + x)
             & ldaImm (fromIntegral (addr `shiftR` 8)) & staZpg (ind + x + 1)
             & ldaImm w' & staAbs (fromIntegral addr)
             & ldaImm w
             & ldxImm x
             & sec
             & sbcIndX (ind)
    (cpu' & rA) === (w - w')
    (cpu' & p & carry) === (w > w')
    (cpu' & p & zero)  === (w - w' == 0)

  it "ind, y" $ requireProperty $ do
    memSize <- forAll $ G.constant 40
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear 30 maxBound)
    w'      <- forAll $ word8 (linear 60 maxBound)
    y       <- forAll $ word8 (linear 1 2)
    ind     <- forAll $ word8 (linear 4 6)
    addr    <- forAll $ word16 (linear 8 20)
    let cpu' = cpu
             & ldaImm (fromIntegral (addr .&. 0x00ff)) & staZpg ind
             & ldaImm (fromIntegral (addr `shiftR` 8)) & staZpg (ind + 1)
             & ldaImm w' & staAbs (fromIntegral addr + fromIntegral y)
             & ldaImm w
             & ldyImm y
             & sec
             & sbcIndY (ind)
    (cpu' & rA) === (w - w')
    (cpu' & p & carry) === (w > w')
    (cpu' & p & zero)  === (w - w' == 0)
