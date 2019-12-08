{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Instructions.IS.ADCSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.ADC
import CPU.Instructions.LDA
import CPU.Instructions.LDX
import CPU.Instructions.LDY
import CPU.Instructions.STA

import Control.Lens
import Data.Bits
import Data.Word

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

shouldCarry :: Word8 -> Word8 -> Bool
shouldCarry a b = v > fromIntegral (maxBound :: Word8)
  where v :: Int = fromIntegral a + fromIntegral b

spec :: Spec
spec = describe "adc" $ do
  it "imm" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- forAll $ genCPU memSize
    a       <- forAll $ word8 (linear minBound maxBound)
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm a
             & adcImm w
    (cpu' & rA) === (a + w)
    (cpu' & p & carry) === shouldCarry a w
    (cpu' & p & zero)  === (a + w == 0)

  it "zeropage" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 2 256)
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word8 (linear minBound (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldaImm w
             & staZpg addr
             & ldaImm w'
             & adcZpg addr
    (cpu' & rA) === (w + w')
    (cpu' & p & carry) === shouldCarry w w'
    (cpu' & p & zero)  === (w + w' == 0)

  it "zeropage, x" $ requireProperty $ do
    memSize <- forAll $ G.constant 40
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound maxBound)
    x       <- forAll $ word8 (linear minBound 10)
    addr    <- forAll $ word8 (linear (x + 2) (x + 4))
    let cpu' = cpu
             & ldaImm w  & ldxImm x & staZpgX addr
             & ldaImm w' & adcZpgX addr
    (cpu' & rA) === (w + w')
    (cpu' & p & carry) === shouldCarry w w'
    (cpu' & p & zero)  === (w + w' == 0)

  it "abs" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 2 256)
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word16 (linear minBound (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldaImm w
             & staAbs (fromIntegral addr)
             & ldaImm w'
             & adcAbs (fromIntegral addr)
    (cpu' & rA) === (w + w')
    (cpu' & p & carry) === shouldCarry w w'
    (cpu' & p & zero)  === (w + w' == 0)

  it "abs, x" $ requireProperty $ do
    memSize <- forAll $ G.constant 80
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word16 (linear minBound 40)
    x       <- forAll $ word8 (linear minBound 10)
    let cpu' = cpu
             & ldaImm w  & ldxImm x & staAbsX (fromIntegral addr)
             & ldaImm w' & adcAbsX (fromIntegral addr)
    (cpu' & rA) === (w + w')
    (cpu' & p & carry) === shouldCarry w w'
    (cpu' & p & zero)  === (w + w' == 0)

  it "abs, y" $ requireProperty $ do
    memSize <- forAll $ G.constant 80
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word16 (linear minBound (fromIntegral memSize `div` 2))
    y       <- forAll $ word8 (linear 0 10)
    let cpu' = cpu
             & ldaImm w  & ldyImm y & staAbsY (fromIntegral addr)
             & ldaImm w' & adcAbsY (fromIntegral addr)
    (cpu' & rA) === (w + w')
    (cpu' & p & carry) === shouldCarry w w'
    (cpu' & p & zero)  === (w + w' == 0)

  it "x, ind" $ requireProperty $ do
    memSize <- forAll $ G.constant 280
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound maxBound)
    x       <- forAll $ word8 (linear 1 2)
    ind     <- forAll $ word8 (linear minBound (maxBound - 2))
    addr    <- forAll $ word16 (linear 256 (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldaImm (fromIntegral (addr .&. 0x00ff)) & staZpg (ind + x)
             & ldaImm (fromIntegral (addr `shiftR` 8)) & staZpg (ind + x + 1)
             & ldaImm w & staAbs (fromIntegral addr)
             & ldaImm w'
             & ldxImm x
             & adcIndX (ind)
    (cpu' & rA) === (w + w')
    (cpu' & p & carry) === shouldCarry w w'
    (cpu' & p & zero)  === (w + w' == 0)

  it "ind, y" $ requireProperty $ do
    memSize <- forAll $ G.constant 40
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear 30 maxBound)
    w'      <- forAll $ word8 (linear 60 maxBound)
    y       <- forAll $ word8 (linear 1 2)
    ind     <- forAll $ word8 (linear 4 6)
    addr    <- forAll $ word16 (linear 8 20)
    let cpu' = cpu
             & ldaImm (fromIntegral (addr .&. 0x00ff)) & staZpg ind
             & ldaImm (fromIntegral (addr `shiftR` 8)) & staZpg (ind + 1)
             & ldaImm w & staAbs (fromIntegral addr + fromIntegral y)
             & ldaImm w'
             & ldyImm y
             & adcIndY (ind)
    (cpu' & rA) === (w + w')
    (cpu' & p & carry) === shouldCarry w w'
    (cpu' & p & zero)  === (w + w' == 0)
