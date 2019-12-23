module CPU.Instructions.IS.CMPSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.IS.CMP
import CPU.Instructions.IS.LDA
import CPU.Instructions.IS.LDX
import CPU.Instructions.IS.LDY
import CPU.Instructions.IS.STA

import Control.Lens
import Data.Bits
import Data.Word

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "cmp" $ do
  it "imm" $ requireProperty $ do
    cpu     <- genCPU 0
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    let cpu' = cpu
             & ldaImm w
             & cmpImm w'
    (cpu' & p & carry)    === (w >= w')
    (cpu' & p & zero)     === (w == w')
    (cpu' & p & negative) === msb (w - w')

  it "zpg" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    addr    <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w' & staZpg addr
             & ldaImm w
             & cmpZpg addr
    (cpu' & p & carry)    === (w >= w')
    (cpu' & p & zero)     === (w == w')
    (cpu' & p & negative) === msb (w - w')

  it "zpg, x" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    x       <- forAll $ word8 (linear minBound (maxBound `div` 2))
    addr    <- forAll $ word8 (linear minBound (maxBound - x))
    let cpu' = cpu
             & ldxImm x
             & ldaImm w' & staZpgX addr
             & ldaImm w
             & cmpZpgX addr
    (cpu' & p & carry)    === (w >= w')
    (cpu' & p & zero)     === (w == w')
    (cpu' & p & negative) === msb (w - w')

  it "abs" $ requireProperty $ do
    memSize <- forAll $ G.constant (maxBound :: Word16)
    cpu     <- genCPU (fromIntegral memSize)
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    addr    <- forAll $ word16 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w' & staAbs (fromIntegral addr)
             & ldaImm w
             & cmpAbs (fromIntegral addr)
    (cpu' & p & carry)    === (w >= w')
    (cpu' & p & zero)     === (w == w')
    (cpu' & p & negative) === msb (w - w')

  it "abs, x" $ requireProperty $ do
    memSize <- forAll $ G.constant (maxBound :: Word8)
    cpu     <- genCPU (fromIntegral memSize)
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    x       <- forAll $ word8 (linear minBound (maxBound `div` 2))
    addr    <- forAll $ word8 (linear minBound (maxBound - x))
    let cpu' = cpu
             & ldxImm x
             & ldaImm w' & staAbsX (fromIntegral addr)
             & ldaImm w
             & cmpAbsX (fromIntegral addr)
    (cpu' & p & carry)    === (w >= w')
    (cpu' & p & zero)     === (w == w')
    (cpu' & p & negative) === msb (w - w')

  it "abs, y" $ requireProperty $ do
    memSize <- forAll $ G.constant (maxBound :: Word8)
    cpu     <- genCPU (fromIntegral memSize)
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    y       <- forAll $ word8 (linear minBound (maxBound `div` 2))
    addr    <- forAll $ word8 (linear minBound (maxBound - y))
    let cpu' = cpu
             & ldxImm y
             & ldaImm w' & staAbsY (fromIntegral addr)
             & ldaImm w
             & cmpAbsY (fromIntegral addr)
    (cpu' & p & carry)    === (w >= w')
    (cpu' & p & zero)     === (w == w')
    (cpu' & p & negative) === msb (w - w')

  it "x, ind" $ requireProperty $ do
    memSize <- forAll $ G.constant 40
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    x       <- forAll $ G.constant 2
    ind     <- forAll $ G.constant 8
    addr    <- forAll $ word16 (linear 20 30)
    let cpu' = cpu
             & ldxImm x
             & ldaImm (fromIntegral (addr .&. 0x00ff)) & staZpgX ind
             & ldaImm (fromIntegral (addr `shiftR` 8)) & staZpgX (ind + 1)
             & ldaImm w' & staAbs (fromIntegral addr)
             & ldaImm w  & cmpIndX ind
    (cpu' & p & carry)    === (w >= w')
    (cpu' & p & zero)     === (w == w')
    (cpu' & p & negative) === msb (w - w')

  it "ind, y" $ requireProperty $ do
    memSize <- forAll $ G.constant 40
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    y       <- forAll $ G.constant 2
    ind     <- forAll $ G.constant 8
    addr    <- forAll $ word16 (linear 20 30)
    let cpu' = cpu
             & ldaImm (fromIntegral (addr .&. 0x00ff)) & staZpg ind
             & ldaImm (fromIntegral (addr `shiftR` 8)) & staZpg (ind + 1)
             & ldxImm y & ldaImm w' & staAbsX (fromIntegral addr)
             & ldyImm y & ldaImm w  & cmpIndY ind
    (cpu' & p & carry)    === (w >= w')
    (cpu' & p & zero)     === (w == w')
    (cpu' & p & negative) === msb (w - w')
