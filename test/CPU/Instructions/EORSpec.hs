module CPU.Instructions.EORSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.EOR
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

spec :: Spec
spec = describe "eor" $ do
  it "imm" $ requireProperty $ do
    cpu     <- forAll $ genCPU 1
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    let cpu' = cpu
             & ldaImm w
             & eorImm w'
    let r = w .|. w'
    (cpu' & rA) === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === msb r

  it "zpg" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    addr    <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w' & staZpg addr
             & ldaImm w
             & eorZpg addr
    let r = w .|. w'
    (cpu' & rA) === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === msb r

  it "zpg, x" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    x       <- forAll $ word8 (linear minBound (maxBound `div` 2))
    addr    <- forAll $ word8 (linear minBound (maxBound - x))
    let cpu' = cpu
             & ldxImm x
             & ldaImm w' & staZpgX addr
             & ldaImm w
             & eorZpgX addr
    let r = w .|. w'
    (cpu' & rA) === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === msb r

  it "abs" $ requireProperty $ do
    memSize <- forAll $ G.constant (maxBound :: Word16)
    cpu     <- forAll $ genCPU (fromIntegral memSize)
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    addr    <- forAll $ word16 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w' & staAbs (fromIntegral addr)
             & ldaImm w
             & eorAbs (fromIntegral addr)
    let r = w .|. w'
    (cpu' & rA) === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === msb r

  it "abs, x" $ requireProperty $ do
    memSize <- forAll $ G.constant (maxBound :: Word8)
    cpu     <- forAll $ genCPU (fromIntegral memSize)
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    x       <- forAll $ word8 (linear minBound (maxBound `div` 2))
    addr    <- forAll $ word8 (linear minBound x)
    let cpu' = cpu
             & ldxImm x
             & ldaImm w' & staAbsX (fromIntegral addr)
             & ldaImm w
             & eorAbsX (fromIntegral addr)
    let r = w .|. w'
    (cpu' & rA) === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === msb r

  it "abs, y" $ requireProperty $ do
    memSize <- forAll $ G.constant (maxBound :: Word8)
    cpu     <- forAll $ genCPU (fromIntegral memSize)
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    y       <- forAll $ word8 (linear minBound (maxBound `div` 2))
    addr    <- forAll $ word8 (linear minBound (maxBound - y))
    let cpu' = cpu
             & ldxImm y
             & ldaImm w' & staAbsY (fromIntegral addr)
             & ldaImm w
             & eorAbsY (fromIntegral addr)
    let r = w .|. w'
    (cpu' & rA) === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === msb r

  it "x, ind" $ requireProperty $ do
    memSize <- forAll $ G.constant 40
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    x       <- forAll $ G.constant 2
    ind     <- forAll $ G.constant 8
    addr    <- forAll $ word16 (linear 20 30)
    let cpu' = cpu
             & ldxImm x
             & ldaImm (fromIntegral (addr .&. 0x00ff)) & staZpgX (ind)
             & ldaImm (fromIntegral (addr `shiftR` 8)) & staZpgX (ind + 1)
             & ldaImm w' & staAbs (fromIntegral addr)
             & ldaImm w  & eorIndX ind
    let r = w .|. w'
    (cpu' & rA) === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === msb r

  it "ind, y" $ requireProperty $ do
    memSize <- forAll $ G.constant 40
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    y       <- forAll $ G.constant 2
    ind     <- forAll $ G.constant 8
    addr    <- forAll $ word16 (linear 20 30)
    let cpu' = cpu
             & ldaImm (fromIntegral (addr .&. 0x00ff)) & staZpg ind
             & ldaImm (fromIntegral (addr `shiftR` 8)) & staZpg (ind + 1)
             & ldxImm y & ldaImm w' & staAbsX addr
             & ldyImm y & ldaImm w  & eorIndY ind
    let r = w .|. w'
    (cpu' & rA) === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === msb r
