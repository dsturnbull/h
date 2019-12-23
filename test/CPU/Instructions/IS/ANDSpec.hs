module CPU.Instructions.IS.ANDSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.IS.AND
import CPU.Instructions.IS.LDA
import CPU.Instructions.IS.LDX
import CPU.Instructions.IS.LDY
import CPU.Instructions.IS.STA

import Control.Lens
import Data.Bits
import Data.Bits.Lens

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "and" $ do
  it "imm" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w
             & andImm w'
    (cpu' & rA)           === (w .&. w')
    (cpu' & p & zero)     === ((w .&. w') == 0)
    (cpu' & p & negative) === ((w .&. w') ^. bitAt 7)

  it "zpg" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word8 (linear 0 255)
    let cpu' = cpu
             & ldaImm w & staZpg addr
             & ldaImm w'
             & andZpg addr
    (cpu' & rA)           === (w .&. w')
    (cpu' & p & zero)     === ((w .&. w') == 0)
    (cpu' & p & negative) === ((w .&. w') ^. bitAt 7)

  it "zpg, x" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound maxBound)
    x       <- forAll $ word8 (linear minBound 10)
    addr    <- forAll $ word8 (linear 0 (255 - 10))
    let cpu' = cpu
             & ldxImm x
             & ldaImm w & staZpgX addr
             & ldaImm w'
             & andZpgX addr
    (cpu' & rA)           === (w .&. w')
    (cpu' & p & zero)     === ((w .&. w') == 0)
    (cpu' & p & negative) === ((w .&. w') ^. bitAt 7)

  it "abs" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word16 (linear 0 255)
    let cpu' = cpu
             & ldaImm w & staAbs addr
             & ldaImm w'
             & andAbs addr
    (cpu' & rA)           === (w .&. w')
    (cpu' & p & zero)     === ((w .&. w') == 0)
    (cpu' & p & negative) === ((w .&. w') ^. bitAt 7)

  it "abs, x" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound (maxBound - 10))
    x       <- forAll $ word8 (linear minBound 10)
    addr    <- forAll $ word16 (linear 0 (255 - fromIntegral x))
    let cpu' = cpu
             & ldaImm w & ldxImm x & staAbsX addr
             & ldaImm w'
             & andAbsX addr
    (cpu' & rA)           === (w .&. w')
    (cpu' & p & zero)     === ((w .&. w') == 0)
    (cpu' & p & negative) === ((w .&. w') ^. bitAt 7)

  it "abs, y" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound (maxBound - 10))
    y       <- forAll $ word8 (linear minBound 10)
    addr    <- forAll $ word16 (linear 0 (255 - fromIntegral y))
    let cpu' = cpu
             & ldaImm w & ldyImm y & staAbsY addr
             & ldaImm w'
             & andAbsY addr
    (cpu' & rA)           === (w .&. w')
    (cpu' & p & zero)     === ((w .&. w') == 0)
    (cpu' & p & negative) === ((w .&. w') ^. bitAt 7)

  it "x, ind" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear 1 maxBound)
    w'      <- forAll $ word8 (linear 1 maxBound)
    x       <- forAll $ word8 (linear 1 10)
    ind     <- forAll $ word8 (linear 20 40)
    addr    <- forAll $ word16 (linear (fromIntegral ind + fromIntegral x + 4) (255 - fromIntegral x))
    let cpu' = cpu
             & ldxImm x
             & ldaImm (fromIntegral (addr .&. 0x00ff)) & staZpgX ind
             & ldaImm (fromIntegral (addr `shiftR` 8)) & staZpgX (ind + 1)
             & ldaImm w & staAbs addr
             & ldaImm w'
             & andIndX ind
    (cpu' & rA)           === (w .&. w')
    (cpu' & p & zero)     === ((w .&. w') == 0)
    (cpu' & p & negative) === ((w .&. w') ^. bitAt 7)

  it "ind, y" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear 1 maxBound)
    w'      <- forAll $ word8 (linear 1 maxBound)
    y       <- forAll $ word8 (linear 1 10)
    ind     <- forAll $ word8 (linear 20 40)
    addr    <- forAll $ word16 (linear (fromIntegral ind + fromIntegral y + 4) (255 - fromIntegral y))
    let cpu' = cpu
             & ldxImm y
             & ldaImm (fromIntegral (addr .&. 0x00ff)) & staZpg ind
             & ldaImm (fromIntegral (addr `shiftR` 8)) & staZpg (ind + 1)
             & ldaImm w & staAbsX addr
             & ldaImm w'
             & ldyImm y
             & andIndY ind
    (cpu' & rA)           === (w .&. w')
    (cpu' & p & zero)     === ((w .&. w') == 0)
    (cpu' & p & negative) === ((w .&. w') ^. bitAt 7)
