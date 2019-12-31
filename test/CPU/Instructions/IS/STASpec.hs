module CPU.Instructions.IS.STASpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.IS.LDA
import CPU.Instructions.IS.LDX
import CPU.Instructions.IS.LDY
import CPU.Instructions.IS.STA

import Control.Lens
import Data.Bits
import Data.Vector.Storable

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

{-# ANN spec "HLint: ignore Reduce duplication" #-}

spec :: Spec
spec = describe "sta" $ do
  it "abs" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 1 256)
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word16 (linear minBound (fromIntegral memSize - 1))
    let mem' = cpu
             & ldaImm w
             & staAbs (fromIntegral addr)
             & mem
    mem' ! fromIntegral addr === w

  it "zpg" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 1 256)
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word16 (linear minBound (fromIntegral memSize - 1))
    let mem' = cpu
             & ldaImm w
             & staZpg (fromIntegral addr)
             & mem
    mem' ! fromIntegral addr === w

  it "abs, x" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 3 256)
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    x       <- forAll $ word8 (linear minBound (fromIntegral memSize `div` 2))
    addr    <- forAll $ word16 (linear minBound (fromIntegral memSize - fromIntegral x - 1))
    let cpu' = cpu
             & ldaImm w
             & ldxImm x
             & staAbsX (fromIntegral addr)
    (cpu' & mem) ! fromIntegral (addr + fromIntegral x) === w

  it "abs, y" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 4 256)
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    y       <- forAll $ word8 (linear minBound (fromIntegral (memSize `div` 3)))
    addr    <- forAll $ word16 (linear (fromIntegral y) (fromIntegral memSize `div` 3))
    let cpu' = cpu
             & ldaImm w
             & ldyImm y
             & staAbsY (fromIntegral addr)
    (cpu' & mem) ! fromIntegral (addr + fromIntegral y) === w

  it "x, ind" $ requireProperty $ do
    memSize <- forAll $ G.constant 280
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear 7 maxBound)
    x       <- forAll $ word8 (linear 1 2)
    ind     <- forAll $ word8 (linear minBound (maxBound - 2))
    addr    <- forAll $ word16 (linear 256 (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldaImm  (fromIntegral (addr .&. 0x00ff)) & staZpg (ind + x)
             & ldaImm  (fromIntegral (addr `shiftR` 8)) & staZpg (ind + x + 1)
             & ldaImm  w
             & ldxImm  x
             & staIndX ind
    (cpu' & mem) ! fromIntegral addr === w

  it "ind, y" $ requireProperty $ do
    memSize <- forAll $ G.constant 40
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear 28 maxBound)
    y       <- forAll $ word8 (linear 1 2)
    ind     <- forAll $ word8 (linear 4 6)
    addr    <- forAll $ word16 (linear 8 20)
    let cpu' = cpu
             & ldaImm  (fromIntegral (addr .&. 0x00ff)) & staZpg ind
             & ldaImm  (fromIntegral (addr `shiftR` 8)) & staZpg (ind + 1)
             & ldaImm  w
             & ldyImm  y
             & staIndY ind
    (cpu' & mem) ! (fromIntegral addr + fromIntegral y) === w

  it "zeropage" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 2 256)
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word8 (linear minBound (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldaImm w
             & staZpg (fromIntegral addr)
    (cpu' & mem) ! fromIntegral addr === w

  it "zeropage, x" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 30 256)
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear 6 maxBound)
    x       <- forAll $ word8 (linear minBound 10)
    addr    <- forAll $ word8 (linear minBound 10)
    let cpu' = cpu
             & ldaImm w
             & ldxImm x
             & staZpgX (fromIntegral addr)
    (cpu' & mem) ! (fromIntegral addr + fromIntegral x) === w
