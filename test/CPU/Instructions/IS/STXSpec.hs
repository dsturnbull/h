module CPU.Instructions.IS.STXSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.IS.LDX
import CPU.Instructions.IS.LDY
import CPU.Instructions.IS.STX

import Control.Lens
import Data.Vector.Storable

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "stx" $ do
  it "abs" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 1 256)
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word8 (linear minBound (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldxImm w
             & stxAbs (fromIntegral addr)
    (cpu' & mem) ! fromIntegral addr === w

  it "zeropage" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 2 256)
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word8 (linear minBound (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldxImm w
             & stxZpg (fromIntegral addr)
    (cpu' & mem) ! fromIntegral addr === w

  it "zeropage, y" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 30 256)
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear 6 maxBound)
    y       <- forAll $ word8 (linear minBound 10)
    addr    <- forAll $ word8 (linear minBound 10)
    let cpu' = cpu
             & ldxImm w
             & ldyImm y
             & stxZpgY addr
    (cpu' & mem) ! (fromIntegral addr + fromIntegral y) === w
