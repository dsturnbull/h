module CPU.Instructions.STYSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.LDX
import CPU.Instructions.LDY
import CPU.Instructions.STY

import Control.Lens
import Data.Vector.Storable

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "sty" $ do
  it "abs" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 1 256)
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word16 (linear minBound (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldyImm w
             & styAbs addr
    (cpu' & mem) ! fromIntegral addr === w

  it "zeropage" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 2 256)
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word8 (linear minBound (fromIntegral memSize - 1))
    let cpu' = cpu
             & ldyImm w
             & styZpg (fromIntegral addr)
    (cpu' & mem) ! fromIntegral addr === w

  it "zeropage, x" $ requireProperty $ do
    memSize <- forAll $ word16 (linear 30 256)
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear 6 maxBound)
    x       <- forAll $ word8 (linear minBound 10)
    addr    <- forAll $ word8 (linear minBound 10)
    let cpu' = cpu
             & ldxImm x
             & ldyImm w
             & styZpgX (fromIntegral addr)
    (cpu' & mem) ! (fromIntegral addr + fromIntegral x) === w
