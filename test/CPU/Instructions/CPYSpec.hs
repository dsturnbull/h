module CPU.Instructions.CPYSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.CPY
import CPU.Instructions.LDA
import CPU.Instructions.LDX
import CPU.Instructions.STA

import Control.Lens

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "cpy" $ do
  it "imm" $ requireProperty $ do
    cpu     <- forAll $ genCPU 0
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    let cpu' = cpu
             & ldxImm w
             & cpyImm w'
    (cpu' & p & carry)    === (w >= w')
    (cpu' & p & zero)     === (w == w')
    (cpu' & p & negative) === msb (w - w')

  it "zpg" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    addr    <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w' & staZpg addr
             & ldxImm w
             & cpyZpg addr
    annotateShow cpu'
    (cpu' & p & carry)    === (w >= w')
    (cpu' & p & zero)     === (w == w')
    (cpu' & p & negative) === msb (w - w')

  it "abs" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU (fromIntegral memSize)
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    addr    <- forAll $ word16 (linear minBound memSize)
    let cpu' = cpu
             & ldaImm w' & staAbs (fromIntegral addr)
             & ldxImm w
             & cpyAbs (fromIntegral addr)
    (cpu' & p & carry)    === (w >= w')
    (cpu' & p & zero)     === (w == w')
    (cpu' & p & negative) === msb (w - w')
