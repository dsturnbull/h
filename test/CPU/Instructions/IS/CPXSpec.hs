module CPU.Instructions.IS.CPXSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.IS.CPX
import CPU.Instructions.IS.LDA
import CPU.Instructions.IS.LDX
import CPU.Instructions.IS.STA

import Control.Lens

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "cpx" $ do
  it "imm" $ requireProperty $ do
    cpu     <- genCPU 0
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    let cpu' = cpu
             & ldxImm w
             & cpxImm w'
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
             & ldxImm w
             & cpxZpg addr
    (cpu' & p & carry)    === (w >= w')
    (cpu' & p & zero)     === (w == w')
    (cpu' & p & negative) === msb (w - w')

  it "abs" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU (fromIntegral memSize)
    w       <- forAll $ word8 (linear 7 maxBound)
    w'      <- forAll $ word8 (linear 7 maxBound)
    addr    <- forAll $ word16 (linear minBound memSize)
    let cpu' = cpu
             & ldaImm w' & staAbs (fromIntegral addr)
             & ldxImm w
             & cpxAbs (fromIntegral addr)
    (cpu' & p & carry)    === (w >= w')
    (cpu' & p & zero)     === (w == w')
    (cpu' & p & negative) === msb (w - w')
