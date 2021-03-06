module CPU.Instructions.IS.LSRSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.IS.LDA
import CPU.Instructions.IS.LDX
import CPU.Instructions.IS.LSR
import CPU.Instructions.IS.STA

import Control.Lens
import Data.Bits
import Data.Bits.Lens

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

{-# ANN spec "HLint: ignore Reduce duplication" #-}

spec :: Spec
spec = describe "lsr" $ do
  it "imm" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w
             & lsrAcc
    (cpu' & rA)           === w `shiftR` 1
    (cpu' & p & zero)     === (w `shiftR` 1 == 0)
    (cpu' & p & negative) === False
    (cpu' & p & carry)    === (w ^. bitAt 0)

  it "zpg" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    addr    <- forAll $ word8 (linear minBound maxBound)
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w
             & staZpg addr
             & lsrZpg addr
             & ldaZpg addr
    (cpu' & rA)           === w `shiftR` 1
    (cpu' & p & zero)     === (w `shiftR` 1 == 0)
    (cpu' & p & negative) === False
    (cpu' & p & carry)    === (w ^. bitAt 0)

  it "zpg, x" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    x       <- forAll $ word8 (linear 10 20)
    addr    <- forAll $ word8 (linear minBound (maxBound - 20))
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldxImm x
             & ldaImm w
             & staZpgX addr
             & lsrZpgX addr
             & ldaZpg addr
    (cpu' & rA)           === w `shiftR` 1
    (cpu' & p & zero)     === (w `shiftR` 1 == 0)
    (cpu' & p & negative) === False
    (cpu' & p & carry)    === (w ^. bitAt 0)

  it "abs" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    addr    <- forAll $ word16 (linear minBound (memSize - 1))
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w
             & staAbs addr
             & lsrAbs addr
             & ldaAbs addr
    (cpu' & rA)           === w `shiftR` 1
    (cpu' & p & zero)     === (w `shiftR` 1 == 0)
    (cpu' & p & negative) === False
    (cpu' & p & carry)    === (w ^. bitAt 0)

  it "abs, x" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    x       <- forAll $ word8 (linear 10 20)
    addr    <- forAll $ word16 (linear minBound (memSize - 20))
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldxImm x
             & ldaImm w
             & staAbsX addr
             & lsrAbsX addr
             & ldaAbs addr
    (cpu' & rA)           === w `shiftR` 1
    (cpu' & p & zero)     === (w `shiftR` 1 == 0)
    (cpu' & p & negative) === False
    (cpu' & p & carry)    === (w ^. bitAt 0)
