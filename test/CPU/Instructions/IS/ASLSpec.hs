module CPU.Instructions.IS.ASLSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.IS.ASL
import CPU.Instructions.IS.LDA
import CPU.Instructions.IS.LDX
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
spec = describe "asl" $ do
  it "imm" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w
             & aslAcc
    (cpu' & rA)           === w `shiftL` 1
    (cpu' & p & zero)     === (w `shiftL` 1 == 0)
    (cpu' & p & negative) === (w `shiftL` 1 < 0)
    (cpu' & p & carry)    === (w ^. bitAt 7)

  it "zpg" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    addr    <- forAll $ word8 (linear minBound maxBound)
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w
             & staZpg addr
             & aslZpg addr
             & ldaZpg addr
    (cpu' & rA)           === w `shiftL` 1
    (cpu' & p & zero)     === (w `shiftL` 1 == 0)
    (cpu' & p & negative) === (w `shiftL` 1 < 0)
    (cpu' & p & carry)    === (w ^. bitAt 7)

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
             & aslZpgX addr
             & ldaZpg addr
    (cpu' & rA)           === w `shiftL` 1
    (cpu' & p & zero)     === (w `shiftL` 1 == 0)
    (cpu' & p & negative) === (w `shiftL` 1 < 0)
    (cpu' & p & carry)    === (w ^. bitAt 7)

  it "abs" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    addr    <- forAll $ word16 (linear minBound (memSize - 1))
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w
             & staAbs addr
             & aslAbs addr
             & ldaAbs addr
    (cpu' & rA)           === w `shiftL` 1
    (cpu' & p & zero)     === (w `shiftL` 1 == 0)
    (cpu' & p & negative) === (w `shiftL` 1 < 0)
    (cpu' & p & carry)    === (w ^. bitAt 7)

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
             & aslAbsX addr
             & ldaAbs addr
    (cpu' & rA)           === w `shiftL` 1
    (cpu' & p & zero)     === (w `shiftL` 1 == 0)
    (cpu' & p & negative) === (w `shiftL` 1 < 0)
    (cpu' & p & carry)    === (w ^. bitAt 7)
