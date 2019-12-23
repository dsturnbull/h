{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Instructions.IS.DESpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.IS.DE
import CPU.Instructions.IS.LDA
import CPU.Instructions.IS.LDX
import CPU.Instructions.IS.LDY
import CPU.Instructions.IS.STA

import Control.Lens

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

{-# ANN spec "HLint: ignore Reduce duplication" #-}

spec :: Spec
spec = describe "dec" $ do
  it "zpg" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w
             & staZpg addr
             & decZpg addr
             & ldaZpg addr
    (cpu' & rA)           === w - 1
    (cpu' & p & zero)     === (w - 1 == 0)
    (cpu' & p & negative) === (w - 1 > 127)

  it "zpg, x" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    x       <- forAll $ word8 (linear minBound 20)
    addr    <- forAll $ word8 (linear minBound (maxBound - 20))
    let cpu' = cpu
             & ldaImm w
             & ldxImm x
             & staZpgX addr
             & decZpgX addr
             & ldaZpgX addr
    (cpu' & rA)           === w - 1
    (cpu' & p & zero)     === (w - 1 == 0)
    (cpu' & p & negative) === (w - 1 > 127)

  it "abs" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word16 (linear minBound memSize)
    let cpu' = cpu
             & ldaImm w
             & staAbs addr
             & decAbs addr
             & ldaAbs addr
    (cpu' & rA)           === w - 1
    (cpu' & p & zero)     === (w - 1 == 0)
    (cpu' & p & negative) === (w - 1 > 127)

  it "abs, x" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    x       <- forAll $ word8 (linear minBound 20)
    addr    <- forAll $ word16 (linear minBound (255 - 20))
    let cpu' = cpu
             & ldaImm w
             & ldxImm x
             & staAbsX addr
             & decAbsX addr
             & ldaAbsX addr
    (cpu' & rA)           === w - 1
    (cpu' & p & zero)     === (w - 1 == 0)
    (cpu' & p & negative) === (w - 1 > 127)

  it "dex" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- genCPU memSize
    x       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldxImm x
             & dex
    (cpu' & rX)           === x - 1
    (cpu' & p & zero)     === (x - 1 == 0)
    (cpu' & p & negative) === (x - 1 > 127)

  it "dey" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- genCPU memSize
    y       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldyImm y
             & dey
    (cpu' & rY)           === y - 1
    (cpu' & p & zero)     === (y - 1 == 0)
    (cpu' & p & negative) === (y - 1 > 127)
