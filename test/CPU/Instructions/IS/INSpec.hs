{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Instructions.IS.INSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.IS.IN
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
spec = describe "inc" $ do
  it "zpg" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w
             & staZpg addr
             & incZpg addr
             & ldaZpg addr
    (cpu' & rA)           === w + 1
    (cpu' & p & zero)     === (w + 1 == 0)
    (cpu' & p & negative) === (w + 1 < 0)

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
             & incZpgX addr
             & ldaZpgX addr
    (cpu' & rA)           === w + 1
    (cpu' & p & zero)     === (w + 1 == 0)
    (cpu' & p & negative) === (w + 1 < 0)

  it "abs" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    addr    <- forAll $ word16 (linear minBound memSize)
    let cpu' = cpu
             & ldaImm w
             & staAbs addr
             & incAbs addr
             & ldaAbs addr
    (cpu' & rA)           === w + 1
    (cpu' & p & zero)     === (w + 1 == 0)
    (cpu' & p & negative) === (w + 1 < 0)

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
             & incAbsX addr
             & ldaAbsX addr
    (cpu' & rA)           === w + 1
    (cpu' & p & zero)     === (w + 1 == 0)
    (cpu' & p & negative) === (w + 1 < 0)

  it "inx" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- genCPU memSize
    x       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldxImm x
             & inx
    (cpu' & rX)           === x + 1
    (cpu' & p & zero)     === (x + 1 == 0)
    (cpu' & p & negative) === (x + 1 < 0)

  it "iny" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- genCPU memSize
    y       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldyImm y
             & iny
    (cpu' & rY)           === y + 1
    (cpu' & p & zero)     === (y + 1 == 0)
    (cpu' & p & negative) === (y + 1 < 0)
