{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Instructions.IS.RORSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.IS.LDA
import CPU.Instructions.IS.LDX
import CPU.Instructions.IS.ROR
import CPU.Instructions.IS.SE
import CPU.Instructions.IS.STA

import Control.Lens
import Data.Bits
import Data.Bits.Lens
import Foreign.Marshal.Utils (fromBool)

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

{-# ANN spec "HLint: ignore Reduce duplication" #-}

spec :: Spec
spec = describe "ror" $ do
  it "acc" $ requireProperty $ do
    cpu     <- genCPU 1
    w       <- forAll $ word8 (linear minBound maxBound)
    c       <- forAll bool
    let cpu' = cpu
             & (if c then sec else id)
             & ldaImm w
             & rorAcc
    let r = w `shiftR` 1 .|. ((c & fromBool) `shiftL` 7)
    (cpu' & rA)           === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === (r < 0)
    (cpu' & p & carry)    === (w ^. bitAt 0)

  it "zpg" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    addr    <- forAll $ word8 (linear minBound maxBound)
    w       <- forAll $ word8 (linear minBound maxBound)
    c       <- forAll bool
    let cpu' = cpu
             & (if c then sec else id)
             & ldaImm w
             & staZpg addr
             & rorZpg addr
             & ldaZpg addr
    let r = w `shiftR` 1 .|. ((c & fromBool) `shiftL` 7)
    (cpu' & rA)           === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === (r < 0)
    (cpu' & p & carry)    === (w ^. bitAt 0)

  it "zpg, x" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    x       <- forAll $ word8 (linear 10 20)
    addr    <- forAll $ word8 (linear minBound (maxBound - 20))
    w       <- forAll $ word8 (linear minBound maxBound)
    c       <- forAll bool
    let cpu' = cpu
             & (if c then sec else id)
             & ldxImm x
             & ldaImm w
             & staZpgX addr
             & rorZpgX addr
             & ldaZpgX addr
    let r = w `shiftR` 1 .|. ((c & fromBool) `shiftL` 7)
    (cpu' & rA)           === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === (r < 0)
    (cpu' & p & carry)    === (w ^. bitAt 0)

  it "abs" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    addr    <- forAll $ word16 (linear minBound (memSize - 1))
    w       <- forAll $ word8 (linear minBound maxBound)
    c       <- forAll bool
    let cpu' = cpu
             & (if c then sec else id)
             & ldaImm w
             & staAbs addr
             & rorAbs addr
             & ldaAbs addr
    let r = w `shiftR` 1 .|. ((c & fromBool) `shiftL` 7)
    (cpu' & rA)           === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === (r < 0)
    (cpu' & p & carry)    === (w ^. bitAt 0)

  it "abs, x" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize
    x       <- forAll $ word8 (linear 10 20)
    addr    <- forAll $ word16 (linear minBound (memSize - 20))
    w       <- forAll $ word8 (linear minBound maxBound)
    c       <- forAll bool
    let cpu' = cpu
             & (if c then sec else id)
             & ldxImm x
             & ldaImm w
             & staAbsX addr
             & rorAbsX addr
             & ldaAbsX addr
    let r = w `shiftR` 1 .|. ((c & fromBool) `shiftL` 7)
    (cpu' & rA)           === r
    (cpu' & p & zero)     === (r == 0)
    (cpu' & p & negative) === (r < 0)
    (cpu' & p & carry)    === (w ^. bitAt 0)
