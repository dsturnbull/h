{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.TSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.LDA
import CPU.Instructions.LDX
import CPU.Instructions.LDY
import CPU.Instructions.T

import Control.Lens
-- import Data.Generics.Product.Fields
-- import Data.Vector.Storable

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "transfers" $ do
  it "tax" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w
             & tax
    (cpu' & rX) === w

  it "txa" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldxImm w
             & txa
    (cpu' & rA) === w

  it "tay" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w
             & tay
    (cpu' & rY) === w

  it "tya" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldyImm w
             & tya
    (cpu' & rA) === w

  it "tsx" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- forAll $ genCPU memSize
    s'      <- forAll $ word8 (linear 40 maxBound)
    let cpu' = cpu
             & ldxImm s'
             & txs
             & ldxImm 0
             & tsx
    (cpu' & rX) === s'

  it "txs" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- forAll $ genCPU memSize
    s'      <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldxImm s'
             & txs
    (cpu' & s) === s'
