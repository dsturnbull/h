{-# LANGUAGE DataKinds #-}

module CPU.Instructions.PLSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.LDA
import CPU.Instructions.PH
import CPU.Instructions.PL
import CPU.Instructions.STA

import Control.Lens         hiding (ignored)
import Data.Bits.Lens
import Data.Vector.Storable hiding (break)
import Prelude              hiding (break)

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "pl" $ do
  it "pla" $ requireProperty $ do
    memSize <- forAll $ G.constant 512
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    w'      <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & ldaImm w & pha
             & ldaImm w'
             & pla
    (cpu' & mem) ! 0x01ff === w
    (cpu' & s) === fromIntegral (stack + fromIntegral page)
    (cpu' & rA) === w

  it "plp" $ requireProperty $ do
    memSize <- forAll $ G.constant 512
    cpu     <- forAll $ genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & pha -- push nothing, avoid stack underflow when pulling
             & ldaImm w & staAbs 0x1ff -- simulate pushing some real flags
             & plp -- restore the fake flags
    (cpu' & s) === fromIntegral (stack + fromIntegral page)
    (cpu' & p & negative)  === (w ^. bitAt 7)
    (cpu' & p & overflow)  === (w ^. bitAt 6)
    (cpu' & p & ignored)   === (w ^. bitAt 5)
    (cpu' & p & break)     === (w ^. bitAt 4)
    (cpu' & p & decimal)   === (w ^. bitAt 3)
    (cpu' & p & interrupt) === (w ^. bitAt 2)
    (cpu' & p & zero)      === (w ^. bitAt 1)
    (cpu' & p & carry)     === (w ^. bitAt 0)
