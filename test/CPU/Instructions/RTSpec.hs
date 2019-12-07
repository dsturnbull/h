{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.RTSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.JSR
import CPU.Instructions.LDA
import CPU.Instructions.PH
import CPU.Instructions.RT

import Control.Lens                 hiding (ignored)
import Data.Bits
import Data.Bits.Lens
import Data.Generics.Product.Fields
import Prelude                      hiding (break)

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "returning" $ do
  it "rts" $ requireProperty $ do
    memSize <- forAll $ G.constant 0x200
    cpu     <- forAll $ genCPU memSize
    addr    <- forAll $ word16 (linear 0x0280 maxBound)
    pc'     <- forAll $ word16 (linear 0x0260 maxBound)
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu
             & field @"pc" .~ pc'
             & jsrAbs (fromIntegral addr)
             & ldaImm w
    (cpu' & rts & s)  === 0xff
    (cpu' & rts & pc) === pc' + 3

  it "rti" $ requireProperty $ do
    memSize <- forAll $ G.constant 0x200
    cpu     <- forAll $ genCPU memSize
    pc'     <- forAll $ word16 (linear 0x0260 maxBound)
    n       <- forAll (G.constant True)
    v       <- forAll bool
    g       <- forAll bool
    b       <- forAll bool
    d       <- forAll bool
    i       <- forAll bool
    z       <- forAll bool
    c       <- forAll bool

    let cpu0 = cpu
             & field @"p" . field @"negative"  .~ n
             & field @"p" . field @"overflow"  .~ v
             & field @"p" . field @"ignored"   .~ g
             & field @"p" . field @"break"     .~ b
             & field @"p" . field @"decimal"   .~ d
             & field @"p" . field @"interrupt" .~ i
             & field @"p" . field @"zero"      .~ z
             & field @"p" . field @"carry"     .~ c
             & field @"pc" .~ pc'

    let sr = cpu0 & p & flagsToWord

    let cpu1 = cpu0
             & ldaImm 44                              & pha
             & ldaImm (fromIntegral (pc' `shiftR` 8)) & pha
             & ldaImm (fromIntegral (pc' .&. 0x00ff)) & pha
             & ldaImm sr                              & pha

    annotateShow cpu1
    (cpu1 & s) === 0xff - 4

    let cpu' = cpu1 & rti

    annotateShow cpu'
    (cpu' & s) === 0xff - 1
    (cpu' & pc) === pc'
    (cpu' & p & negative)  === (sr ^. bitAt 7)
    (cpu' & p & overflow)  === (sr ^. bitAt 6)
    (cpu' & p & ignored)   === (sr ^. bitAt 5)
    (cpu' & p & break)     === (sr ^. bitAt 4)
    (cpu' & p & decimal)   === (sr ^. bitAt 3)
    (cpu' & p & interrupt) === (sr ^. bitAt 2)
    (cpu' & p & zero)      === (sr ^. bitAt 1)
    (cpu' & p & carry)     === (sr ^. bitAt 0)
