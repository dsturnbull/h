{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.PHSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.IS.LDA
import CPU.Instructions.IS.PH

import Control.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "ph" $ do
  it "pha" $ requireProperty $ do
    memSize <- forAll $ G.constant 512
    cpu     <- genCPU memSize
    w       <- forAll $ word8 (linear minBound maxBound)
    let cpu' = cpu & ldaImm w & pha
    (cpu' & s) === fromIntegral (stack + fromIntegral page - 1)
    (cpu' & mem) ! fromIntegral (fromIntegral (cpu' & s) + stack + 1) === w

  it "php" $ requireProperty $ do
    memSize <- forAll $ G.constant 512
    cpu     <- genCPU memSize
    n       <- forAll bool
    v       <- forAll bool
    g       <- forAll bool
    b       <- forAll bool
    d       <- forAll bool
    i       <- forAll bool
    z       <- forAll bool
    c       <- forAll bool
    let cpu' = cpu
             & field @"p" . field @"negative"  .~ n
             & field @"p" . field @"overflow"  .~ v
             & field @"p" . field @"ignored"   .~ g
             & field @"p" . field @"break"     .~ b
             & field @"p" . field @"decimal"   .~ d
             & field @"p" . field @"interrupt" .~ i
             & field @"p" . field @"zero"      .~ z
             & field @"p" . field @"carry"     .~ c
             & php
    (cpu' & s) === fromIntegral (stack + fromIntegral page - 1)
    (cpu' & mem) ! fromIntegral (fromIntegral (cpu' & s) + stack + 1) === (cpu' & p & flagsToWord)
