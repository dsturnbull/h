{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.SESpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.SE

import Control.Lens
import Data.Generics.Product.Fields

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Test.Hspec

spec :: Spec
spec = describe "set" $ do
  it "sec" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    c       <- forAll $ bool
    let cpu' = cpu
            & field @"p" . field @"carry" .~ c
            & sec
    (cpu' & p & carry) === True

  it "sed" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    d       <- forAll $ bool
    let cpu' = cpu
            & field @"p" . field @"carry" .~ d
            & sed
    (cpu' & p & decimal) === True

  it "sei" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    i       <- forAll $ bool
    let cpu' = cpu
            & field @"p" . field @"carry" .~ i
            & sei
    (cpu' & p & interrupt) === True
