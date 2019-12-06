{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.CLSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.CL

import Control.Lens
import Data.Generics.Product.Fields

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Test.Hspec

spec :: Spec
spec = describe "clear" $ do
  it "clc" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    c       <- forAll $ bool
    let cpu' = cpu
            & field @"p" . field @"carry" .~ c
            & clc
    (cpu' & p & carry) === False

  it "cld" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    d       <- forAll $ bool
    let cpu' = cpu
            & field @"p" . field @"carry" .~ d
            & cld
    (cpu' & p & decimal) === False

  it "cli" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    i       <- forAll $ bool
    let cpu' = cpu
            & field @"p" . field @"carry" .~ i
            & cli
    (cpu' & p & interrupt) === False

  it "clv" $ requireProperty $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPU memSize
    i       <- forAll $ bool
    let cpu' = cpu
            & field @"p" . field @"carry" .~ i
            & clv
    (cpu' & p & overflow) === False
