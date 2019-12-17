{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.BSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.B

import Control.Lens
import Data.Generics.Product.Fields

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "branch" $ do
  it "bcc" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- forAll $ genCPU memSize
    carry'  <- forAll bool_
    rel     <- forAll $ int8 (linear (-4) 4)
    pc'     <- forAll $ word16 (linear 10 maxBound)
    let cpu' = cpu
             & field @"pc" .~ pc'
             & field @"p" . field @"carry" .~ carry'
             & bcc rel
    (cpu' & pc) === fromIntegral ((fromIntegral pc' :: Int) + if carry' then 0 else fromIntegral rel)

  it "bcs" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- forAll $ genCPU memSize
    carry'  <- forAll bool
    rel     <- forAll $ int8 (linear (-4) 4)
    pc'     <- forAll $ word16 (linear 10 maxBound)
    let cpu' = cpu
             & field @"pc" .~ pc'
             & field @"p" . field @"carry" .~ carry'
             & bcs rel
    (cpu' & pc) === fromIntegral ((fromIntegral pc' :: Int) + if carry' then fromIntegral rel else 0)

  it "beq" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- forAll $ genCPU memSize
    zero'   <- forAll bool
    rel     <- forAll $ int8 (linear (-4) 4)
    pc'     <- forAll $ word16 (linear 10 maxBound)
    let cpu' = cpu
             & field @"pc" .~ pc'
             & field @"p" . field @"zero" .~ zero'
             & beq rel
    (cpu' & pc) === fromIntegral ((fromIntegral pc' :: Int) + if zero' then fromIntegral rel else 0)

  it "bmi" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- forAll $ genCPU memSize
    neg'    <- forAll bool
    rel     <- forAll $ int8 (linear (-4) 4)
    pc'     <- forAll $ word16 (linear 10 maxBound)
    let cpu' = cpu
             & field @"pc" .~ pc'
             & field @"p" . field @"negative" .~ neg'
             & bmi rel
    (cpu' & pc) === fromIntegral ((fromIntegral pc' :: Int) + if neg' then fromIntegral rel else 0)

  it "bne" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- forAll $ genCPU memSize
    zero'   <- forAll bool
    rel     <- forAll $ int8 (linear (-4) 4)
    pc'     <- forAll $ word16 (linear 10 maxBound)
    let cpu' = cpu
             & field @"pc" .~ pc'
             & field @"p" . field @"zero" .~ zero'
             & bne rel
    (cpu' & pc) === fromIntegral ((fromIntegral pc' :: Int) + if zero' then 0 else fromIntegral rel)

  it "bpl" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- forAll $ genCPU memSize
    neg'    <- forAll bool
    rel     <- forAll $ int8 (linear (-4) 4)
    pc'     <- forAll $ word16 (linear 10 maxBound)
    let cpu' = cpu
             & field @"pc" .~ pc'
             & field @"p" . field @"negative" .~ neg'
             & bpl rel
    (cpu' & pc) === fromIntegral ((fromIntegral pc' :: Int) + if neg' then 0 else fromIntegral rel )

  it "bvc" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- forAll $ genCPU memSize
    over'   <- forAll bool
    rel     <- forAll $ int8 (linear (-4) 4)
    pc'     <- forAll $ word16 (linear 10 maxBound)
    let cpu' = cpu
             & field @"pc" .~ pc'
             & field @"p" . field @"overflow" .~ over'
             & bvc rel
    (cpu' & pc) === fromIntegral ((fromIntegral pc' :: Int) + if over' then 0 else fromIntegral rel)

  it "bvs" $ requireProperty $ do
    memSize <- forAll $ G.constant 1
    cpu     <- forAll $ genCPU memSize
    over'   <- forAll bool
    rel     <- forAll $ int8 (linear (-4) 4)
    pc'     <- forAll $ word16 (linear 10 maxBound)
    let cpu' = cpu
             & field @"pc" .~ pc'
             & field @"p" . field @"overflow" .~ over'
             & bvs rel
    (cpu' & pc) === fromIntegral ((fromIntegral pc' :: Int) + if over' then fromIntegral rel else 0)
