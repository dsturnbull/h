{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.JSRSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.JSR

import Control.Lens
import Data.Bits.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "jsr" $
  it "abs" $ requireProperty $ do
    memSize <- forAll $ G.constant 512
    cpu     <- forAll $ genCPU memSize
    addr    <- forAll $ word16 (linear minBound maxBound)
    pc'     <- forAll $ word16 (linear 260 maxBound)
    let cpu' = cpu
             & field @"pc" .~ pc'
             & jsrAbs (fromIntegral addr)
    (cpu' & s) === 0xfd
    (cpu' & mem) ! (fromIntegral (cpu' & s) + fromIntegral stack + 1) === fromIntegral ((pc' + 2) ^. byteAt 0)
    (cpu' & mem) ! (fromIntegral (cpu' & s) + fromIntegral stack + 2) === fromIntegral ((pc' + 2) ^. byteAt 1)
    (cpu' & pc) === addr
