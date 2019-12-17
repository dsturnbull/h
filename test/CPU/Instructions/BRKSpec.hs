{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CPU.Instructions.BRKSpec
  ( spec
  ) where

import CPU
import CPU.Gen
import CPU.Instructions.BRK
import CPU.Instructions.JMP
import CPU.Instructions.NOP

import Control.Lens
import Data.Bits
import Data.Generics.Product.Fields
import Data.Vector.Storable         hiding (break, reverse, sum, zipWith)
import Prelude                      hiding (break)

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "brk" $
  it "breaks" $ requireProperty $ do
    memSize <- forAll $ G.constant 512
    cpu     <- forAll $ genCPU memSize
    addr    <- forAll $ word16 (linear minBound memSize)
    n       <- forAll bool
    v       <- forAll bool
    g       <- forAll bool
    b       <- forAll $ G.constant False
    d       <- forAll bool
    i       <- forAll $ G.constant False
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
             & jmpAbs addr
             & brk
             & nop
    annotateShow cpu'

    (cpu' & p & break)     === True
    (cpu' & p & interrupt) === True
    (cpu' & pc)            === (addr + 1)
    (cpu' & s)             === (maxBound - 3)

    let sr   = (cpu' & mem) ! (fromIntegral (cpu' & s) + fromIntegral stack + 1)
    let retL = (cpu' & mem) ! (fromIntegral (cpu' & s) + fromIntegral stack + 2)
    let retH = (cpu' & mem) ! (fromIntegral (cpu' & s) + fromIntegral stack + 3)
    annotateShow retH
    annotateShow retL
    annotateShow sr
    retL === fromIntegral ((addr + 2) .&. 0x00ff)
    retH === fromIntegral ((addr + 2) `shiftR` 8)

    let nonBreakingFlags =
          cpu' & field @"p" . field @"interrupt" .~ False
               & field @"p" . field @"break"     .~ False
               & p & flagsToWord
    sr === nonBreakingFlags
