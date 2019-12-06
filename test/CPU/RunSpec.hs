{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.RunSpec
  ( spec
  ) where

import ASM.Assembler
import CPU
import CPU.Gen
import CPU.Instructions.Gen
import CPU.Program
import CPU.Run

import Control.Lens
import Text.InterpolatedString.QM

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = fdescribe "cpu runner" $ do
  it "assembles and loads a program" $ requireTest $ do
    memSize <- forAll $ G.constant 256
    cpu     <- forAll $ genCPURandom memSize

    let code =
          [qnb|
            lda #$20
          |]

    let cpu' = load 0 (assemble code) cpu
    ((step cpu') & rA) === 0x20
    ((step cpu') & pc) === 0x2

  it "executes instructions" $ requireProperty $ do
    memSize <- forAll $ G.constant 512
    cpu     <- forAll $ genCPURandom memSize
    code    <- forAll $ genCodeBreaking (R.linear 1 2) genInstruction
    prog    <- forAll $ genProg code

    let cpu' = load 0x0000 prog cpu
    annotateShow prog
    annotateShow cpu'
    ((run cpu') & pc) === (fromIntegral $ sum (insLength <$> code) + 1)
