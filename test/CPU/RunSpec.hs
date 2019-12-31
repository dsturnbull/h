{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.RunSpec
  ( spec
  ) where

import ASM.Assembler
import ASM.Length
import CPU
import CPU.Gen
import CPU.Instructions.Gen
import CPU.Run

import Control.Lens
import Control.Monad.IO.Class
import Prelude                    hiding (break)
import Text.InterpolatedString.QM

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as G
import Hedgehog.Range              as R
import Test.Hspec

spec :: Spec
spec = describe "cpu runner" $ do
  it "assembles and loads a program" $ requireTest $ do
    memSize <- forAll $ G.constant 256
    cpu     <- genCPU memSize

    let code = [qnb| lda #$20 |]

    prog <- liftIO $ assemble code 0x0000 0x0000
    let cpu' = load prog cpu
    (step cpu' & rA) === 0x20
    (step cpu' & pc) === 0x2

  it "executes instructions" $ requireProperty $ do
    memSize <- forAll $ G.constant 512
    cpu     <- genCPU memSize
    code    <- forAll $ genCodeBreaking (R.linear 1 2) genInstruction
    prog    <- forAll $ genProg code

    let cpu' = load prog cpu
    (run cpu' & pc) === fromIntegral (sum (insLength <$> code) + 2) -- +2 for brk

run :: CPU -> CPU
run cpu = do
  let cpu' = step cpu
  if cpu' & p & break
    then cpu'
    else run cpu'
