{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ASM.AssemblerSpec
  ( spec
  ) where

import ASM.Assembler
import ASM.Program

import Control.Monad.IO.Class
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec
import Text.InterpolatedString.QM

import qualified Data.Text            as T
import qualified Data.Vector.Storable as DVS

spec :: Spec
spec = describe "run" $
  it "assembles lda instructions" $ requireTest $ do
    let code :: T.Text =
          [qnb|lda #$20
               lda $80
               lda $80,X
               lda $2000
               lda $2000,X
               lda $2000,Y
          |]

    let prog = Program
                 (0x0000, DVS.fromList [0xa9, 0x20, 0xa5, 0x80, 0xb5, 0x80, 0xad, 0x00, 0x20, 0xbd, 0x00, 0x20, 0xb9, 0x00, 0x20])
                 (0x0000, DVS.empty)
                 []
    r <- liftIO $ assemble code 0x0000 0x0000
    r === prog
