{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.JSR
  ( jsr
  ) where

import CPU

import Control.Lens
import Data.Bits.Lens
import Data.Generics.Product.Fields
import Data.Word

jsr :: Word16 -> CPU -> CPU
jsr addr cpu =
  cpu & st s1 r1
      & st s2 r2
      & field @"s" %~ (flip (-) 2)
      & field @"pc" .~ addr
  where s1 = fromIntegral (cpu & s) + stack
        s2 = fromIntegral (cpu & s) + stack - 1
        r1 = ((cpu & pc) + 2) ^. byteAt 1
        r2 = ((cpu & pc) + 2) ^. byteAt 0
