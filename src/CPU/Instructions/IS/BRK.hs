{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.BRK
  ( brk
  ) where

import CPU

import Control.Lens
import Data.Bits
import Data.Generics.Product.Fields

brk :: CPU -> CPU
brk cpu =
  cpu & field @"p" . field @"break"     .~ True
      & field @"p" . field @"interrupt" .~ True
      & field @"s" %~ flip (-) 3
      & field @"pc" %~ flip (+) 2
      & st s0 addrH
      & st s1 addrL
      & st s2 sr
  where s0    = ((cpu & s)     & fromIntegral) + stack
        s1    = ((cpu & s) - 1 & fromIntegral) + stack
        s2    = ((cpu & s) - 2 & fromIntegral) + stack
        addrL = fromIntegral (ret .&. 0x00ff)
        addrH = fromIntegral (ret `shiftR` 8)
        sr    = cpu & p & flagsToWord
        ret   = (cpu & pc) + 2
