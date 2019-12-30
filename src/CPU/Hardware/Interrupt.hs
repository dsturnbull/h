{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CPU.Hardware.Interrupt
  ( intr
  , stop
  , continue
  ) where

import CPU
import CPU.Instructions.Impl
import Data.Generics.Product.Fields

import Control.Lens
import Data.Bits

intr :: CPU -> CPU
intr cpu =
  cpu & push (fromIntegral (pc' `shiftR` 8))
      & push (fromIntegral (pc' .&. 0x00ff))
      & push sr
      & jmpInd irqV
      & sei
  where
    pc' = cpu & pc
    sr  = cpu & p & flagsToWord

stop :: CPU -> CPU
stop cpu = cpu & field @"p" . field @"break"     .~ True
               & field @"p" . field @"interrupt" .~ True

continue :: CPU -> CPU
continue cpu =
  cpu & field @"p" . field @"break" .~ False
      & field @"p" . field @"interrupt" .~ False
