{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Hardware.Interrupt
  ( intr
  ) where

import CPU
import CPU.Instructions.Impl

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
