{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.STX
  ( stxZpg
  , stxZpgY
  , stxAbs
  ) where

import CPU

import Control.Lens
import Data.Word

stx :: (CPU -> Word8) -> Word16 -> CPU -> CPU
stx f addr cpu = cpu & st addr' v
  where addr' = fromIntegral (addr + fromIntegral (f cpu))
        v     = cpu & rX

stxZpg :: Word8 -> CPU -> CPU
stxZpg = stxAbs . fromIntegral

stxZpgY :: Word8 -> CPU -> CPU
stxZpgY = stx rY . fromIntegral

stxAbs :: Word16 -> CPU -> CPU
stxAbs = stx (const 0)
