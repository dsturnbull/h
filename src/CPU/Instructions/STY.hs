{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.STY
  ( styZpg
  , styZpgX
  , styAbs
  ) where

import CPU

import Control.Lens
import Data.Word

sty :: (CPU -> Word8) -> Word16 -> CPU -> CPU
sty f addr cpu = cpu & st addr' v
  where addr' = fromIntegral (addr + fromIntegral (f cpu))
        v     = cpu & rY

styZpg :: Word8 -> CPU -> CPU
styZpg = styAbs . fromIntegral

styZpgX :: Word8 -> CPU -> CPU
styZpgX = sty rX . fromIntegral

styAbs :: Word16 -> CPU -> CPU
styAbs = sty (const 0)
