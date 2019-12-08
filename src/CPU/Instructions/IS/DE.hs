{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.DE
  ( decZpg
  , decZpgX
  , decAbs
  , decAbsX
  , dex
  , dey
  ) where

import CPU

import Control.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable
import Data.Word

setZero :: Word8 -> CPU -> CPU
setZero w cpu = cpu & field @"p" . field @"zero" .~ (w == 0)

setNegative :: Word8 -> CPU -> CPU
setNegative w cpu = cpu & field @"p" . field @"negative" .~ (w < 0)

dec :: (CPU -> Word8) -> Word16 -> CPU -> CPU
dec o m cpu =
  cpu & setZero v
      & setNegative v
      & st addr v
  where
    r    = (cpu & mem) ! fromIntegral addr
    v    = r - 1
    addr = fromIntegral (m + fromIntegral (o cpu))

decZpg :: Word8 -> CPU -> CPU
decZpg = decAbs . fromIntegral

decZpgX :: Word8 -> CPU -> CPU
decZpgX = decAbsX . fromIntegral

decAbs :: Word16 -> CPU -> CPU
decAbs = dec (const 0)

decAbsX :: Word16 -> CPU -> CPU
decAbsX = dec rX

dex :: CPU -> CPU
dex cpu =
  cpu & setZero v
      & setNegative v
      & field @"rX" .~ v
  where v = r - 1
        r = cpu & rX

dey :: CPU -> CPU
dey cpu =
  cpu & setZero v
      & setNegative v
      & field @"rY" .~ v
  where v = r - 1
        r = cpu & rY
