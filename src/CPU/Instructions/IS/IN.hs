{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.IN
  ( incZpg
  , incZpgX
  , incAbs
  , incAbsX
  , inx
  , iny
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

inc :: (CPU -> Word8) -> Word16 -> CPU -> CPU
inc o m cpu =
  cpu & setZero v
      & setNegative v
      & st addr v
  where
    r    = (cpu & mem) ! fromIntegral addr
    v    = r + 1
    addr = fromIntegral (m + fromIntegral (o cpu))

incZpg :: Word8 -> CPU -> CPU
incZpg = incAbs . fromIntegral

incZpgX :: Word8 -> CPU -> CPU
incZpgX = incAbsX . fromIntegral

incAbs :: Word16 -> CPU -> CPU
incAbs = inc (const 0)

incAbsX :: Word16 -> CPU -> CPU
incAbsX = inc rX

inx :: CPU -> CPU
inx cpu =
  cpu & setZero v
      & setNegative v
      & field @"rX" .~ v
  where v = r + 1
        r = cpu & rX

iny :: CPU -> CPU
iny cpu =
  cpu & setZero v
      & setNegative v
      & field @"rY" .~ v
  where v = r + 1
        r = cpu & rY
