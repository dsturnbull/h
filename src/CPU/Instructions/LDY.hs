{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.LDY
  ( ldyImm
  , ldyZpg
  , ldyZpgX
  , ldyAbs
  , ldyAbsX
  ) where

import CPU

import Control.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable
import Data.Word

setZero :: Word8 -> CPU -> CPU
setZero v cpu = cpu & field @"p" . field @"zero" .~ (v == 0)

ldy :: Word8 -> CPU -> CPU
ldy v cpu = cpu & field @"rY" .~ v & setZero v

ldyImm :: Word8 -> CPU -> CPU
ldyImm = ldy

ldy_ :: (CPU -> Word8) -> Word16 -> CPU -> CPU
ldy_ f addr cpu = cpu & ldy v
  where v = (cpu & mem) ! fromIntegral (addr + fromIntegral (f cpu))

ldyZpg :: Word8 -> CPU -> CPU
ldyZpg = ldy_ (const 0) . fromIntegral

ldyZpgX :: Word8 -> CPU -> CPU
ldyZpgX = ldy_ rX . fromIntegral

ldyAbs :: Word16 -> CPU -> CPU
ldyAbs = ldy_ (const 0)

ldyAbsX :: Word16 -> CPU -> CPU
ldyAbsX = ldy_ rX
