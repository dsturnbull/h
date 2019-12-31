{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.LDX
  ( ldxImm
  , ldxZpg
  , ldxZpgY
  , ldxAbs
  , ldxAbsY
  ) where

import CPU

import Control.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable
import Data.Word

setZero :: Word8 -> CPU -> CPU
setZero v cpu = cpu & field @"p" . field @"zero" .~ (v == 0)

ldx :: Word8 -> CPU -> CPU
ldx v cpu = cpu & field @"rX" .~ v & setZero v

ldxImm :: Word8 -> CPU -> CPU
ldxImm = ldx

ldx_ :: (CPU -> Word8) -> Word16 -> CPU -> CPU
ldx_ f addr cpu = cpu & ldx v
  where v = (cpu & mem) ! fromIntegral (addr + fromIntegral (f cpu))

ldxZpg :: Word8 -> CPU -> CPU
ldxZpg = ldx_ (const 0) . fromIntegral

ldxZpgY :: Word8 -> CPU -> CPU
ldxZpgY = ldx_ rY . fromIntegral

ldxAbs :: Word16 -> CPU -> CPU
ldxAbs = ldx_ (const 0)

ldxAbsY :: Word16 -> CPU -> CPU
ldxAbsY = ldx_ rY
