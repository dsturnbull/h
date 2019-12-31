{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.ORA
  ( oraImm
  , oraZpg
  , oraZpgX
  , oraAbs
  , oraAbsX
  , oraAbsY
  , oraIndX
  , oraIndY
  ) where

import CPU

import Control.Lens
import Data.Bits
import Data.Bits.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable
import Data.Word

ora' :: Word8 -> CPU -> CPU
ora' m cpu =
  cpu & field @"rA" .~ v & zeroable rA & negable rA
      & field @"p" . field @"negative" .~ v ^. bitAt 7
      & field @"p" . field @"zero"     .~ (v == 0)
  where a = cpu & rA
        v = a .|. m

oraImm :: Word8 -> CPU -> CPU
oraImm = ora'

ora_ :: (CPU -> Word8) -> Word16 -> CPU -> CPU
ora_ f addr cpu = cpu & ora' v
  where v = (cpu & mem) ! fromIntegral (addr + fromIntegral (f cpu))

oraZpg :: Word8 -> CPU -> CPU
oraZpg = ora_ (const 0) . fromIntegral

oraZpgX :: Word8 -> CPU -> CPU
oraZpgX = ora_ rX . fromIntegral

oraAbs :: Word16 -> CPU -> CPU
oraAbs = ora_ (const 0)

oraAbsX :: Word16 -> CPU -> CPU
oraAbsX = ora_ rX

oraAbsY :: Word16 -> CPU -> CPU
oraAbsY = ora_ rY

ora_' :: (CPU -> Word8) -> (CPU -> Word8) -> Word8 -> CPU -> CPU
ora_' f g ind cpu = cpu & ora' v
  where v    = (cpu & mem) ! fromIntegral (addr + fromIntegral (g cpu))
        addr = cpu & indirectZpg ind (f cpu)

oraIndX :: Word8 -> CPU -> CPU
oraIndX = ora_' rX (const 0)

oraIndY :: Word8 -> CPU -> CPU
oraIndY = ora_' (const 0) rY
