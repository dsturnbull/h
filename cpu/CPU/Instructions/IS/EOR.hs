{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.EOR
  ( eorImm
  , eorZpg
  , eorZpgX
  , eorAbs
  , eorAbsX
  , eorAbsY
  , eorIndX
  , eorIndY
  ) where

import CPU

import Control.Lens
import Data.Bits
import Data.Generics.Product.Fields
import Data.Vector.Storable
import Data.Word

eor :: Word8 -> CPU -> CPU
eor o2 cpu =
  cpu & field @"p" . field @"zero"     .~ z
      & field @"p" . field @"negative" .~ n
      & field @"rA" .~ r
  where
    z  = r == 0
    n  = msb r
    r  = o1 .|. o2
    o1 = cpu & rA

eorImm :: Word8 -> CPU -> CPU
eorImm = eor

eorZpg :: Word8 -> CPU -> CPU
eorZpg = eorAbs . fromIntegral

eorZpgX :: Word8 -> CPU -> CPU
eorZpgX = eorAbsX . fromIntegral

eorAbs_ :: (CPU -> Word8) -> Word16 -> CPU -> CPU
eorAbs_ f addr cpu = cpu & eor m
  where m = (cpu & mem) ! fromIntegral (addr + fromIntegral (f cpu))

eorAbs :: Word16 -> CPU -> CPU
eorAbs = eorAbs_ (const 0)

eorAbsX :: Word16 -> CPU -> CPU
eorAbsX = eorAbs_ rX

eorAbsY :: Word16 -> CPU -> CPU
eorAbsY = eorAbs_ rY

eorInd_ :: (CPU -> Word8) -> (CPU -> Word8) -> Word8 -> CPU -> CPU
eorInd_ f g ind cpu = cpu & eor m
  where m    = (cpu & mem) ! (addr + fromIntegral (g cpu))
        addr = cpu & indirectZpg ind (f cpu) & fromIntegral

eorIndX :: Word8 -> CPU -> CPU
eorIndX = eorInd_ rX (const 0)

eorIndY :: Word8 -> CPU -> CPU
eorIndY = eorInd_ (const 0) rY
