{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.AND
  ( andImm
  , andZpg
  , andZpgX
  , andAbs
  , andAbsX
  , andAbsY
  , andIndX
  , andIndY
  ) where

import CPU

import Control.Lens
import Data.Bits
import Data.Bits.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable
import Data.Word

and' :: Word8 -> CPU -> CPU
and' m cpu =
  cpu & field @"rA" .~ v & zeroable rA & negable rA
      & field @"p" . field @"negative" .~ v ^. bitAt 7
      & field @"p" . field @"zero"     .~ (v == 0)
  where v = a .&. m
        a = cpu & rA

andImm :: Word8 -> CPU -> CPU
andImm = and'

andZpg :: Word8 -> CPU -> CPU
andZpg = andAbs . fromIntegral

andZpgX :: Word8 -> CPU -> CPU
andZpgX = andAbsX . fromIntegral

andAbs_ :: (CPU -> Word8) -> Word16 -> CPU -> CPU
andAbs_ f addr cpu = cpu & and' m
  where m = (cpu & mem) ! fromIntegral (addr + fromIntegral (f cpu))

andAbs :: Word16 -> CPU -> CPU
andAbs = andAbs_ (const 0)

andAbsX :: Word16 -> CPU -> CPU
andAbsX = andAbs_ rX

andAbsY :: Word16 -> CPU -> CPU
andAbsY = andAbs_ rY

andInd_ :: (CPU -> Word8) -> (CPU -> Word8) -> Word8 -> CPU -> CPU
andInd_ f g ind cpu = cpu & and' m
  where m    = (cpu & mem) ! (addr + fromIntegral (g cpu))
        addr = cpu & indirectZpg ind (f cpu) & fromIntegral

andIndX :: Word8 -> CPU -> CPU
andIndX = andInd_ rX (const 0)

andIndY :: Word8 -> CPU -> CPU
andIndY = andInd_ (const 0) rY
