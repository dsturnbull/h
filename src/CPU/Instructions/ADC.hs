{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.ADC
  ( adcImm
  , adcZpg
  , adcZpgX
  , adcAbs
  , adcAbsX
  , adcAbsY
  , adcIndX
  , adcIndY
  ) where

import CPU

import Control.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable
import Data.Word
import Foreign.Marshal.Utils        (fromBool)

setCarry :: Word8 -> Word8 -> CPU -> CPU
setCarry o1 o2 cpu = cpu & field @"p" . field @"carry" .~ ((fromIntegral o1 :: Int) + (fromIntegral o2 :: Int) > fromIntegral (maxBound :: Word8))

setZero :: Word8 -> CPU -> CPU
setZero v cpu = cpu & field @"p" . field @"zero" .~ (v == 0)

setOverflow :: Word8 -> CPU -> CPU
setOverflow = undefined

adc :: Word8 -> CPU -> CPU
adc o2 cpu =
  cpu & setCarry o1 o2 & setZero v
      & field @"rA" .~ v
  where
    o1 = cpu & rA
    v  = o1 + o2 + c'
    c' = cpu & p & carry & fromBool

adcImm :: Word8 -> CPU -> CPU
adcImm = adc

adcZpg :: Word8 -> CPU -> CPU
adcZpg = adcAbs . fromIntegral

adcZpgX :: Word8 -> CPU -> CPU
adcZpgX = adcAbsX . fromIntegral

adcAbs_ :: (CPU -> Word8) -> Word16 -> CPU -> CPU
adcAbs_ f addr cpu = cpu & adc v
  where v = (cpu & mem) ! fromIntegral (addr + fromIntegral (f cpu))

adcAbs :: Word16 -> CPU -> CPU
adcAbs = adcAbs_ (const 0)

adcAbsX :: Word16 -> CPU -> CPU
adcAbsX = adcAbs_ rX

adcAbsY :: Word16 -> CPU -> CPU
adcAbsY = adcAbs_ rY

adcInd_ :: (CPU -> Word8) -> (CPU -> Word8) -> Word8 -> CPU -> CPU
adcInd_ f g ind cpu = cpu & adc v
  where v = (cpu & mem) ! (addr + fromIntegral (g cpu))
        addr = cpu & indirectZpg ind (f cpu) & fromIntegral

adcIndX :: Word8 -> CPU -> CPU
adcIndX = adcInd_ rX (const 0)

adcIndY :: Word8 -> CPU -> CPU
adcIndY = adcInd_ (const 0) rY
