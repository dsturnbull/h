{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.ROR
  ( rorAcc
  , rorZpg
  , rorZpgX
  , rorAbs
  , rorAbsX
  ) where

import CPU

import Control.Lens
import Data.Bits
import Data.Bits.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable
import Data.Word
import Foreign.Marshal.Utils        (fromBool)

setCarry :: Bool -> CPU -> CPU
setCarry c cpu = cpu & field @"p" . field @"carry" .~ c

setZero :: Word8 -> CPU -> CPU
setZero w cpu = cpu & field @"p" . field @"zero" .~ (w == 0)

setNegative :: Word8 -> CPU -> CPU
setNegative w cpu = cpu & field @"p" . field @"negative" .~ (w < 0)

rorAcc :: CPU -> CPU
rorAcc cpu =
  cpu & setZero v
      & setNegative v
      & field @"rA" .~ v
      & setCarry h
  where a = cpu & rA
        h = a ^. bitAt 0
        l = cpu & p & carry & fromBool
        v = a `shiftR` 1 .|. (l `shiftL` 7)

rorZpg :: Word8 -> CPU -> CPU
rorZpg = rorAbs . fromIntegral

rorZpgX :: Word8 -> CPU -> CPU
rorZpgX = rorAbsX . fromIntegral

ror_ :: (CPU -> Word8) -> Word16 -> CPU -> CPU
ror_ f addr cpu =
  cpu & setZero v
      & setNegative v
      & setCarry h
      & st m v
  where r = (cpu & mem) ! fromIntegral m
        h = r ^. bitAt 0
        l = cpu & p & carry & fromBool
        v = r `shiftR` 1 .|. (l `shiftL` 7)
        m = fromIntegral (addr + fromIntegral (f cpu))

rorAbs :: Word16 -> CPU -> CPU
rorAbs = ror_ (const 0)

rorAbsX :: Word16 -> CPU -> CPU
rorAbsX = ror_ rX
