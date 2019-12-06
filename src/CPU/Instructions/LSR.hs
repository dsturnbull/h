{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.LSR
  ( lsrAcc
  , lsrZpg
  , lsrZpgX
  , lsrAbs
  , lsrAbsX
  ) where

import CPU

import Control.Lens
import Data.Bits
import Data.Bits.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable
import Data.Word

setCarry :: Word8 -> CPU -> CPU
setCarry w cpu = cpu & field @"p" . field @"carry" .~ c
  where c = w ^. bitAt 0

setZero :: Word8 -> CPU -> CPU
setZero w cpu = cpu & field @"p" . field @"zero" .~ (w == 0)

setNegative :: CPU -> CPU
setNegative cpu = cpu & field @"p" . field @"negative" .~ False

lsr :: (CPU -> Word8) -> CPU -> CPU
lsr f cpu =
  cpu & setZero v
      & setNegative
      & setCarry m
      & field @"rA" .~ v
  where m = f cpu
        v = m `shiftR` 1

lsrAcc :: CPU -> CPU
lsrAcc = lsr rA

lsrZpg :: Word8 -> CPU -> CPU
lsrZpg = lsrAbs . fromIntegral

lsrZpgX :: Word8 -> CPU -> CPU
lsrZpgX = lsrAbsX . fromIntegral

lsr_ :: (CPU -> Word8) -> Word16 -> CPU -> CPU
lsr_ f addr cpu =
  cpu & setZero v
      & setNegative
      & setCarry r
      & st (fromIntegral addr) v
  where r = (cpu & mem) ! fromIntegral (addr + fromIntegral (f cpu))
        v = r `shiftR` 1

lsrAbs :: Word16 -> CPU -> CPU
lsrAbs = lsr_ (const 0)

lsrAbsX :: Word16 -> CPU -> CPU
lsrAbsX = lsr_ rX
