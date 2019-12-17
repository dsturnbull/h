{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CPU.Instructions.IS.ASL
  ( aslAcc
  , aslZpg
  , aslZpgX
  , aslAbs
  , aslAbsX
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
  where c = w ^. bitAt 7

setZero :: Word8 -> CPU -> CPU
setZero w cpu = cpu & field @"p" . field @"zero" .~ (w == 0)

setNegative :: Word8 -> CPU -> CPU
setNegative w cpu = cpu & field @"p" . field @"negative" .~ (w < 0)

asl :: (Word8 -> CPU -> CPU) -> (CPU -> Word8) -> CPU -> CPU
asl w m cpu =
  cpu & setZero v
      & setNegative v
      & setCarry (m cpu)
      & w v
  where v = (cpu & m) `shiftL` 1

aslM :: (Word8 -> CPU -> CPU) -> (Vector Word8 -> Word8) -> CPU -> CPU
aslM w m cpu =
  cpu & setZero r
      & setNegative r
      & setCarry v
      & w r
  where v = cpu & mem & m
        r = (cpu & mem & m) `shiftL` 1

aslAcc :: CPU -> CPU
aslAcc = asl (field @"rA" .~) rA

aslZpg :: Word8 -> CPU -> CPU
aslZpg = aslAbs . fromIntegral

aslZpgX :: Word8 -> CPU -> CPU
aslZpgX = aslAbsX . fromIntegral

aslAbs :: Word16 -> CPU -> CPU
aslAbs addr = aslM (st addr) (! fromIntegral addr)

aslAbsX :: Word16 -> CPU -> CPU
aslAbsX addr cpu = cpu & aslM (st addr) (! fromIntegral (addr + fromIntegral x))
  where x = cpu & rX
