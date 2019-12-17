{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.ROL
  ( rolAcc
  , rolZpg
  , rolZpgX
  , rolAbs
  , rolAbsX
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

rolAcc :: CPU -> CPU
rolAcc cpu =
  cpu & setZero v
      & setNegative v
      & field @"rA" .~ v
      & setCarry hi
  where a = cpu & rA
        hi = a ^. bitAt 7
        lo = cpu & p & carry & fromBool
        v = a `shiftL` 1 .|. lo

rolZpg :: Word8 -> CPU -> CPU
rolZpg = rolAbs . fromIntegral

rolZpgX :: Word8 -> CPU -> CPU
rolZpgX = rolAbsX . fromIntegral

rol_ :: (CPU -> Word8) -> Word16 -> CPU -> CPU
rol_ f addr cpu =
  cpu & setZero v
      & setNegative v
      & setCarry hi
      & st m v
  where r  = (cpu & mem) ! fromIntegral m
        hi = r ^. bitAt 7
        lo = cpu & p & carry & fromBool
        v  = r `shiftL` 1 .|. lo
        m  = fromIntegral (addr + fromIntegral (f cpu))

rolAbs :: Word16 -> CPU -> CPU
rolAbs = rol_ (const 0)

rolAbsX :: Word16 -> CPU -> CPU
rolAbsX = rol_ rX
