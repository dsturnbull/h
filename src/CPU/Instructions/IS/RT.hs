{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.RT
  ( rts
  , rti
  ) where

import CPU

import Control.Lens
import Data.Bits
import Data.Generics.Product.Fields
import Data.Vector.Storable

rts :: CPU -> CPU
rts cpu =
  cpu & field @"s" %~ (flip (+) 2)
      & field @"pc" .~ addr + 1
  where addrL = (cpu & mem) ! fromIntegral s1
        addrH = (cpu & mem) ! fromIntegral s2
        s1    = fromIntegral (cpu & s) + stack + 1
        s2    = fromIntegral (cpu & s) + stack + 2
        addr  = (fromIntegral addrH `shiftL` 8) .|. fromIntegral addrL

rti :: CPU -> CPU
rti cpu =
  cpu & field @"s" %~ (flip (+) 3)
      & field @"pc" .~ addr
      & wordToFlags sr
  where sr    = (cpu & mem) ! fromIntegral s1
        addrL = (cpu & mem) ! fromIntegral s2
        addrH = (cpu & mem) ! fromIntegral s3
        s1    = fromIntegral (cpu & s) + stack + 1
        s2    = fromIntegral (cpu & s) + stack + 2
        s3    = fromIntegral (cpu & s) + stack + 3
        addr  = (fromIntegral addrH `shiftL` 8) .|. fromIntegral addrL
