{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.CPX
  ( cpxImm
  , cpxZpg
  , cpxAbs
  ) where

import CPU

import Control.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable
import Data.Word

cpx :: Word8 -> CPU -> CPU
cpx o2 cpu =
  cpu & field @"p" . field @"carry"    .~ c
      & field @"p" . field @"zero"     .~ z
      & field @"p" . field @"negative" .~ n
  where
    c  = o1 >= o2
    z  = o1 == o2
    n  = msb r
    r  = o1 - o2
    o1 = cpu & rX

cpxImm :: Word8 -> CPU -> CPU
cpxImm = cpx

cpxZpg :: Word8 -> CPU -> CPU
cpxZpg = cpxAbs . fromIntegral

cpxAbs :: Word16 -> CPU -> CPU
cpxAbs addr cpu = cpu & cpx m
  where m = (cpu & mem) ! fromIntegral addr
