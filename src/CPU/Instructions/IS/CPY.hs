{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.CPY
  ( cpyImm
  , cpyZpg
  , cpyAbs
  ) where

import CPU

import Control.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable
import Data.Word

cpy :: Word8 -> CPU -> CPU
cpy o2 cpu =
  cpu & field @"p" . field @"carry"    .~ c
      & field @"p" . field @"zero"     .~ z
      & field @"p" . field @"negative" .~ n
  where
    c  = o1 >= o2
    z  = o1 == o2
    n  = msb r
    r  = o1 - o2
    o1 = cpu & rX

cpyImm :: Word8 -> CPU -> CPU
cpyImm = cpy

cpyZpg :: Word8 -> CPU -> CPU
cpyZpg = cpyAbs . fromIntegral

cpyAbs :: Word16 -> CPU -> CPU
cpyAbs addr cpu = cpu & cpy m
  where m = (cpu & mem) ! fromIntegral addr
