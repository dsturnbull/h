{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.CMP
  ( cmpImm
  , cmpZpg
  , cmpZpgX
  , cmpAbs
  , cmpAbsX
  , cmpAbsY
  , cmpIndX
  , cmpIndY
  ) where

import CPU

import Control.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable
import Data.Word

cmp :: Word8 -> CPU -> CPU
cmp o2 cpu =
  cpu & field @"p" . field @"carry"    .~ c
      & field @"p" . field @"zero"     .~ z
      & field @"p" . field @"negative" .~ n
  where
    c  = o1 >= o2
    z  = o1 == o2
    n  = msb r
    r  = o1 - o2
    o1 = cpu & rA

cmpImm :: Word8 -> CPU -> CPU
cmpImm = cmp

cmpZpg :: Word8 -> CPU -> CPU
cmpZpg = cmpAbs . fromIntegral

cmpZpgX :: Word8 -> CPU -> CPU
cmpZpgX = cmpAbsX . fromIntegral

cmpAbs_ :: (CPU -> Word8) -> Word16 -> CPU -> CPU
cmpAbs_ f addr cpu = cpu & cmp m
  where m = (cpu & mem) ! fromIntegral (addr + fromIntegral (f cpu))

cmpAbs :: Word16 -> CPU -> CPU
cmpAbs = cmpAbs_ (const 0)

cmpAbsX :: Word16 -> CPU -> CPU
cmpAbsX = cmpAbs_ rX

cmpAbsY :: Word16 -> CPU -> CPU
cmpAbsY = cmpAbs_ rY

cmpInd_ :: (CPU -> Word8) -> (CPU -> Word8) -> Word8 -> CPU -> CPU
cmpInd_ f g ind cpu = cpu & cmp m
  where m    = (cpu & mem) ! (addr + fromIntegral (g cpu))
        addr = cpu & indirectZpg ind (f cpu) & fromIntegral

cmpIndX :: Word8 -> CPU -> CPU
cmpIndX = cmpInd_ rX (const 0)

cmpIndY :: Word8 -> CPU -> CPU
cmpIndY = cmpInd_ (const 0) rY
