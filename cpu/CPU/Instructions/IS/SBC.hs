{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.SBC
  ( sbcImm
  , sbcZpg
  , sbcZpgX
  , sbcAbs
  , sbcAbsX
  , sbcAbsY
  , sbcIndX
  , sbcIndY
  ) where

import CPU

import Control.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable
import Data.Word
import Foreign.Marshal.Utils        (fromBool)

setCarry :: Word8 -> Word8 -> CPU -> CPU
setCarry o1 o2 cpu = cpu & field @"p" . field @"carry" .~ (o1 > o2)

setZero :: Word8 -> CPU -> CPU
setZero v cpu = cpu & field @"p" . field @"zero" .~ (v == 0)

-- XXX
-- setOverflow :: Word8 -> CPU -> CPU
-- setOverflow = undefined

sbc :: Word8 -> CPU -> CPU
sbc o2 cpu =
  cpu & setCarry o1 o2 & setZero v
      & field @"rA" .~ v
  where
    o1 = cpu & rA
    v  = o1 - o2 - c'
    c' = cpu & p & carry & not & fromBool

sbcImm :: Word8 -> CPU -> CPU
sbcImm = sbc

sbcZpg :: Word8 -> CPU -> CPU
sbcZpg = sbcAbs . fromIntegral

sbcZpgX :: Word8 -> CPU -> CPU
sbcZpgX = sbcAbsX . fromIntegral

sbcAbs_ :: (CPU -> Word8) -> Word16 -> CPU -> CPU
sbcAbs_ f addr cpu = cpu & sbc v
  where v = (cpu & mem) ! fromIntegral (addr + fromIntegral (f cpu))

sbcAbs :: Word16 -> CPU -> CPU
sbcAbs = sbcAbs_ (const 0)

sbcAbsX :: Word16 -> CPU -> CPU
sbcAbsX = sbcAbs_ rX

sbcAbsY :: Word16 -> CPU -> CPU
sbcAbsY = sbcAbs_ rY

sbcInd_ :: (CPU -> Word8) -> (CPU -> Word8) -> Word8 -> CPU -> CPU
sbcInd_ f g ind cpu = cpu & sbc v
  where v = (cpu & mem) ! (addr + fromIntegral (g cpu))
        addr = cpu & indirectZpg ind (f cpu) & fromIntegral

sbcIndX :: Word8 -> CPU -> CPU
sbcIndX = sbcInd_ rX (const 0)

sbcIndY :: Word8 -> CPU -> CPU
sbcIndY = sbcInd_ (const 0) rY
