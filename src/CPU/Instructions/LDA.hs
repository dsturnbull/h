{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.LDA
  ( ldaImm
  , ldaZpg
  , ldaZpgX
  , ldaAbs
  , ldaAbsX
  , ldaAbsY
  , ldaIndX
  , ldaIndY
  ) where

import CPU

import Control.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable
import Data.Word

setZero :: Word8 -> CPU -> CPU
setZero v cpu = cpu & field @"p" . field @"zero" .~ (v == 0)

lda :: Word8 -> CPU -> CPU
lda w cpu = cpu & field @"rA" .~ w & setZero w

ldaImm :: Word8 -> CPU -> CPU
ldaImm = lda

ldaZpg :: Word8 -> CPU -> CPU
ldaZpg = ldaAbs . fromIntegral

ldaZpgX :: Word8 -> CPU -> CPU
ldaZpgX = ldaAbsX . fromIntegral

ldaAbs_ :: (CPU -> Word8) -> Word16 -> CPU -> CPU
ldaAbs_ f a cpu = cpu & lda v
  where v = (cpu & mem) ! fromIntegral (a + fromIntegral (f cpu))

ldaAbs :: Word16 -> CPU -> CPU
ldaAbs = ldaAbs_ (const 0)

ldaAbsX :: Word16 -> CPU -> CPU
ldaAbsX = ldaAbs_ rX

ldaAbsY :: Word16 -> CPU -> CPU
ldaAbsY = ldaAbs_ rY

ldaInd_ :: (CPU -> Word8) -> (CPU -> Word8) -> Word8 -> CPU -> CPU
ldaInd_ f g ind cpu = cpu & lda v
  where v    = (cpu & mem) ! (addr + fromIntegral (g cpu))
        addr = cpu & indirectZpg ind (f cpu) & fromIntegral

ldaIndX :: Word8 -> CPU -> CPU
ldaIndX = ldaInd_ rX (const 0)

ldaIndY :: Word8 -> CPU -> CPU
ldaIndY = ldaInd_ (const 0) rY
