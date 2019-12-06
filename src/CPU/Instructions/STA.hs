{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.STA
  ( staZpg
  , staZpgX
  , staAbs
  , staAbsX
  , staAbsY
  , staIndX
  , staIndY
  ) where

import CPU

import Control.Lens
import Data.Word

sta :: (CPU -> Word8) -> Word16 -> CPU -> CPU
sta f addr cpu = cpu & st addr' v
  where addr' = fromIntegral (addr + fromIntegral (f cpu))
        v     = cpu & rA

staZpg :: Word8 -> CPU -> CPU
staZpg = staAbs . fromIntegral

staZpgX :: Word8 -> CPU -> CPU
staZpgX = staAbsX . fromIntegral

staAbs :: Word16 -> CPU -> CPU
staAbs = sta (const 0)

staAbsX :: Word16 -> CPU -> CPU
staAbsX = sta rX

staAbsY :: Word16 -> CPU -> CPU
staAbsY = sta rY

staInd_ :: (CPU -> Word8) -> (CPU -> Word8) -> Word8 -> CPU -> CPU
staInd_ f g ind cpu = cpu & st (addr' + fromIntegral (g cpu)) v
  where v     = cpu & rA
        addr' = cpu & indirectZpg ind (f cpu)

staIndX :: Word8 -> CPU -> CPU
staIndX = staInd_ rX (const 0)

staIndY :: Word8 -> CPU -> CPU
staIndY = staInd_ (const 0) rY
