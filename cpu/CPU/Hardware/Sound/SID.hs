{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Hardware.Sound.SID
  ( SID(..)
  , mkSID
  , volumeControl
  , filterMode
  , v1
  , v2
  , v3
  , rate
  , µs
  )
  where

import CPU.Hardware.Sound.Voice

import Data.Time.Clock
import Data.Word
import GHC.Generics

data SID = SID
  { volume :: Word8
  , voice1 :: Voice
  , voice2 :: Voice
  , voice3 :: Voice
  , clock  :: UTCTime
  , dt     :: Double
  } deriving (Generic, Eq)

mkSID :: UTCTime -> SID
mkSID t0 = SID 0 mkVoice mkVoice mkVoice t0 0.0

µs :: Double
µs = secs * 1000 * 1000
  where secs = (1 :: Double) / fromInteger rate

rate :: Integer
rate = 4000

volumeControl :: Word16 -> Word16
volumeControl = (+0x18)

filterMode :: Word16 -> Word16
filterMode = volumeControl

v1 :: Word16 -> Word16
v1 = id

v2 :: Word16 -> Word16
v2 = (+7)

v3 :: Word16 -> Word16
v3 = (+14)
