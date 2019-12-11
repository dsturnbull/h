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
  )
  where

import CPU.Hardware.Sound.Voice

import Data.Word
import GHC.Generics
import Text.Printf

data SID = SID
  { volume :: Word8
  , voice1 :: Voice
  , voice2 :: Voice
  , voice3 :: Voice
  } deriving (Generic, Eq)

instance Show SID where
  show sid = printf "vol: %01x%s" (volume sid) voices
    where voices = foldMap (\s -> "\n" ++ s) $ (\(i :: Int,v) -> (printf "v%i: " i) ++ show (v sid)) <$> (zip [0..] [voice1, voice2, voice3])

mkSID :: SID
mkSID = SID 0 mkVoice mkVoice mkVoice

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
