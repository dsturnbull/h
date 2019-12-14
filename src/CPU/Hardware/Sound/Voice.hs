{-# LANGUAGE DeriveGeneric #-}

module CPU.Hardware.Sound.Voice
  ( Voice(..)
  , Waveform(..)
  , mkVoice
  , attackTable
  , drTable
  , sidFreq
  , voiceFreqH
  , voiceFreqL
  , voicePWDCH
  , voicePWDCL
  , voiceControl
  , voiceAD
  , voiceSR
  )
  where

import Data.Word
import GHC.Generics
import Text.Printf

data Waveform = Pulse | Sawtooth | Triangle | Noise
  deriving (Generic, Eq)

instance Show Waveform where
  show Pulse    = "pul"
  show Sawtooth = "saw"
  show Triangle = "tri"
  show Noise    = "nse"

data Voice = Voice
  { gate     :: Bool
  , attackW  :: Word8
  , attack   :: Double
  , sustainW :: Word8
  , sustain  :: Double
  , decayW   :: Word8
  , decay    :: Double
  , releaseW :: Word8
  , release  :: Double
  , freqW    :: Word16
  , freq     :: Double
  , wave     :: Waveform
  } deriving (Generic, Eq)

instance Show Voice where
  show v = printf "w:%s a:%02x(%7.4f) d:%02x(%7.4f) r:%02x(%7.4f) s:%02x(%7.4f) f:%11.4f fW:%04x (%s)"
            (show $ wave v)
            (attackW v) (attack v)
            (decayW v) (decay v)
            (releaseW v) (release v)
            (sustainW v) (sustain v)
            (freq v)
            (freqW v)
            (if gate v then "on" else "off")

attackTable :: Word8 -> Double
attackTable 0x0 =    2 / ms
attackTable 0x1 =    8 / ms
attackTable 0x2 =   16 / ms
attackTable 0x3 =   24 / ms
attackTable 0x4 =   38 / ms
attackTable 0x5 =   56 / ms
attackTable 0x6 =   68 / ms
attackTable 0x7 =   80 / ms
attackTable 0x8 =  100 / ms
attackTable 0x9 =  250 / ms
attackTable 0xa =  500 / ms
attackTable 0xb =  800 / ms
attackTable 0xc =    1 / s
attackTable 0xd =    3 / s
attackTable 0xe =    5 / s
attackTable 0xf =    8 / s
attackTable _   = undefined

drTable :: Word8 -> Double
drTable 0x0 =     6 / ms
drTable 0x1 =    24 / ms
drTable 0x2 =    48 / ms
drTable 0x3 =    72 / ms
drTable 0x4 =   114 / ms
drTable 0x5 =   168 / ms
drTable 0x6 =   204 / ms
drTable 0x7 =   240 / ms
drTable 0x8 =   300 / ms
drTable 0x9 =   750 / ms
drTable 0xa =   1.5 / s
drTable 0xb =   2.4 / s
drTable 0xc =   3   / s
drTable 0xd =   9   / s
drTable 0xe =  15   / s
drTable 0xf =  24   / s
drTable _   = undefined

sidFreq :: Double
sidFreq = 16.94

ms :: Double
ms = 1000

s :: Double
s = 1

mkVoice :: Voice
mkVoice = Voice { gate     = False
                , attackW  = 0
                , attack   = 0.0
                , sustainW = 0
                , sustain  = 0.0
                , decayW   = 0
                , decay    = 0.0
                , releaseW = 0
                , release  = 0.0
                , freqW    = 0
                , freq     = 0.0
                , wave     = Noise
                }

voiceFreqH :: Word16 -> Word16
voiceFreqH = id

voiceFreqL :: Word16 -> Word16
voiceFreqL = (+1)

voicePWDCH :: Word16 -> Word16
voicePWDCH = (+2)

voicePWDCL :: Word16 -> Word16
voicePWDCL = (+3)

voiceControl :: Word16 -> Word16
voiceControl = (+4)

voiceAD :: Word16 -> Word16
voiceAD = (+5)

voiceSR :: Word16 -> Word16
voiceSR = (+6)
