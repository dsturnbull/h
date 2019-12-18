{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CPU
  ( CPU(..)
  , Flags(..)
  , mkCPU
  , zeroable
  , negable
  , st
  , indirectZpg
  , indirect
  , msb
  , zeropage
  , stack
  , kbd
  , irqV
  , brkV
  , nmiV
  , timerAV
  , screenV
  , soundV
  , colourV
  , page
  , flagsToWord
  , wordToFlags
  , bitShow
  , µs
  , h
  , l
  ) where

import CPU.Debugger.Mode             (DebugMode (..))
import CPU.Hardware.Sound.SID        (SID, mkSID)
import CPU.Hardware.Timer.JiffyTimer (JiffyTimer, mkJiffyTimer)

import Bindings.PortAudio
import Control.Lens                 hiding (elements, ignored)
import Data.Bits
import Data.Bits.Lens
import Data.Generics.Product.Fields
import Data.Maybe
import Data.Time.Clock
import Data.Vector.Storable         as V hiding (break)
import Data.Vector.Storable.Mutable (write)
import Data.Word
import Foreign
import Foreign.Marshal.Utils        (fromBool)
import GHC.Generics
import Prelude                      hiding (break, replicate)

import qualified Data.Vector.Storable as DVS

-- N	....	Negative
-- V	....	Overflow
-- -	....	ignored
-- B	....	Break
-- D	....	Decimal (use BCD for arithmetics)
-- I	....	Interrupt (IRQ disable)
-- Z	....	Zero
-- C	....	Carry

data Flags = Flags
  { negative  :: Bool
  , overflow  :: Bool
  , ignored   :: Bool
  , break     :: Bool
  , decimal   :: Bool
  , interrupt :: Bool
  , zero      :: Bool
  , carry     :: Bool
  } deriving (Show, Generic, Eq)

data CPU = CPU
  { mem       :: Vector Word8
  , rA        :: Word8
  , rX        :: Word8
  , rY        :: Word8
  , pc        :: Word16
  , s         :: Word8
  , p         :: Flags
  , tim       :: Int
  , clock     :: UTCTime
  , dt        :: Double
  , ttyName   :: Maybe String
  , audio     :: Maybe (Ptr C'PaStream)
  , sid       :: SID
  , timerA    :: JiffyTimer
  , hz        :: Integer
  , debugMode :: DebugMode
  , loadPos   :: Word16
  } deriving (Generic, Eq)

mkFlags :: Flags
mkFlags = Flags False False False False False False False False

mkCPU :: UTCTime -> Integer -> DVS.Vector Word8 -> Word16 -> CPU
mkCPU t0 h' m = CPU m 0 0 0 0 0xff mkFlags 0 t0 0 Nothing Nothing (mkSID t0) (mkJiffyTimer t0) h' Debug

µs :: Integer -> Double
µs rate = secs * 1000 * 1000
  where secs = (1 :: Double) / fromInteger rate

st :: Word16 -> Word8 -> CPU -> CPU
st addr v cpu = cpu & field @"mem" %~ modify (\vec -> write vec (fromIntegral addr) v)

indirectZpg :: Word8 -> Word8 -> CPU -> Word16
indirectZpg ind offset cpu = (fromIntegral addrH `shiftL` 8) .|. fromIntegral addrL
  where
    addrH = (cpu & mem) ! fromIntegral (ind + offset + 1)
    addrL = (cpu & mem) ! fromIntegral (ind + offset)

indirect :: Word16 -> CPU -> Word16
indirect ind cpu = (fromIntegral addrH `shiftL` 8) .|. fromIntegral addrL
  where
    addrH = (cpu & mem) ! fromIntegral (ind + 1)
    addrL = (cpu & mem) ! fromIntegral ind

zeroable :: (CPU -> Word8) -> CPU -> CPU
zeroable r cpu = cpu & field @"p" . field @"zero" .~ ((cpu & r) == 0)

negable :: (CPU -> Word8) -> CPU -> CPU
negable r cpu = cpu & field @"p" . field @"negative" .~ ((cpu & r) < 0)

msb :: (Num w, Bits w) => w -> Bool
msb w = w .&. 0x80 == 0x80

zeropage :: Word16
zeropage = 0

stack :: Word16
stack = 0x0100

kbd :: Word16
kbd = 0x300

irqV :: Word16
irqV = 0xfffe

brkV :: Word16
brkV = 0x0316

nmiV :: Word16
nmiV = 0x0318

timerAV :: Word16
timerAV = 0x0320

screenV :: Word16
screenV = 0x0400

soundV :: Word16
soundV = 0xd400

colourV :: Word16
colourV = 0xd800

page :: Word8
page = maxBound

flagsToWord :: Flags -> Word8
flagsToWord f =
   0 .|. ((f & negative  & fromBool) `shiftL` 7)
     .|. ((f & overflow  & fromBool) `shiftL` 6)
     .|. ((f & ignored   & fromBool) `shiftL` 5)
     .|. ((f & break     & fromBool) `shiftL` 4)
     .|. ((f & decimal   & fromBool) `shiftL` 3)
     .|. ((f & interrupt & fromBool) `shiftL` 2)
     .|. ((f & zero      & fromBool) `shiftL` 1)
     .|. ((f & carry     & fromBool) `shiftL` 0)

wordToFlags :: Word8 -> CPU -> CPU
wordToFlags w cpu =
  cpu & field @"p" . field @"negative"  .~ w ^. bitAt 7
      & field @"p" . field @"overflow"  .~ w ^. bitAt 6
      & field @"p" . field @"ignored"   .~ w ^. bitAt 5
      & field @"p" . field @"break"     .~ w ^. bitAt 4
      & field @"p" . field @"decimal"   .~ w ^. bitAt 3
      & field @"p" . field @"interrupt" .~ w ^. bitAt 2
      & field @"p" . field @"zero"      .~ w ^. bitAt 1
      & field @"p" . field @"carry"     .~ w ^. bitAt 0

bitShow :: Bits w => w -> String
bitShow w =
      (if w ^. bitAt 7 then ('1':) else ('0':))
    . (if w ^. bitAt 6 then ('1':) else ('0':))
    . (if w ^. bitAt 5 then ('1':) else ('0':))
    . (if w ^. bitAt 4 then ('1':) else ('0':))
    . (if w ^. bitAt 3 then ('1':) else ('0':))
    . (if w ^. bitAt 2 then ('1':) else ('0':))
    . (if w ^. bitAt 1 then ('1':) else ('0':))
    . (if w ^. bitAt 0 then ('1':) else ('0':))
    $ ""

h :: Word16 -> Word8
h w = fromIntegral $ w `shiftR` 8

l :: Word16 -> Word8
l w = fromIntegral $ w .&. 0x00ff
