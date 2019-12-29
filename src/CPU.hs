{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
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
  , spritePointerV
  , irqV
  , brkV
  , nmiV
  , timerAV
  , screenV
  , vicV
  , soundV
  , colourV
  , page
  , flagsToWord
  , wordToFlags
  , bitShow
  , nanos
  , h
  , l
  ) where

import CPU.Debugger.Mode             (DebugMode (..))
import CPU.Hardware.Sound.SID        (SID, mkSID)
import CPU.Hardware.Timer.JiffyTimer (JiffyTimer, mkJiffyTimer)
import CPU.Hardware.TTY

import Bindings.PortAudio
import Control.Concurrent.STM
import Control.Lens                 hiding (elements, ignored)
import Data.Bits
import Data.Bits.Lens
import Data.Char
import Data.Generics.Product.Fields
import Data.Maybe
import Data.Time.Clock
import Data.Vector.Storable         (modify, slice, (!), (//))
import Data.Vector.Storable.Mutable (write)
import Data.Word
import Foreign
import Foreign.Marshal.Utils        (fromBool)
import GHC.Generics
import Prelude                      hiding (break, replicate)
import System.Console.ANSI
import Text.Printf

import qualified Data.Vector.Storable as DVS

-- N	....	Negative
-- V	....	Overflow
-- -	....	ignored
-- B	....	Break
-- D	....	Decimal (use BCD for arithmetics)
-- I	....	Interrupt (IRQ disable)
-- Z	....	Zero
-- C	....	Carry

-- XXX unify with Status.hs
instance Show CPU where
  show cpu = printf "%s%s\n%s\n%s\n%s" regs pc' sleep status mem'
    where
          regs   = foldMap (++ " ")
                     (showReg <$> [ ("A", cpu & rA)
                                  , ("X", cpu & rX)
                                  , ("Y", cpu & rY)
                                  , ("S", cpu &  s)])
          status = foldMap (++ " ")
                     (showStatus <$> [ ("N", cpu & p & negative)
                                     , ("V", cpu & p & overflow)
                                     , ("B", cpu & p & break)
                                     , ("D", cpu & p & decimal)
                                     , ("I", cpu & p & interrupt)
                                     , ("Z", cpu & p & zero)
                                     , ("C", cpu & p & carry) ])
          sleep :: String      = printf "%ss: %i" (setSGRCode [Reset]) (cpu & tim)
          showStatus :: (String, Bool) -> String
          showStatus (n, f)    = if f then on n else off n
          on :: String -> String
          on n                 = printf "%s%s%s" onc n normal
          off :: String -> String
          off                  = printf "%s%s" (setSGRCode [Reset])
          pc' :: String        = showPc (cpu & pc)
          showPc               = printf "%sPC: %04x" (setSGRCode [Reset])
          showReg :: (String, Word8) -> String
          showReg (r, v)       = printf "%s%s: %02x" (setSGRCode [Reset]) r v
          mem'              = header ++ foldMap (++ "\n") (showRow <$> rows)
          header            = "    : " ++ foldMap (++ " ") (printf "%02x" <$> [0 .. rowLength - 1]) ++ "\n"
          showRow (o, row)  = printf "%04x: %s |%s|" o (elements o row) (ascii row)
          elements o eles   = foldMap (++ " ") (
                                (\(i, v) ->
                                   if | i == fromIntegral (cpu & pc)                       -> printf "%s%02x%s" pcHere v normal
                                      | i == fromIntegral (fromIntegral (cpu & s) + stack) -> printf "%s%02x%s" spHere v normal
                                      | otherwise                                          -> printf "%s%02x" normal v
                                ) <$> zip [o..] (DVS.toList eles))
          onc :: String        = setSGRCode [SetColor Foreground Vivid Red]
          pcHere :: String     = setSGRCode [SetColor Foreground Vivid Red]
          spHere :: String     = setSGRCode [SetColor Foreground Vivid Blue]
          normal :: String     = setSGRCode [Reset]
          ascii eles        = foldMap (++ "") (printf "%c" . toPrintable . chr . fromIntegral <$> DVS.toList eles)
          toPrintable c     = if isPrint c then c else '.'
          rows              = (\i -> (i, (cpu & mem) & slice i rowLength)) <$> rowStarts
          rowStarts         = (* rowLength) <$> [0 .. (cpu & mem & DVS.length) `div` rowLength - 1]
          rowLength         = min 32 (cpu & mem & DVS.length)


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
  { mem       :: DVS.Vector Word8
  , rA        :: Word8
  , rX        :: Word8
  , rY        :: Word8
  , pc        :: Word16
  , s         :: Word8
  , p         :: Flags
  , tim       :: Int
  , tty       :: TTY
  , audio     :: Maybe (Ptr C'PaStream)
  , sid       :: SID
  , timerA    :: JiffyTimer
  , hz        :: Integer
  , debugMode :: DebugMode
  , loadPos   :: Word16
  , ready     :: TMVar ()
  } deriving (Generic, Eq)

mkFlags :: Flags
mkFlags = Flags False False False False False False False False

mkCPU :: UTCTime -> TTY -> Integer -> DVS.Vector Word8 -> Word16 -> TMVar () -> CPU
mkCPU t0 t h' m = CPU m 0 0 0 0 0xff mkFlags 0 t Nothing (mkSID t0) (mkJiffyTimer t0) h' Debug

_installRoutines :: DVS.Vector Word8 -> DVS.Vector Word8
_installRoutines = _installISR

_installISR :: DVS.Vector Word8 -> DVS.Vector Word8
_installISR v = v // isr
  where isr = [ (0xff48, 0x48) -- PHA         ;push A onto stack
              , (0xff49, 0x8a) -- TXA         ;load X into A
              , (0xff4a, 0x48) -- push A onto stack (X register)
              , (0xff4b, 0x98) -- TYA         ;load Y into A
              , (0xff4c, 0x48) -- push A onto stack (Y register)
              , (0xff4d, 0xba) -- transfer stack pointer to X
              , (0xff4e, 0x04) --
              , (0xff4f, 0x01) --
              , (0xff50, 0xbd) -- LDA $0104,X ;load processor status into A ?
              , (0xff51, 0x29) --
              , (0xff52, 0x10) -- AND #$10		;check for break condition
              , (0xff53, 0xf0) --
              , (0xff54, 0x03) -- BEQ $FF58
              , (0xff55, 0x6c) --
              , (0xff56, 0x16) --
              , (0xff57, 0x03) -- JMP ($0316)	;vector to break ISR
              , (0xff58, 0x6c) --
              , (0xff59, 0x14) --
              , (0xff5a, 0x03) -- JMP ($0314)	;vector	to ISR
              ]

nanos :: Integer -> Double
nanos rate = secs * 1000 * 1000 * 1000
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

spritePointerV :: Word16
spritePointerV = 0x7f8

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

vicV :: Word16
vicV = 0xd000

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
