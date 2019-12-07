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
  , stack
  , page
  , flagsToWord
  , wordToFlags
  , bitShow
  ) where

import Control.Lens                 hiding (elements, ignored)
import Data.Bits
import Data.Bits.Lens
import Data.Char
import Data.Generics.Product.Fields
import Data.Vector.Storable         as V hiding (break, (++))
import Data.Vector.Storable.Mutable (write)
import Data.Word
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
  { mem :: Vector Word8
  , rA  :: Word8
  , rX  :: Word8
  , rY  :: Word8
  , pc  :: Word16
  , s   :: Word8
  , p   :: Flags
  } deriving (Generic, Eq)

instance Show CPU where
  show cpu = printf "%s%s\n%s\n%s\n%s" regs pc' stb status mem'
    where regs   = foldMap (++ " ")
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
          stb :: String     = printf "%sp: %s" (setSGRCode [Reset]) (bitShow (cpu & p & flagsToWord))
          showStatus (n, f) = if f then on n else off n
          on n              = printf "%s%s%s" here n normal
          off n             = printf "%s%s" (setSGRCode [Reset]) n
          pc' :: String     = showPc (cpu & pc)
          showPc v          = printf "%sPC: %04x" (setSGRCode [Reset]) v
          showReg (r, v)    = printf "%s%s: %02x" (setSGRCode [Reset]) r v
          mem'              = header ++ foldMap (++ "\n") (showRow <$> rows)
          header            = "    : " ++ foldMap (++ " ") (printf "%02x" <$> [0 .. rowLength - 1]) ++ "\n"
          showRow (o, row)  = printf "%04x: %s |%s|" o (elements o row) (ascii row)
          elements o eles   = foldMap (++ " ") ((\(i, v) -> if i == fromIntegral (cpu & pc) then printf "%s%02x" here v else printf "%s%02x" normal v) <$> zip [o..] (V.toList eles))
          here              = setSGRCode [SetColor Foreground Vivid Red]
          normal            = setSGRCode [Reset]
          ascii eles        = foldMap (++ "") (printf "%c" . toPrintable . chr . fromIntegral <$> V.toList eles)
          toPrintable c     = if isPrint c then c else '.'
          rows              = (\i -> (i, (cpu & mem) & slice i rowLength)) <$> rowStarts
          rowStarts         = (* rowLength) <$> [0 .. (cpu & mem & V.length) `div` rowLength - 1]
          rowLength         = min 32 (cpu & mem & V.length)

mkFlags :: Flags
mkFlags = Flags False False False False False False False False

mkCPU :: DVS.Vector Word8 -> CPU
mkCPU m = CPU m 0 0 0 0 0xff mkFlags

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

stack :: Word16
stack = 0x0100

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
