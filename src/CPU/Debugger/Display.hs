{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Debugger.Display
  ( initDebugger
  , updateDebugger
  ) where

import CPU
import CPU.Hardware.Sound.SID   as SID (SID (..))
import CPU.Hardware.Sound.Voice (Voice (..))

import Control.Applicative
import Control.Lens            hiding (elements)
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Word
import Foreign                 hiding (void)
import GHC.Generics
import Prelude                 hiding (break)
import System.Console.ANSI
import System.IO
import System.IO.Echo.Internal
import Text.Printf

import qualified Data.Vector.Storable as DVS

-- import Foreign.C.Types
-- import Foreign.Marshal.Alloc
-- import Foreign.Storable

data DebugState a = Broken a | Step a | Continue a
  deriving Generic

type Row = Int
type Col = Int
type Label = String
type Value = String
type Element = (Label, CPU -> Value)
type Line = [Element]
type Screen = [Line]
type LineMap = (Row, Col, Col, Label, CPU -> Value)
type ScreenMap = [LineMap]

screen :: CPU -> Screen
screen cpu = [ [ ("tty: ", \cpu' -> fromMaybe "/dev/ttysXXX" (cpu' & ttyName))
               , ("hz: ",  \cpu' -> printf "%.3f MHz" ((fromInteger (CPU.hz cpu') :: Double) / 1e6))
            -- , ("dt: ",  \cpu' -> printf "%9.4e" (cpu' & CPU.dt))
               , ("s:",    \cpu' -> printf "%2i"   (cpu' & tim)) ]
             , [ ("A: ",   \cpu' -> printf "%02x"  (cpu' & rA))
               , ("X: ",   \cpu' -> printf "%02x"  (cpu' & rX))
               , ("Y: ",   \cpu' -> printf "%02x"  (cpu' & rY))
               , ("S: ",   \cpu' -> printf "%02x"  (cpu' &  s))
               , ("PC: ",  \cpu' -> printf "%04x"  (cpu' & pc)) ]
             , [ ("vol: ", \cpu' -> printf "%01x"  (cpu' & sid & volume))
            -- , ("dt: ",  \cpu' -> printf "%9.4e" (cpu' & sid & SID.dt))
               ]
             ] ++ [ showVoice 1 SID.voice1
                  , showVoice 2 SID.voice2
                  , showVoice 3 SID.voice3 ]
               ++ [ showFlags ]
               ++ [ showMemHeader ]
               ++ showMemRows cpu

showMemHeader :: Line
showMemHeader = [ (header, const "") ]
  where header = "    : " ++ foldMap (++ " ") (printf "%02x" <$> [0 .. rowLength - 1]) ++ "\n"

showMemRows :: CPU -> [Line]
showMemRows cpu = uncurry showMemRow <$> rows
  where
    rows      = (\i -> (i, (cpu & mem) & DVS.slice i rowLength)) <$> rowStarts
    rowStarts = (* rowLength) <$> [0 .. (cpu & mem & DVS.length) `div` rowLength - 1]

showMemRow :: Int -> DVS.Vector Word8 -> Line
showMemRow o eles = [(printf "%04x: " o, elements)]
  where elements cpu =
          foldMap (++ " ") (
            (\(i, v) ->
              if | i == fromIntegral (cpu & pc)                       -> printf "%s%02x%s" pcHere v normal
                 | i == fromIntegral (fromIntegral (cpu & s) + stack) -> printf "%s%02x%s" spHere v normal
                 | otherwise                                          -> printf "%s%02x" normal v
            ) <$> zip [o..] (DVS.toList eles))
        pcHere = setSGRCode [SetColor Foreground Vivid Red]
        spHere = setSGRCode [SetColor Foreground Vivid Blue]
        normal = setSGRCode [Reset]

rowLength :: Int
rowLength = 32

showVoice :: Int -> (SID -> Voice) -> Line
showVoice n f =
  [ ("v" <> show n <> ": ", const "")
  , ("w: ",  \cpu -> show $ cpu & v & wave)
  , ("a: ",  \cpu -> printf "%02x(%7.4f)" (cpu & v & attackW)  (cpu & v & attack))
  , ("d: ",  \cpu -> printf "%02x(%7.4f)" (cpu & v & decayW)   (cpu & v & decay))
  , ("r: ",  \cpu -> printf "%02x(%7.4f)" (cpu & v & releaseW) (cpu & v & release))
  , ("s: ",  \cpu -> printf "%02x(%7.4f)" (cpu & v & sustainW) (cpu & v & sustain))
  , ("f: ",  \cpu -> printf "%11.4f"      (cpu & v & freq))
  , ("fW: ", \cpu -> printf "%04x"        (cpu & v & freqW))
  , ("",     \cpu -> if cpu & v & gate then "on " else "off") -- 3 char wide for on is important
  ]
  where v cpu = cpu & sid & f

showFlags :: Line
showFlags = [ ("", status) ]
  where status cpu = foldMap (++ " ")
                      (showStatus <$> [ ("N", cpu & p & negative)
                                      , ("V", cpu & p & overflow)
                                      , ("B", cpu & p & break)
                                      , ("D", cpu & p & decimal)
                                      , ("I", cpu & p & interrupt)
                                      , ("Z", cpu & p & zero)
                                      , ("C", cpu & p & carry) ])
        showStatus (n, f) = if f then on n else off n
        on n              = printf "%s%s%s" onc n normal
        off               = printf "%s%s" (setSGRCode [Reset])
        onc               = setSGRCode [SetColor Foreground Vivid Red]
        normal            = setSGRCode [Reset]

screenMap :: CPU -> Row -> ScreenMap
screenMap cpu = screenMap' cpu (screen cpu)

screenMap' :: CPU -> Screen -> Row -> ScreenMap
screenMap' cpu (l : ls) row = screenMapLine row 0 cpu l ++ screenMap' cpu ls (row + 1)
screenMap' _ [] _           = []

screenMapLine :: Row -> Col -> CPU -> Line -> [LineMap]
screenMapLine row col cpu ((lbl, f) : es) = (row, col, lcol, lbl, f) : screenMapLine row vcol cpu es
  where val' = f cpu
        lcol = col + length lbl
        vcol = col + length lbl + length val' + 1
screenMapLine _ _ _ [] = []

drawScreen :: ScreenMap -> CPU -> IO ()
drawScreen m cpu = traverse_ draw m
  where draw (row, lcol, vcol, lbl, val) =
          let val' = val cpu in do
            setCursorPosition row lcol
            putStr lbl
            setCursorPosition row vcol
            putStr val'

updateScreen :: ScreenMap -> CPU -> IO ()
updateScreen m cpu = traverse_ draw m
  where draw (row, _, vcol, _, val) =
          let val' = val cpu in do
            setCursorPosition row vcol
            putStr val'

initDebugger :: CPU -> IO ()
initDebugger cpu = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  void $ sttyRaw "-ixon"
  clearScreen
  cpu & drawScreen (screenMap cpu 0)
  hFlush stdout

updateDebugger :: CPU -> IO ()
updateDebugger cpu = do
  cpu & updateScreen (screenMap cpu 0)
  hFlush stdout
