{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module CPU.Debugger.Status
  ( drawScreen
  , updateScreen
  , screen
  , screenMap
  , Line
  , Row
  , Screen
  , ScreenMap
  ) where

import CPU
import CPU.Hardware.Sound.SID   as SID (SID (..))
import CPU.Hardware.Sound.Voice (Voice (..))

import Control.Applicative
import Control.Lens        hiding (elements)
import Data.Char
import Data.Coerce
import Data.Foldable
import Data.List           hiding (break)
import Data.Maybe
import Data.Word
import Foreign             hiding (void)
import GHC.Generics
import Prelude             hiding (break)
import System.Console.ANSI
import Text.Printf

import qualified Data.Vector.Storable as DVS

data DebugState a = Broken a | Step a | Continue a
  deriving Generic

newtype Row = Row Int deriving Num
newtype Col = Col Int deriving Num
newtype Label = Label String
newtype Value = Value String deriving (Semigroup)
newtype Element = Element (Label, CPU -> Value)
newtype Line = Line [Element]
newtype Screen = Screen [Line]
newtype LineMap = LineMap (Row, Col, Col, Label, CPU -> Value)
newtype ScreenMap a = ScreenMap [a] deriving (Functor, Traversable, Foldable)

screen :: CPU -> Screen
screen cpu = Screen $ coerce
  [ [ ("tty: ", \cpu' -> fromMaybe "/dev/ttysXXX" (cpu' & ttyName))
    , ("hz: ",  \cpu' -> printf "%.3f MHz" ((fromInteger (CPU.hz cpu') :: Double) / 1e6))
    , ("m: ",   \cpu' -> show $ cpu' & debugMode)
    , ("s:",    \cpu' -> printf "%2i"   (cpu' & tim)) ]
  , [ ("A: ",   \cpu' -> printf "%02x"  (cpu' & rA))
    , ("X: ",   \cpu' -> printf "%02x"  (cpu' & rX))
    , ("Y: ",   \cpu' -> printf "%02x"  (cpu' & rY))
    , ("S: ",   \cpu' -> printf "%02x"  (cpu' &  s))
    , ("PC: ",  \cpu' -> printf "%04x"  (cpu' & pc)) ]
  , [ ("vol: ", \cpu' -> printf "%01x"  (cpu' & sid & volume)) ]
  ]
  ++ [ showVoice 1 SID.voice1
     , showVoice 2 SID.voice2
     , showVoice 3 SID.voice3 ]
  ++ [ showFlags ]
  ++ [ showMemHeader ]
  ++ showMemRows cpu

showMemHeader :: Line
showMemHeader = Line [ Element (Label header, const (Value "")) ]
  where header = "    : " ++ foldMap (++ " ") (printf "%02x" <$> [0 .. rowLength - 1]) ++ "\n"

showMemRows :: CPU -> [Line]
showMemRows cpu = uncurry showMemRow <$> rows
  where
    rows      = (\i -> (i, (cpu & mem) & DVS.slice i rowLength)) <$> rowStarts
    rowStarts = (* rowLength) <$> [w `div` rowLength .. e `div` rowLength]
    w         = max 0 (m - fromIntegral n * rowLength)
    e         = max m (fromIntegral ((fromIntegral m :: Word16) + n * fromIntegral rowLength) + r)
    r         = fromIntegral n * rowLength - (fromIntegral m - w)
    m         = fromIntegral (cpu & pc) `div` rowLength * rowLength
    n         = 8

showMemRow :: Int -> DVS.Vector Word8 -> Line
showMemRow o eles = Line [Element (Label "", \cpu -> Value $ printf "%04x: " o <> memory cpu <> ascii cpu)]
  where memory cpu =
          foldMap (++ " ") (
            (\(i, v) ->
              if | i == fromIntegral (cpu & pc)                       -> printf "%s%02x" pcHere v
                 | i == fromIntegral (fromIntegral (cpu & s) + stack) -> printf "%s%02x%s" spHere v normal
                 | otherwise                                          -> printf "%s%02x" normal v
            ) <$> zip [o..] eles')
        ascii cpu = printf "|%s|" $
          foldMap (++ "") (
            (\(i, v) ->
              if | i == fromIntegral (cpu & pc)                       -> printf "%s%c%s" pcHere' (pr (chr (fromIntegral v))) normal
                 | otherwise                                          -> printf "%c" (pr (chr (fromIntegral v)))
            ) <$> zip [o..] eles')
        eles'   = DVS.toList eles
        pcHere  = saveCursorCode
        pcHere' = setSGRCode [SetColor Foreground Vivid Red]
        spHere  = setSGRCode [SetColor Foreground Vivid Blue]
        normal  = setSGRCode [Reset]
        pr c    = if isPrint c then c else '.'

rowLength :: Int
rowLength = 32

showVoice :: Int -> (SID -> Voice) -> Line
showVoice n f = Line $ Element <$>
  [ (Label ("v" <> show n <> ": "), const (Value ""))
  , (Label "w: ",  \cpu -> Value $ show $ cpu & v & wave)
  , (Label "a: ",  \cpu -> Value $ printf "%02x(%7.4f)" (cpu & v & attackW)  (cpu & v & attack))
  , (Label "d: ",  \cpu -> Value $ printf "%02x(%7.4f)" (cpu & v & decayW)   (cpu & v & decay))
  , (Label "r: ",  \cpu -> Value $ printf "%02x(%7.4f)" (cpu & v & releaseW) (cpu & v & release))
  , (Label "s: ",  \cpu -> Value $ printf "%02x(%7.4f)" (cpu & v & sustainW) (cpu & v & sustain))
  , (Label "f: ",  \cpu -> Value $ printf "%11.4f"      (cpu & v & freq))
  , (Label "fW: ", \cpu -> Value $ printf "%04x"        (cpu & v & freqW))
  , (Label "",     \cpu -> if cpu & v & gate then Value "on " else Value "off") -- 3 char wide for on is important
  ]
  where v cpu = cpu & sid & f

showFlags :: Line
showFlags = Line [ Element (Label "", status) ]
  where status cpu = Value $ foldMap (++ " ") (flags cpu)
        flags cpu = ss <$>
          [ ("N", cpu & p & negative)
          , ("V", cpu & p & overflow)
          , ("B", cpu & p & break)
          , ("D", cpu & p & decimal)
          , ("I", cpu & p & interrupt)
          , ("Z", cpu & p & zero)
          , ("C", cpu & p & carry) ]
        ss (n, f) = if f then on n else off n
        on n      = printf "%s%s%s" onc n normal
        off       = printf "%s%s" (setSGRCode [Reset])
        onc       = setSGRCode [SetColor Foreground Vivid Red]
        normal    = setSGRCode [Reset]

screenMap :: CPU -> ScreenMap LineMap
screenMap cpu = ScreenMap $ mapLines (screen cpu) 0
 where
  -- render the elements in each line
  mapLines :: Screen -> Row -> [LineMap]
  mapLines (Screen (ln : lns)) row = mapLine row 0 ln ++ mapLines (Screen lns) (row + 1)
  mapLines (Screen []) _           = []

  -- render a series of labels and values, remembering the offsets of both
  mapLine :: Row -> Col -> Line -> [LineMap]
  mapLine row col@(Col coli) (Line (Element (lbl@(Label lbli), f) : es)) = LineMap (row, col, lcol, lbl, f) : mapLine row vcol (Line es)
    where
      (Value val') = f cpu
      lcol = Col $ coli + length lbli
      vcol = Col $ coli + length lbli + length val' + 1
  mapLine _ _ (Line []) = []

drawScreen :: CPU -> IO ()
drawScreen cpu = traverse_ draw (screenMap cpu)
  where draw (LineMap ((Row row), (Col lcol), (Col vcol), (Label lbl), val)) = do
            setCursorPosition row lcol
            putStr lbl
            setCursorPosition row vcol
            putStr val'
          where
            (Value val') = val cpu

updateScreen :: CPU -> IO ()
updateScreen cpu = traverse_ draw (screenMap cpu)
  where draw (LineMap ((Row row), _, (Col vcol), _, val)) = do
            setCursorPosition row vcol
            putStr val'
          where
            (Value val') = val cpu
