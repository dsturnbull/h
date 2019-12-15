{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CPU.Debugger
  ( initDebugger
  , updateDebugger
  , debugger
  , debuggerInput
  , DebugState(..)
  ) where

import CPU
import CPU.Hardware.Terminal
import CPU.Instructions.Impl (rti)
import CPU.Terminal

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Foldable
import Data.Generics.Product.Fields
import Data.Maybe
import Data.Word
import Foreign                      hiding (void)
import GHC.Generics
import Prelude                      hiding (break)
import System.Console.ANSI
import System.IO
import System.IO.Echo.Internal
import System.Posix.Types           (Fd)
import Text.Printf

-- import Foreign.C.Types
-- import Foreign.Marshal.Alloc
-- import Foreign.Storable

data DebugState a = Broken a | Step a | Continue a
  deriving Generic

type Row = Int
type Col = Int
type Label = String
type Value = String
type Line = [(Label, CPU -> Value)]
type Screen = [Line]
type LineMap = (Row, Col, Col, Label, CPU -> Value)
type ScreenMap = [LineMap]

screen :: Screen
screen = [ [ ("tty: ", \cpu -> fromMaybe "/dev/ttysXXX" (cpu & ttyName))
           , ("dt: ",  \cpu -> printf "%9.4e" (cpu & dt))
           ]
         , [ ("A: ",   \cpu -> printf "%02x" (cpu & rA))
           , ("X: ",   \cpu -> printf "%02x" (cpu & rX))
           , ("Y: ",   \cpu -> printf "%02x" (cpu & rY))
           , ("S: ",   \cpu -> printf "%02x" (cpu &  s))
           ]
         ]

screenMap :: Row -> CPU -> Screen -> ScreenMap
screenMap row cpu (l : ls) = (screenMapLine row 0 cpu l) ++ screenMap (row + 1) cpu ls
screenMap _ _ []           = []

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
  cpu & drawScreen (screenMap 0 cpu screen)
  hFlush stdout

updateDebugger :: CPU -> IO ()
updateDebugger cpu = do
  cpu & updateScreen (screenMap 0 cpu screen)
  hFlush stdout

debugger :: Maybe Word8 -> CPU -> IO (DebugState CPU)
debugger mc cpu =
  case mc of
    Just 17 -> return $ Continue $ cpu & continue
    Just 24 -> return $ Step cpu
    Just c  -> return $ Broken $ cpu & processInput c
    Nothing -> return $ Broken cpu

continue :: CPU -> CPU
continue cpu =
  cpu & rti
      & field @"p" . field @"break" .~ False
      & field @"pc" %~ flip (-) 2

debuggerInput :: TMVar Word8 -> Fd -> CPU -> IO (DebugState CPU)
debuggerInput wS tty cpu = do
  mc <- getInput tty
  k <- atomically $ tryTakeTMVar wS
  cpu & debugger (mc <|> k)
