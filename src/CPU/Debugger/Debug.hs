{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Debugger.Debug
  ( updateScreen
  , disasm
  ) where

import CPU
import CPU.Program (Program (..))

import Control.Lens        hiding (elements)
import Data.Foldable
import Data.List
import Data.Maybe
import System.Console.ANSI
import Text.Printf

import qualified ASM.Assembler        as A (disasm)
import qualified CPU.Debugger.Status  as DBG
import qualified Data.Vector.Storable as DVS

disasm :: CPU -> IO ()
disasm cpu = for_ relevant $ \(o, ins) -> putStrLn $ showMe o ins
  where (cdat, _)    = A.disasm (Program (0, DVS.slice 0 memL (cpu & mem)) (0, DVS.fromList []))
        memL         = DVS.length (cpu & mem)
        listPos      = fromMaybe 0 $ elemIndex position (fst <$> cdat)
        position     = fromMaybe 0 . listToMaybe . reverse $ fst <$> takeWhile (\(o, _) -> o <= (cpu & pc)) cdat
        (bef, aft)   = splitAt listPos cdat
        relevant     = reverse (take befL (reverse bef ++ empty)) ++ take aftL aft
        pcHere       = setSGRCode [SetColor Foreground Vivid Red]
        normal       = setSGRCode [Reset]
        empty        = replicate (befL - length bef) (0, "")
        befL         = 9
        aftL         = 10
        showIn       = printf "%04x: %s"
        showMe o ins | o == position = pcHere ++ showIn o ins ++ normal
        showMe o ins = showIn o ins

updateScreen :: CPU -> IO ()
updateScreen cpu = do
  DBG.updateScreen cpu
  clearFromCursorToScreenEnd
  -- putStr "\n\n"
  -- cpu & disasm

  -- redraw PC specially
  restoreCursor
  setSGR [SetBlinkSpeed RapidBlink]
  setSGR [SetUnderlining SingleUnderline]
  setSGR [SetColor Foreground Vivid Red]
  let m = (cpu & mem) DVS.! fromIntegral (cpu & pc)
  putStr (printf "%02x" m)
  setSGR [Reset]
  cursorBackward 2
