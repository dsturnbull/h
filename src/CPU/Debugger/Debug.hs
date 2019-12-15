{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Debugger.Debug
  ( updateScreen
  ) where

import CPU
import CPU.Program (Program (..))

import Control.Lens        hiding (elements)
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Word
import System.Console.ANSI
import Text.Printf

import qualified ASM.Assembler       as A (disasm)
import qualified CPU.Debugger.Status as DBG

disasm :: CPU -> IO ()
disasm cpu = for_ relevant $ \(o, ins) -> putStrLn $ showMe o ins
  where code          = A.disasm (Program (cpu & mem))
        position      = fromMaybe 0 $ findIndex (\(o, _) -> o == (cpu & pc)) code
        (b, a)        = splitAt position code
        relevant      = take 4 (replicate (4 - length b) empty ++ reverse b) ++ take 5 a
        showMe o ins | o == (cpu & pc) = pcHere ++ showIn o ins ++ normal
        showMe o ins = showIn o ins
        showIn = printf "%04x: %s"
        pcHere = setSGRCode [SetColor Foreground Vivid Red]
        normal = setSGRCode [Reset]
        empty :: (Word16, String) = (0, "")

updateScreen :: CPU -> IO ()
updateScreen cpu = do
  DBG.updateScreen cpu
  clearFromCursorToScreenEnd
  putStr "\n\n"
  cpu & disasm
