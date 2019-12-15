{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module ASM.Assembler
  ( assemble
  , disasm
  , insPositions
  ) where

import ASM.Parser
import CPU.Instructions.Assembles
import CPU.Instructions.Decodes
import CPU.Instructions.Length
import CPU.Instructions.Opcode
import CPU.Program

import Control.Monad
import Data.Either
import Data.Word
import Prelude       hiding (lines)
import Text.Printf

import qualified Data.Text            as T
import qualified Data.Vector.Storable as DVS

assemble :: T.Text -> Program
assemble prog = Program (DVS.fromList ws)
  where inss = parseAssembly (T.unpack prog)
        ws   = (\(o, i) -> asm o (fmap snd ins) i) =<< ins
        ins  = insPositions 0 (fromRight [] inss)

disasm :: Program -> [(Word16, String)]
disasm (Program prog) = showIns <$> instructions 0 prog
  where showIns (o, w, i) = (fromIntegral o, printf "%-9s %20s %s" (showWords w) ";" (show i))
        showWords ws = foldMap (++ " ") (printf "%02x" <$> ws)

instructions :: Int -> DVS.Vector Word8 -> [(Int, [Word8], Opcode)]
instructions o ws =
  if DVS.length ws > 0
    then (o, w, d) : instructions (o + s) (DVS.drop s ws)
    else []
  where d = decode @Opcode ws
        s = fromIntegral $ insLength d
        w = DVS.toList $ DVS.take s ws

insPositions :: Int -> [Opcode] -> [(Int, Opcode)]
insPositions o (i:is) =
  (o, i) : insPositions (o + s) is
  where s = fromIntegral $ insLength i
insPositions _ [] = []
