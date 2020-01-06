{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CPU.Disassembler
  ( disasm
  ) where

import ASM.Length
import ASM.Operand
import ASM.Program
import CPU.Instructions.Decodes

import Data.Function
import Data.List
import Data.Word
import Prelude       hiding (all, lines)
import Text.Printf

import qualified Data.Text            as T
import qualified Data.Vector.Storable as DVS

disasm :: Program -> ([(Word16, T.Text)], [(Word16, T.Text)])
disasm (Program (coff, cs') (doff, ds') _) = (showIns <$> instructions (fromIntegral coff) cs', showData <$> bytes (fromIntegral doff) ds')
  where showIns (o, w, i)  = (fromIntegral o, T.pack $ printf "%-9s %20s %s" (showWords w) ";" (show i))
        showData (o, w) = (fromIntegral o, T.pack $ printf "%02x" w)
        showWords ws = foldMap (++ " ") (printf "%02x" <$> ws)

instructions :: Int -> DVS.Vector Word8 -> [(Int, [Word8], Opcode)]
instructions o ws =
  if DVS.length ws > 0
    then (o, w, d) : instructions (o + s) (DVS.drop s ws)
    else []
  where d = decode @Opcode ws
        s = insLength d
        w = DVS.toList $ DVS.take s ws

bytes :: Int -> DVS.Vector Word8 -> [(Int, Word8)]
bytes o ws =
  if DVS.length ws > 0
    then (o, w) : bytes (o + 1) (DVS.drop 1 ws)
    else []
  where w = head . DVS.toList $ DVS.take 1 ws
