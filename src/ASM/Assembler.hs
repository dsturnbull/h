{-# LANGUAGE FlexibleContexts    #-}
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

import Data.Bifunctor
import Data.Either
import Data.Function
import Data.List
import Data.Vector.Storable ((//))
import Data.Word
import Prelude              hiding (all, lines)
import Text.Printf

import qualified Data.Vector.Storable as DVS

assemble :: Word16 -> Word16 -> String -> Program
assemble codeLoc dataLoc prog = Program code
  where inss = parseAssembly prog
        ws   = insPositions CodeSegment (fromIntegral codeLoc) (fromIntegral codeLoc) (fromIntegral dataLoc) (fromIntegral dataLoc) ins ins [] []
        ins  = fromRight [] inss
        code = DVS.replicate (fromIntegral $ codeLoc + 0x100) 0 // ws

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
        s = insLength d
        w = DVS.toList $ DVS.take s ws

insPositions :: Segment -> Int -> Int -> Int -> Int -> [Opcode] -> [Opcode] -> [(Int, Word8)] -> [(Int, Word8)] -> [(Int, Word8)]
insPositions seg coff coff' doff doff' ins (i:is) cs ds =
  case i of
    Code       -> insPositions CodeSegment coff coff' doff doff' ins is cs ds
    Data       -> insPositions DataSegment coff coff' doff doff' ins is cs ds
    (Bytes ws) -> (first (doff' +) <$> zip [0..] ws) ++ insPositions seg coff coff' doff (doff' + length ws)   ins is cs ds
    _          -> (first (coff' +) <$> zip [0..]  c) ++ insPositions seg coff (coff' + insLength i) doff doff' ins is cs ds
  where c = asm coff' seg (fromIntegral coff) (fromIntegral doff) ins i
insPositions _ _ _ _ _ _ [] _ _ = []
