{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module ASM.Assembler
  ( assemble
  , assembleOpcodes
  , disasm
  , insPositions
  , writeProgram
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
import Data.Word
import Prelude        hiding (all, lines)
import Text.Printf

import qualified Data.ByteString.Builder         as BB
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.Text                       as T
import qualified Data.Vector.Storable            as DVS
import qualified Data.Vector.Storable.ByteString as DVSB

assemble :: T.Text -> Word16 -> Word16 -> Program
assemble prog = assembleOpcodes ins
  where ins = fromRight [] $ parseAssembly (T.unpack prog)

assembleOpcodes :: [Opcode] -> Word16 -> Word16 -> Program
assembleOpcodes ins codeLoc dataLoc = Program (codeLoc, DVS.fromList (snd <$> sortOn fst cs)) (dataLoc, DVS.fromList (snd <$> sortOn fst ds))
  where (cs, ds) = insPositions DataSegment (fromIntegral codeLoc) (fromIntegral codeLoc) (fromIntegral dataLoc) (fromIntegral dataLoc) ins ins [] []

disasm :: Program -> ([(Word16, T.Text)], [(Word16, T.Text)])
disasm (Program (coff, cs') (doff, ds')) = (showIns <$> instructions (fromIntegral coff) cs', showData <$> bytes (fromIntegral doff) ds')
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

insPositions :: Segment -> Int -> Int -> Int -> Int -> [Opcode] -> [Opcode] -> [(Int, Word8)] -> [(Int, Word8)] -> ([(Int, Word8)], [(Int, Word8)])
insPositions seg coff coff' doff doff' ins (i:is) cs ds =
  case i of
    Code       -> insPositions CodeSegment coff coff' doff doff' ins is cs ds
    Data       -> insPositions DataSegment coff coff' doff doff' ins is cs ds
    (Bytes ws) -> insPositions seg coff coff' doff (doff' + length ws)   ins is cs ((first (doff' +) <$> zip [0..] ws) ++ ds)
    _          -> insPositions seg coff (coff' + insLength i) doff doff' ins is    ((first (coff' +) <$> zip [0..]  c) ++ cs) ds
  where c = asm coff' seg (fromIntegral coff) (fromIntegral doff) ins i
insPositions _ _ _ _ _ _ [] cs ds = (cs, ds)

writeProgram :: Program -> LBS.ByteString
writeProgram (Program (cloc, cdat) (dloc, ddat)) =
  cloc' <> clen' <> cdat' <> dloc' <> dlen' <> ddat'
  where cloc' = BB.toLazyByteString . BB.word16BE $ cloc
        clen' = BB.toLazyByteString . BB.word16BE . fromIntegral $ DVS.length cdat
        cdat' = LBS.fromStrict . DVSB.vectorToByteString $ cdat
        dloc' = BB.toLazyByteString . BB.word16BE $ dloc
        dlen' = BB.toLazyByteString . BB.word16BE . fromIntegral $ DVS.length ddat
        ddat' = LBS.fromStrict . DVSB.vectorToByteString $ ddat
