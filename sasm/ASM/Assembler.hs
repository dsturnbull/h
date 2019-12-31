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
import qualified Data.Map.Strict                 as M
import qualified Data.Text                       as T
import qualified Data.Vector.Storable            as DVS
import qualified Data.Vector.Storable.ByteString as DVSB

assemble :: T.Text -> Word16 -> Word16 -> IO Program
assemble prog coff doff = do
  ins <- fromRight [] <$> parseAssembly (T.unpack prog)
  return $ assembleOpcodes ins coff doff

assembleOpcodes :: [Opcode] -> Word16 -> Word16 -> Program
assembleOpcodes ins codeLoc dataLoc = Program (codeLoc, wo cs) (dataLoc, wo ds) other
  where (cs, ds, oss) = insPositions DataSegment 0 (fromIntegral codeLoc) (fromIntegral codeLoc) (fromIntegral dataLoc) (fromIntegral dataLoc) ins ins [] [] []
        other :: [(Word16, DVS.Vector Word8)] = second wo <$> M.toList (M.fromListWith (++) oss)
        wo seg = DVS.fromList (snd <$> sortOn fst seg)

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

insPositions :: ()
  => Segment -> Int -> Int -> Int -> Int -> Int
  -> [Opcode] -> [Opcode]
  -> [(Int, Word8)] -> [(Int, Word8)] -> [(Word16, [(Int, Word8)])]
  -> ([(Int, Word8)], [(Int, Word8)], [(Word16, [(Int, Word8)])])
insPositions seg segOff coff coff' doff doff' ins (i:is) cs ds oss =
  case i of
    Data       -> insPositions DataSegment            0 coff coff' doff doff' ins is cs ds oss
    Origin l   -> insPositions (OffsetSegment l)      0 coff coff' doff doff' ins is cs ds oss
    Code       -> insPositions CodeSegment            0 coff coff' doff doff' ins is cs ds oss
    (Bytes ws) ->
      case seg of
        DataSegment ->
                  insPositions seg segOff coff coff' doff (doff' + length ws)   ins is cs ((first (doff' +) <$> zip [0..] ws) ++ ds) oss
        OffsetSegment l ->
                  insPositions seg (segOff + length ws) coff coff' doff  doff'  ins is cs ds ((l, first (fromIntegral l + segOff +)
                                                                                                            <$> zip [0..] ws) : oss)
        CodeSegment -> undefined -- we shouldn't put bytes in code, I think
    _          -> insPositions seg segOff coff (coff' + insLength i) doff doff' ins is    ((first (coff' +) <$> zip [0..]  c) ++ cs) ds oss
  where c = asm coff' seg (fromIntegral coff) (fromIntegral doff) ins i
insPositions _ _ _ _ _ _ _ [] cs ds oss = (cs, ds, oss)

writeProgram :: Program -> LBS.ByteString
writeProgram (Program c d others) = seg c <> seg d <> otherN <> others'
  where
    otherN  = BB.toLazyByteString . BB.word8 . fromIntegral $ length others
    others' = foldMap (<>) (seg <$> others) mempty
    seg (loc, dat) = loc' <> len' <> dat'
      where loc' = BB.toLazyByteString . BB.word16BE $ loc
            len' = BB.toLazyByteString . BB.word16BE . fromIntegral $ DVS.length dat
            dat' = LBS.fromStrict . DVSB.vectorToByteString $ dat
