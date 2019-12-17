{-# LANGUAGE LambdaCase #-}

module CPU.Instructions.Assembles
  ( Assembles(..)
  , Segment(..)
  , findLabel
  )
  where

import CPU                     (h, l)
import CPU.Instructions.Length
import CPU.Instructions.Opcode
import CPU.Operand
import CPU.Segment

import Data.List
import Data.Maybe
import Data.Word

class Assembles a where
  asm :: Int -> Segment -> Word16 -> Word16 -> [Opcode] -> a -> [Word8]

instance Assembles Opcode where
  asm _ _ _ _ _ (ADC (Imm  w))  = [0x69, w]
  asm _ _ _ _ _ (ADC (Zpg  w))  = [0x65, w]
  asm _ _ _ _ _ (ADC (ZpgX w))  = [0x75, w]
  asm _ _ _ _ _ (ADC (Abs  w))  = [0x6D, l w, h w]
  asm _ _ _ _ _ (ADC (AbsX w))  = [0x7D, l w, h w]
  asm _ _ _ _ _ (ADC (AbsY w))  = [0x79, l w, h w]
  asm _ _ _ _ _ (ADC (IndX w))  = [0x61, w]
  asm _ _ _ _ _ (ADC (IndY w))  = [0x71, w]
  asm _ _ _ _ _ (ADC _)         = undefined

  asm _ _ _ _ _ (AND (Imm  w))  = [0x29, w]
  asm _ _ _ _ _ (AND (Zpg  w))  = [0x25, w]
  asm _ _ _ _ _ (AND (ZpgX w))  = [0x35, w]
  asm _ _ _ _ _ (AND (Abs  w))  = [0x2D, l w, h w]
  asm _ _ _ _ _ (AND (AbsX w))  = [0x3D, l w, h w]
  asm _ _ _ _ _ (AND (AbsY w))  = [0x39, l w, h w]
  asm _ _ _ _ _ (AND (IndX w))  = [0x21, w]
  asm _ _ _ _ _ (AND (IndY w))  = [0x31, w]
  asm _ _ _ _ _ (AND _)         = undefined

  asm _ _ _ _ _ (ASL Acc)       = [0x0A]
  asm _ _ _ _ _ (ASL (Zpg  w))  = [0x06, w]
  asm _ _ _ _ _ (ASL (ZpgX w))  = [0x16, w]
  asm _ _ _ _ _ (ASL (Abs  w))  = [0x0E, l w, h w]
  asm _ _ _ _ _ (ASL (AbsX w))  = [0x1E, l w, h w]
  asm _ _ _ _ _ (ASL _)         = undefined

  asm _ _ _ _ _ (BPL (Rel w))   = [0x10, fromIntegral w]
  asm _ _ _ _ _ (BMI (Rel w))   = [0x30, fromIntegral w]
  asm _ _ _ _ _ (BVC (Rel w))   = [0x50, fromIntegral w]
  asm _ _ _ _ _ (BVS (Rel w))   = [0x70, fromIntegral w]
  asm _ _ _ _ _ (BCC (Rel w))   = [0x90, fromIntegral w]
  asm _ _ _ _ _ (BCS (Rel w))   = [0xB0, fromIntegral w]
  asm _ _ _ _ _ (BNE (Rel w))   = [0xD0, fromIntegral w]
  asm _ _ _ _ _ (BEQ (Rel w))   = [0xF0, fromIntegral w]

  asm o _ c _ a (BPL (Label s)) = 0x10 : relLabel c a o s
  asm o _ c _ a (BMI (Label s)) = 0x30 : relLabel c a o s
  asm o _ c _ a (BVC (Label s)) = 0x50 : relLabel c a o s
  asm o _ c _ a (BVS (Label s)) = 0x70 : relLabel c a o s
  asm o _ c _ a (BCC (Label s)) = 0x90 : relLabel c a o s
  asm o _ c _ a (BCS (Label s)) = 0xB0 : relLabel c a o s
  asm o _ c _ a (BNE (Label s)) = 0xD0 : relLabel c a o s
  asm o _ c _ a (BEQ (Label s)) = 0xF0 : relLabel c a o s

  asm _ _ _ _ _ (BPL _)         = undefined
  asm _ _ _ _ _ (BMI _)         = undefined
  asm _ _ _ _ _ (BVC _)         = undefined
  asm _ _ _ _ _ (BVS _)         = undefined
  asm _ _ _ _ _ (BCC _)         = undefined
  asm _ _ _ _ _ (BCS _)         = undefined
  asm _ _ _ _ _ (BNE _)         = undefined
  asm _ _ _ _ _ (BEQ _)         = undefined

  asm _ _ _ _ _ CLC             = [0x18]
  asm _ _ _ _ _ CLI             = [0x58]
  asm _ _ _ _ _ CLV             = [0xB8]
  asm _ _ _ _ _ CLD             = [0xD8]

  asm _ _ _ _ _ (CMP (Imm  w))  = [0xC9, w]
  asm _ _ _ _ _ (CMP (Zpg  w))  = [0xC5, w]
  asm _ _ _ _ _ (CMP (ZpgX w))  = [0xD5, w]
  asm _ _ _ _ _ (CMP (Abs  w))  = [0xCD, l w, h w]
  asm _ _ _ _ _ (CMP (AbsX w))  = [0xDD, l w, h w]
  asm _ _ _ _ _ (CMP (AbsY w))  = [0xD9, l w, h w]
  asm _ _ _ _ _ (CMP (IndX w))  = [0xC1, w]
  asm _ _ _ _ _ (CMP (IndY w))  = [0xD1, w]
  asm _ _ _ _ _ (CMP _)         = undefined

  asm _ _ _ _ _ (CPX (Imm  w))  = [0xE0, w]
  asm _ _ _ _ _ (CPX (Zpg  w))  = [0xE4, w]
  asm _ _ _ _ _ (CPX (Abs  w))  = [0xEC, l w, h w]
  asm _ _ _ _ _ (CPX _)         = undefined

  asm _ _ _ _ _ (CPY (Imm  w))  = [0xC0, w]
  asm _ _ _ _ _ (CPY (Zpg  w))  = [0xC4, w]
  asm _ _ _ _ _ (CPY (Abs  w))  = [0xCC, l w, h w]
  asm _ _ _ _ _ (CPY _)         = undefined

  asm _ _ _ _ _ (DEC (Zpg w))   = [0xC6, w]
  asm _ _ _ _ _ (DEC (ZpgX w))  = [0xD6, w]
  asm _ _ _ _ _ (DEC (Abs w))   = [0xCE, l w, h w]
  asm _ _ _ _ _ (DEC (AbsX w))  = [0xDE, l w, h w]
  asm _ _ _ _ _ (DEC _)         = undefined

  asm _ _ _ _ _ DEX             = [0xCA]
  asm _ _ _ _ _ DEY             = [0x88]

  asm _ _ _ _ _ (EOR (Imm w))   = [0x49, w]
  asm _ _ _ _ _ (EOR (Zpg w))   = [0x45, w]
  asm _ _ _ _ _ (EOR (ZpgX w))  = [0x55, w]
  asm _ _ _ _ _ (EOR (Abs w))   = [0x4D, l w, h w]
  asm _ _ _ _ _ (EOR (AbsX w))  = [0x5D, l w, h w]
  asm _ _ _ _ _ (EOR (AbsY w))  = [0x59, l w, h w]
  asm _ _ _ _ _ (EOR (IndX w))  = [0x41, w]
  asm _ _ _ _ _ (EOR (IndY w))  = [0x51, w]
  asm _ _ _ _ _ (EOR _)         = undefined

  asm _ _ _ _ _ (INC (Zpg w))   = [0xE6, w]
  asm _ _ _ _ _ (INC (ZpgX w))  = [0xF6, w]
  asm _ _ _ _ _ (INC (Abs w))   = [0xEE, l w, h w]
  asm _ _ _ _ _ (INC (AbsX w))  = [0xFE, l w, h w]
  asm _ _ _ _ _ (INC _)         = undefined

  asm _ _ _ _ _ INX             = [0xE8]
  asm _ _ _ _ _ INY             = [0xC8]

  asm _ _ _ _ _ (JMP (Abs w))   = [0x4C, l w, h w]
  asm _ _ _ _ _ (JMP (Ind w))   = [0x6C, l w, h w]
  asm _ _ c d a (JMP (Label s)) = findLabel Nothing 0x4C c d a s
  asm _ _ _ _ _ (JMP _)         = undefined

  asm _ _ _ _ _ (JSR (Abs w))   = [0x20, l w, h w]
  asm _ _ c d a (JSR (Label s)) = findLabel Nothing 0x20 c d a s
  asm _ _ _ _ _ (JSR _)         = undefined

  asm _ _ _ _ _ (LDA (Imm  w))  = [0xA9, w]
  asm _ _ _ _ _ (LDA (Zpg  w))  = [0xA5, w]
  asm _ _ _ _ _ (LDA (ZpgX w))  = [0xB5, w]
  asm _ _ _ _ _ (LDA (Abs  w))  = [0xAD, l w, h w]
  asm _ _ _ _ _ (LDA (AbsX w))  = [0xBD, l w, h w]
  asm _ _ _ _ _ (LDA (AbsY w))  = [0xB9, l w, h w]
  asm _ _ _ _ _ (LDA (IndX w))  = [0xA1, w]
  asm _ _ _ _ _ (LDA (IndY w))  = [0xB1, w]
  asm _ _ c d a (LDA (Label s)) = findLabel Nothing 0xAD c d a s
  asm _ _ c _ a (LDA (LabelLowByte s))
                              = [0xA9, label c a s !! 0]
  asm _ _ c _ a (LDA (LabelHighByte s))
                              = [0xA9, label c a s !! 1]
  asm _ _ _ _ _ (LDA _)         = undefined

  asm _ _ _ _ _ (LDX (Imm  w))  = [0xA2, w]
  asm _ _ _ _ _ (LDX (Zpg  w))  = [0xA6, w]
  asm _ _ _ _ _ (LDX (ZpgY w))  = [0xB6, w]
  asm _ _ _ _ _ (LDX (Abs  w))  = [0xAE, l w, h w]
  asm _ _ _ _ _ (LDX (AbsY w))  = [0xBE, l w, h w]
  asm _ _ _ _ _ (LDX _)         = undefined

  asm _ _ _ _ _ (LDY (Imm  w))  = [0xA0, w]
  asm _ _ _ _ _ (LDY (Zpg  w))  = [0xA4, w]
  asm _ _ _ _ _ (LDY (ZpgX w))  = [0xB4, w]
  asm _ _ _ _ _ (LDY (Abs  w))  = [0xAC, l w, h w]
  asm _ _ _ _ _ (LDY (AbsX w))  = [0xBC, l w, h w]
  asm _ _ _ _ _ (LDY _)         = undefined

  asm _ _ _ _ _ (LSR Acc)       = [0x4A]
  asm _ _ _ _ _ (LSR (Zpg  w))  = [0x46, w]
  asm _ _ _ _ _ (LSR (ZpgX w))  = [0x56, w]
  asm _ _ _ _ _ (LSR (Abs  w))  = [0x4E, l w, h w]
  asm _ _ _ _ _ (LSR (AbsX w))  = [0x5E, l w, h w]
  asm _ _ _ _ _ (LSR _)         = undefined

  asm _ _ _ _ _ NOP             = [0xEA]

  asm _ _ _ _ _ (ORA (Imm w))   = [0x09, w]
  asm _ _ _ _ _ (ORA (Zpg w))   = [0x05, w]
  asm _ _ _ _ _ (ORA (ZpgX w))  = [0x15, w]
  asm _ _ _ _ _ (ORA (Abs w))   = [0x0D, l w, h w]
  asm _ _ _ _ _ (ORA (AbsX w))  = [0x1D, l w, h w]
  asm _ _ _ _ _ (ORA (AbsY w))  = [0x19, l w, h w]
  asm _ _ _ _ _ (ORA (IndX w))  = [0x01, w]
  asm _ _ _ _ _ (ORA (IndY w))  = [0x11, w]
  asm _ _ _ _ _ (ORA _)         = undefined

  asm _ _ _ _ _ PHA             = [0x48]
  asm _ _ _ _ _ PHP             = [0x08]
  asm _ _ _ _ _ PLA             = [0x68]
  asm _ _ _ _ _ PLP             = [0x28]

  asm _ _ _ _ _ (ROL Acc)       = [0xAA]
  asm _ _ _ _ _ (ROL (Zpg  w))  = [0x26, w]
  asm _ _ _ _ _ (ROL (ZpgX w))  = [0x36, w]
  asm _ _ _ _ _ (ROL (Abs  w))  = [0x2E, l w, h w]
  asm _ _ _ _ _ (ROL (AbsX w))  = [0x3E, l w, h w]
  asm _ _ _ _ _ (ROL _)         = undefined

  asm _ _ _ _ _ (ROR Acc)       = [0x6A]
  asm _ _ _ _ _ (ROR (Zpg  w))  = [0x66, w]
  asm _ _ _ _ _ (ROR (ZpgX w))  = [0x76, w]
  asm _ _ _ _ _ (ROR (Abs  w))  = [0x6E, l w, h w]
  asm _ _ _ _ _ (ROR (AbsX w))  = [0x7E, l w, h w]
  asm _ _ _ _ _ (ROR _)         = undefined

  asm _ _ _ _ _ RTI             = [0x40]
  asm _ _ _ _ _ RTS             = [0x60]

  asm _ _ _ _ _ (SBC (Imm  w))  = [0xE9, w]
  asm _ _ _ _ _ (SBC (Zpg  w))  = [0xE5, w]
  asm _ _ _ _ _ (SBC (ZpgX w))  = [0xF5, w]
  asm _ _ _ _ _ (SBC (Abs  w))  = [0xED, l w, h w]
  asm _ _ _ _ _ (SBC (AbsX w))  = [0xFD, l w, h w]
  asm _ _ _ _ _ (SBC (AbsY w))  = [0xF9, l w, h w]
  asm _ _ _ _ _ (SBC (IndX w))  = [0xE1, w]
  asm _ _ _ _ _ (SBC (IndY w))  = [0xF1, w]
  asm _ _ _ _ _ (SBC _)         = undefined

  asm _ _ _ _ _ SEC             = [0x38]
  asm _ _ _ _ _ SEI             = [0x78]
  asm _ _ _ _ _ SED             = [0xF8]

  asm _ _ _ _ _ (STA (Zpg  w))  = [0x85, w]
  asm _ _ _ _ _ (STA (ZpgX w))  = [0x95, w]
  asm _ _ _ _ _ (STA (Abs  w))  = [0x8D, l w, h w]
  asm _ _ _ _ _ (STA (AbsX w))  = [0x9D, l w, h w]
  asm _ _ _ _ _ (STA (AbsY w))  = [0x99, l w, h w]
  asm _ _ _ _ _ (STA (IndX w))  = [0x81, w]
  asm _ _ _ _ _ (STA (IndY w))  = [0x91, w]
  asm _ _ _ _ _ (STA _)         = undefined

  asm _ _ _ _ _ (STX (Zpg  w))  = [0x86, w]
  asm _ _ _ _ _ (STX (ZpgY w))  = [0x96, w]
  asm _ _ _ _ _ (STX (Abs  w))  = [0x8E, l w, h w]
  asm _ _ _ _ _ (STX _)         = undefined

  asm _ _ _ _ _ (STY (Zpg  w))  = [0x84, w]
  asm _ _ _ _ _ (STY (ZpgX w))  = [0x94, w]
  asm _ _ _ _ _ (STY (Abs  w))  = [0x8C, l w, h w]
  asm _ _ _ _ _ (STY _)         = undefined

  asm _ _ _ _ _ TAX             = [0xAA]
  asm _ _ _ _ _ TXA             = [0x8A]
  asm _ _ _ _ _ TAY             = [0xA8]
  asm _ _ _ _ _ TYA             = [0x98]
  asm _ _ _ _ _ TSX             = [0x9A]
  asm _ _ _ _ _ TXS             = [0xBA]

  asm _ _ _ _ _ BRK             = [0x00]

  asm _ _ _ _ _ (LabelDef _)    = []
  asm _ _ _ _ _ Code            = []
  asm _ _ _ _ _ Data            = []
  asm _ _ _ _ _ (Bytes ws)      = ws

label :: Word16 -> [Opcode] -> String -> [Word8]
label base a s =
  case findIndex (\case
                    LabelDef s' -> s' == s
                    _           -> False) a of
    Just p  -> [l w, h w]
      where w = base + (fromIntegral . sum $ insLength <$> fst (splitAt p a))
    Nothing -> error $ "cannot find label " <> s

relLabel :: Word16 -> [Opcode] -> Int -> String -> [Word8]
relLabel base a o s =
  case findIndex (\case
                    LabelDef s' -> s' == s
                    _           -> False) a of
    Just p  -> [fromIntegral w]
      where w = fromIntegral base + (fromIntegral . sum $ insLength <$> fst (splitAt p a)) - o
    Nothing -> error $ "cannot find label " <> s

findLabel :: Maybe Word8 -> Word8 -> Word16 -> Word16 -> [Opcode] -> String -> [Word8]
findLabel _ a c d ins s =
  case seg of
    CodeSegment -> [a, l codeLoc, h codeLoc]
    DataSegment -> [a, l dataLoc, h dataLoc]
  where
    (codes, datas) = splitAt (fromMaybe (length ins) $ elemIndex Data ins) ins
    codeLoc  = fromIntegral . fst . fromMaybe (0, NOP) $ find (\(_, i) -> case i of LabelDef s' -> s' == s; _           -> False) codeLocs
    dataLoc  = fromIntegral . fst . fromMaybe (0, NOP) $ find (\(_, i) -> case i of LabelDef s' -> s' == s; _           -> False) dataLocs
    codeLocs = snd $ mapAccumL (\p i -> (p + insLength i, (p + insLength i, i))) (fromIntegral c) codes
    dataLocs = snd $ mapAccumL (\p i -> (p + insLength i, (p + insLength i, i))) (fromIntegral d) datas
    seg      = if insPos >= length codes then DataSegment else CodeSegment
    insPos   = fromMaybe 0 $ findIndex (\case LabelDef s' -> s' == s; _           -> False) ins
