{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module ASM.Assembles
  ( Assembles(..)
  , Segment(..)
  , findLabel
  ) where

import ASM.Length
import ASM.Operand
import ASM.Segment

import Data.Bits
import Data.List
import Data.Maybe
import Data.Word

class Assembles a where
  asm :: a -> Int -> Word16 -> Word16 -> [Opcode] -> [Word8]

instance Assembles Opcode where
  asm (ADC (Imm  w))      _ _ _ _ = [0x69, w]
  asm (ADC (Zpg  w))      _ _ _ _ = [0x65, w]
  asm (ADC (ZpgX w))      _ _ _ _ = [0x75, w]
  asm (ADC (Abs  w))      _ _ _ _ = [0x6D, lo w, hi w]
  asm (ADC (AbsX w))      _ _ _ _ = [0x7D, lo w, hi w]
  asm (ADC (AbsY w))      _ _ _ _ = [0x79, lo w, hi w]
  asm (ADC (IndX w))      _ _ _ _ = [0x61, w]
  asm (ADC (IndY w))      _ _ _ _ = [0x71, w]
  asm (ADC l@(Label _ s)) f c d a = asm (unLabelWith @Integer ADC (findLabel c d a s) l) f c d a
  asm (ADC _)             _ _ _ _ = undefined

  asm (AND (Imm  w))      _ _ _ _ = [0x29, w]
  asm (AND (Zpg  w))      _ _ _ _ = [0x25, w]
  asm (AND (ZpgX w))      _ _ _ _ = [0x35, w]
  asm (AND (Abs  w))      _ _ _ _ = [0x2D, lo w, hi w]
  asm (AND (AbsX w))      _ _ _ _ = [0x3D, lo w, hi w]
  asm (AND (AbsY w))      _ _ _ _ = [0x39, lo w, hi w]
  asm (AND (IndX w))      _ _ _ _ = [0x21, w]
  asm (AND (IndY w))      _ _ _ _ = [0x31, w]
  asm (AND l@(Label _ s)) f c d a = asm (unLabelWith @Integer AND (findLabel c d a s) l) f c d a
  asm (AND _)             _ _ _ _ = undefined

  asm (ASL Acc)           _ _ _ _ = [0x0A]
  asm (ASL (Zpg  w))      _ _ _ _ = [0x06, w]
  asm (ASL (ZpgX w))      _ _ _ _ = [0x16, w]
  asm (ASL (Abs  w))      _ _ _ _ = [0x0E, lo w, hi w]
  asm (ASL (AbsX w))      _ _ _ _ = [0x1E, lo w, hi w]
  asm (ASL l@(Label _ s)) f c d a = asm (unLabelWith @Integer ASL (findLabel c d a s) l) f c d a
  asm (ASL _)             _ _ _ _ = undefined

  asm (BPL (Rel w))       _ _ _ _ = [0x10, fromIntegral w]
  asm (BMI (Rel w))       _ _ _ _ = [0x30, fromIntegral w]
  asm (BVC (Rel w))       _ _ _ _ = [0x50, fromIntegral w]
  asm (BVS (Rel w))       _ _ _ _ = [0x70, fromIntegral w]
  asm (BCC (Rel w))       _ _ _ _ = [0x90, fromIntegral w]
  asm (BCS (Rel w))       _ _ _ _ = [0xB0, fromIntegral w]
  asm (BNE (Rel w))       _ _ _ _ = [0xD0, fromIntegral w]
  asm (BEQ (Rel w))       _ _ _ _ = [0xF0, fromIntegral w]

  asm (BPL l@(Label _ s)) o c d a = asm (unLabelWith @Integer BPL (findLabel c d a s - fromIntegral o) l) o c d a
  asm (BMI l@(Label _ s)) o c d a = asm (unLabelWith @Integer BMI (findLabel c d a s - fromIntegral o) l) o c d a
  asm (BVC l@(Label _ s)) o c d a = asm (unLabelWith @Integer BVC (findLabel c d a s - fromIntegral o) l) o c d a
  asm (BVS l@(Label _ s)) o c d a = asm (unLabelWith @Integer BVS (findLabel c d a s - fromIntegral o) l) o c d a
  asm (BCC l@(Label _ s)) o c d a = asm (unLabelWith @Integer BCC (findLabel c d a s - fromIntegral o) l) o c d a
  asm (BCS l@(Label _ s)) o c d a = asm (unLabelWith @Integer BCS (findLabel c d a s - fromIntegral o) l) o c d a
  asm (BNE l@(Label _ s)) o c d a = asm (unLabelWith @Integer BNE (findLabel c d a s - fromIntegral o) l) o c d a
  asm (BEQ l@(Label _ s)) o c d a = asm (unLabelWith @Integer BEQ (findLabel c d a s - fromIntegral o) l) o c d a

  asm (BPL _)             _ _ _ _ = undefined
  asm (BMI _)             _ _ _ _ = undefined
  asm (BVC _)             _ _ _ _ = undefined
  asm (BVS _)             _ _ _ _ = undefined
  asm (BCC _)             _ _ _ _ = undefined
  asm (BCS _)             _ _ _ _ = undefined
  asm (BNE _)             _ _ _ _ = undefined
  asm (BEQ _)             _ _ _ _ = undefined

  asm CLC                 _ _ _ _ = [0x18]
  asm CLI                 _ _ _ _ = [0x58]
  asm CLV                 _ _ _ _ = [0xB8]
  asm CLD                 _ _ _ _ = [0xD8]

  asm (CMP (Imm  w))      _ _ _ _ = [0xC9, w]
  asm (CMP (Zpg  w))      _ _ _ _ = [0xC5, w]
  asm (CMP (ZpgX w))      _ _ _ _ = [0xD5, w]
  asm (CMP (Abs  w))      _ _ _ _ = [0xCD, lo w, hi w]
  asm (CMP (AbsX w))      _ _ _ _ = [0xDD, lo w, hi w]
  asm (CMP (AbsY w))      _ _ _ _ = [0xD9, lo w, hi w]
  asm (CMP (IndX w))      _ _ _ _ = [0xC1, w]
  asm (CMP (IndY w))      _ _ _ _ = [0xD1, w]
  asm (CMP l@(Label _ s)) f c d a = asm (unLabelWith @Integer CMP (findLabel c d a s) l) f c d a
  asm (CMP _)             _ _ _ _ = undefined

  asm (CPX (Imm  w))      _ _ _ _ = [0xE0, w]
  asm (CPX (Zpg  w))      _ _ _ _ = [0xE4, w]
  asm (CPX (Abs  w))      _ _ _ _ = [0xEC, lo w, hi w]
  asm (CPX l@(Label _ s)) f c d a = asm (unLabelWith @Integer CPX (findLabel c d a s) l) f c d a
  asm (CPX _)             _ _ _ _ = undefined

  asm (CPY (Imm  w))      _ _ _ _ = [0xC0, w]
  asm (CPY (Zpg  w))      _ _ _ _ = [0xC4, w]
  asm (CPY (Abs  w))      _ _ _ _ = [0xCC, lo w, hi w]
  asm (CPY l@(Label _ s)) f c d a = asm (unLabelWith @Integer CPY (findLabel c d a s) l) f c d a
  asm (CPY _)             _ _ _ _ = undefined

  asm (DEC (Zpg w))       _ _ _ _ = [0xC6, w]
  asm (DEC (ZpgX w))      _ _ _ _ = [0xD6, w]
  asm (DEC (Abs w))       _ _ _ _ = [0xCE, lo w, hi w]
  asm (DEC (AbsX w))      _ _ _ _ = [0xDE, lo w, hi w]
  asm (DEC l@(Label _ s)) f c d a = asm (unLabelWith @Integer DEC (findLabel c d a s) l) f c d a
  asm (DEC _)             _ _ _ _ = undefined

  asm DEX                 _ _ _ _ = [0xCA]
  asm DEY                 _ _ _ _ = [0x88]

  asm (EOR (Imm w))       _ _ _ _ = [0x49, w]
  asm (EOR (Zpg w))       _ _ _ _ = [0x45, w]
  asm (EOR (ZpgX w))      _ _ _ _ = [0x55, w]
  asm (EOR (Abs w))       _ _ _ _ = [0x4D, lo w, hi w]
  asm (EOR (AbsX w))      _ _ _ _ = [0x5D, lo w, hi w]
  asm (EOR (AbsY w))      _ _ _ _ = [0x59, lo w, hi w]
  asm (EOR (IndX w))      _ _ _ _ = [0x41, w]
  asm (EOR (IndY w))      _ _ _ _ = [0x51, w]
  asm (EOR l@(Label _ s)) f c d a = asm (unLabelWith @Integer EOR (findLabel c d a s) l) f c d a
  asm (EOR _)             _ _ _ _ = undefined

  asm (INC (Zpg w))       _ _ _ _ = [0xE6, w]
  asm (INC (ZpgX w))      _ _ _ _ = [0xF6, w]
  asm (INC (Abs w))       _ _ _ _ = [0xEE, lo w, hi w]
  asm (INC (AbsX w))      _ _ _ _ = [0xFE, lo w, hi w]
  asm (INC l@(Label _ s)) f c d a = asm (unLabelWith @Integer INC (findLabel c d a s) l) f c d a
  asm (INC _)             _ _ _ _ = undefined

  asm INX                 _ _ _ _ = [0xE8]
  asm INY                 _ _ _ _ = [0xC8]

  asm (JMP (Abs w))       _ _ _ _ = [0x4C, lo w, hi w]
  asm (JMP (Ind w))       _ _ _ _ = [0x6C, lo w, hi w]
  asm (JMP l@(Label _ s)) f c d a = asm (unLabelWith @Integer JMP (findLabel c d a s) l) f c d a
  asm (JMP _)             _ _ _ _ = undefined

  asm (JSR (Abs w))       _ _ _ _ = [0x20, lo w, hi w]
  asm (JSR l@(Label _ s)) f c d a = asm (unLabelWith @Integer JSR (findLabel c d a s) l) f c d a
  asm (JSR _)             _ _ _ _ = undefined

  asm (LDA (Imm  w))      _ _ _ _ = [0xA9, w]
  asm (LDA (Zpg  w))      _ _ _ _ = [0xA5, w]
  asm (LDA (ZpgX w))      _ _ _ _ = [0xB5, w]
  asm (LDA (Abs  w))      _ _ _ _ = [0xAD, lo w, hi w]
  asm (LDA (AbsX w))      _ _ _ _ = [0xBD, lo w, hi w]
  asm (LDA (AbsY w))      _ _ _ _ = [0xB9, lo w, hi w]
  asm (LDA (IndX w))      _ _ _ _ = [0xA1, w]
  asm (LDA (IndY w))      _ _ _ _ = [0xB1, w]
  asm (LDA l@(Label _ s)) f c d a = asm (unLabelWith @Integer LDA (findLabel c d a s) l) f c d a
  asm (LDA _)             _ _ _ _ = undefined

  asm (LDX (Imm  w))      _ _ _ _ = [0xA2, w]
  asm (LDX (Zpg  w))      _ _ _ _ = [0xA6, w]
  asm (LDX (ZpgY w))      _ _ _ _ = [0xB6, w]
  asm (LDX (Abs  w))      _ _ _ _ = [0xAE, lo w, hi w]
  asm (LDX (AbsY w))      _ _ _ _ = [0xBE, lo w, hi w]
  asm (LDX l@(Label _ s)) f c d a = asm (unLabelWith @Integer LDX (findLabel c d a s) l) f c d a
  asm (LDX _)             _ _ _ _ = undefined

  asm (LDY (Imm  w))      _ _ _ _ = [0xA0, w]
  asm (LDY (Zpg  w))      _ _ _ _ = [0xA4, w]
  asm (LDY (ZpgX w))      _ _ _ _ = [0xB4, w]
  asm (LDY (Abs  w))      _ _ _ _ = [0xAC, lo w, hi w]
  asm (LDY (AbsX w))      _ _ _ _ = [0xBC, lo w, hi w]
  asm (LDY l@(Label _ s)) f c d a = asm (unLabelWith @Integer LDY (findLabel c d a s) l) f c d a
  asm (LDY _)             _ _ _ _ = undefined

  asm (LSR Acc)           _ _ _ _ = [0x4A]
  asm (LSR (Zpg  w))      _ _ _ _ = [0x46, w]
  asm (LSR (ZpgX w))      _ _ _ _ = [0x56, w]
  asm (LSR (Abs  w))      _ _ _ _ = [0x4E, lo w, hi w]
  asm (LSR (AbsX w))      _ _ _ _ = [0x5E, lo w, hi w]
  asm (LSR l@(Label _ s)) f c d a = asm (unLabelWith @Integer LSR (findLabel c d a s) l) f c d a
  asm (LSR _)             _ _ _ _ = undefined

  asm NOP _ _ _ _                 = [0xEA]

  asm (ORA (Imm w))       _ _ _ _ = [0x09, w]
  asm (ORA (Zpg w))       _ _ _ _ = [0x05, w]
  asm (ORA (ZpgX w))      _ _ _ _ = [0x15, w]
  asm (ORA (Abs w))       _ _ _ _ = [0x0D, lo w, hi w]
  asm (ORA (AbsX w))      _ _ _ _ = [0x1D, lo w, hi w]
  asm (ORA (AbsY w))      _ _ _ _ = [0x19, lo w, hi w]
  asm (ORA (IndX w))      _ _ _ _ = [0x01, w]
  asm (ORA (IndY w))      _ _ _ _ = [0x11, w]
  asm (ORA l@(Label _ s)) f c d a = asm (unLabelWith @Integer ORA (findLabel c d a s) l) f c d a
  asm (ORA _)             _ _ _ _ = undefined

  asm PHA                 _ _ _ _ = [0x48]
  asm PHP                 _ _ _ _ = [0x08]
  asm PLA                 _ _ _ _ = [0x68]
  asm PLP                 _ _ _ _ = [0x28]

  asm (ROL Acc)           _ _ _ _ = [0xAA]
  asm (ROL (Zpg  w))      _ _ _ _ = [0x26, w]
  asm (ROL (ZpgX w))      _ _ _ _ = [0x36, w]
  asm (ROL (Abs  w))      _ _ _ _ = [0x2E, lo w, hi w]
  asm (ROL (AbsX w))      _ _ _ _ = [0x3E, lo w, hi w]
  asm (ROL l@(Label _ s)) f c d a = asm (unLabelWith @Integer ROL (findLabel c d a s) l) f c d a
  asm (ROL _)             _ _ _ _ = undefined

  asm (ROR Acc)           _ _ _ _ = [0x6A]
  asm (ROR (Zpg  w))      _ _ _ _ = [0x66, w]
  asm (ROR (ZpgX w))      _ _ _ _ = [0x76, w]
  asm (ROR (Abs  w))      _ _ _ _ = [0x6E, lo w, hi w]
  asm (ROR (AbsX w))      _ _ _ _ = [0x7E, lo w, hi w]
  asm (ROR l@(Label _ s)) f c d a = asm (unLabelWith @Integer ROR (findLabel c d a s) l) f c d a
  asm (ROR _)             _ _ _ _ = undefined

  asm RTI                 _ _ _ _ = [0x40]
  asm RTS                 _ _ _ _ = [0x60]

  asm (SBC (Imm  w))      _ _ _ _ = [0xE9, w]
  asm (SBC (Zpg  w))      _ _ _ _ = [0xE5, w]
  asm (SBC (ZpgX w))      _ _ _ _ = [0xF5, w]
  asm (SBC (Abs  w))      _ _ _ _ = [0xED, lo w, hi w]
  asm (SBC (AbsX w))      _ _ _ _ = [0xFD, lo w, hi w]
  asm (SBC (AbsY w))      _ _ _ _ = [0xF9, lo w, hi w]
  asm (SBC (IndX w))      _ _ _ _ = [0xE1, w]
  asm (SBC (IndY w))      _ _ _ _ = [0xF1, w]
  asm (SBC l@(Label _ s)) f c d a = asm (unLabelWith @Integer SBC (findLabel c d a s) l) f c d a
  asm (SBC _)             _ _ _ _ = undefined

  asm SEC                 _ _ _ _ = [0x38]
  asm SEI                 _ _ _ _ = [0x78]
  asm SED                 _ _ _ _ = [0xF8]

  asm (STA (Zpg  w))      _ _ _ _ = [0x85, w]
  asm (STA (ZpgX w))      _ _ _ _ = [0x95, w]
  asm (STA (Abs  w))      _ _ _ _ = [0x8D, lo w, hi w]
  asm (STA (AbsX w))      _ _ _ _ = [0x9D, lo w, hi w]
  asm (STA (AbsY w))      _ _ _ _ = [0x99, lo w, hi w]
  asm (STA (IndX w))      _ _ _ _ = [0x81, w]
  asm (STA (IndY w))      _ _ _ _ = [0x91, w]
  asm (STA l@(Label _ s)) f c d a = asm (unLabelWith @Integer STA (findLabel c d a s) l) f c d a
  asm (STA _)             _ _ _ _ = undefined

  asm (STX (Zpg  w))      _ _ _ _ = [0x86, w]
  asm (STX (ZpgY w))      _ _ _ _ = [0x96, w]
  asm (STX (Abs  w))      _ _ _ _ = [0x8E, lo w, hi w]
  asm (STX l@(Label _ s)) f c d a = asm (unLabelWith @Integer STX (findLabel c d a s) l) f c d a
  asm (STX _)             _ _ _ _ = undefined

  asm (STY (Zpg  w))      _ _ _ _ = [0x84, w]
  asm (STY (ZpgX w))      _ _ _ _ = [0x94, w]
  asm (STY (Abs  w))      _ _ _ _ = [0x8C, lo w, hi w]
  asm (STY l@(Label _ s)) f c d a = asm (unLabelWith @Integer STY (findLabel c d a s) l) f c d a
  asm (STY _)             _ _ _ _ = undefined

  asm TAX                 _ _ _ _ = [0xAA]
  asm TXA                 _ _ _ _ = [0x8A]
  asm TAY                 _ _ _ _ = [0xA8]
  asm TYA                 _ _ _ _ = [0x98]
  asm TSX                 _ _ _ _ = [0x9A]
  asm TXS                 _ _ _ _ = [0xBA]

  asm BRK                 _ _ _ _ = [0x00]

  -- handled in the actual assembler
  asm (LabelDef _)        _ _ _ _ = []
  asm (Variable8 _ _)     _ _ _ _ = []
  asm (Variable16 _ _)    _ _ _ _ = []
  asm Code                _ _ _ _ = []
  asm Data                _ _ _ _ = []
  asm (Bytes _)           _ _ _ _ = []
  asm (Origin _)          _ _ _ _ = []

findLabel :: Num a => Word16 -> Word16 -> [Opcode] -> String -> a
findLabel c d ins s =
  case seg of
    CodeSegment     -> codeLoc
    DataSegment     -> dataVal
    OffsetSegment _ -> undefined
  where
    (datas, codes) = splitAt (fromMaybe (length ins) $ elemIndex Code ins) ins
    codeLoc   = findLoc (locate c codes)
    dataVal   = findLoc (locate d datas)
    findLoc i =
      case findVal i of
        (loc, LabelDef _)    -> fromIntegral loc
        (_, Variable8 _ l')  -> fromIntegral l'
        (_, Variable16 _ l') -> fromIntegral l'
        _                    -> undefined
    findVal   = fromMaybe (undefined, NOP) . find (\(_, i) -> case i of LabelDef s'     -> s' == s
                                                                        Variable8 s' _  -> s' == s
                                                                        Variable16 s' _ -> s' == s
                                                                        _               -> False)
    locate o  = snd . mapAccumL (\p i -> (p + insLength i, (p + insLength i, i))) (fromIntegral o)
    seg       = if insPos >= length datas then CodeSegment else DataSegment
    insPos    = fromMaybe 0 $ findIndex (\case LabelDef s'     -> s' == s
                                               Variable8 s' _  -> s' == s
                                               Variable16 s' _ -> s' == s
                                               _               -> False) ins

hi :: Word16 -> Word8
hi w = fromIntegral $ w `shiftR` 8

lo :: Word16 -> Word8
lo w = fromIntegral $ w .&. 0x00ff
