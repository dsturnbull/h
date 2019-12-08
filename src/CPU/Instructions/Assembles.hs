module CPU.Instructions.Assembles
  ( Assembles(..)
  , h
  , l
  )
  where

import CPU.Instructions.Length
import CPU.Instructions.Opcode
import CPU.Operand

import Data.Bits
import Data.List
import Data.Word

class Assembles a where
  asm :: Int -> [Opcode] -> a -> [Word8]

instance Assembles Opcode where
  asm _ _ (ADC (Imm  w))  = [0x69, w]
  asm _ _ (ADC (Zpg  w))  = [0x65, w]
  asm _ _ (ADC (ZpgX w))  = [0x75, w]
  asm _ _ (ADC (Abs  w))  = [0x6D, l w, h w]
  asm _ _ (ADC (AbsX w))  = [0x7D, l w, h w]
  asm _ _ (ADC (AbsY w))  = [0x79, l w, h w]
  asm _ _ (ADC (IndX w))  = [0x61, w]
  asm _ _ (ADC (IndY w))  = [0x71, w]
  asm _ _ (ADC _)         = undefined

  asm _ _ (AND (Imm  w))  = [0x29, w]
  asm _ _ (AND (Zpg  w))  = [0x25, w]
  asm _ _ (AND (ZpgX w))  = [0x35, w]
  asm _ _ (AND (Abs  w))  = [0x2D, l w, h w]
  asm _ _ (AND (AbsX w))  = [0x3D, l w, h w]
  asm _ _ (AND (AbsY w))  = [0x39, l w, h w]
  asm _ _ (AND (IndX w))  = [0x21, w]
  asm _ _ (AND (IndY w))  = [0x31, w]
  asm _ _ (AND _)         = undefined

  asm _ _ (ASL Acc)       = [0x0A]
  asm _ _ (ASL (Zpg  w))  = [0x06, w]
  asm _ _ (ASL (ZpgX w))  = [0x16, w]
  asm _ _ (ASL (Abs  w))  = [0x0E, l w, h w]
  asm _ _ (ASL (AbsX w))  = [0x1E, l w, h w]
  asm _ _ (ASL _)         = undefined

  asm _ _ (BPL (Rel w))   = [0x10, fromIntegral w]
  asm _ _ (BMI (Rel w))   = [0x30, fromIntegral w]
  asm _ _ (BVC (Rel w))   = [0x50, fromIntegral w]
  asm _ _ (BVS (Rel w))   = [0x70, fromIntegral w]
  asm _ _ (BCC (Rel w))   = [0x90, fromIntegral w]
  asm _ _ (BCS (Rel w))   = [0xB0, fromIntegral w]
  asm _ _ (BNE (Rel w))   = [0xD0, fromIntegral w]
  asm _ _ (BEQ (Rel w))   = [0xF0, fromIntegral w]

  asm o a (BPL (Label s)) = 0x10 : relLabel a o s
  asm o a (BMI (Label s)) = 0x30 : relLabel a o s
  asm o a (BVC (Label s)) = 0x50 : relLabel a o s
  asm o a (BVS (Label s)) = 0x70 : relLabel a o s
  asm o a (BCC (Label s)) = 0x90 : relLabel a o s
  asm o a (BCS (Label s)) = 0xB0 : relLabel a o s
  asm o a (BNE (Label s)) = 0xD0 : relLabel a o s
  asm o a (BEQ (Label s)) = 0xF0 : relLabel a o s

  asm _ _ (BPL _)         = undefined
  asm _ _ (BMI _)         = undefined
  asm _ _ (BVC _)         = undefined
  asm _ _ (BVS _)         = undefined
  asm _ _ (BCC _)         = undefined
  asm _ _ (BCS _)         = undefined
  asm _ _ (BNE _)         = undefined
  asm _ _ (BEQ _)         = undefined

  asm _ _ CLC             = [0x18]
  asm _ _ CLI             = [0x58]
  asm _ _ CLV             = [0xB8]
  asm _ _ CLD             = [0xD8]

  asm _ _ (CMP (Imm  w))  = [0xC9, w]
  asm _ _ (CMP (Zpg  w))  = [0xC5, w]
  asm _ _ (CMP (ZpgX w))  = [0xD5, w]
  asm _ _ (CMP (Abs  w))  = [0xCD, l w, h w]
  asm _ _ (CMP (AbsX w))  = [0xDD, l w, h w]
  asm _ _ (CMP (AbsY w))  = [0xD9, l w, h w]
  asm _ _ (CMP (IndX w))  = [0xC1, w]
  asm _ _ (CMP (IndY w))  = [0xD1, w]
  asm _ _ (CMP _)         = undefined

  asm _ _ (CPX (Imm  w))  = [0xE0, w]
  asm _ _ (CPX (Zpg  w))  = [0xE4, w]
  asm _ _ (CPX (Abs  w))  = [0xEC, l w, h w]
  asm _ _ (CPX _)         = undefined

  asm _ _ (CPY (Imm  w))  = [0xC0, w]
  asm _ _ (CPY (Zpg  w))  = [0xC4, w]
  asm _ _ (CPY (Abs  w))  = [0xCC, l w, h w]
  asm _ _ (CPY _)         = undefined

  asm _ _ (DEC (Zpg w))   = [0xC6, w]
  asm _ _ (DEC (ZpgX w))  = [0xD6, w]
  asm _ _ (DEC (Abs w))   = [0xCE, l w, h w]
  asm _ _ (DEC (AbsX w))  = [0xDE, l w, h w]
  asm _ _ (DEC _)         = undefined

  asm _ _ DEX             = [0xCA]
  asm _ _ DEY             = [0x88]

  asm _ _ (EOR (Imm w))   = [0x49, w]
  asm _ _ (EOR (Zpg w))   = [0x45, w]
  asm _ _ (EOR (ZpgX w))  = [0x55, w]
  asm _ _ (EOR (Abs w))   = [0x4D, l w, h w]
  asm _ _ (EOR (AbsX w))  = [0x5D, l w, h w]
  asm _ _ (EOR (AbsY w))  = [0x59, l w, h w]
  asm _ _ (EOR (IndX w))  = [0x41, w]
  asm _ _ (EOR (IndY w))  = [0x51, w]
  asm _ _ (EOR _)         = undefined

  asm _ _ (INC (Zpg w))   = [0xE6, w]
  asm _ _ (INC (ZpgX w))  = [0xF6, w]
  asm _ _ (INC (Abs w))   = [0xEE, l w, h w]
  asm _ _ (INC (AbsX w))  = [0xFE, l w, h w]
  asm _ _ (INC _)         = undefined

  asm _ _ INX             = [0xE8]
  asm _ _ INY             = [0xC8]

  asm _ _ (JMP (Abs w))   = [0x4C, l w, h w]
  asm _ _ (JMP (Ind w))   = [0x6C, l w, h w]
  asm _ a (JMP (Label s)) = 0x4c : label a s
  asm _ _ (JMP _)         = undefined

  asm _ _ (JSR (Abs w))   = [0x20, l w, h w]
  asm _ a (JSR (Label s)) = 0x20 : label a s
  asm _ _ (JSR _)         = undefined

  asm _ _ (LDA (Imm  w))  = [0xA9, w]
  asm _ _ (LDA (Zpg  w))  = [0xA5, w]
  asm _ _ (LDA (ZpgX w))  = [0xB5, w]
  asm _ _ (LDA (Abs  w))  = [0xAD, l w, h w]
  asm _ _ (LDA (AbsX w))  = [0xBD, l w, h w]
  asm _ _ (LDA (AbsY w))  = [0xB9, l w, h w]
  asm _ _ (LDA (IndX w))  = [0xA1, w]
  asm _ _ (LDA (IndY w))  = [0xB1, w]
  asm _ a (LDA (LabelLowByte s))
                          = 0xA9 : label a s !! 0 : []
  asm _ a (LDA (LabelHighByte s))
                          = 0xA9 : label a s !! 1 : []
  asm _ _ (LDA _)         = undefined

  asm _ _ (LDX (Imm  w))  = [0xA2, w]
  asm _ _ (LDX (Zpg  w))  = [0xA6, w]
  asm _ _ (LDX (ZpgY w))  = [0xB6, w]
  asm _ _ (LDX (Abs  w))  = [0xAE, l w, h w]
  asm _ _ (LDX (AbsY w))  = [0xBE, l w, h w]
  asm _ _ (LDX _)         = undefined

  asm _ _ (LDY (Imm  w))  = [0xA0, w]
  asm _ _ (LDY (Zpg  w))  = [0xA4, w]
  asm _ _ (LDY (ZpgX w))  = [0xB4, w]
  asm _ _ (LDY (Abs  w))  = [0xAC, l w, h w]
  asm _ _ (LDY (AbsX w))  = [0xBC, l w, h w]
  asm _ _ (LDY _)         = undefined

  asm _ _ (LSR Acc)       = [0x4A]
  asm _ _ (LSR (Zpg  w))  = [0x46, w]
  asm _ _ (LSR (ZpgX w))  = [0x56, w]
  asm _ _ (LSR (Abs  w))  = [0x4E, l w, h w]
  asm _ _ (LSR (AbsX w))  = [0x5E, l w, h w]
  asm _ _ (LSR _)         = undefined

  asm _ _ NOP             = [0xEA]

  asm _ _ (ORA (Imm w))   = [0x09, w]
  asm _ _ (ORA (Zpg w))   = [0x05, w]
  asm _ _ (ORA (ZpgX w))  = [0x15, w]
  asm _ _ (ORA (Abs w))   = [0x0D, l w, h w]
  asm _ _ (ORA (AbsX w))  = [0x1D, l w, h w]
  asm _ _ (ORA (AbsY w))  = [0x19, l w, h w]
  asm _ _ (ORA (IndX w))  = [0x01, w]
  asm _ _ (ORA (IndY w))  = [0x11, w]
  asm _ _ (ORA _)         = undefined

  asm _ _ PHA             = [0x48]
  asm _ _ PHP             = [0x08]
  asm _ _ PLA             = [0x68]
  asm _ _ PLP             = [0x28]

  asm _ _ (ROL Acc)       = [0xAA]
  asm _ _ (ROL (Zpg  w))  = [0x26, w]
  asm _ _ (ROL (ZpgX w))  = [0x36, w]
  asm _ _ (ROL (Abs  w))  = [0x2E, l w, h w]
  asm _ _ (ROL (AbsX w))  = [0x3E, l w, h w]
  asm _ _ (ROL _)         = undefined

  asm _ _ (ROR Acc)       = [0x6A]
  asm _ _ (ROR (Zpg  w))  = [0x66, w]
  asm _ _ (ROR (ZpgX w))  = [0x76, w]
  asm _ _ (ROR (Abs  w))  = [0x6E, l w, h w]
  asm _ _ (ROR (AbsX w))  = [0x7E, l w, h w]
  asm _ _ (ROR _)         = undefined

  asm _ _ RTI             = [0x40]
  asm _ _ RTS             = [0x60]

  asm _ _ (SBC (Imm  w))  = [0xE9, w]
  asm _ _ (SBC (Zpg  w))  = [0xE5, w]
  asm _ _ (SBC (ZpgX w))  = [0xF5, w]
  asm _ _ (SBC (Abs  w))  = [0xED, l w, h w]
  asm _ _ (SBC (AbsX w))  = [0xFD, l w, h w]
  asm _ _ (SBC (AbsY w))  = [0xF9, l w, h w]
  asm _ _ (SBC (IndX w))  = [0xE1, w]
  asm _ _ (SBC (IndY w))  = [0xF1, w]
  asm _ _ (SBC _)         = undefined

  asm _ _ SEC             = [0x38]
  asm _ _ SEI             = [0x78]
  asm _ _ SED             = [0xF8]

  asm _ _ (STA (Zpg  w))  = [0x85, w]
  asm _ _ (STA (ZpgX w))  = [0x95, w]
  asm _ _ (STA (Abs  w))  = [0x8D, l w, h w]
  asm _ _ (STA (AbsX w))  = [0x9D, l w, h w]
  asm _ _ (STA (AbsY w))  = [0x99, l w, h w]
  asm _ _ (STA (IndX w))  = [0x81, w]
  asm _ _ (STA (IndY w))  = [0x91, w]
  asm _ _ (STA _)         = undefined

  asm _ _ (STX (Zpg  w))  = [0x86, w]
  asm _ _ (STX (ZpgY w))  = [0x96, w]
  asm _ _ (STX (Abs  w))  = [0x8E, l w, h w]
  asm _ _ (STX _)         = undefined

  asm _ _ (STY (Zpg  w))  = [0x84, w]
  asm _ _ (STY (ZpgX w))  = [0x94, w]
  asm _ _ (STY (Abs  w))  = [0x8C, l w, h w]
  asm _ _ (STY _)         = undefined

  asm _ _ TAX             = [0xAA]
  asm _ _ TXA             = [0x8A]
  asm _ _ TAY             = [0xA8]
  asm _ _ TYA             = [0x98]
  asm _ _ TSX             = [0x9A]
  asm _ _ TXS             = [0xBA]

  -- PHA = [0x48]
  -- PLA = [0x68]
  -- PHP = [0x08]
  -- PLP = [0x28]

  asm _ _ BRK             = [0x00]

  asm _ _ (LabelDef _ _)  = []

label :: [Opcode] -> String -> [Word8]
label a s =
  case findIndex (\i -> case i of
                          LabelDef s' _ -> s' == s
                          _             -> False) a of
    Just p  -> [l w, h w]
      where w = fromIntegral . sum $ insLength <$> (fst $ splitAt p a)
    Nothing -> error $ "cannot find label " <> s

relLabel :: [Opcode] -> Int -> String -> [Word8]
relLabel a o s =
  case findIndex (\i -> case i of
                          LabelDef s' _ -> s' == s
                          _             -> False) a of
    Just p  -> [fromIntegral w]
      where w = (fromIntegral . sum $ insLength <$> (fst $ splitAt p a)) - o
    Nothing -> error $ "cannot find label " <> s

h :: Word16 -> Word8
h w = fromIntegral $ w `shiftR` 8

l :: Word16 -> Word8
l w = fromIntegral $ w .&. 0x00ff
