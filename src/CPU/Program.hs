{-# LANGUAGE DeriveGeneric #-}

module CPU.Program
  ( Program(..)
  , InstructionLength(..)
  , Assembles(..)
  , Instruction(..)
  , Jumps(..)
  , Oper(..)
  ) where

import Control.Lens
import Control.Monad
import Data.Bits
import Data.Char
import Data.Int
import Data.Word
import GHC.Generics
import Text.Printf

import qualified Data.Vector.Storable as DVS

data Program = Program
  { program :: DVS.Vector Word8
  }
  deriving (Eq, Generic)

instance Show Program where
  show (Program prog)       = header ++ foldMap (++ "\n") (showRow <$> rows)
    where header            = printf "    : 00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f\n"
          showRow (o, row)  = printf "%04x: %s |%s|" o (showEles o row) (ascii row)
          showEles o eles   = pad o (foldMap (++ " ") (printf "%02x" <$> DVS.toList eles))
          pad i s           = s ++ if i + rowLength < progLen then "" else (join $ replicate (rowLength - progLen `mod` rowLength) "   ")
          ascii eles        = foldMap (++ "") (printf "%c" . toPrintable . chr . fromIntegral <$> DVS.toList eles)
          toPrintable c     = if isPrint c then c else '.'
          rows              = (\i -> (i, (crimp i))) <$> rowStarts
          rowStarts         = (* rowLength) <$> [0 .. progLen `div` rowLength]
          rowLength         = 16
          crimp i           = if i + rowLength < progLen then sl i rowLength else sl i (progLen `mod` rowLength)
          sl i len          = prog & DVS.slice i len
          progLen           = DVS.length prog

data Instruction
  = ADC Oper
  | AND Oper
  | ASL Oper
  | BCC Oper
  | BCS Oper
  | BEQ Oper
  | BMI Oper
  | BNE Oper
  | BPL Oper
  | BVC Oper
  | BVS Oper
  | CLC
  | CLI
  | CLV
  | CLD
  | CMP Oper
  | CPX Oper
  | CPY Oper
  | DEC Oper
  | DEX
  | DEY
  | EOR Oper
  | INC Oper
  | INX
  | INY
  | JMP Oper
  | LDA Oper
  | LDX Oper
  | LDY Oper
  | LSR Oper
  | NOP
  | ORA Oper
  | SEC
  | SEI
  | SED
  | ROL Oper
  | TAX
  | TXA
  | TAY
  | TYA
  | TSX
  | TXS
  | BRK
  deriving (Eq, Show)

class Jumps a where
  jumps :: a -> Bool

instance Jumps Instruction where
  jumps (ADC _) = False
  jumps (AND _) = False
  jumps (ASL _) = False
  jumps (BCC _) = True
  jumps (BCS _) = True
  jumps (BEQ _) = True
  jumps (BMI _) = True
  jumps (BNE _) = True
  jumps (BPL _) = True
  jumps (BVC _) = True
  jumps (BVS _) = True
  jumps CLC     = False
  jumps CLI     = False
  jumps CLV     = False
  jumps CLD     = False
  jumps (CMP _) = False
  jumps (CPX _) = False
  jumps (CPY _) = False
  jumps (DEC _) = False
  jumps DEX     = False
  jumps DEY     = False
  jumps (EOR _) = False
  jumps (INC _) = False
  jumps INX     = False
  jumps INY     = False
  jumps (JMP _) = True
  jumps (LDA _) = False
  jumps (LDX _) = False
  jumps (LDY _) = False
  jumps (LSR _) = False
  jumps NOP     = False
  jumps (ORA _) = False
  jumps SEC     = False
  jumps SEI     = False
  jumps SED     = False
  jumps (ROL _) = False
  jumps TAX     = False
  jumps TXA     = False
  jumps TAY     = False
  jumps TYA     = False
  jumps TSX     = False
  jumps TXS     = False
  jumps BRK     = False

class Assembles a where
  asm :: a -> [Word8]

instance Assembles Instruction where
  asm (ADC (Imm  w)) = [0x69, w]
  asm (ADC (Zpg  w)) = [0x65, w]
  asm (ADC (ZpgX w)) = [0x75, w]
  asm (ADC (Abs  w)) = [0x6D, l w, h w]
  asm (ADC (AbsX w)) = [0x7D, l w, h w]
  asm (ADC (AbsY w)) = [0x79, l w, h w]
  asm (ADC (IndX w)) = [0x61, w]
  asm (ADC (IndY w)) = [0x71, w]
  asm (ADC _)        = undefined

  asm (AND (Imm  w)) = [0x29, w]
  asm (AND (Zpg  w)) = [0x25, w]
  asm (AND (ZpgX w)) = [0x35, w]
  asm (AND (Abs  w)) = [0x2D, l w, h w]
  asm (AND (AbsX w)) = [0x3D, l w, h w]
  asm (AND (AbsY w)) = [0x39, l w, h w]
  asm (AND (IndX w)) = [0x21, w]
  asm (AND (IndY w)) = [0x31, w]
  asm (AND _)        = undefined

  asm (ASL Acc)      = [0x0A]
  asm (ASL (Zpg  w)) = [0x06, w]
  asm (ASL (ZpgX w)) = [0x16, w]
  asm (ASL (Abs  w)) = [0x0E, l w, h w]
  asm (ASL (AbsX w)) = [0x1E, l w, h w]
  asm (ASL _)        = undefined

  asm (BPL (Addr a)) = [0x10, rel a]
  asm (BMI (Addr a)) = [0x30, rel a]
  asm (BVC (Addr a)) = [0x50, rel a]
  asm (BVS (Addr a)) = [0x70, rel a]
  asm (BCC (Addr a)) = [0x90, rel a]
  asm (BCS (Addr a)) = [0xB0, rel a]
  asm (BNE (Addr a)) = [0xD0, rel a]
  asm (BEQ (Addr a)) = [0xF0, rel a]

  asm (BPL _)        = undefined
  asm (BMI _)        = undefined
  asm (BVC _)        = undefined
  asm (BVS _)        = undefined
  asm (BCC _)        = undefined
  asm (BCS _)        = undefined
  asm (BNE _)        = undefined
  asm (BEQ _)        = undefined

  asm CLC            = [0x18]
  asm CLI            = [0x58]
  asm CLV            = [0xB8]
  asm CLD            = [0xD8]

  asm (CMP (Imm  w)) = [0xC9, w]
  asm (CMP (Zpg  w)) = [0xC5, w]
  asm (CMP (ZpgX w)) = [0xD5, w]
  asm (CMP (Abs  w)) = [0xCD, l w, h w]
  asm (CMP (AbsX w)) = [0xDD, l w, h w]
  asm (CMP (AbsY w)) = [0xD9, l w, h w]
  asm (CMP (IndX w)) = [0xC1, w]
  asm (CMP (IndY w)) = [0xD1, w]
  asm (CMP _)        = undefined

  asm (CPX (Imm  w)) = [0xE0, w]
  asm (CPX (Zpg  w)) = [0xE4, w]
  asm (CPX (Abs  w)) = [0xEC, l w, h w]
  asm (CPX _)        = undefined

  asm (CPY (Imm  w)) = [0xC0, w]
  asm (CPY (Zpg  w)) = [0xC4, w]
  asm (CPY (Abs  w)) = [0xCC, l w, h w]
  asm (CPY _)        = undefined

  asm (DEC (Zpg w))  = [0xC6, w]
  asm (DEC (ZpgX w)) = [0xD6, w]
  asm (DEC (Abs w))  = [0xCE, l w, h w]
  asm (DEC (AbsX w)) = [0xDE, l w, h w]
  asm (DEC _)        = undefined

  asm DEX            = [0xCA]
  asm DEY            = [0x88]

  asm (EOR (Imm w))  = [0x49, w]
  asm (EOR (Zpg w))  = [0x45, w]
  asm (EOR (ZpgX w)) = [0x55, w]
  asm (EOR (Abs w))  = [0x4D, l w, h w]
  asm (EOR (AbsX w)) = [0x5D, l w, h w]
  asm (EOR (AbsY w)) = [0x59, l w, h w]
  asm (EOR (IndX w)) = [0x41, w]
  asm (EOR (IndY w)) = [0x51, w]
  asm (EOR _)        = undefined

  asm (INC (Zpg w))  = [0xE6, w]
  asm (INC (ZpgX w)) = [0xF6, w]
  asm (INC (Abs w))  = [0xEE, l w, h w]
  asm (INC (AbsX w)) = [0xFE, l w, h w]
  asm (INC _)        = undefined

  asm INX            = [0xE8]
  asm INY            = [0xC8]

  asm (JMP (Abs w))  = [0x4C, l w, h w]
  asm (JMP (Ind w))  = [0x6C, l w, h w]
  asm (JMP _)        = undefined

  asm (LDA (Imm  w)) = [0xA9, w]
  asm (LDA (Zpg  w)) = [0xA5, w]
  asm (LDA (ZpgX w)) = [0xB5, w]
  asm (LDA (Abs  w)) = [0xAD, l w, h w]
  asm (LDA (AbsX w)) = [0xBD, l w, h w]
  asm (LDA (AbsY w)) = [0xB9, l w, h w]
  asm (LDA (IndX w)) = [0xA1, w]
  asm (LDA (IndY w)) = [0xB1, w]
  asm (LDA _)        = undefined

  asm (LDX (Imm  w)) = [0xA2, w]
  asm (LDX (Zpg  w)) = [0xA6, w]
  asm (LDX (ZpgY w)) = [0xB6, w]
  asm (LDX (Abs  w)) = [0xAE, l w, h w]
  asm (LDX (AbsY w)) = [0xBE, l w, h w]
  asm (LDX _)        = undefined

  asm (LDY (Imm  w)) = [0xA0, w]
  asm (LDY (Zpg  w)) = [0xA4, w]
  asm (LDY (ZpgX w)) = [0xB4, w]
  asm (LDY (Abs  w)) = [0xAC, l w, h w]
  asm (LDY (AbsX w)) = [0xBC, l w, h w]
  asm (LDY _)        = undefined

  asm (LSR Acc)      = [0x4A]
  asm (LSR (Zpg  w)) = [0x46, w]
  asm (LSR (ZpgX w)) = [0x56, w]
  asm (LSR (Abs  w)) = [0x4E, l w, h w]
  asm (LSR (AbsX w)) = [0x5E, l w, h w]
  asm (LSR _)        = undefined

  asm NOP            = [0xEA]

  asm (ORA (Imm w))  = [0x09, w]
  asm (ORA (Zpg w))  = [0x05, w]
  asm (ORA (ZpgX w)) = [0x15, w]
  asm (ORA (Abs w))  = [0x0D, l w, h w]
  asm (ORA (AbsX w)) = [0x1D, l w, h w]
  asm (ORA (AbsY w)) = [0x19, l w, h w]
  asm (ORA (IndX w)) = [0x01, w]
  asm (ORA (IndY w)) = [0x11, w]
  asm (ORA _)        = undefined

  asm SEC            = [0x38]
  asm SEI            = [0x78]
  asm SED            = [0xF8]

  asm (ROL Acc)      = [0xAA]
  asm (ROL (Zpg  w)) = [0x26, w]
  asm (ROL (ZpgX w)) = [0x36, w]
  asm (ROL (Abs  w)) = [0x2E, l w, h w]
  asm (ROL (AbsX w)) = [0x3E, l w, h w]
  asm (ROL _)        = undefined

  asm TAX            = [0xAA]
  asm TXA            = [0x8A]
  asm TAY            = [0xA8]
  asm TYA            = [0x98]
  asm TSX            = [0x9A]
  asm TXS            = [0xBA]

  -- PHA = [0x48]
  -- PLA = [0x68]
  -- PHP = [0x08]
  -- PLP = [0x28]

  asm BRK            = [0x00]

class InstructionLength a where
  insLength :: a -> Integer

instance InstructionLength Instruction where
  insLength (ADC (Imm _))  = 2
  insLength (ADC (Zpg _))  = 2
  insLength (ADC (ZpgX _)) = 2
  insLength (ADC (Abs _))  = 3
  insLength (ADC (AbsX _)) = 3
  insLength (ADC (AbsY _)) = 3
  insLength (ADC (IndX _)) = 2
  insLength (ADC (IndY _)) = 2
  insLength (ADC _)        = undefined

  insLength (AND (Imm _))  = 2
  insLength (AND (Zpg _))  = 2
  insLength (AND (ZpgX _)) = 2
  insLength (AND (Abs _))  = 3
  insLength (AND (AbsX _)) = 3
  insLength (AND (AbsY _)) = 3
  insLength (AND (IndX _)) = 2
  insLength (AND (IndY _)) = 2
  insLength (AND _)        = undefined

  insLength (ASL Acc)      = 1
  insLength (ASL (Zpg _))  = 2
  insLength (ASL (ZpgX _)) = 2
  insLength (ASL (Abs _))  = 3
  insLength (ASL (AbsX _)) = 3
  insLength (ASL _)        = undefined

  insLength (BPL (Rel _))  = 2
  insLength (BMI (Rel _))  = 2
  insLength (BVC (Rel _))  = 2
  insLength (BVS (Rel _))  = 2
  insLength (BCC (Rel _))  = 2
  insLength (BCS (Rel _))  = 2
  insLength (BNE (Rel _))  = 2
  insLength (BEQ (Rel _))  = 2

  insLength (BPL _)        = undefined
  insLength (BMI _)        = undefined
  insLength (BVC _)        = undefined
  insLength (BVS _)        = undefined
  insLength (BCC _)        = undefined
  insLength (BCS _)        = undefined
  insLength (BNE _)        = undefined
  insLength (BEQ _)        = undefined

  insLength CLC            = 1
  insLength CLI            = 1
  insLength CLV            = 1
  insLength CLD            = 1

  insLength (CMP (Imm  _)) = 2
  insLength (CMP (Zpg  _)) = 2
  insLength (CMP (ZpgX _)) = 2
  insLength (CMP (Abs  _)) = 3
  insLength (CMP (AbsX _)) = 3
  insLength (CMP (AbsY _)) = 3
  insLength (CMP (IndX _)) = 2
  insLength (CMP (IndY _)) = 2
  insLength (CMP _)        = undefined

  insLength (CPX (Imm _))  = 2
  insLength (CPX (Zpg _))  = 2
  insLength (CPX (Abs _))  = 3
  insLength (CPX _)        = undefined

  insLength (CPY (Imm _))  = 2
  insLength (CPY (Zpg _))  = 2
  insLength (CPY (Abs _))  = 3
  insLength (CPY _)        = undefined

  insLength (DEC (Zpg _))  = 2
  insLength (DEC (ZpgX _)) = 2
  insLength (DEC (Abs _))  = 3
  insLength (DEC (AbsX _)) = 3
  insLength (DEC _)        = undefined

  insLength DEX            = 1
  insLength DEY            = 1

  insLength (EOR (Imm  _)) = 2
  insLength (EOR (Zpg  _)) = 2
  insLength (EOR (ZpgX _)) = 2
  insLength (EOR (Abs  _)) = 3
  insLength (EOR (AbsX _)) = 3
  insLength (EOR (AbsY _)) = 3
  insLength (EOR (IndX _)) = 2
  insLength (EOR (IndY _)) = 2
  insLength (EOR _)        = undefined

  insLength (INC (Zpg _))  = 2
  insLength (INC (ZpgX _)) = 2
  insLength (INC (Abs _))  = 3
  insLength (INC (AbsX _)) = 3
  insLength (INC _)        = undefined

  insLength INX            = 1
  insLength INY            = 1

  insLength (JMP (Abs _))  = 3
  insLength (JMP (Ind _))  = 3
  insLength (JMP _)        = undefined

  insLength (LDA (Imm _))  = 2
  insLength (LDA (Zpg _))  = 2
  insLength (LDA (ZpgX _)) = 2
  insLength (LDA (Abs _))  = 3
  insLength (LDA (AbsX _)) = 3
  insLength (LDA (AbsY _)) = 3
  insLength (LDA (IndX _)) = 2
  insLength (LDA (IndY _)) = 2
  insLength (LDA _)        = undefined

  insLength (LDX (Imm _))  = 2
  insLength (LDX (Zpg _))  = 2
  insLength (LDX (ZpgY _)) = 2
  insLength (LDX (Abs _))  = 3
  insLength (LDX (AbsY _)) = 3
  insLength (LDX (IndX _)) = 2
  insLength (LDX (IndY _)) = 2
  insLength (LDX _)        = undefined

  insLength (LDY (Imm _))  = 2
  insLength (LDY (Zpg _))  = 2
  insLength (LDY (ZpgX _)) = 2
  insLength (LDY (Abs _))  = 3
  insLength (LDY (AbsX _)) = 3
  insLength (LDY (IndX _)) = 2
  insLength (LDY (IndY _)) = 2
  insLength (LDY _)        = undefined

  insLength (LSR Acc)      = 1
  insLength (LSR (Zpg _))  = 2
  insLength (LSR (ZpgX _)) = 2
  insLength (LSR (Abs _))  = 3
  insLength (LSR (AbsX _)) = 3
  insLength (LSR _)        = undefined

  insLength NOP            = 1

  insLength (ORA (Imm  _)) = 2
  insLength (ORA (Zpg  _)) = 2
  insLength (ORA (ZpgX _)) = 2
  insLength (ORA (Abs  _)) = 3
  insLength (ORA (AbsX _)) = 3
  insLength (ORA (AbsY _)) = 3
  insLength (ORA (IndX _)) = 2
  insLength (ORA (IndY _)) = 2
  insLength (ORA _)        = undefined

  insLength SEC            = 1
  insLength SEI            = 1
  insLength SED            = 1

  insLength (ROL Acc)      = 1
  insLength (ROL (Zpg _))  = 2
  insLength (ROL (ZpgX _)) = 2
  insLength (ROL (Abs _))  = 3
  insLength (ROL (AbsX _)) = 3
  insLength (ROL _)        = undefined

  insLength TAX            = 1
  insLength TXA            = 1
  insLength TAY            = 1
  insLength TYA            = 1
  insLength TSX            = 1
  insLength TXS            = 1

  insLength BRK            = 1

data Oper
  = Acc
  | Imm  Word8
  | Zpg  Word8
  | ZpgX Word8
  | ZpgY Word8
  | Abs  Word16
  | AbsX Word16
  | AbsY Word16
  | Ind Word16
  | IndX Word8
  | IndY Word8
  | Addr Word16
  | Rel Int8
  deriving (Eq, Show)

h :: Word16 -> Word8
h w = fromIntegral $ w `shiftR` 8

l :: Word16 -> Word8
l w = fromIntegral $ w .&. 0x00ff

rel :: Word16 -> Word8
rel = fromIntegral
