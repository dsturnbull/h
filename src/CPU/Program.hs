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
import Data.List
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
          pad i s           = s ++ if i + rowLength <= progLen then "" else (join $ replicate (rowLength - progLen `mod` rowLength) "   ")
          ascii eles        = foldMap (++ "") (printf "%c" . toPrintable . chr . fromIntegral <$> DVS.toList eles)
          toPrintable c     = if isPrint c then c else '.'
          rows              = (\i -> (i, (crimp i))) <$> rowStarts
          rowStarts         = (* rowLength) <$> [0 .. progLen `div` rowLength]
          rowLength         = 16
          crimp i           = if i + rowLength <= progLen then sl i rowLength else sl i (progLen `mod` rowLength)
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
  | JSR Oper
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
  | ROR Oper
  | RTI
  | RTS
  | SBC Oper
  | STA Oper
  | STX Oper
  | STY Oper
  | TAX
  | TXA
  | TAY
  | TYA
  | TSX
  | TXS
  | BRK
  | LabelDef String Word16
  deriving (Eq, Show)

class Jumps a where
  jumps :: a -> Bool

instance Jumps Instruction where
  jumps (ADC _)        = False
  jumps (AND _)        = False
  jumps (ASL _)        = False
  jumps (BCC _)        = True
  jumps (BCS _)        = True
  jumps (BEQ _)        = True
  jumps (BMI _)        = True
  jumps (BNE _)        = True
  jumps (BPL _)        = True
  jumps (BVC _)        = True
  jumps (BVS _)        = True
  jumps CLC            = False
  jumps CLI            = False
  jumps CLV            = False
  jumps CLD            = False
  jumps (CMP _)        = False
  jumps (CPX _)        = False
  jumps (CPY _)        = False
  jumps (DEC _)        = False
  jumps DEX            = False
  jumps DEY            = False
  jumps (EOR _)        = False
  jumps (INC _)        = False
  jumps INX            = False
  jumps INY            = False
  jumps (JMP _)        = True
  jumps (JSR _)        = True
  jumps (LDA _)        = False
  jumps (LDX _)        = False
  jumps (LDY _)        = False
  jumps (LSR _)        = False
  jumps NOP            = False
  jumps (ORA _)        = False
  jumps SEC            = False
  jumps SEI            = False
  jumps SED            = False
  jumps (ROL _)        = False
  jumps (ROR _)        = False
  jumps RTI            = True
  jumps RTS            = True
  jumps (SBC _)        = False
  jumps (STA _)        = False
  jumps (STX _)        = False
  jumps (STY _)        = False
  jumps TAX            = False
  jumps TXA            = False
  jumps TAY            = False
  jumps TYA            = False
  jumps TSX            = False
  jumps TXS            = False
  jumps BRK            = False
  jumps (LabelDef _ _) = undefined

class Assembles a where
  asm :: [Instruction] -> a -> [Word8]

instance Assembles Instruction where
  asm _ (ADC (Imm  w))  = [0x69, w]
  asm _ (ADC (Zpg  w))  = [0x65, w]
  asm _ (ADC (ZpgX w))  = [0x75, w]
  asm _ (ADC (Abs  w))  = [0x6D, l w, h w]
  asm _ (ADC (AbsX w))  = [0x7D, l w, h w]
  asm _ (ADC (AbsY w))  = [0x79, l w, h w]
  asm _ (ADC (IndX w))  = [0x61, w]
  asm _ (ADC (IndY w))  = [0x71, w]
  asm _ (ADC _)         = undefined

  asm _ (AND (Imm  w))  = [0x29, w]
  asm _ (AND (Zpg  w))  = [0x25, w]
  asm _ (AND (ZpgX w))  = [0x35, w]
  asm _ (AND (Abs  w))  = [0x2D, l w, h w]
  asm _ (AND (AbsX w))  = [0x3D, l w, h w]
  asm _ (AND (AbsY w))  = [0x39, l w, h w]
  asm _ (AND (IndX w))  = [0x21, w]
  asm _ (AND (IndY w))  = [0x31, w]
  asm _ (AND _)         = undefined

  asm _ (ASL Acc)       = [0x0A]
  asm _ (ASL (Zpg  w))  = [0x06, w]
  asm _ (ASL (ZpgX w))  = [0x16, w]
  asm _ (ASL (Abs  w))  = [0x0E, l w, h w]
  asm _ (ASL (AbsX w))  = [0x1E, l w, h w]
  asm _ (ASL _)         = undefined

  asm _ (BPL (Rel w))   = [0x10, fromIntegral w]
  asm _ (BMI (Rel w))   = [0x30, fromIntegral w]
  asm _ (BVC (Rel w))   = [0x50, fromIntegral w]
  asm _ (BVS (Rel w))   = [0x70, fromIntegral w]
  asm _ (BCC (Rel w))   = [0x90, fromIntegral w]
  asm _ (BCS (Rel w))   = [0xB0, fromIntegral w]
  asm _ (BNE (Rel w))   = [0xD0, fromIntegral w]
  asm _ (BEQ (Rel w))   = [0xF0, fromIntegral w]

  asm a (BPL (Label s)) = 0x10 : relLabel a s
  asm a (BMI (Label s)) = 0x30 : relLabel a s
  asm a (BVC (Label s)) = 0x50 : relLabel a s
  asm a (BVS (Label s)) = 0x70 : relLabel a s
  asm a (BCC (Label s)) = 0x90 : relLabel a s
  asm a (BCS (Label s)) = 0xB0 : relLabel a s
  asm a (BNE (Label s)) = 0xD0 : relLabel a s
  asm a (BEQ (Label s)) = 0xF0 : relLabel a s

  asm _ (BPL _)         = undefined
  asm _ (BMI _)         = undefined
  asm _ (BVC _)         = undefined
  asm _ (BVS _)         = undefined
  asm _ (BCC _)         = undefined
  asm _ (BCS _)         = undefined
  asm _ (BNE _)         = undefined
  asm _ (BEQ _)         = undefined

  asm _ CLC             = [0x18]
  asm _ CLI             = [0x58]
  asm _ CLV             = [0xB8]
  asm _ CLD             = [0xD8]

  asm _ (CMP (Imm  w))  = [0xC9, w]
  asm _ (CMP (Zpg  w))  = [0xC5, w]
  asm _ (CMP (ZpgX w))  = [0xD5, w]
  asm _ (CMP (Abs  w))  = [0xCD, l w, h w]
  asm _ (CMP (AbsX w))  = [0xDD, l w, h w]
  asm _ (CMP (AbsY w))  = [0xD9, l w, h w]
  asm _ (CMP (IndX w))  = [0xC1, w]
  asm _ (CMP (IndY w))  = [0xD1, w]
  asm _ (CMP _)         = undefined

  asm _ (CPX (Imm  w))  = [0xE0, w]
  asm _ (CPX (Zpg  w))  = [0xE4, w]
  asm _ (CPX (Abs  w))  = [0xEC, l w, h w]
  asm _ (CPX _)         = undefined

  asm _ (CPY (Imm  w))  = [0xC0, w]
  asm _ (CPY (Zpg  w))  = [0xC4, w]
  asm _ (CPY (Abs  w))  = [0xCC, l w, h w]
  asm _ (CPY _)         = undefined

  asm _ (DEC (Zpg w))   = [0xC6, w]
  asm _ (DEC (ZpgX w))  = [0xD6, w]
  asm _ (DEC (Abs w))   = [0xCE, l w, h w]
  asm _ (DEC (AbsX w))  = [0xDE, l w, h w]
  asm _ (DEC _)         = undefined

  asm _ DEX             = [0xCA]
  asm _ DEY             = [0x88]

  asm _ (EOR (Imm w))   = [0x49, w]
  asm _ (EOR (Zpg w))   = [0x45, w]
  asm _ (EOR (ZpgX w))  = [0x55, w]
  asm _ (EOR (Abs w))   = [0x4D, l w, h w]
  asm _ (EOR (AbsX w))  = [0x5D, l w, h w]
  asm _ (EOR (AbsY w))  = [0x59, l w, h w]
  asm _ (EOR (IndX w))  = [0x41, w]
  asm _ (EOR (IndY w))  = [0x51, w]
  asm _ (EOR _)         = undefined

  asm _ (INC (Zpg w))   = [0xE6, w]
  asm _ (INC (ZpgX w))  = [0xF6, w]
  asm _ (INC (Abs w))   = [0xEE, l w, h w]
  asm _ (INC (AbsX w))  = [0xFE, l w, h w]
  asm _ (INC _)         = undefined

  asm _ INX             = [0xE8]
  asm _ INY             = [0xC8]

  asm _ (JMP (Abs w))   = [0x4C, l w, h w]
  asm _ (JMP (Ind w))   = [0x6C, l w, h w]
  asm a (JMP (Label s)) = 0x4c : label a s
  asm _ (JMP _)         = undefined

  asm _ (JSR (Abs w))   = [0x20, l w, h w]
  asm a (JSR (Label s)) = 0x20 : label a s
  asm _ (JSR _)         = undefined

  asm _ (LDA (Imm  w))  = [0xA9, w]
  asm _ (LDA (Zpg  w))  = [0xA5, w]
  asm _ (LDA (ZpgX w))  = [0xB5, w]
  asm _ (LDA (Abs  w))  = [0xAD, l w, h w]
  asm _ (LDA (AbsX w))  = [0xBD, l w, h w]
  asm _ (LDA (AbsY w))  = [0xB9, l w, h w]
  asm _ (LDA (IndX w))  = [0xA1, w]
  asm _ (LDA (IndY w))  = [0xB1, w]
  asm _ (LDA _)         = undefined

  asm _ (LDX (Imm  w))  = [0xA2, w]
  asm _ (LDX (Zpg  w))  = [0xA6, w]
  asm _ (LDX (ZpgY w))  = [0xB6, w]
  asm _ (LDX (Abs  w))  = [0xAE, l w, h w]
  asm _ (LDX (AbsY w))  = [0xBE, l w, h w]
  asm _ (LDX _)         = undefined

  asm _ (LDY (Imm  w))  = [0xA0, w]
  asm _ (LDY (Zpg  w))  = [0xA4, w]
  asm _ (LDY (ZpgX w))  = [0xB4, w]
  asm _ (LDY (Abs  w))  = [0xAC, l w, h w]
  asm _ (LDY (AbsX w))  = [0xBC, l w, h w]
  asm _ (LDY _)         = undefined

  asm _ (LSR Acc)       = [0x4A]
  asm _ (LSR (Zpg  w))  = [0x46, w]
  asm _ (LSR (ZpgX w))  = [0x56, w]
  asm _ (LSR (Abs  w))  = [0x4E, l w, h w]
  asm _ (LSR (AbsX w))  = [0x5E, l w, h w]
  asm _ (LSR _)         = undefined

  asm _ NOP             = [0xEA]

  asm _ (ORA (Imm w))   = [0x09, w]
  asm _ (ORA (Zpg w))   = [0x05, w]
  asm _ (ORA (ZpgX w))  = [0x15, w]
  asm _ (ORA (Abs w))   = [0x0D, l w, h w]
  asm _ (ORA (AbsX w))  = [0x1D, l w, h w]
  asm _ (ORA (AbsY w))  = [0x19, l w, h w]
  asm _ (ORA (IndX w))  = [0x01, w]
  asm _ (ORA (IndY w))  = [0x11, w]
  asm _ (ORA _)         = undefined

  asm _ SEC             = [0x38]
  asm _ SEI             = [0x78]
  asm _ SED             = [0xF8]

  asm _ (ROL Acc)       = [0xAA]
  asm _ (ROL (Zpg  w))  = [0x26, w]
  asm _ (ROL (ZpgX w))  = [0x36, w]
  asm _ (ROL (Abs  w))  = [0x2E, l w, h w]
  asm _ (ROL (AbsX w))  = [0x3E, l w, h w]
  asm _ (ROL _)         = undefined

  asm _ (ROR Acc)       = [0x6A]
  asm _ (ROR (Zpg  w))  = [0x66, w]
  asm _ (ROR (ZpgX w))  = [0x76, w]
  asm _ (ROR (Abs  w))  = [0x6E, l w, h w]
  asm _ (ROR (AbsX w))  = [0x7E, l w, h w]
  asm _ (ROR _)         = undefined

  asm _ RTI             = [0x40]
  asm _ RTS             = [0x60]

  asm _ (SBC (Imm  w))  = [0xE9, w]
  asm _ (SBC (Zpg  w))  = [0xE5, w]
  asm _ (SBC (ZpgX w))  = [0xF5, w]
  asm _ (SBC (Abs  w))  = [0xED, l w, h w]
  asm _ (SBC (AbsX w))  = [0xFD, l w, h w]
  asm _ (SBC (AbsY w))  = [0xF9, l w, h w]
  asm _ (SBC (IndX w))  = [0xE1, w]
  asm _ (SBC (IndY w))  = [0xF1, w]
  asm _ (SBC _)         = undefined

  asm _ (STA (Zpg  w))  = [0x85, w]
  asm _ (STA (ZpgX w))  = [0x95, w]
  asm _ (STA (Abs  w))  = [0x8D, l w, h w]
  asm _ (STA (AbsX w))  = [0x9D, l w, h w]
  asm _ (STA (AbsY w))  = [0x99, l w, h w]
  asm _ (STA (IndX w))  = [0x81, w]
  asm _ (STA (IndY w))  = [0x91, w]
  asm _ (STA _)         = undefined

  asm _ (STX (Zpg  w))  = [0x86, w]
  asm _ (STX (ZpgY w))  = [0x96, w]
  asm _ (STX (Abs  w))  = [0x8E, l w, h w]
  asm _ (STX _)         = undefined

  asm _ (STY (Zpg  w))  = [0x84, w]
  asm _ (STY (ZpgX w))  = [0x94, w]
  asm _ (STY (Abs  w))  = [0x8C, l w, h w]
  asm _ (STY _)         = undefined

  asm _ TAX             = [0xAA]
  asm _ TXA             = [0x8A]
  asm _ TAY             = [0xA8]
  asm _ TYA             = [0x98]
  asm _ TSX             = [0x9A]
  asm _ TXS             = [0xBA]

  -- PHA = [0x48]
  -- PLA = [0x68]
  -- PHP = [0x08]
  -- PLP = [0x28]

  asm _ BRK             = [0x00]

  asm _ (LabelDef _ _)  = []

label :: [Instruction] -> String -> [Word8]
label a s =
  case findIndex (\i -> case i of
                          LabelDef s' _ -> s' == s
                          _             -> False) a of
    Just p  -> [l w, h w]
      where w = fromIntegral . sum $ insLength <$> (fst $ splitAt p a)
    Nothing -> error $ "cannot find label " <> s

relLabel :: [Instruction] -> String -> [Word8]
relLabel a s =
  case findIndex (\i -> case i of
                          LabelDef s' _ -> s' == s
                          _             -> False) a of
    Just p  -> [fromIntegral w]
      where w = (fromIntegral . sum $ insLength <$> (fst $ splitAt p a)) - p - 2
    Nothing -> error $ "cannot find label " <> s

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

  insLength (BPL _)        = 2
  insLength (BMI _)        = 2
  insLength (BVC _)        = 2
  insLength (BVS _)        = 2
  insLength (BCC _)        = 2
  insLength (BCS _)        = 2
  insLength (BNE _)        = 2
  insLength (BEQ _)        = 2

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

  insLength (JMP _)        = 3

  insLength (JSR _)        = 3

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

  insLength (ROR Acc)      = 1
  insLength (ROR (Zpg _))  = 2
  insLength (ROR (ZpgX _)) = 2
  insLength (ROR (Abs _))  = 3
  insLength (ROR (AbsX _)) = 3
  insLength (ROR _)        = undefined

  insLength RTI            = 1
  insLength RTS            = 1

  insLength (SBC (Imm _))  = 2
  insLength (SBC (Zpg _))  = 2
  insLength (SBC (ZpgX _)) = 2
  insLength (SBC (Abs _))  = 3
  insLength (SBC (AbsX _)) = 3
  insLength (SBC (AbsY _)) = 3
  insLength (SBC (IndX _)) = 2
  insLength (SBC (IndY _)) = 2
  insLength (SBC _)        = undefined

  insLength (STA (Zpg  _)) = 2
  insLength (STA (ZpgX _)) = 2
  insLength (STA (Abs  _)) = 3
  insLength (STA (AbsX _)) = 3
  insLength (STA (AbsY _)) = 3
  insLength (STA (IndX _)) = 2
  insLength (STA (IndY _)) = 2
  insLength (STA _)        = undefined

  insLength (STX (Zpg  _)) = 2
  insLength (STX (ZpgY _)) = 2
  insLength (STX (Abs  _)) = 3
  insLength (STX _)        = undefined

  insLength (STY (Zpg  _)) = 2
  insLength (STY (ZpgX _)) = 2
  insLength (STY (Abs  _)) = 3
  insLength (STY _)        = undefined

  insLength TAX            = 1
  insLength TXA            = 1
  insLength TAY            = 1
  insLength TYA            = 1
  insLength TSX            = 1
  insLength TXS            = 1

  insLength BRK            = 1

  insLength (LabelDef _ _) = 0

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
  | Label String
  deriving (Eq, Show)

h :: Word16 -> Word8
h w = fromIntegral $ w `shiftR` 8

l :: Word16 -> Word8
l w = fromIntegral $ w .&. 0x00ff
