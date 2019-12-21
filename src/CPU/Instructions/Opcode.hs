module CPU.Instructions.Opcode
  ( Opcode(..)
  ) where

import CPU.Instructions.Operand

import Control.Monad
import Data.List
import Data.Word

data Opcode
  = ADC Operand
  | AND Operand
  | ASL Operand
  | BCC Operand
  | BCS Operand
  | BEQ Operand
  | BMI Operand
  | BNE Operand
  | BPL Operand
  | BVC Operand
  | BVS Operand
  | CLC
  | CLI
  | CLV
  | CLD
  | CMP Operand
  | CPX Operand
  | CPY Operand
  | DEC Operand
  | DEX
  | DEY
  | EOR Operand
  | INC Operand
  | INX
  | INY
  | JMP Operand
  | JSR Operand
  | LDA Operand
  | LDX Operand
  | LDY Operand
  | LSR Operand
  | NOP
  | ORA Operand
  | PHA
  | PHP
  | PLA
  | PLP
  | ROL Operand
  | ROR Operand
  | RTI
  | RTS
  | SBC Operand
  | SEC
  | SEI
  | SED
  | STA Operand
  | STX Operand
  | STY Operand
  | TAX
  | TXA
  | TAY
  | TYA
  | TSX
  | TXS
  | BRK
  | LabelDef String
  | Code
  | Data
  | Bytes [Word8]
  deriving Eq

instance Show Opcode where
  show (ADC o)      = "ADC " ++ show o
  show (AND o)      = "AND " ++ show o
  show (ASL o)      = "ASL " ++ show o
  show (BCC o)      = "BCC " ++ show o
  show (BCS o)      = "BCS " ++ show o
  show (BEQ o)      = "BEQ " ++ show o
  show (BMI o)      = "BMI " ++ show o
  show (BNE o)      = "BNE " ++ show o
  show (BPL o)      = "BPL " ++ show o
  show (BVC o)      = "BVC " ++ show o
  show (BVS o)      = "BVS " ++ show o
  show  CLC         = "CLC"
  show  CLI         = "CLI"
  show  CLV         = "CLV"
  show  CLD         = "CLD"
  show (CMP o)      = "CMP " ++ show o
  show (CPX o)      = "CPX " ++ show o
  show (CPY o)      = "CPY " ++ show o
  show (DEC o)      = "DEC " ++ show o
  show  DEX         = "DEX"
  show  DEY         = "DEY"
  show (EOR o)      = "EOR " ++ show o
  show (INC o)      = "INC " ++ show o
  show  INX         = "INX"
  show  INY         = "INY"
  show (JMP o)      = "JMP " ++ show o
  show (JSR o)      = "JSR " ++ show o
  show (LDA o)      = "LDA " ++ show o
  show (LDX o)      = "LDX " ++ show o
  show (LDY o)      = "LDY " ++ show o
  show (LSR o)      = "LSR " ++ show o
  show  NOP         = "NOP"
  show  PHA         = "PHA"
  show  PHP         = "PHP"
  show  PLA         = "PLA"
  show  PLP         = "PLP"
  show (ORA o)      = "ORA " ++ show o
  show (ROL o)      = "ROL " ++ show o
  show (ROR o)      = "ROR " ++ show o
  show  RTI         = "RTI"
  show  RTS         = "RTS"
  show (SBC o)      = "SBC " ++ show o
  show  SEC         = "SEC"
  show  SEI         = "SEI"
  show  SED         = "SED"
  show (STA o)      = "STA " ++ show o
  show (STX o)      = "STX " ++ show o
  show (STY o)      = "STY " ++ show o
  show  TAX         = "TAX"
  show  TXA         = "TXA"
  show  TAY         = "TAY"
  show  TYA         = "TYA"
  show  TSX         = "TSX"
  show  TXS         = "TXS"
  show  BRK         = "BRK"

  show (LabelDef s) = s
  show Code         = ".code"
  show Data         = ".data"
  show (Bytes ws)   = ".byte " ++ join (intersperse " " (show <$> ws))
