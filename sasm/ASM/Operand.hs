{-# LANGUAGE TypeApplications #-}

module ASM.Operand
  ( Operand(..)
  , Opcode(..)
  , LabelOpt(..)
  , LabelMode(..)
  , LabelByteMode(..)
  , LabelModification(..)
  , unLabel
  , unLabelWith
  , unLabelOpWith
  )
  where

import Control.Monad
import Data.Bits
import Data.Int
import Data.List
import Data.Word
import Text.Printf

data Operand
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
  | Rel Int8
  | Label LabelOpt String
  deriving Eq

instance Show Operand where
  show Acc           = ""
  show (Imm w)       = showImm w
  show (Zpg w)       = showW8 w
  show (ZpgX w)      = showW8 w ++ ",X"
  show (ZpgY w)      = showW8 w ++ ",Y"
  show (Abs w)       = showW16 w
  show (AbsX w)      = showW16 w ++ ",X"
  show (AbsY w)      = showW16 w ++ ",Y"
  show (Ind w)       = "(" ++ showW16 w ++ ")"
  show (IndY w)      = "(" ++ showW8 w ++ "),Y"
  show (IndX w)      = "(" ++ showW8 w ++ ",X)"
  show (Rel i)       = showI8 i
  show l@(Label _ s) = show (unLabelOpWith @Integer 0 l) <> " ; " <> s

data LabelOpt = LabelOpt
  { mode         :: LabelMode
  , modification :: LabelModification
  } deriving (Eq, Show)

data LabelModification = Plus Integer | Minus Integer | NoMod
  deriving (Eq, Show)

data LabelMode
  = LabelImm
  | LabelAbs | LabelAbsX | LabelAbsY
  | LabelIndirectX | LabelIndirectY | LabelIndirect
  | LabelHighByte LabelMode
  | LabelLowByte LabelMode
  | LabelRel
  deriving (Eq, Show)

data LabelByteMode = LabelLow | LabelHigh
  deriving (Eq, Show)

modToNum :: Num a => LabelModification -> a
modToNum (Plus n)  = fromIntegral n
modToNum (Minus n) = - fromIntegral n
modToNum NoMod     = 0

showImm :: Word8 -> String
showImm w = '#' : showW8 w

showI8 :: Int8 -> String
showI8 i = printf "%s$%02x" (if i < 0 then "-" else "") (abs i)

showW8 :: Word8 -> String
showW8 = printf "$%02x"

showW16 :: Word16 -> String
showW16 = printf "$%04x"

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
  | Variable8 String Word8
  | Variable16 String Word16
  | Code
  | Data
  | Bytes [Word8]
  | Origin Word16
  deriving Eq

instance Show Opcode where
  show (ADC o)          = "ADC " ++ show o
  show (AND o)          = "AND " ++ show o
  show (ASL o)          = "ASL " ++ show o
  show (BCC o)          = "BCC " ++ show o
  show (BCS o)          = "BCS " ++ show o
  show (BEQ o)          = "BEQ " ++ show o
  show (BMI o)          = "BMI " ++ show o
  show (BNE o)          = "BNE " ++ show o
  show (BPL o)          = "BPL " ++ show o
  show (BVC o)          = "BVC " ++ show o
  show (BVS o)          = "BVS " ++ show o
  show  CLC             = "CLC"
  show  CLI             = "CLI"
  show  CLV             = "CLV"
  show  CLD             = "CLD"
  show (CMP o)          = "CMP " ++ show o
  show (CPX o)          = "CPX " ++ show o
  show (CPY o)          = "CPY " ++ show o
  show (DEC o)          = "DEC " ++ show o
  show  DEX             = "DEX"
  show  DEY             = "DEY"
  show (EOR o)          = "EOR " ++ show o
  show (INC o)          = "INC " ++ show o
  show  INX             = "INX"
  show  INY             = "INY"
  show (JMP o)          = "JMP " ++ show o
  show (JSR o)          = "JSR " ++ show o
  show (LDA o)          = "LDA " ++ show o
  show (LDX o)          = "LDX " ++ show o
  show (LDY o)          = "LDY " ++ show o
  show (LSR o)          = "LSR " ++ show o
  show  NOP             = "NOP"
  show  PHA             = "PHA"
  show  PHP             = "PHP"
  show  PLA             = "PLA"
  show  PLP             = "PLP"
  show (ORA o)          = "ORA " ++ show o
  show (ROL o)          = "ROL " ++ show o
  show (ROR o)          = "ROR " ++ show o
  show  RTI             = "RTI"
  show  RTS             = "RTS"
  show (SBC o)          = "SBC " ++ show o
  show  SEC             = "SEC"
  show  SEI             = "SEI"
  show  SED             = "SED"
  show (STA o)          = "STA " ++ show o
  show (STX o)          = "STX " ++ show o
  show (STY o)          = "STY " ++ show o
  show  TAX             = "TAX"
  show  TXA             = "TXA"
  show  TAY             = "TAY"
  show  TYA             = "TYA"
  show  TSX             = "TSX"
  show  TXS             = "TXS"
  show  BRK             = "BRK"

  show (LabelDef s)     = s
  show (Variable8 s v)  = s <> ": " <> printf "%02x" v
  show (Variable16 s v) = s <> ": " <> printf "%04x" v
  show Code             = ".code"
  show Data             = ".data"
  show (Bytes ws)       = ".byte " <> join (intersperse " " (show <$> ws))
  show (Origin l)       = ".org " <> printf "%04x" l

unLabel :: (Operand -> Opcode) -> Operand -> Opcode
unLabel f = unLabelWith f (0 :: Word8)

unLabelWith :: Integral a => (Operand -> Opcode) -> a -> Operand -> Opcode
unLabelWith c n l = c (unLabelOpWith n l)

unLabelOpWith :: Integral a => a -> Operand -> Operand
unLabelOpWith n (Label (LabelOpt LabelImm m) _)          = Imm $ fromIntegral (n + modToNum m)
unLabelOpWith n (Label (LabelOpt LabelIndirect m) _)     = Ind $ fromIntegral (n + modToNum m)
unLabelOpWith n (Label (LabelOpt LabelIndirectX m) _)    = IndX $ fromIntegral (n + modToNum m)
unLabelOpWith n (Label (LabelOpt LabelIndirectY m) _)    = IndY $ fromIntegral (n + modToNum m)
unLabelOpWith n (Label (LabelOpt LabelAbs m) _)          = Abs $ fromIntegral (n + modToNum m)
unLabelOpWith n (Label (LabelOpt LabelAbsX m) _)         = AbsX $ fromIntegral (n + modToNum m)
unLabelOpWith n (Label (LabelOpt LabelAbsY m) _)         = AbsY $ fromIntegral (n + modToNum m)
unLabelOpWith n (Label (LabelOpt LabelRel m) _)          = Rel $ fromIntegral (n + modToNum m)
unLabelOpWith n (Label (LabelOpt (LabelHighByte l) m) s) = unLabelOpWith (hi (fromIntegral n :: Word16)) (Label (LabelOpt l m) s)
unLabelOpWith n (Label (LabelOpt (LabelLowByte l) m) s)  = unLabelOpWith (lo (fromIntegral n :: Word16)) (Label (LabelOpt l m) s)
unLabelOpWith _ o                                        = o

hi :: Word16 -> Word8
hi w = fromIntegral $ w `shiftR` 8

lo :: Word16 -> Word8
lo w = fromIntegral $ w .&. 0x00ff
