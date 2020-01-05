module ASM.Operand
  ( Operand(..)
  )
  where

import Data.Int
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
  | Label String
  | IndirectLabel String
  | LabelLowByte String
  | LabelHighByte String
  | ArithLabel String Int16
  deriving Eq

instance Show Operand where
  show Acc               = ""
  show (Imm w)           = showImm w
  show (Zpg w)           = showW8 w
  show (ZpgX w)          = showW8 w ++ ",X"
  show (ZpgY w)          = showW8 w ++ ",Y"
  show (Abs w)           = showW16 w
  show (AbsX w)          = showW16 w ++ ",X"
  show (AbsY w)          = showW16 w ++ ",Y"
  show (Ind w)           = "(" ++ showW16 w ++ ")"
  show (IndY w)          = "(" ++ showW8 w ++ "),Y"
  show (IndX w)          = "(" ++ showW8 w ++ ",X)"
  show (Rel i)           = showI8 i
  show (Label s)         = s
  show (IndirectLabel s) = "(" <> s <> ")"
  show (LabelLowByte s)  = "<" <> s
  show (LabelHighByte s) = ">" <> s
  show (ArithLabel s v)  = s <> if v > 0 then "+" else "" <> show v

showImm :: Word8 -> String
showImm w = '#' : showW8 w

showI8 :: Int8 -> String
showI8 i = printf "%s$%02x" (if i < 0 then "-" else "") (abs i)

showW8 :: Word8 -> String
showW8 = printf "$%02x"

showW16 :: Word16 -> String
showW16 = printf "$%04x"
