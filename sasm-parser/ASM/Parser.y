{
{-# LANGUAGE ScopedTypeVariables #-}

module ASM.Parser (
  assembly,
  parseAssembly,
  parseTokens,
) where

import ASM.Lexer
import ASM.Operand
import ASM.Program

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Bits
import Data.Word

import qualified Data.ByteString as BS

}

-- Lexer structure
%tokentype { Token }

-- Token Names
%token
    adc   { TokenADC _ }
    and   { TokenAND _ }
    asl   { TokenASL _ }
    bcc   { TokenBCC _ }
    bcs   { TokenBCS _ }
    beq   { TokenBEQ _ }
    bmi   { TokenBMI _ }
    bne   { TokenBNE _ }
    bpl   { TokenBPL _ }
    bvc   { TokenBVC _ }
    bvs   { TokenBVS _ }
    clc   { TokenCLC _ }
    cld   { TokenCLD _ }
    cli   { TokenCLI _ }
    clv   { TokenCLV _ }
    cmp   { TokenCMP _ }
    cpx   { TokenCPX _ }
    cpy   { TokenCPY _ }
    dec   { TokenDEC _ }
    dex   { TokenDEX _ }
    dey   { TokenDEY _ }
    eor   { TokenEOR _ }
    inc   { TokenINC _ }
    inx   { TokenINX _ }
    iny   { TokenINY _ }
    jmp   { TokenJMP _ }
    jsr   { TokenJSR _ }
    lda   { TokenLDA _ }
    ldx   { TokenLDX _ }
    ldy   { TokenLDY _ }
    lsr   { TokenLSR _ }
    nop   { TokenNOP _ }
    ora   { TokenORA _ }
    pha   { TokenPHA _ }
    php   { TokenPHP _ }
    pla   { TokenPLA _ }
    plp   { TokenPLP _ }
    rol   { TokenROL _ }
    ror   { TokenROR _ }
    rti   { TokenRTI _ }
    rts   { TokenRTS _ }
    sbc   { TokenSBC _ }
    sec   { TokenSEC _ }
    sed   { TokenSED _ }
    sei   { TokenSEI _ }
    sta   { TokenSTA _ }
    stx   { TokenSTX _ }
    sty   { TokenSTY _ }
    tax   { TokenTAX _ }
    txa   { TokenTXA _ }
    tay   { TokenTAY _ }
    tya   { TokenTYA _ }
    tsx   { TokenTSX _ }
    txs   { TokenTXS _ }
    brk   { TokenBRK _ }

    w8    { TokenWord8  _ $$ }
    w16   { TokenWord16 _ $$ }
    num   { TokenNum  _ $$ }
    '#'   { TokenHash _ }
    ','   { TokenComma _ }
    'X'   { TokenX _ }
    'Y'   { TokenY _ }
    lbl   { TokenLabel _ $$ }
    '('   { TokenOpenParen _ }
    ')'   { TokenCloseParen _ }
    ':'   { TokenColon _ }
    '<'   { TokenLowByte _ }
    '>'   { TokenHighByte _ }
    '%'   { TokenPercent _ }
    '\''  { TokenQuote _ }
    '='   { TokenEquals _ }
    '+'   { TokenPlus _ }
    '-'   { TokenMinus _ }

    code  { TokenCode _ }
    data  { TokenData _ }
    byte  { TokenBytes _ }
    bin   { TokenBinary _ }
    str   { TokenString _ $$ }
    org   { TokenOrigin _ }

-- Parser monad
%monad { ExceptT String IO } { (>>=) } { return }
%error { parseError }

-- Entry point
%name assembly

-- Operators
%left '+' '-'
%left '*'
%%

assembly     : instruction             { [$1] }
             | instruction assembly    { $1 : $2 }

instruction  : adc oper                { ADC $2   }
             | and oper                { AND $2   }
             | asl oper                { ASL $2   }
             | asl                     { ASL Acc  }
             | bcc rel                 { BCC $2   }
             | bcs rel                 { BCS $2   }
             | beq rel                 { BEQ $2   }
             | bmi rel                 { BMI $2   }
             | bne rel                 { BNE $2   }
             | bne rel                 { BNE $2   }
             | bne rel                 { BNE $2   }
             | bpl rel                 { BPL $2   }
             | bvc rel                 { BVC $2   }
             | bvs rel                 { BVS $2   }
             | clc                     { CLC      }
             | cli                     { CLI      }
             | clv                     { CLV      }
             | cld                     { CLD      }
             | cmp oper                { CMP $2   }
             | cpx oper                { CPX $2   }
             | cpy oper                { CPY $2   }
             | dec oper                { DEC $2   }
             | dex                     { DEX      }
             | dey                     { DEY      }
             | eor oper                { EOR $2   }
             | inc oper                { INC $2   }
             | inx                     { INX      }
             | iny                     { INY      }
             | jmp oper                { JMP $2   }
             | jsr oper                { JSR $2   }
             | lda oper                { LDA $2   }
             | ldx oper                { LDX $2   }
             | ldy oper                { LDY $2   }
             | lsr oper                { LSR $2   }
             | nop                     { NOP      }
             | ora oper                { ORA $2   }
             | pha                     { PHA      }
             | php                     { PHP      }
             | pla                     { PLA      }
             | plp                     { PLP      }
             | rol oper                { ROL $2   }
             | ror oper                { ROR $2   }
             | rti                     { RTI      }
             | rts                     { RTS      }
             | sbc oper                { SBC $2   }
             | sec                     { SEC      }
             | sei                     { SEI      }
             | sed                     { SED      }
             | sta oper                { STA $2   }
             | stx oper                { STX $2   }
             | sty oper                { STY $2   }
             | tax                     { TAX      }
             | txa                     { TXA      }
             | tay                     { TAY      }
             | tya                     { TYA      }
             | tsx                     { TXS      }
             | txs                     { TSX      }
             | brk                     { BRK      }
             | label                   { $1       }
             | variable                { $1       }
             | code                    { Code     }
             | data                    { Data     }
             | byte bytes              { Bytes $2 }
             | bin str                 {% do contents <- ExceptT (fmap Right (BS.readFile $2)); return (Bytes (BS.unpack contents)) }
             | org w16                 { Origin $2 }

oper         : '#'  nm                 { Imm $2 }
oper         : '#'  lbl                { Label (LabelOpt LabelImm NoMod) $2 }
oper         : '#'  lbl '+' num        { Label (LabelOpt LabelImm (Plus (fromIntegral $4))) $2 }
oper         : '#'  lbl '-' num        { Label (LabelOpt LabelImm (Minus (fromIntegral $4))) $2 }
oper         : '<' '#' w16             { Imm (l $3) }
oper         : '>' '#' w16             { Imm (h $3) }
oper         :     w16                 { Abs  $1 }
oper         :     w16 ',' 'X'         { AbsX $1 }
oper         :     lbl ',' 'X'         { Label (LabelOpt LabelAbsX NoMod) $1 } -- no zpg
oper         :     lbl '+' num ',' 'X' { Label (LabelOpt LabelAbsX (Plus (fromIntegral $3))) $1 } -- no zpg
oper         :     w16 ',' 'Y'         { AbsY $1 }
oper         :     lbl '+' num ',' 'Y' { Label (LabelOpt LabelAbsY (Minus (fromIntegral $3))) $1 } -- no zpg
oper         :     lbl ',' 'Y'         { Label (LabelOpt LabelAbsY NoMod) $1 } -- no zpg
oper         :      nm                 { Zpg  $1 }
oper         :      nm     ',' 'X'     { ZpgX $1 }
oper         :      nm     ',' 'Y'     { ZpgY $1 }
oper         : '(' w16 ')'             { Ind $2 }
oper         : '(' lbl     ')'         { Label (LabelOpt LabelIndirect NoMod) $2 }
oper         : '('  nm     ',' 'X' ')' { IndX $2 }
oper         : '(' lbl     ',' 'X' ')' { Label (LabelOpt LabelIndirectX NoMod) $2 }
oper         : '('  nm     ')' ',' 'Y' { IndY $2 }
oper         : '(' lbl     ')' ',' 'Y' { Label (LabelOpt LabelIndirectY NoMod) $2 }
oper         : lbl                     { Label (LabelOpt LabelAbs NoMod) $1 }
oper         : lbl '+' num             { Label (LabelOpt LabelAbs (Plus (fromIntegral $3))) $1 }
oper         : lbl '-' num             { Label (LabelOpt LabelAbs (Minus (fromIntegral $3))) $1 }
oper         : '<' '#' lbl             { Label (LabelOpt (LabelLowByte LabelImm) NoMod) $3 }
oper         : '>' '#' lbl             { Label (LabelOpt (LabelHighByte LabelImm) NoMod) $3 }

rel          : w8                      { Rel (fromIntegral $1) }
rel          : lbl                     { Label (LabelOpt LabelRel NoMod) $1 }

nm           : w8                      { $1 }

label        : lbl ':'                 { LabelDef $1 }

variable     : lbl '=' w8              { Variable8 $1 $3 }
variable     : lbl '=' w16             { Variable16 $1 $3 }

bytes        : byteval                 { [$1] }
             | byteval ',' bytes       { $1 : $3 }
byteval      : w8                      { $1 }

{
parseError :: [Token] -> ExceptT String IO a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseAssembly :: String -> IO (Either String [Opcode])
parseAssembly input =
  case runExcept (scanTokens input) of
    Left e -> error e
    Right tokens -> runExceptT (assembly tokens)

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens

h :: Word16 -> Word8
h w = fromIntegral $ w `shiftR` 8

l :: Word16 -> Word8
l w = fromIntegral $ w .&. 0x00ff
}
