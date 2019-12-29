{

module ASM.Parser (
  assembly,
  parseAssembly,
  parseTokens,
) where

import CPU (l, h)
import ASM.Lexer
import CPU.Instructions.Opcode
import CPU.Instructions.Operand
import CPU.Program

import Control.Monad.Except

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
    '$'   { TokenDollar _ }
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

    code  { TokenCode _ }
    data  { TokenData _ }
    byte  { TokenBytes _ }
    bin   { TokenBinary _ }
    string { TokenString _ $$ }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Entry point
%name assembly

-- Operators
%left '+' '-'
%left '*'
%%

assembly     : instruction                 { [$1] }
             | instruction assembly        { $1 : $2 }

instruction  : adc oper { ADC $2 }
             | and oper { AND $2 }
             | asl oper { ASL $2 }
             | bcc rel  { BCC $2 }
             | bcs rel  { BCS $2 }
             | beq rel  { BEQ $2 }
             | bmi rel  { BMI $2 }
             | bne rel  { BNE $2 }
             | bne rel  { BNE $2 }
             | bne rel  { BNE $2 }
             | bpl rel  { BPL $2 }
             | bvc rel  { BVC $2 }
             | bvs rel  { BVS $2 }
             | clc      { CLC    }
             | cli      { CLI    }
             | clv      { CLV    }
             | cld      { CLD    }
             | cmp oper { CMP $2 }
             | cpx oper { CPX $2 }
             | cpy oper { CPY $2 }
             | dec oper { DEC $2 }
             | dex      { DEX    }
             | dey      { DEY    }
             | eor oper { EOR $2 }
             | inc oper { INC $2 }
             | inx      { INX    }
             | iny      { INY    }
             | jmp oper { JMP $2 }
             | jsr oper { JSR $2 }
             | lda oper { LDA $2 }
             | ldx oper { LDX $2 }
             | ldy oper { LDY $2 }
             | lsr oper { LSR $2 }
             | nop      { NOP    }
             | ora oper { ORA $2 }
             | pha      { PHA    }
             | php      { PHP    }
             | pla      { PLA    }
             | plp      { PLP    }
             | rol oper { ROL $2 }
             | ror oper { ROR $2 }
             | rti      { RTI    }
             | rts      { RTS    }
             | sbc oper { SBC $2 }
             | sec      { SEC    }
             | sei      { SEI    }
             | sed      { SED    }
             | sta oper { STA $2 }
             | stx oper { STX $2 }
             | sty oper { STY $2 }
             | tax      { TAX    }
             | txa      { TXA    }
             | tay      { TAY    }
             | tya      { TYA    }
             | tsx      { TXS    }
             | txs      { TSX    }
             | brk      { BRK    }
             | labeldef { $1     }
             | code     { Code   }
             | data     { Data   }
             | byte bytes { Bytes $2 }
             | bin string { Binary $2 }

oper         : '#'  nm                 { Imm  $2 }
oper         : '<' '#' '$' w16         { Imm (l $4) }
oper         : '>' '#' '$' w16         { Imm (h $4) }
oper         :     '$' w16             { Abs  $2 }
oper         :     '$' w16 ',' 'X'     { AbsX $2 }
oper         :     '$' w16 ',' 'Y'     { AbsY $2 }
oper         :      nm                 { Zpg  $1 }
oper         :      nm     ',' 'X'     { ZpgX $1 }
oper         :      nm     ',' 'Y'     { ZpgY $1 }
oper         : '(' '$' w16 ',' 'X' ')' { Ind $3  }
oper         : '('  nm     ',' 'X' ')' { IndX $2 }
oper         : '('  nm     ')' ',' 'Y' { IndY $2 }
oper         : '(' lbl ')'             { IndirectLabel $2 }
oper         : lbl                     { Label $1 }
oper         : '<' lbl                 { LabelLowByte $2 }
oper         : '>' lbl                 { LabelHighByte $2 }

rel          : '$'  w8                 { Rel (fromIntegral $2) }
rel          : lbl                     { Label $1 }

nm           : '$'  w8                 { $2 }
nm           : '%'  w8                 { $2 }
nm           : w8                      { $1 } -- single-quoted

labeldef     : lbl ':'                 { LabelDef $1 }

bytes        : byteval                 { [$1] }
             | byteval ',' bytes       { $1 : $3 }
byteval      : '$' w8                  { $2 }

{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseAssembly :: String -> Either String [Opcode]
parseAssembly input = runExcept $ do
  tokenStream <- scanTokens input
  assembly tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens
}
