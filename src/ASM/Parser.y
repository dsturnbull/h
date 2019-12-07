{

module ASM.Parser (
  asm,
  assembly,
  parseAssembly,
  parseTokens,
) where

import ASM.Lexer
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
    sec   { TokenSEC _ }
    sed   { TokenSED _ }
    sei   { TokenSEI _ }
    rol   { TokenROL _ }
    ror   { TokenROR _ }
    rti   { TokenRTI _ }
    rts   { TokenRTS _ }
    sbc   { TokenSBC _ }
    sta   { TokenSTA _ }
    stx   { TokenSTX _ }
    syy   { TokenSTY _ }
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
    '('   { TokenOpenParen _ }
    ')'   { TokenCloseParen _ }
    ':'   { TokenColon _ }
    lbl   { TokenLabel _ $$ }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Entry point
%name assembly

-- Operators
%left '+' '-'
%left '*'
%%

 assembly    : instruction                 { [$1] }
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
             | sec      { SEC    }
             | sei      { SEI    }
             | sed      { SED    }
             | rol oper { ROL $2 }
             | ror oper { ROR $2 }
             | rti      { RTI    }
             | rts      { RTS    }
             | sbc oper { SBC $2 }
             | tax      { TAX    }
             | txa      { TXA    }
             | tay      { TAY    }
             | tya      { TYA    }
             | tsx      { TXS    }
             | txs      { TSX    }
             | labeldef { $1     }
             | brk      { BRK    }

oper         : '#' '$' w8              { Imm  $3 }
oper         :     '$' w16             { Abs  $2 }
oper         :     '$' w16 ',' 'X'     { AbsX $2 }
oper         :     '$' w16 ',' 'Y'     { AbsY $2 }
oper         :     '$' w8              { Zpg  $2 }
oper         :     '$' w8  ',' 'X'     { ZpgX $2 }
oper         :     '$' w8  ',' 'Y'     { ZpgY $2 }
oper         : '(' '$' w16 ',' 'X' ')' { Ind $3  }
oper         : '(' '$' w8  ',' 'X' ')' { IndX $3 }
oper         : '(' '$' w8  ')' ',' 'Y' { IndY $3 }
oper         : lbl                     { Label $1 }

rel          : '$' w8                  { Rel (fromIntegral $2) }
rel          : lbl                     { Label $1 }

labeldef     : lbl ':'                 { LabelDef $1 0 }

{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseAssembly :: String -> Either String [Instruction]
parseAssembly input = runExcept $ do
  tokenStream <- scanTokens input
  assembly tokenStream

-- resolveLabels :: Either String [Instruction]
-- resolveLabels ins = 

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens
}
