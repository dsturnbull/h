{

module ASM.Parser (
  asm,
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
    adc   { TokenADC }
    and   { TokenAND }
    asl   { TokenASL }
    bcc   { TokenBCC }
    bcs   { TokenBCS }
    beq   { TokenBEQ }
    bmi   { TokenBMI }
    bne   { TokenBNE }
    bpl   { TokenBPL }
    bvc   { TokenBVC }
    bvs   { TokenBVS }
    clc   { TokenCLC }
    cld   { TokenCLD }
    cli   { TokenCLI }
    clv   { TokenCLV }
    cmp   { TokenCMP }
    cpx   { TokenCPX }
    cpy   { TokenCPY }
    dec   { TokenDEC }
    dex   { TokenDEX }
    dey   { TokenDEY }
    eor   { TokenEOR }
    inc   { TokenINC }
    inx   { TokenINX }
    iny   { TokenINY }
    jmp   { TokenJMP }
    lda   { TokenLDA }
    ldx   { TokenLDX }
    ldy   { TokenLDY }
    lsr   { TokenLSR }
    nop   { TokenNOP }
    ora   { TokenORA }
    sec   { TokenSEC }
    sed   { TokenSED }
    sei   { TokenSEI }
    rol   { TokenROL }
    tax   { TokenTAX }
    txa   { TokenTXA }
    tay   { TokenTAY }
    tya   { TokenTYA }
    tsx   { TokenTSX }
    txs   { TokenTXS }

    w8    { TokenWord8  $$ }
    w16   { TokenWord16 $$ }
    '$'   { TokenDollar }
    '#'   { TokenHash }
    ','   { TokenComma }
    'X'   { TokenX }
    'Y'   { TokenY }
    '('   { TokenOpenParen }
    ')'   { TokenCloseParen }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Entry point
%name assembly

-- Operators
%left '+' '-'
%left '*'
%%

instructions : instruction                 { [$1] }
             | instruction instructions    { $1 : $2 }

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
             | tax      { TAX    }
             | txa      { TXA    }
             | tay      { TAY    }
             | tya      { TYA    }
             | tsx      { TXS    }
             | txs      { TSX    }

oper         : '#' '$' w8              { Imm  $3 }
oper         :     '$' w16             { Abs  $2 }
oper         :     '$' w16 ',' 'X'     { AbsX $2 }
oper         :     '$' w16 ',' 'Y'     { AbsY $2 }
oper         :     '$' w8              { Zpg  $2 }
oper         :     '$' w8  ',' 'X'     { ZpgX $2 }
oper         :     '$' w8  ',' 'Y'     { ZpgY $2 }
oper         : '(' '$' w16 ',' 'X' ')' { Ind $3 }
oper         : '(' '$' w8  ',' 'X' ')' { IndX $3 }
oper         : '(' '$' w8  ')' ',' 'Y' { IndY $3 }

rel          :     '$' w16            { Addr $2 }
{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseAssembly :: String -> Either String [Instruction]
parseAssembly input = runExcept $ do
  tokenStream <- scanTokens input
  assembly tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens

}
