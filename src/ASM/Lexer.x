{
{-# LANGUAGE FlexibleContexts #-}

module ASM.Lexer (
  Token(..),
  scanTokens
) where

import Control.Monad.Except
import Data.Word

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$hex   = [0-9a-fA-F]
$eol   = [\n]

tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  ";".*                         ;

  -- Syntax
  $hex{2}                       { \s -> TokenWord8  (read ("0x" <> s)) }
  $hex{4}                       { \s -> TokenWord16 (read ("0x" <> s)) }
  
  -- Instructions
  adc                           { const TokenADC }
  and                           { const TokenAND }
  asl                           { const TokenASL }
  bcc                           { const TokenBCC }
  bcs                           { const TokenBCS }
  beq                           { const TokenBEQ }
  bmi                           { const TokenBMI }
  bne                           { const TokenBNE }
  bpl                           { const TokenBPL }
  bvc                           { const TokenBVC }
  bvs                           { const TokenBVS }
  clc                           { const TokenCLC }
  cli                           { const TokenCLI }
  clv                           { const TokenCLV }
  cld                           { const TokenCLD }
  cmp                           { const TokenCMP }
  cpx                           { const TokenCPX }
  cpy                           { const TokenCPY }
  dec                           { const TokenDEC }
  dex                           { const TokenDEX }
  dey                           { const TokenDEY }
  eor                           { const TokenEOR }
  inc                           { const TokenINC }
  inx                           { const TokenINX }
  iny                           { const TokenINY }
  jmp                           { const TokenJMP }
  lda                           { const TokenLDA }
  ldx                           { const TokenLDX }
  ldy                           { const TokenLDY }
  lsr                           { const TokenLSR }
  nop                           { const TokenNOP }
  ora                           { const TokenORA }
  sec                           { const TokenSEC }
  sei                           { const TokenSEI }
  sed                           { const TokenSED }
  rol                           { const TokenROL }
  tax                           { const TokenTAX }
  txa                           { const TokenTXA }
  tay                           { const TokenTAY }
  tya                           { const TokenTYA }
  tsx                           { const TokenTSX }
  txs                           { const TokenTXS }

  -- Syntax
  "$"                           { const TokenDollar }
  "#"                           { const TokenHash }
  ","                           { const TokenComma }
  "X"                           { const TokenX }
  "Y"                           { const TokenY }
  "("                           { const TokenOpenParen }
  ")"                           { const TokenCloseParen }

{
data Token 
  = TokenADC
  | TokenAND
  | TokenASL
  | TokenBCC
  | TokenBCS
  | TokenBEQ
  | TokenBMI
  | TokenBNE
  | TokenBPL
  | TokenBVC
  | TokenBVS
  | TokenCLC
  | TokenCLI
  | TokenCLV
  | TokenCLD
  | TokenCMP
  | TokenCPX
  | TokenCPY
  | TokenDEC
  | TokenDEX
  | TokenDEY
  | TokenEOR
  | TokenINC
  | TokenINX
  | TokenINY
  | TokenJMP
  | TokenLDA
  | TokenLDX
  | TokenLDY
  | TokenLSR
  | TokenNOP
  | TokenORA
  | TokenSEC
  | TokenSEI
  | TokenSED
  | TokenROL
  | TokenTAX
  | TokenTXA
  | TokenTAY
  | TokenTYA
  | TokenTSX
  | TokenTXS
  | TokenWord8  Word8
  | TokenWord16 Word16
  | TokenDollar
  | TokenHash
  | TokenComma
  | TokenX
  | TokenY
  | TokenOpenParen
  | TokenCloseParen
  | TokenEOF
  deriving (Eq,Show)

scanTokens :: String -> Except String [Token]
scanTokens str = go ('\n',[],str) where 
  go inp@(_,_bs,str') =
    case alexScan inp 0 of
     AlexEOF -> return []
     AlexError _ -> throwError "Invalid lexeme."
     AlexSkip  inp' _       -> go inp'
     AlexToken inp' len act -> do
      res <- go inp'
      let rest = act (take len str')
      return (rest : res)
}
