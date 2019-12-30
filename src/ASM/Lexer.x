{
{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE FlexibleContexts #-}

module ASM.Lexer (
  Token(..),
  scanTokens
) where

import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Word
import Control.Monad.Except
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$hex   = [0-9a-fA-F]
$eol   = [\n]
$label = [a-zA-Z0-9_]
$bits  = [01]
$chr   = .
@character = . # \" # \\
@string = \" (@character+)? \"

tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  ";".*                         ;

  -- Instructions
  adc                           { tok (\p _ -> TokenADC p) }
  and                           { tok (\p _ -> TokenAND p) }
  asl                           { tok (\p _ -> TokenASL p) }
  bcc                           { tok (\p _ -> TokenBCC p) }
  bcs                           { tok (\p _ -> TokenBCS p) }
  beq                           { tok (\p _ -> TokenBEQ p) }
  bmi                           { tok (\p _ -> TokenBMI p) }
  bne                           { tok (\p _ -> TokenBNE p) }
  bpl                           { tok (\p _ -> TokenBPL p) }
  bvc                           { tok (\p _ -> TokenBVC p) }
  bvs                           { tok (\p _ -> TokenBVS p) }
  clc                           { tok (\p _ -> TokenCLC p) }
  cli                           { tok (\p _ -> TokenCLI p) }
  clv                           { tok (\p _ -> TokenCLV p) }
  cld                           { tok (\p _ -> TokenCLD p) }
  cmp                           { tok (\p _ -> TokenCMP p) }
  cpx                           { tok (\p _ -> TokenCPX p) }
  cpy                           { tok (\p _ -> TokenCPY p) }
  dec                           { tok (\p _ -> TokenDEC p) }
  dex                           { tok (\p _ -> TokenDEX p) }
  dey                           { tok (\p _ -> TokenDEY p) }
  eor                           { tok (\p _ -> TokenEOR p) }
  inc                           { tok (\p _ -> TokenINC p) }
  inx                           { tok (\p _ -> TokenINX p) }
  iny                           { tok (\p _ -> TokenINY p) }
  jmp                           { tok (\p _ -> TokenJMP p) }
  jsr                           { tok (\p _ -> TokenJSR p) }
  lda                           { tok (\p _ -> TokenLDA p) }
  ldx                           { tok (\p _ -> TokenLDX p) }
  ldy                           { tok (\p _ -> TokenLDY p) }
  lsr                           { tok (\p _ -> TokenLSR p) }
  nop                           { tok (\p _ -> TokenNOP p) }
  ora                           { tok (\p _ -> TokenORA p) }
  pha                           { tok (\p _ -> TokenPHA p) }
  php                           { tok (\p _ -> TokenPHP p) }
  pla                           { tok (\p _ -> TokenPLA p) }
  plp                           { tok (\p _ -> TokenPLP p) }
  rol                           { tok (\p _ -> TokenROL p) }
  ror                           { tok (\p _ -> TokenROR p) }
  rti                           { tok (\p _ -> TokenRTI p) }
  rts                           { tok (\p _ -> TokenRTS p) }
  sbc                           { tok (\p _ -> TokenSBC p) }
  sec                           { tok (\p _ -> TokenSEC p) }
  sei                           { tok (\p _ -> TokenSEI p) }
  sed                           { tok (\p _ -> TokenSED p) }
  sta                           { tok (\p _ -> TokenSTA p) }
  stx                           { tok (\p _ -> TokenSTX p) }
  sty                           { tok (\p _ -> TokenSTY p) }
  tax                           { tok (\p _ -> TokenTAX p) }
  txa                           { tok (\p _ -> TokenTXA p) }
  tay                           { tok (\p _ -> TokenTAY p) }
  tya                           { tok (\p _ -> TokenTYA p) }
  tsx                           { tok (\p _ -> TokenTSX p) }
  txs                           { tok (\p _ -> TokenTXS p) }
  brk                           { tok (\p _ -> TokenBRK p) }

  -- Syntax
  $hex{2}                       { tok (\p s -> TokenWord8  p (read ("0x" <> s))) }
  $hex{4}                       { tok (\p s -> TokenWord16 p (read ("0x" <> s))) }
  "_" $label+                   { tok TokenLabel }
  "$"                           { tok (\p _ -> TokenDollar p) }
  "#"                           { tok (\p _ -> TokenHash p) }
  ","                           { tok (\p _ -> TokenComma p) }
  "X"                           { tok (\p _ -> TokenX p) }
  "Y"                           { tok (\p _ -> TokenY p) }
  "("                           { tok (\p _ -> TokenOpenParen p) }
  ")"                           { tok (\p _ -> TokenCloseParen p) }
  ":"                           { tok (\p _ -> TokenColon p) }
  "<"                           { tok (\p _ -> TokenLowByte p) }
  ">"                           { tok (\p _ -> TokenHighByte p) }
  "%"                           { tok (\p _ -> TokenPercent p) }
  "'"                           { tok (\p _ -> TokenQuote p) }
  $bits{8}                      { tok (\p s -> TokenWord8 p (fromIntegral $ toDec s)) }
  '$chr{1}'                     { tok (\p s -> TokenWord8 p (fromIntegral $ ord (s !! 1))) }

  -- Control
  .code                         { tok (\p _ -> TokenCode p) }
  .data                         { tok (\p _ -> TokenData p) }
  .byte                         { tok (\p _ -> TokenBytes p) }
  .org                          { tok (\p _ -> TokenOrigin p) }

  -- Macros
  !bin                          { tok (\p _ -> TokenBinary p) }
  @string                       { tok (\p s -> TokenString p (read s)) }
{
tok f = f

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

data Token 
  = TokenADC AlexPosn
  | TokenAND AlexPosn
  | TokenASL AlexPosn
  | TokenBCC AlexPosn
  | TokenBCS AlexPosn
  | TokenBEQ AlexPosn
  | TokenBMI AlexPosn
  | TokenBNE AlexPosn
  | TokenBPL AlexPosn
  | TokenBVC AlexPosn
  | TokenBVS AlexPosn
  | TokenCLC AlexPosn
  | TokenCLI AlexPosn
  | TokenCLV AlexPosn
  | TokenCLD AlexPosn
  | TokenCMP AlexPosn
  | TokenCPX AlexPosn
  | TokenCPY AlexPosn
  | TokenDEC AlexPosn
  | TokenDEX AlexPosn
  | TokenDEY AlexPosn
  | TokenEOR AlexPosn
  | TokenINC AlexPosn
  | TokenINX AlexPosn
  | TokenINY AlexPosn
  | TokenJMP AlexPosn
  | TokenJSR AlexPosn
  | TokenLDA AlexPosn
  | TokenLDX AlexPosn
  | TokenLDY AlexPosn
  | TokenLSR AlexPosn
  | TokenNOP AlexPosn
  | TokenORA AlexPosn
  | TokenPHA AlexPosn
  | TokenPHP AlexPosn
  | TokenPLA AlexPosn
  | TokenPLP AlexPosn
  | TokenROL AlexPosn
  | TokenROR AlexPosn
  | TokenRTI AlexPosn
  | TokenRTS AlexPosn
  | TokenSBC AlexPosn
  | TokenSEC AlexPosn
  | TokenSEI AlexPosn
  | TokenSED AlexPosn
  | TokenSTA AlexPosn
  | TokenSTX AlexPosn
  | TokenSTY AlexPosn
  | TokenTAX AlexPosn
  | TokenTXA AlexPosn
  | TokenTAY AlexPosn
  | TokenTYA AlexPosn
  | TokenTSX AlexPosn
  | TokenTXS AlexPosn
  | TokenBRK AlexPosn
  | TokenWord8  AlexPosn Word8
  | TokenWord16 AlexPosn Word16
  | TokenDollar AlexPosn
  | TokenHash AlexPosn
  | TokenComma AlexPosn
  | TokenX AlexPosn
  | TokenY AlexPosn
  | TokenOpenParen AlexPosn
  | TokenCloseParen AlexPosn
  | TokenEOF AlexPosn
  | TokenLabel AlexPosn String
  | TokenColon AlexPosn
  | TokenHighByte AlexPosn
  | TokenLowByte AlexPosn
  | TokenPercent AlexPosn
  | TokenQuote AlexPosn
  | TokenCode AlexPosn
  | TokenData AlexPosn
  | TokenBytes AlexPosn
  | TokenBinary AlexPosn
  | TokenString AlexPosn String
  | TokenOrigin AlexPosn
  deriving (Eq,Show)

scanTokens :: String -> Except String [Token]
scanTokens str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str') =
          case alexScan inp 0 of
                AlexEOF -> return []
                AlexError ((AlexPn _ line column),_,_,_) -> throwError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                AlexSkip  inp' _       -> go inp'
                AlexToken inp' len act -> do
                  res <- go inp'
                  let rest = act pos (take len str')
                  return (rest : res)
}
