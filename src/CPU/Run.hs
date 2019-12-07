{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module CPU.Run
  ( load
  , run
  , runShow
  , step
  , Decodes(..)
  ) where

import CPU
import CPU.Instruction
import CPU.Program

import Control.Concurrent
import Control.Lens
import Data.Bits
import Data.Generics.Product.Fields
import Data.Vector.Storable         ((!), (//))
import Data.Word
import Prelude                      hiding (break)

import qualified Data.Vector.Storable as DVS

load :: Int -> Program -> CPU -> CPU
load o (Program bin) cpu = do
  let w = zip [o..] (DVS.toList bin)
  cpu & field @"mem" %~ (\m -> m // w)

run :: CPU -> CPU
run cpu =
  if cpu & p & break
    then cpu
    else run $ step cpu

runShow :: Int -> CPU -> IO CPU
runShow delay cpu = do
  print cpu
  threadDelay delay
  if cpu & p & break
    then return cpu
    else runShow delay $ step cpu

step :: CPU -> CPU
step cpu = do
  cpu & execute ins & updatePC
  where ins      = decode @Instruction mem'
        mem'     = DVS.drop (fromIntegral (pc cpu)) (mem cpu)
        len      = fromIntegral $ insLength ins
        updatePC = if jumps ins then id else field @"pc" %~ (flip (+) len)

class Decodes a where
  decode :: DVS.Vector Word8 -> Instruction

instance Decodes Instruction where
  decode m =
     case instruction of
      0x69 -> ADC (Imm  imm)
      0x65 -> ADC (Zpg  imm)
      0x75 -> ADC (ZpgX imm)
      0x6D -> ADC (Abs  addr)
      0x7D -> ADC (AbsX addr)
      0x79 -> ADC (AbsY addr)
      0x61 -> ADC (IndX imm)
      0x71 -> ADC (IndY imm)

      0x29 -> ADC (Imm  imm)
      0x25 -> ADC (Zpg  imm)
      0x35 -> ADC (ZpgX imm)
      0x2D -> ADC (Abs  addr)
      0x3D -> ADC (AbsX addr)
      0x39 -> ADC (AbsY addr)
      0x21 -> ADC (IndX imm)
      0x31 -> ADC (IndY imm)

      0x10 -> BPL (Rel (fromIntegral imm))
      0x30 -> BMI (Rel (fromIntegral imm))
      0x50 -> BVC (Rel (fromIntegral imm))
      0x70 -> BVS (Rel (fromIntegral imm))
      0x90 -> BCC (Rel (fromIntegral imm))
      0xB0 -> BCS (Rel (fromIntegral imm))
      0xD0 -> BNE (Rel (fromIntegral imm))
      0xF0 -> BEQ (Rel (fromIntegral imm))

      0x0A -> ASL Acc
      0x06 -> ASL (Zpg  imm)
      0x16 -> ASL (ZpgX imm)
      0x0E -> ASL (Abs  addr)
      0x1E -> ASL (AbsX addr)

      0x18 -> CLC
      0x58 -> CLI
      0xB8 -> CLV
      0xD8 -> CLD

      0xC9 -> CMP (Imm imm)
      0xC5 -> CMP (Zpg imm)
      0xD5 -> CMP (ZpgX imm)
      0xCD -> CMP (Abs addr)
      0xDD -> CMP (AbsX addr)
      0xD9 -> CMP (AbsY addr)
      0xC1 -> CMP (IndX imm)
      0xD1 -> CMP (IndY imm)

      0xE0 -> CPX (Imm imm)
      0xE4 -> CPX (Zpg imm)
      0xEC -> CPX (Abs addr)

      0xC0 -> CPY (Imm imm)
      0xC4 -> CPY (Zpg imm)
      0xCC -> CPY (Abs addr)

      0xC6 -> DEC (Zpg imm)
      0xD6 -> DEC (Zpg imm)
      0xCE -> DEC (Abs addr)
      0xDE -> DEC (Abs addr)

      0xCA -> DEX
      0x88 -> DEY

      0x49 -> EOR (Imm imm)
      0x45 -> EOR (Zpg imm)
      0x55 -> EOR (ZpgX imm)
      0x4D -> EOR (Abs addr)
      0x5D -> EOR (AbsX addr)
      0x59 -> EOR (AbsY addr)
      0x41 -> EOR (IndX imm)
      0x51 -> EOR (IndY imm)

      0xE6 -> INC (Zpg imm)
      0xF6 -> INC (Zpg imm)
      0xEE -> INC (Abs addr)
      0xFE -> INC (Abs addr)

      0xE8 -> INX
      0xC8 -> INY

      0x4C -> JMP (Abs addr)
      0x6C -> JMP (Ind addr)

      0x20 -> JSR (Abs addr)

      0xA9 -> LDA (Imm  imm)
      0xAD -> LDA (Abs  addr)
      0xBD -> LDA (AbsX addr)
      0xB9 -> LDA (AbsY addr)
      0xA5 -> LDA (Zpg  imm)
      0xB5 -> LDA (ZpgX imm)
      0xA1 -> LDA (IndX imm)
      0xB1 -> LDA (IndY imm)

      0xA2 -> LDX (Imm  imm)
      0xAE -> LDX (Abs  addr)
      0xBE -> LDX (AbsY addr)
      0xA6 -> LDX (Zpg  imm)
      0xB6 -> LDX (ZpgY imm)

      0xA0 -> LDY (Imm  imm)
      0xAC -> LDY (Abs  addr)
      0xBC -> LDY (AbsX addr)
      0xA4 -> LDY (Zpg  imm)
      0xB4 -> LDY (ZpgX imm)

      0x4A -> LSR Acc
      0x46 -> LSR (Zpg  imm)
      0x56 -> LSR (ZpgX imm)
      0x4E -> LSR (Abs  addr)
      0x5E -> LSR (AbsX addr)

      0xEA -> NOP

      0x09 -> ORA (Imm imm)
      0x05 -> ORA (Zpg imm)
      0x15 -> ORA (ZpgX imm)
      0x0D -> ORA (Abs addr)
      0x1D -> ORA (AbsX addr)
      0x19 -> ORA (AbsY addr)
      0x01 -> ORA (IndX imm)
      0x11 -> ORA (IndY imm)

      0x38 -> SEC
      0x78 -> SEI
      0xF8 -> SED

      0x2A -> ROL Acc
      0x26 -> ROL (Zpg  imm)
      0x36 -> ROL (ZpgX imm)
      0x2E -> ROL (Abs  addr)
      0x3E -> ROL (AbsX addr)

      0x6A -> ROR Acc
      0x66 -> ROR (Zpg  imm)
      0x76 -> ROR (ZpgX imm)
      0x6E -> ROR (Abs  addr)
      0x7E -> ROR (AbsX addr)

      0x40 -> RTI
      0x60 -> RTS

      0xE9 -> SBC (Imm  imm)
      0xE5 -> SBC (Zpg  imm)
      0xF5 -> SBC (ZpgX imm)
      0xED -> SBC (Abs  addr)
      0xFD -> SBC (AbsX addr)
      0xF9 -> SBC (AbsY addr)
      0xE1 -> SBC (IndX imm)
      0xF1 -> SBC (IndY imm)

      0x85 -> STA (Zpg imm)
      0x95 -> STA (ZpgX imm)
      0x8D -> STA (Abs addr)
      0x9D -> STA (AbsX addr)
      0x99 -> STA (AbsY addr)
      0x81 -> STA (IndY imm)
      0x91 -> STA (IndX imm)

      0x86 -> STX (Zpg imm)
      0x96 -> STX (ZpgY imm)
      0x8E -> STX (Abs addr)

      0x84 -> STY (Zpg imm)
      0x94 -> STY (ZpgX imm)
      0x8C -> STY (Abs addr)

      0xAA -> TAX
      0x8A -> TXA
      0xA8 -> TAY
      0x98 -> TYA
      0x9A -> TSX
      0xBA -> TXS

      0x00 -> BRK
      _    -> undefined

    where instruction = m ! 0
          imm         = m ! 1
          addr        = (w16 $ DVS.take 2 (DVS.drop 1 m))

execute :: Instruction -> CPU -> CPU
execute i =
    case i of
      ADC (Imm w)  -> adcImm w
      ADC (Zpg w)  -> adcZpg w
      ADC (ZpgX w) -> adcZpgX w
      ADC (Abs w)  -> adcAbs w
      ADC (AbsX w) -> adcAbsX w
      ADC (AbsY w) -> adcAbsY w
      ADC (IndX w) -> adcIndX w
      ADC (IndY w) -> adcIndY w
      ADC _        -> undefined

      AND (Imm w)  -> andImm w
      AND (Zpg w)  -> andZpg w
      AND (ZpgX w) -> andZpgX w
      AND (Abs w)  -> andAbs w
      AND (AbsX w) -> andAbsX w
      AND (AbsY w) -> andAbsY w
      AND (IndX w) -> andIndX w
      AND (IndY w) -> andIndY w
      AND _        -> undefined

      ASL Acc      -> aslAcc
      ASL (Zpg w)  -> aslZpg w
      ASL (ZpgX w) -> aslZpgX w
      ASL (Abs w)  -> aslAbs w
      ASL (AbsX w) -> aslAbsX w
      ASL _        -> undefined

      BPL (Rel w)  -> bpl w
      BMI (Rel w)  -> bmi w
      BVC (Rel w)  -> bvc w
      BVS (Rel w)  -> bvs w
      BCC (Rel w)  -> bcc w
      BCS (Rel w)  -> bcs w
      BNE (Rel w)  -> bne w
      BEQ (Rel w)  -> beq w

      BPL _        -> undefined
      BMI _        -> undefined
      BVC _        -> undefined
      BVS _        -> undefined
      BCC _        -> undefined
      BCS _        -> undefined
      BNE _        -> undefined
      BEQ _        -> undefined

      CLC          -> clc
      CLI          -> cli
      CLV          -> clv
      CLD          -> cld

      CMP (Imm  w) -> cmpImm w
      CMP (Zpg  w) -> cmpZpg w
      CMP (ZpgX w) -> cmpZpgX w
      CMP (Abs  w) -> cmpAbs w
      CMP (AbsX w) -> cmpAbsX w
      CMP (AbsY w) -> cmpAbsY w
      CMP (IndX w) -> cmpIndX w
      CMP (IndY w) -> cmpIndY w
      CMP _        -> undefined

      CPX (Imm w)  -> cpxImm w
      CPX (Zpg w)  -> cpxZpg w
      CPX (Abs w)  -> cpxAbs w
      CPX _        -> undefined

      CPY (Imm w)  -> cpyImm w
      CPY (Zpg w)  -> cpyZpg w
      CPY (Abs w)  -> cpyAbs w
      CPY _        -> undefined

      DEC (Zpg w)  -> decZpg w
      DEC (ZpgX w) -> decZpgX w
      DEC (Abs w)  -> decAbs w
      DEC (AbsX w) -> decAbsX w
      DEC _        -> undefined

      DEX          -> dex
      DEY          -> dey

      EOR (Imm  w) -> eorImm w
      EOR (Zpg  w) -> eorZpg w
      EOR (ZpgX w) -> eorZpgX w
      EOR (Abs  w) -> eorAbs w
      EOR (AbsX w) -> eorAbsX w
      EOR (AbsY w) -> eorAbsY w
      EOR (IndX w) -> eorIndX w
      EOR (IndY w) -> eorIndY w
      EOR _        -> undefined

      INC (Zpg w)  -> incZpg w
      INC (ZpgX w) -> incZpgX w
      INC (Abs w)  -> incAbs w
      INC (AbsX w) -> incAbsX w
      INC _        -> undefined

      INX          -> inx
      INY          -> iny

      JMP (Abs w)  -> jmpAbs w
      JMP (Ind w)  -> jmpInd w
      JMP _        -> undefined

      JSR (Abs w)  -> jsrAbs w
      JSR _        -> undefined

      LDA (Imm w)  -> ldaImm w
      LDA (Zpg w)  -> ldaZpg w
      LDA (ZpgX w) -> ldaZpgX w
      LDA (Abs w)  -> ldaAbs w
      LDA (AbsX w) -> ldaAbsX w
      LDA (AbsY w) -> ldaAbsY w
      LDA (IndX w) -> ldaIndX w
      LDA (IndY w) -> ldaIndY w
      LDA _        -> undefined

      LDX (Imm w)  -> ldxImm w
      LDX (Zpg w)  -> ldxZpg w
      LDX (ZpgY w) -> ldxZpgY w
      LDX (Abs w)  -> ldxAbs w
      LDX (AbsY w) -> ldxAbsY w
      LDX _        -> undefined

      LDY (Imm w)  -> ldyImm w
      LDY (Zpg w)  -> ldyZpg w
      LDY (ZpgX w) -> ldyZpgX w
      LDY (Abs w)  -> ldyAbs w
      LDY (AbsX w) -> ldyAbsX w
      LDY _        -> undefined

      LSR Acc      -> lsrAcc
      LSR (Zpg w)  -> lsrZpg w
      LSR (ZpgX w) -> lsrZpgX w
      LSR (Abs w)  -> lsrAbs w
      LSR (AbsX w) -> lsrAbsX w
      LSR _        -> undefined

      NOP          -> nop

      ORA (Imm  w) -> oraImm w
      ORA (Zpg  w) -> oraZpg w
      ORA (ZpgX w) -> oraZpgX w
      ORA (Abs  w) -> oraAbs w
      ORA (AbsX w) -> oraAbsX w
      ORA (AbsY w) -> oraAbsY w
      ORA (IndX w) -> oraIndX w
      ORA (IndY w) -> oraIndY w
      ORA _        -> undefined

      RTI          -> rti
      RTS          -> rts

      SBC (Imm w)  -> sbcImm w
      SBC (Zpg w)  -> sbcZpg w
      SBC (ZpgX w) -> sbcZpgX w
      SBC (Abs w)  -> sbcAbs w
      SBC (AbsX w) -> sbcAbsX w
      SBC (AbsY w) -> sbcAbsY w
      SBC (IndX w) -> sbcIndX w
      SBC (IndY w) -> sbcIndY w
      SBC _        -> undefined

      STA (Zpg  w) -> staZpg w
      STA (ZpgX w) -> staZpgX w
      STA (Abs  w) -> staAbs w
      STA (AbsX w) -> staAbsX w
      STA (AbsY w) -> staAbsY w
      STA (IndX w) -> staIndX w
      STA (IndY w) -> staIndY w
      STA _        -> undefined

      STX (Zpg  w) -> stxZpg w
      STX (ZpgY w) -> stxZpgY w
      STX (Abs  w) -> stxAbs w
      STX _        -> undefined

      STY (Zpg  w) -> styZpg w
      STY (ZpgX w) -> styZpgX w
      STY (Abs  w) -> styAbs w
      STY _        -> undefined

      TAX          -> tax
      TXA          -> txa
      TAY          -> tay
      TYA          -> tya
      TSX          -> tsx
      TXS          -> txs

      ROL Acc      -> rolAcc
      ROL (Zpg w)  -> rolZpg w
      ROL (ZpgX w) -> rolZpgX w
      ROL (Abs w)  -> rolAbs w
      ROL (AbsX w) -> rolAbsX w
      ROL _        -> undefined

      ROR Acc      -> rorAcc
      ROR (Zpg w)  -> rorZpg w
      ROR (ZpgX w) -> rorZpgX w
      ROR (Abs w)  -> rorAbs w
      ROR (AbsX w) -> rorAbsX w
      ROR _        -> undefined

      SEC          -> sec
      SEI          -> sei
      SED          -> sed

      BRK          -> brk

w16 :: DVS.Vector Word8 -> Word16
w16 v = (addrH `shiftL` 8) .|. addrL
  where addrL = fromIntegral (v ! 0)
        addrH = fromIntegral (v ! 1)
