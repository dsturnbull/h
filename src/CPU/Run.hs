{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
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

runShow :: CPU -> IO CPU
runShow cpu = do
  print cpu
  threadDelay 400000
  if cpu & p & break
    then return cpu
    else runShow $ step cpu

step :: CPU -> CPU
step cpu = do
  cpu & execute ins
      & field @"pc" %~ (flip (+) len)
  where ins  = decode @Instruction mem'
        mem' = DVS.drop (fromIntegral (pc cpu)) (mem cpu)
        len  = fromIntegral $ insLength ins

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

      0x00 -> BRK
      _    -> undefined

    where instruction = m ! 0
          imm         = m ! 1
          addr        = (w16 $ DVS.take 2 (DVS.drop 1 m))

w16 :: DVS.Vector Word8 -> Word16
w16 v = (addrH `shiftL` 8) .|. addrL
  where addrL = fromIntegral (v ! 0)
        addrH = fromIntegral (v ! 1)

execute :: Instruction -> CPU -> CPU
execute i cpu =
  case i of
    ADC (Imm w)  -> cpu & adcImm w
    ADC (Zpg w)  -> cpu & adcZpg w
    ADC (ZpgX w) -> cpu & adcZpgX w
    ADC (Abs w)  -> cpu & adcAbs w
    ADC (AbsX w) -> cpu & adcAbsX w
    ADC (AbsY w) -> cpu & adcAbsY w
    ADC (IndX w) -> cpu & adcIndX w
    ADC (IndY w) -> cpu & adcIndY w
    ADC _        -> undefined

    AND (Imm w)  -> cpu & andImm w
    AND (Zpg w)  -> cpu & andZpg w
    AND (ZpgX w) -> cpu & andZpgX w
    AND (Abs w)  -> cpu & andAbs w
    AND (AbsX w) -> cpu & andAbsX w
    AND (AbsY w) -> cpu & andAbsY w
    AND (IndX w) -> cpu & andIndX w
    AND (IndY w) -> cpu & andIndY w
    AND _        -> undefined

    ASL Acc      -> cpu & aslAcc
    ASL (Zpg w)  -> cpu & aslZpg w
    ASL (ZpgX w) -> cpu & aslZpgX w
    ASL (Abs w)  -> cpu & aslAbs w
    ASL (AbsX w) -> cpu & aslAbsX w
    ASL _        -> undefined

    BPL (Rel w)  -> cpu & bpl w
    BPL _        -> undefined
    BMI (Rel w)  -> cpu & bmi w
    BMI _        -> undefined
    BVC (Rel w)  -> cpu & bvc w
    BVC _        -> undefined
    BVS (Rel w)  -> cpu & bvs w
    BVS _        -> undefined
    BCC (Rel w)  -> cpu & bcc w
    BCC _        -> undefined
    BCS (Rel w)  -> cpu & bcs w
    BCS _        -> undefined
    BNE (Rel w)  -> cpu & bne w
    BNE _        -> undefined
    BEQ (Rel w)  -> cpu & beq w
    BEQ _        -> undefined

    CMP (Imm  w) -> cpu & cmpImm w
    CMP (Zpg  w) -> cpu & cmpZpg w
    CMP (ZpgX w) -> cpu & cmpZpgX w
    CMP (Abs  w) -> cpu & cmpAbs w
    CMP (AbsX w) -> cpu & cmpAbsX w
    CMP (AbsY w) -> cpu & cmpAbsY w
    CMP (IndX w) -> cpu & cmpIndX w
    CMP (IndY w) -> cpu & cmpIndY w
    CMP _        -> undefined

    CPX (Imm w)  -> cpu & cpxImm w
    CPX (Zpg w)  -> cpu & cpxZpg w
    CPX (Abs w)  -> cpu & cpxAbs w
    CPX _        -> undefined

    CPY (Imm w)  -> cpu & cpyImm w
    CPY (Zpg w)  -> cpu & cpyZpg w
    CPY (Abs w)  -> cpu & cpyAbs w
    CPY _        -> undefined

    DEC (Zpg w)  -> cpu & decZpg w
    DEC (ZpgX w) -> cpu & decZpgX w
    DEC (Abs w)  -> cpu & decAbs w
    DEC (AbsX w) -> cpu & decAbsX w
    DEC _        -> undefined

    DEX          -> cpu & dex
    DEY          -> cpu & dey

    EOR (Imm  w) -> cpu & eorImm w
    EOR (Zpg  w) -> cpu & eorZpg w
    EOR (ZpgX w) -> cpu & eorZpgX w
    EOR (Abs  w) -> cpu & eorAbs w
    EOR (AbsX w) -> cpu & eorAbsX w
    EOR (AbsY w) -> cpu & eorAbsY w
    EOR (IndX w) -> cpu & eorIndX w
    EOR (IndY w) -> cpu & eorIndY w
    EOR _        -> undefined

    LDA (Imm w)  -> cpu & ldaImm w
    LDA (Zpg w)  -> cpu & ldaZpg w
    LDA (ZpgX w) -> cpu & ldaZpgX w
    LDA (Abs w)  -> cpu & ldaAbs w
    LDA (AbsX w) -> cpu & ldaAbsX w
    LDA (AbsY w) -> cpu & ldaAbsY w
    LDA (IndX w) -> cpu & ldaIndX w
    LDA (IndY w) -> cpu & ldaIndY w
    LDA _        -> undefined

    LDX (Imm w)  -> cpu & ldxImm w
    LDX (Zpg w)  -> cpu & ldxZpg w
    LDX (ZpgY w) -> cpu & ldxZpgY w
    LDX (Abs w)  -> cpu & ldxAbs w
    LDX (AbsY w) -> cpu & ldxAbsY w
    LDX _        -> undefined

    LDY (Imm w)  -> cpu & ldyImm w
    LDY (Zpg w)  -> cpu & ldyZpg w
    LDY (ZpgX w) -> cpu & ldyZpgX w
    LDY (Abs w)  -> cpu & ldyAbs w
    LDY (AbsX w) -> cpu & ldyAbsX w
    LDY _        -> undefined

    BRK          -> cpu & brk
