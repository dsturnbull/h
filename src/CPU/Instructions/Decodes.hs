{-# LANGUAGE AllowAmbiguousTypes #-}

module CPU.Instructions.Decodes
  ( Decodes(..)
  , w16
  ) where

import CPU.Instructions.Opcode
import CPU.Instructions.Operand

import Data.Bits
import Data.Vector.Storable
import Data.Word

import Prelude hiding (drop, take)

class Decodes a where
  decode :: Vector Word8 -> Opcode

instance Decodes Opcode where
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
      0xD6 -> DEC (ZpgX imm)
      0xCE -> DEC (Abs addr)
      0xDE -> DEC (AbsX addr)

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
      0xF6 -> INC (ZpgX imm)
      0xEE -> INC (Abs addr)
      0xFE -> INC (AbsX addr)

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

      0x48 -> PHA
      0x08 -> PHP
      0x68 -> PLA
      0x28 -> PLP

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

      0x38 -> SEC
      0x78 -> SEI
      0xF8 -> SED

      0x85 -> STA (Zpg imm)
      0x95 -> STA (ZpgX imm)
      0x8D -> STA (Abs addr)
      0x9D -> STA (AbsX addr)
      0x99 -> STA (AbsY addr)
      0x81 -> STA (IndX imm)
      0x91 -> STA (IndY imm)

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
      a    -> Bytes [a]

    where instruction = m ! 0
          imm         = m ! 1
          addr        = w16 $ take 2 (drop 1 m)

w16 :: Vector Word8 -> Word16
w16 v = (addrH `shiftL` 8) .|. addrL
  where addrL = fromIntegral (v ! 0)
        addrH = fromIntegral (v ! 1)
