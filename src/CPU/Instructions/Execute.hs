module CPU.Instructions.Execute
  ( execute
  )
  where

import CPU
import CPU.Instructions.Impl
import CPU.Instructions.Opcode
import CPU.Instructions.Operand

execute :: Opcode -> CPU -> CPU
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

      PHA          -> pha
      PHP          -> php
      PLA          -> pla
      PLP          -> plp

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

      SEC          -> sec
      SEI          -> sei
      SED          -> sed

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

      BRK          -> brk

      LabelDef _   -> undefined
      Code         -> undefined
      Data         -> undefined
      Bytes _      -> undefined
      Binary _     -> undefined
