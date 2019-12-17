module CPU.Instructions.Timing
  ( Timed(..)
  )
  where

import CPU.Instructions.Opcode
import CPU.Operand

class Timed a where
  cycles :: a -> Int

instance Timed Opcode where
  cycles (ADC (Imm _))  = 2
  cycles (ADC (Zpg _))  = 3
  cycles (ADC (ZpgX _)) = 4
  cycles (ADC (Abs _))  = 4
  cycles (ADC (AbsX _)) = 4
  cycles (ADC (AbsY _)) = 4
  cycles (ADC (IndX _)) = 6
  cycles (ADC (IndY _)) = 5
  cycles (ADC _)        = undefined

  cycles (AND (Imm _))  = 2
  cycles (AND (Zpg _))  = 3
  cycles (AND (ZpgX _)) = 4
  cycles (AND (Abs _))  = 4
  cycles (AND (AbsX _)) = 4
  cycles (AND (AbsY _)) = 4
  cycles (AND (IndX _)) = 6
  cycles (AND (IndY _)) = 5
  cycles (AND _)        = undefined

  cycles (ASL Acc)      = 2
  cycles (ASL (Zpg _))  = 5
  cycles (ASL (ZpgX _)) = 6
  cycles (ASL (Abs _))  = 6
  cycles (ASL (AbsX _)) = 7
  cycles (ASL _)        = undefined

  cycles (BPL _)        = 2
  cycles (BMI _)        = 2
  cycles (BVC _)        = 2
  cycles (BVS _)        = 2
  cycles (BCC _)        = 2
  cycles (BCS _)        = 2
  cycles (BNE _)        = 2
  cycles (BEQ _)        = 2

  cycles CLC            = 1
  cycles CLI            = 1
  cycles CLV            = 1
  cycles CLD            = 1

  cycles (CMP (Imm  _)) = 2
  cycles (CMP (Zpg  _)) = 3
  cycles (CMP (ZpgX _)) = 4
  cycles (CMP (Abs  _)) = 4
  cycles (CMP (AbsX _)) = 4
  cycles (CMP (AbsY _)) = 4
  cycles (CMP (IndX _)) = 6
  cycles (CMP (IndY _)) = 5
  cycles (CMP _)        = undefined

  cycles (CPX (Imm _))  = 2
  cycles (CPX (Zpg _))  = 3
  cycles (CPX (Abs _))  = 4
  cycles (CPX _)        = undefined

  cycles (CPY (Imm _))  = 2
  cycles (CPY (Zpg _))  = 3
  cycles (CPY (Abs _))  = 4
  cycles (CPY _)        = undefined

  cycles (DEC (Zpg _))  = 5
  cycles (DEC (ZpgX _)) = 6
  cycles (DEC (Abs _))  = 6
  cycles (DEC (AbsX _)) = 7
  cycles (DEC _)        = undefined

  cycles DEX            = 2
  cycles DEY            = 2

  cycles (EOR (Imm  _)) = 2
  cycles (EOR (Zpg  _)) = 3
  cycles (EOR (ZpgX _)) = 4
  cycles (EOR (Abs  _)) = 4
  cycles (EOR (AbsX _)) = 4
  cycles (EOR (AbsY _)) = 4
  cycles (EOR (IndX _)) = 6
  cycles (EOR (IndY _)) = 5
  cycles (EOR _)        = undefined

  cycles (INC (Zpg _))  = 5
  cycles (INC (ZpgX _)) = 6
  cycles (INC (Abs _))  = 6
  cycles (INC (AbsX _)) = 7
  cycles (INC _)        = undefined

  cycles INX            = 2
  cycles INY            = 2

  cycles (JMP (Abs _))  = 3
  cycles (JMP (Ind _))  = 5
  cycles (JMP _)        = undefined

  cycles (JSR _)        = 6

  cycles (LDA (Imm _))  = 2
  cycles (LDA (Zpg _))  = 3
  cycles (LDA (ZpgX _)) = 4
  cycles (LDA (Abs _))  = 4
  cycles (LDA (AbsX _)) = 4
  cycles (LDA (AbsY _)) = 4
  cycles (LDA (IndX _)) = 6
  cycles (LDA (IndY _)) = 5
  cycles (LDA _)        = undefined

  cycles (LDX (Imm _))  = 2
  cycles (LDX (Zpg _))  = 3
  cycles (LDX (ZpgY _)) = 4
  cycles (LDX (Abs _))  = 4
  cycles (LDX (AbsY _)) = 4
  cycles (LDX _)        = undefined

  cycles (LDY (Imm _))  = 2
  cycles (LDY (Zpg _))  = 3
  cycles (LDY (ZpgX _)) = 4
  cycles (LDY (Abs _))  = 4
  cycles (LDY (AbsX _)) = 4
  cycles (LDY _)        = undefined

  cycles (LSR Acc)      = 2
  cycles (LSR (Zpg _))  = 5
  cycles (LSR (ZpgX _)) = 6
  cycles (LSR (Abs _))  = 6
  cycles (LSR (AbsX _)) = 7
  cycles (LSR _)        = undefined

  cycles NOP            = 2

  cycles (ORA (Imm  _)) = 2
  cycles (ORA (Zpg  _)) = 3
  cycles (ORA (ZpgX _)) = 4
  cycles (ORA (Abs  _)) = 4
  cycles (ORA (AbsX _)) = 4
  cycles (ORA (AbsY _)) = 4
  cycles (ORA (IndX _)) = 6
  cycles (ORA (IndY _)) = 5
  cycles (ORA _)        = undefined

  cycles PHA            = 3
  cycles PHP            = 3
  cycles PLA            = 4
  cycles PLP            = 4

  cycles (ROL Acc)      = 2
  cycles (ROL (Zpg _))  = 5
  cycles (ROL (ZpgX _)) = 6
  cycles (ROL (Abs _))  = 6
  cycles (ROL (AbsX _)) = 7
  cycles (ROL _)        = undefined

  cycles (ROR Acc)      = 2
  cycles (ROR (Zpg _))  = 5
  cycles (ROR (ZpgX _)) = 6
  cycles (ROR (Abs _))  = 6
  cycles (ROR (AbsX _)) = 7
  cycles (ROR _)        = undefined

  cycles RTI            = 6
  cycles RTS            = 6

  cycles (SBC (Imm _))  = 2
  cycles (SBC (Zpg _))  = 3
  cycles (SBC (ZpgX _)) = 4
  cycles (SBC (Abs _))  = 4
  cycles (SBC (AbsX _)) = 4
  cycles (SBC (AbsY _)) = 4
  cycles (SBC (IndX _)) = 6
  cycles (SBC (IndY _)) = 5
  cycles (SBC _)        = undefined

  cycles SEC            = 2
  cycles SEI            = 2
  cycles SED            = 2

  cycles (STA (Zpg  _)) = 3
  cycles (STA (ZpgX _)) = 4
  cycles (STA (Abs  _)) = 4
  cycles (STA (AbsX _)) = 5
  cycles (STA (AbsY _)) = 5
  cycles (STA (IndX _)) = 6
  cycles (STA (IndY _)) = 6
  cycles (STA _)        = undefined

  cycles (STX (Zpg  _)) = 3
  cycles (STX (ZpgY _)) = 4
  cycles (STX (Abs  _)) = 4
  cycles (STX _)        = undefined

  cycles (STY (Zpg  _)) = 3
  cycles (STY (ZpgX _)) = 4
  cycles (STY (Abs  _)) = 4
  cycles (STY _)        = undefined

  cycles TAX            = 2
  cycles TXA            = 2
  cycles TAY            = 2
  cycles TYA            = 2
  cycles TSX            = 2
  cycles TXS            = 2

  cycles BRK            = 7

  cycles (LabelDef _)   = 0
  cycles Code           = 0
  cycles Data           = 0
  cycles (Bytes _)      = 0
