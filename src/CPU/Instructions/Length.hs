module CPU.Instructions.Length
  ( Length(..)
  ) where

import CPU.Instructions.Opcode
import CPU.Instructions.Operand

class Length a where
  insLength :: a -> Int

instance Length Opcode where
  insLength (ADC (Imm _))             = 2
  insLength (ADC (Zpg _))             = 2
  insLength (ADC (ZpgX _))            = 2
  insLength (ADC (Abs _))             = 3
  insLength (ADC (AbsX _))            = 3
  insLength (ADC (AbsY _))            = 3
  insLength (ADC (IndX _))            = 2
  insLength (ADC (IndY _))            = 2
  insLength (ADC (Label _))           = 3
  insLength (ADC _)                   = undefined

  insLength (AND (Imm _))             = 2
  insLength (AND (Zpg _))             = 2
  insLength (AND (ZpgX _))            = 2
  insLength (AND (Abs _))             = 3
  insLength (AND (AbsX _))            = 3
  insLength (AND (AbsY _))            = 3
  insLength (AND (IndX _))            = 2
  insLength (AND (IndY _))            = 2
  insLength (AND _)                   = undefined

  insLength (ASL Acc)                 = 1
  insLength (ASL (Zpg _))             = 2
  insLength (ASL (ZpgX _))            = 2
  insLength (ASL (Abs _))             = 3
  insLength (ASL (AbsX _))            = 3
  insLength (ASL _)                   = undefined

  insLength (BPL _)                   = 2
  insLength (BMI _)                   = 2
  insLength (BVC _)                   = 2
  insLength (BVS _)                   = 2
  insLength (BCC _)                   = 2
  insLength (BCS _)                   = 2
  insLength (BNE _)                   = 2
  insLength (BEQ _)                   = 2

  insLength CLC                       = 1
  insLength CLI                       = 1
  insLength CLV                       = 1
  insLength CLD                       = 1

  insLength (CMP (Imm  _))            = 2
  insLength (CMP (Zpg  _))            = 2
  insLength (CMP (ZpgX _))            = 2
  insLength (CMP (Abs  _))            = 3
  insLength (CMP (AbsX _))            = 3
  insLength (CMP (AbsY _))            = 3
  insLength (CMP (IndX _))            = 2
  insLength (CMP (IndY _))            = 2
  insLength (CMP _)                   = undefined

  insLength (CPX (Imm _))             = 2
  insLength (CPX (Zpg _))             = 2
  insLength (CPX (Abs _))             = 3
  insLength (CPX _)                   = undefined

  insLength (CPY (Imm _))             = 2
  insLength (CPY (Zpg _))             = 2
  insLength (CPY (Abs _))             = 3
  insLength (CPY _)                   = undefined

  insLength (DEC (Zpg _))             = 2
  insLength (DEC (ZpgX _))            = 2
  insLength (DEC (Abs _))             = 3
  insLength (DEC (AbsX _))            = 3
  insLength (DEC _)                   = undefined

  insLength DEX                       = 1
  insLength DEY                       = 1

  insLength (EOR (Imm  _))            = 2
  insLength (EOR (Zpg  _))            = 2
  insLength (EOR (ZpgX _))            = 2
  insLength (EOR (Abs  _))            = 3
  insLength (EOR (AbsX _))            = 3
  insLength (EOR (AbsY _))            = 3
  insLength (EOR (IndX _))            = 2
  insLength (EOR (IndY _))            = 2
  insLength (EOR _)                   = undefined

  insLength (INC (Zpg _))             = 2
  insLength (INC (ZpgX _))            = 2
  insLength (INC (Abs _))             = 3
  insLength (INC (AbsX _))            = 3
  insLength (INC (Label _))           = 3
  insLength (INC _)                   = undefined

  insLength INX                       = 1
  insLength INY                       = 1

  insLength (JMP _)                   = 3

  insLength (JSR _)                   = 3

  insLength (LDA (Imm _))             = 2
  insLength (LDA (Zpg _))             = 2
  insLength (LDA (ZpgX _))            = 2
  insLength (LDA (Abs _))             = 3
  insLength (LDA (AbsX _))            = 3
  insLength (LDA (AbsY _))            = 3
  insLength (LDA (IndX _))            = 2
  insLength (LDA (IndY _))            = 2
  insLength (LDA (Label _))           = 3
  insLength (LDA (LabelLowByte _))
                                      = 2
  insLength (LDA (LabelHighByte _))
                                      = 2
  insLength (LDA _)                   = undefined

  insLength (LDX (Imm _))             = 2
  insLength (LDX (Zpg _))             = 2
  insLength (LDX (ZpgY _))            = 2
  insLength (LDX (Abs _))             = 3
  insLength (LDX (Label _))           = 3
  insLength (LDX (AbsY _))            = 3
  insLength (LDX (IndX _))            = 2
  insLength (LDX (IndY _))            = 2
  insLength (LDX _)                   = undefined

  insLength (LDY (Imm _))             = 2
  insLength (LDY (Zpg _))             = 2
  insLength (LDY (ZpgX _))            = 2
  insLength (LDY (Abs _))             = 3
  insLength (LDY (AbsX _))            = 3
  insLength (LDY (IndX _))            = 2
  insLength (LDY (IndY _))            = 2
  insLength (LDY _)                   = undefined

  insLength (LSR Acc)                 = 1
  insLength (LSR (Zpg _))             = 2
  insLength (LSR (ZpgX _))            = 2
  insLength (LSR (Abs _))             = 3
  insLength (LSR (AbsX _))            = 3
  insLength (LSR _)                   = undefined

  insLength NOP                       = 1

  insLength (ORA (Imm  _))            = 2
  insLength (ORA (Zpg  _))            = 2
  insLength (ORA (ZpgX _))            = 2
  insLength (ORA (Abs  _))            = 3
  insLength (ORA (AbsX _))            = 3
  insLength (ORA (AbsY _))            = 3
  insLength (ORA (IndX _))            = 2
  insLength (ORA (IndY _))            = 2
  insLength (ORA _)                   = undefined

  insLength PHA                       = 1
  insLength PHP                       = 1
  insLength PLA                       = 1
  insLength PLP                       = 1

  insLength (ROL Acc)                 = 1
  insLength (ROL (Zpg _))             = 2
  insLength (ROL (ZpgX _))            = 2
  insLength (ROL (Abs _))             = 3
  insLength (ROL (AbsX _))            = 3
  insLength (ROL (Label _))           = 3
  insLength (ROL _)                   = undefined

  insLength (ROR Acc)                 = 1
  insLength (ROR (Zpg _))             = 2
  insLength (ROR (ZpgX _))            = 2
  insLength (ROR (Abs _))             = 3
  insLength (ROR (AbsX _))            = 3
  insLength (ROR (Label _))           = 3
  insLength (ROR _)                   = undefined

  insLength RTI                       = 1
  insLength RTS                       = 1

  insLength (SBC (Imm _))             = 2
  insLength (SBC (Zpg _))             = 2
  insLength (SBC (ZpgX _))            = 2
  insLength (SBC (Abs _))             = 3
  insLength (SBC (AbsX _))            = 3
  insLength (SBC (AbsY _))            = 3
  insLength (SBC (IndX _))            = 2
  insLength (SBC (IndY _))            = 2
  insLength (SBC (Label _))           = 3
  insLength (SBC _)                   = undefined

  insLength SEC                       = 1
  insLength SEI                       = 1
  insLength SED                       = 1

  insLength (STA (Zpg  _))            = 2
  insLength (STA (ZpgX _))            = 2
  insLength (STA (Abs  _))            = 3
  insLength (STA (AbsX _))            = 3
  insLength (STA (AbsY _))            = 3
  insLength (STA (IndX _))            = 2
  insLength (STA (IndY _))            = 2
  insLength (STA (Label _))           = 3
  insLength (STA _)                   = undefined

  insLength (STX (Zpg  _))            = 2
  insLength (STX (ZpgY _))            = 2
  insLength (STX (Abs  _))            = 3
  insLength (STX _)                   = undefined

  insLength (STY (Zpg  _))            = 2
  insLength (STY (ZpgX _))            = 2
  insLength (STY (Abs  _))            = 3
  insLength (STY _)                   = undefined

  insLength TAX                       = 1
  insLength TXA                       = 1
  insLength TAY                       = 1
  insLength TYA                       = 1
  insLength TSX                       = 1
  insLength TXS                       = 1

  insLength BRK                       = 1

  insLength (LabelDef _)              = 0
  insLength Code                      = 0
  insLength Data                      = 0
  insLength (Bytes ws)                = fromIntegral $ length ws
  insLength (Binary _)                = 0
