module ASM.Length
  ( Length(..)
  ) where

import ASM.Operand

class Length a where
  insLength :: a -> Int

instance Length Opcode where
  insLength (ADC (Imm _))       = 2
  insLength (ADC (Zpg _))       = 2
  insLength (ADC (ZpgX _))      = 2
  insLength (ADC (Abs _))       = 3
  insLength (ADC (AbsX _))      = 3
  insLength (ADC (AbsY _))      = 3
  insLength (ADC (IndX _))      = 2
  insLength (ADC (IndY _))      = 2
  insLength (ADC l@(Label _ _)) = insLength (unLabel ADC l)
  insLength (ADC _)             = undefined

  insLength (AND (Imm _))       = 2
  insLength (AND (Zpg _))       = 2
  insLength (AND (ZpgX _))      = 2
  insLength (AND (Abs _))       = 3
  insLength (AND (AbsX _))      = 3
  insLength (AND (AbsY _))      = 3
  insLength (AND (IndX _))      = 2
  insLength (AND (IndY _))      = 2
  insLength (AND l@(Label _ _)) = insLength (unLabel AND l)
  insLength (AND _)             = undefined

  insLength (ASL Acc)           = 1
  insLength (ASL (Zpg _))       = 2
  insLength (ASL (ZpgX _))      = 2
  insLength (ASL (Abs _))       = 3
  insLength (ASL (AbsX _))      = 3
  insLength (ASL l@(Label _ _)) = insLength (unLabel ASL l)
  insLength (ASL _)             = undefined

  insLength (BPL _)             = 2
  insLength (BMI _)             = 2
  insLength (BVC _)             = 2
  insLength (BVS _)             = 2
  insLength (BCC _)             = 2
  insLength (BCS _)             = 2
  insLength (BNE _)             = 2
  insLength (BEQ _)             = 2

  insLength CLC                 = 1
  insLength CLI                 = 1
  insLength CLV                 = 1
  insLength CLD                 = 1

  insLength (CMP (Imm  _))      = 2
  insLength (CMP (Zpg  _))      = 2
  insLength (CMP (ZpgX _))      = 2
  insLength (CMP (Abs  _))      = 3
  insLength (CMP (AbsX _))      = 3
  insLength (CMP (AbsY _))      = 3
  insLength (CMP (IndX _))      = 2
  insLength (CMP (IndY _))      = 2
  insLength (CMP l@(Label _ _)) = insLength (unLabel CMP l)
  insLength (CMP _)             = undefined

  insLength (CPX (Imm _))       = 2
  insLength (CPX (Zpg _))       = 2
  insLength (CPX (Abs _))       = 3
  insLength (CPX l@(Label _ _)) = insLength (unLabel CPX l)
  insLength (CPX _)             = undefined

  insLength (CPY (Imm _))       = 2
  insLength (CPY (Zpg _))       = 2
  insLength (CPY (Abs _))       = 3
  insLength (CPY l@(Label _ _)) = insLength (unLabel CPY l)
  insLength (CPY _)             = undefined

  insLength (DEC (Zpg _))       = 2
  insLength (DEC (ZpgX _))      = 2
  insLength (DEC (Abs _))       = 3
  insLength (DEC (AbsX _))      = 3
  insLength (DEC l@(Label _ _)) = insLength (unLabel DEC l)
  insLength (DEC _)             = undefined

  insLength DEX                 = 1
  insLength DEY                 = 1

  insLength (EOR (Imm  _))      = 2
  insLength (EOR (Zpg  _))      = 2
  insLength (EOR (ZpgX _))      = 2
  insLength (EOR (Abs  _))      = 3
  insLength (EOR (AbsX _))      = 3
  insLength (EOR (AbsY _))      = 3
  insLength (EOR (IndX _))      = 2
  insLength (EOR (IndY _))      = 2
  insLength (EOR l@(Label _ _)) = insLength (unLabel EOR l)
  insLength (EOR _)             = undefined

  insLength (INC (Zpg _))       = 2
  insLength (INC (ZpgX _))      = 2
  insLength (INC (Abs _))       = 3
  insLength (INC (AbsX _))      = 3
  insLength (INC l@(Label _ _)) = insLength (unLabel INC l)
  insLength (INC _)             = undefined

  insLength INX                 = 1
  insLength INY                 = 1

  insLength (JMP _)             = 3

  insLength (JSR _)             = 3

  insLength (LDA (Imm _))       = 2
  insLength (LDA (Zpg _))       = 2
  insLength (LDA (ZpgX _))      = 2
  insLength (LDA (Abs _))       = 3
  insLength (LDA (AbsX _))      = 3
  insLength (LDA (AbsY _))      = 3
  insLength (LDA (IndX _))      = 2
  insLength (LDA (IndY _))      = 2
  insLength (LDA l@(Label _ _)) = insLength (unLabel LDA l)
  insLength (LDA _)             = undefined

  insLength (LDX (Imm _))       = 2
  insLength (LDX (Zpg _))       = 2
  insLength (LDX (ZpgY _))      = 2
  insLength (LDX (Abs _))       = 3
  insLength (LDX (AbsY _))      = 3
  insLength (LDX (IndX _))      = 2
  insLength (LDX (IndY _))      = 2
  insLength (LDX l@(Label _ _)) = insLength (unLabel LDX l)
  insLength (LDX _)             = undefined

  insLength (LDY (Imm _))       = 2
  insLength (LDY (Zpg _))       = 2
  insLength (LDY (ZpgX _))      = 2
  insLength (LDY (Abs _))       = 3
  insLength (LDY (AbsX _))      = 3
  insLength (LDY (IndX _))      = 2
  insLength (LDY (IndY _))      = 2
  insLength (LDY l@(Label _ _)) = insLength (unLabel LDY l)
  insLength (LDY _)             = undefined

  insLength (LSR Acc)           = 1
  insLength (LSR (Zpg _))       = 2
  insLength (LSR (ZpgX _))      = 2
  insLength (LSR (Abs _))       = 3
  insLength (LSR (AbsX _))      = 3
  insLength (LSR l@(Label _ _)) = insLength (unLabel LSR l)
  insLength (LSR _)             = undefined

  insLength NOP                 = 1

  insLength (ORA (Imm  _))      = 2
  insLength (ORA (Zpg  _))      = 2
  insLength (ORA (ZpgX _))      = 2
  insLength (ORA (Abs  _))      = 3
  insLength (ORA (AbsX _))      = 3
  insLength (ORA (AbsY _))      = 3
  insLength (ORA (IndX _))      = 2
  insLength (ORA (IndY _))      = 2
  insLength (ORA l@(Label _ _)) = insLength (unLabel ORA l)
  insLength (ORA _)             = undefined

  insLength PHA                 = 1
  insLength PHP                 = 1
  insLength PLA                 = 1
  insLength PLP                 = 1

  insLength (ROL Acc)           = 1
  insLength (ROL (Zpg _))       = 2
  insLength (ROL (ZpgX _))      = 2
  insLength (ROL (Abs _))       = 3
  insLength (ROL (AbsX _))      = 3
  insLength (ROL l@(Label _ _)) = insLength (unLabel ROL l)
  insLength (ROL _)             = undefined

  insLength (ROR Acc)           = 1
  insLength (ROR (Zpg _))       = 2
  insLength (ROR (ZpgX _))      = 2
  insLength (ROR (Abs _))       = 3
  insLength (ROR (AbsX _))      = 3
  insLength (ROR l@(Label _ _)) = insLength (unLabel ROR l)
  insLength (ROR _)             = undefined

  insLength RTI                 = 1
  insLength RTS                 = 1

  insLength (SBC (Imm _))       = 2
  insLength (SBC (Zpg _))       = 2
  insLength (SBC (ZpgX _))      = 2
  insLength (SBC (Abs _))       = 3
  insLength (SBC (AbsX _))      = 3
  insLength (SBC (AbsY _))      = 3
  insLength (SBC (IndX _))      = 2
  insLength (SBC (IndY _))      = 2
  insLength (SBC l@(Label _ _)) = insLength (unLabel SBC l)
  insLength (SBC _)             = undefined

  insLength SEC                 = 1
  insLength SEI                 = 1
  insLength SED                 = 1

  insLength (STA (Zpg  _))      = 2
  insLength (STA (ZpgX _))      = 2
  insLength (STA (Abs  _))      = 3
  insLength (STA (AbsX _))      = 3
  insLength (STA (AbsY _))      = 3
  insLength (STA (IndX _))      = 2
  insLength (STA (IndY _))      = 2
  insLength (STA l@(Label _ _)) = insLength (unLabel STA l)
  insLength (STA _)             = undefined

  insLength (STX (Zpg  _))      = 2
  insLength (STX (ZpgY _))      = 2
  insLength (STX (Abs  _))      = 3
  insLength (STX l@(Label _ _)) = insLength (unLabel STX l)
  insLength (STX _)             = undefined

  insLength (STY (Zpg  _))      = 2
  insLength (STY (ZpgX _))      = 2
  insLength (STY (Abs  _))      = 3
  insLength (STY l@(Label _ _)) = insLength (unLabel STY l)
  insLength (STY _)             = undefined

  insLength TAX                 = 1
  insLength TXA                 = 1
  insLength TAY                 = 1
  insLength TYA                 = 1
  insLength TSX                 = 1
  insLength TXS                 = 1

  insLength BRK                 = 1

  insLength (LabelDef _)        = 0
  insLength (Variable8 _ _)     = 0
  insLength (Variable16 _ _)    = 0
  insLength Code                = 0
  insLength Data                = 0
  insLength (Bytes ws)          = fromIntegral $ length ws
  insLength (Origin _)          = 0
