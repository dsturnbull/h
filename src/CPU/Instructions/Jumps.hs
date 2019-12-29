module CPU.Instructions.Jumps
  ( Jumps(..)
  ) where

import CPU.Instructions.Opcode

class Jumps a where
  jumps :: a -> Bool

instance Jumps Opcode where
  jumps (ADC _)      = False
  jumps (AND _)      = False
  jumps (ASL _)      = False
  jumps (BCC _)      = True
  jumps (BCS _)      = True
  jumps (BEQ _)      = True
  jumps (BMI _)      = True
  jumps (BNE _)      = True
  jumps (BPL _)      = True
  jumps (BVC _)      = True
  jumps (BVS _)      = True
  jumps CLC          = False
  jumps CLI          = False
  jumps CLV          = False
  jumps CLD          = False
  jumps (CMP _)      = False
  jumps (CPX _)      = False
  jumps (CPY _)      = False
  jumps (DEC _)      = False
  jumps DEX          = False
  jumps DEY          = False
  jumps (EOR _)      = False
  jumps (INC _)      = False
  jumps INX          = False
  jumps INY          = False
  jumps (JMP _)      = True
  jumps (JSR _)      = True
  jumps (LDA _)      = False
  jumps (LDX _)      = False
  jumps (LDY _)      = False
  jumps (LSR _)      = False
  jumps NOP          = False
  jumps (ORA _)      = False
  jumps PHA          = False
  jumps PHP          = False
  jumps PLA          = False
  jumps PLP          = False
  jumps (ROL _)      = False
  jumps (ROR _)      = False
  jumps RTI          = True
  jumps RTS          = True
  jumps (SBC _)      = False
  jumps SEC          = False
  jumps SEI          = False
  jumps SED          = False
  jumps (STA _)      = False
  jumps (STX _)      = False
  jumps (STY _)      = False
  jumps TAX          = False
  jumps TXA          = False
  jumps TAY          = False
  jumps TYA          = False
  jumps TSX          = False
  jumps TXS          = False
  jumps BRK          = False
  jumps (LabelDef _) = undefined
  jumps Code         = undefined
  jumps Data         = undefined
  jumps (Bytes _)    = undefined
  jumps (Binary _)   = undefined
