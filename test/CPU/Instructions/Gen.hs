module CPU.Instructions.Gen where

import ASM.Assembler
import ASM.Operand
import ASM.Program

import Data.Int
import Data.Word
import Hedgehog
import Hedgehog.Gen   as G
import Hedgehog.Range as R

genInstruction :: MonadGen m => m Opcode
genInstruction =
  G.choice [ genADC
           , genAND
           , genASL
           -- , genB
           , genCMP
           , genCPX
           , genCPY
           , genDE
           , genEOR
           , genIN
           -- , genJMP
           -- , genJSR
           , genLDA
           , genLDX
           , genLDY
           , genLSR
           , genNOP
           , genORA
           , genCL
           , genSE
           , genROL
           , genROR
           -- , genRTI
           , genSBC
           , genSTA
           , genSTX
           , genSTY
           , genT
           ]

genCodeBreaking :: MonadGen m => Range Int -> m Opcode -> m [Opcode]
genCodeBreaking r is = do
    code <- G.list r is
    return $ code ++ [BRK]

genProg :: MonadGen m => [Opcode] -> m Program
genProg code = return $ assembleOpcodes code 0x0000 0x0000

genADC :: MonadGen m => m Opcode
genADC = ADC <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY, genIndX, genIndY]

genAND :: MonadGen m => m Opcode
genAND = AND <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY, genIndX, genIndY]

genASL :: MonadGen m => m Opcode
genASL = ASL <$> G.choice [genAcc, genZpg, genZpgX, genAbs, genAbsX]

genB :: MonadGen m => m Opcode
genB =
  G.choice [ BCC <$> genRel
           , BCS <$> genRel
           , BEQ <$> genRel
           , BMI <$> genRel
           , BNE <$> genRel
           , BPL <$> genRel
           , BVC <$> genRel
           , BVS <$> genRel ]

genCMP :: MonadGen m => m Opcode
genCMP = CMP <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY, genIndX, genIndY]

genCPX :: MonadGen m => m Opcode
genCPX = CPX <$> G.choice [genImm, genZpg, genAbs]

genCPY :: MonadGen m => m Opcode
genCPY = CPY <$> G.choice [genImm, genZpg, genAbs]

genDE :: MonadGen m => m Opcode
genDE = G.choice [ DEC <$> G.choice [genZpg, genZpgX, genAbs, genAbsX]
                 , pure DEX
                 , pure DEY
                 ]

genEOR :: MonadGen m => m Opcode
genEOR = EOR <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY, genIndX, genIndY]

genIN :: MonadGen m => m Opcode
genIN = G.choice [ INC <$> G.choice [genZpg, genZpgX, genAbs, genAbsX]
                 , pure INX
                 , pure INY
                 ]

genJMP :: MonadGen m => m Opcode
genJMP = JMP <$> G.choice [genAbs, genInd]

genJSR :: MonadGen m => m Opcode
genJSR = JSR <$> G.choice [genAbs]

genLDA :: MonadGen m => m Opcode
genLDA = LDA <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY]

genLDX :: MonadGen m => m Opcode
genLDX = LDX <$> G.choice [genImm, genZpg, genZpgY, genAbs, genAbsY]

genLDY :: MonadGen m => m Opcode
genLDY = LDY <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX]

genLSR :: MonadGen m => m Opcode
genLSR = LSR <$> G.choice [genAcc, genZpg, genZpgX, genAbs, genAbsX]

genNOP :: MonadGen m => m Opcode
genNOP = pure NOP

genORA :: MonadGen m => m Opcode
genORA = ORA <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY, genIndX, genIndY]

genSE :: MonadGen m => m Opcode
genSE = G.choice [ pure SEC
                 , pure SEI
                 , pure SED
                 ]

genCL :: MonadGen m => m Opcode
genCL = G.choice [ pure CLC
                 , pure CLI
                 , pure CLV
                 , pure CLD
                 ]

genROL :: MonadGen m => m Opcode
genROL = ROL <$> G.choice [genAcc, genZpg, genZpgX, genAbs, genAbsX]

genROR :: MonadGen m => m Opcode
genROR = ROR <$> G.choice [genAcc, genZpg, genZpgX, genAbs, genAbsX]

genRTI :: MonadGen m => m Opcode
genRTI = pure RTI

genT :: MonadGen m => m Opcode
genT = G.choice [ pure TAX
                , pure TXA
                , pure TAY
                , pure TYA
                , pure TSX
                , pure TXS
                ]

genSBC :: MonadGen m => m Opcode
genSBC = SBC <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY, genIndX, genIndY]

genSTA :: MonadGen m => m Opcode
genSTA = STA <$> G.choice [genZpg, genZpgX, genAbs, genAbsX, genAbsY, genIndX, genIndY]

genSTX :: MonadGen m => m Opcode
genSTX = STX <$> G.choice [genZpg, genZpgY, genAbs]

genSTY :: MonadGen m => m Opcode
genSTY = STY <$> G.choice [genZpg, genZpgX, genAbs]

genAcc :: MonadGen m => m Operand
genAcc = return Acc

genImm :: MonadGen m => m Operand
genImm = Imm <$> w8

genZpg :: MonadGen m => m Operand
genZpg = Zpg <$> w8

genZpgX :: MonadGen m => m Operand
genZpgX = ZpgX <$> w8

genZpgY :: MonadGen m => m Operand
genZpgY = ZpgY <$> w8

genAbs :: MonadGen m => m Operand
genAbs = Abs <$> w16

genAbsX :: MonadGen m => m Operand
genAbsX = AbsX <$> w16

genAbsY :: MonadGen m => m Operand
genAbsY = AbsY <$> w16

genIndX :: MonadGen m => m Operand
genIndX = IndX <$> w8

genInd :: MonadGen m => m Operand
genInd = Ind <$> w16

genIndY :: MonadGen m => m Operand
genIndY = IndY <$> w8

genRel :: MonadGen m => m Operand
genRel = Rel <$> i8

genBRK :: MonadGen m => m Opcode
genBRK = return BRK

w8 :: MonadGen m => m Word8
w8 = word8 (R.linear 20 40)

w16 :: MonadGen m => m Word16
w16 = fromIntegral <$> w8

i8 :: MonadGen m => m Int8
i8 = fromIntegral <$> int8 R.linearBounded
