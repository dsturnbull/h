module CPU.Instructions.Gen where

import CPU.Program

import Control.Monad
import Data.Int
import Data.Word
import Hedgehog
import Hedgehog.Gen   as G
import Hedgehog.Range as R

import qualified Data.Vector.Storable as DVS

genInstruction :: MonadGen m => m Instruction
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

genCodeBreaking :: MonadGen m => Range Int -> m Instruction -> m [Instruction]
genCodeBreaking r is = do
    code <- G.list r is
    return $ code ++ [BRK]

genProg :: MonadGen m => [Instruction] -> m Program
genProg code = return $ Program (DVS.fromList (asm =<< code))

genADC :: MonadGen m => m Instruction
genADC = ADC <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY, genIndX, genIndY]

genAND :: MonadGen m => m Instruction
genAND = AND <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY, genIndX, genIndY]

genASL :: MonadGen m => m Instruction
genASL = ASL <$> G.choice [genAcc, genZpg, genZpgX, genAbs, genAbsX]

genB :: MonadGen m => m Instruction
genB =
  G.choice [ BCC <$> genAddr
           , BCS <$> genAddr
           , BEQ <$> genAddr
           , BMI <$> genAddr
           , BNE <$> genAddr
           , BPL <$> genAddr
           , BVC <$> genAddr
           , BVS <$> genAddr ]

genCMP :: MonadGen m => m Instruction
genCMP = CMP <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY, genIndX, genIndY]

genCPX :: MonadGen m => m Instruction
genCPX = CPX <$> G.choice [genImm, genZpg, genAbs]

genCPY :: MonadGen m => m Instruction
genCPY = CPY <$> G.choice [genImm, genZpg, genAbs]

genDE :: MonadGen m => m Instruction
genDE = G.choice [ DEC <$> G.choice [genZpg, genZpgX, genAbs, genAbsX]
                 , pure DEX
                 , pure DEY
                 ]

genEOR :: MonadGen m => m Instruction
genEOR = EOR <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY, genIndX, genIndY]

genIN :: MonadGen m => m Instruction
genIN = G.choice [ INC <$> G.choice [genZpg, genZpgX, genAbs, genAbsX]
                 , pure INX
                 , pure INY
                 ]

genJMP :: MonadGen m => m Instruction
genJMP = JMP <$> G.choice [genAbs, genInd]

genJSR :: MonadGen m => m Instruction
genJSR = JSR <$> G.choice [genAbs]

genLDA :: MonadGen m => m Instruction
genLDA = LDA <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY]

genLDX :: MonadGen m => m Instruction
genLDX = LDX <$> G.choice [genImm, genZpg, genZpgY, genAbs, genAbsY]

genLDY :: MonadGen m => m Instruction
genLDY = LDY <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX]

genLSR :: MonadGen m => m Instruction
genLSR = LSR <$> G.choice [genAcc, genZpg, genZpgX, genAbs, genAbsX]

genNOP :: MonadGen m => m Instruction
genNOP = pure NOP

genORA :: MonadGen m => m Instruction
genORA = ORA <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY, genIndX, genIndY]

genSE :: MonadGen m => m Instruction
genSE = G.choice [ pure SEC
                 , pure SEI
                 , pure SED
                 ]

genCL :: MonadGen m => m Instruction
genCL = G.choice [ pure CLC
                 , pure CLI
                 , pure CLV
                 , pure CLD
                 ]

genROL :: MonadGen m => m Instruction
genROL = ROL <$> G.choice [genAcc, genZpg, genZpgX, genAbs, genAbsX]

genROR :: MonadGen m => m Instruction
genROR = ROR <$> G.choice [genAcc, genZpg, genZpgX, genAbs, genAbsX]

genRTI :: MonadGen m => m Instruction
genRTI = pure RTI

genT :: MonadGen m => m Instruction
genT = G.choice [ pure TAX
                , pure TXA
                , pure TAY
                , pure TYA
                , pure TSX
                , pure TXS
                ]

genSBC :: MonadGen m => m Instruction
genSBC = SBC <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY, genIndX, genIndY]

genSTA :: MonadGen m => m Instruction
genSTA = STA <$> G.choice [genZpg, genZpgX, genAbs, genAbsX, genAbsY, genIndX, genIndY]

genSTX :: MonadGen m => m Instruction
genSTX = STX <$> G.choice [genZpg, genZpgY, genAbs]

genSTY :: MonadGen m => m Instruction
genSTY = STY <$> G.choice [genZpg, genZpgX, genAbs]

genAcc :: MonadGen m => m Oper
genAcc = return Acc

genImm :: MonadGen m => m Oper
genImm = Imm <$> w8

genZpg :: MonadGen m => m Oper
genZpg = Zpg <$> w8

genZpgX :: MonadGen m => m Oper
genZpgX = ZpgX <$> w8

genZpgY :: MonadGen m => m Oper
genZpgY = ZpgY <$> w8

genAbs :: MonadGen m => m Oper
genAbs = Abs <$> w16

genAbsX :: MonadGen m => m Oper
genAbsX = AbsX <$> w16

genAbsY :: MonadGen m => m Oper
genAbsY = AbsY <$> w16

genIndX :: MonadGen m => m Oper
genIndX = IndX <$> w8

genInd :: MonadGen m => m Oper
genInd = Ind <$> w16

genIndY :: MonadGen m => m Oper
genIndY = IndY <$> w8

genRel :: MonadGen m => m Oper
genRel = Rel <$> i8

genAddr :: MonadGen m => m Oper
genAddr = Addr <$> word16 (R.linear 0x100 255)

genBRK :: MonadGen m => m Instruction
genBRK = return BRK

w8 :: MonadGen m => m Word8
w8 = word8 (R.linear 20 40)

w16 :: MonadGen m => m Word16
w16 = fromIntegral <$> w8

i8 :: MonadGen m => m Int8
i8 = fromIntegral <$> int8 R.linearBounded
