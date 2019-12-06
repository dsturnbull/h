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
genInstruction = do
  ins <- G.choice [ genADC
                  , genAND
                  , genASL
                  -- , genB
                  , genCMP
                  , genCPX
                  , genCPY
                  , genDE
                  , genEOR
                  , genLDA
                  , genLDX
                  , genLDY
                  ]
  return ins

genCodeBreaking :: MonadGen m => Range Int -> m Instruction -> m [Instruction]
genCodeBreaking r is = do
    code <- G.list r is
    return $ code ++ [BRK]

genProg :: MonadGen m => [Instruction] -> m Program
genProg code = return $ Program (DVS.fromList (join $ asm <$> code))

genADC :: MonadGen m => m Instruction
genADC = ADC <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY, genIndX, genIndY]

genAND :: MonadGen m => m Instruction
genAND = AND <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY, genIndX, genIndY]

genASL :: MonadGen m => m Instruction
genASL = ASL <$> G.choice [genAcc, genZpg, genZpgX, genAbs, genAbsX]

genB :: MonadGen m => m Instruction
genB = do
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

genLDA :: MonadGen m => m Instruction
genLDA = LDA <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX, genAbsY]

genLDX :: MonadGen m => m Instruction
genLDX = LDX <$> G.choice [genImm, genZpg, genZpgY, genAbs, genAbsY]

genLDY :: MonadGen m => m Instruction
genLDY = LDY <$> G.choice [genImm, genZpg, genZpgX, genAbs, genAbsX]

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

genIndY :: MonadGen m => m Oper
genIndY = IndY <$> w8

genRel :: MonadGen m => m Oper
genRel = Rel <$> i8

genAddr :: MonadGen m => m Oper
genAddr = Addr <$> word16 (R.linear 0x100 255)

genBRK :: MonadGen m => m Instruction
genBRK = return BRK

w8 :: MonadGen m => m Word8
w8 = word8 R.linearBounded

w16 :: MonadGen m => m Word16
w16 = fromIntegral <$> word8 R.linearBounded

i8 :: MonadGen m => m Int8
i8 = fromIntegral <$> int8 R.linearBounded
