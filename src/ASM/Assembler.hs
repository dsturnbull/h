{-# LANGUAGE ScopedTypeVariables #-}

module ASM.Assembler
  ( assemble
  , Program(..)
  ) where

import ASM.Parser
import CPU.Program

import Control.Monad
import Data.Either
import Data.Word

import qualified Data.Text            as T
import qualified Data.Vector.Storable as DVS

assemble :: T.Text -> Program
assemble prog = Program (DVS.fromList ws)
  where instructions = parseAssembly (T.unpack prog)
        ws :: [Word8] = join $ asm ins <$> ins
        ins :: [Instruction] = fromRight [] instructions
