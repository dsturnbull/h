{-# LANGUAGE DataKinds #-}

module CPU.Instructions.IS.NOP
  ( nop
  ) where

import CPU

nop :: CPU -> CPU
nop = id
