{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.NOP
  ( nop
  ) where

import CPU

nop :: CPU -> CPU
nop = id
