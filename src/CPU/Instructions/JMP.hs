{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.JMP
  ( jmpAbs
  , jmpInd
  ) where

import CPU

import Control.Lens
import Data.Generics.Product.Fields
import Data.Word

jmpAbs :: Word16 -> CPU -> CPU
jmpAbs addr cpu = cpu & field @"pc" .~ addr

jmpInd :: Word16 -> CPU -> CPU
jmpInd addr cpu = cpu & field @"pc" .~ addr'
  where addr' = cpu & indirect addr
