{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.B
  ( bcc
  , bcs
  , beq
  , bmi
  , bne
  , bpl
  , bvc
  , bvs
  ) where

import CPU

import Control.Lens
import Data.Generics.Product.Fields
import Data.Int

branch :: Int8 -> CPU -> CPU
branch i = field @"pc" %~ (flip (+) (fromIntegral i - 2))

carrying :: CPU -> Bool
carrying cpu = cpu & p & carry

bcc :: Int8 -> CPU -> CPU
bcc i cpu =
  if cpu & carrying & not
    then cpu & branch i
    else cpu

bcs :: Int8 -> CPU -> CPU
bcs i cpu =
  if cpu & carrying
    then cpu & branch i
    else cpu

beq :: Int8 -> CPU -> CPU
beq i cpu =
  if cpu & p & zero
    then cpu & branch i
    else cpu

bmi :: Int8 -> CPU -> CPU
bmi i cpu =
  if cpu & p & negative
    then cpu & branch i
    else cpu

bne :: Int8 -> CPU -> CPU
bne i cpu =
  if cpu & p & zero & not
    then cpu & branch i
    else cpu

bpl :: Int8 -> CPU -> CPU
bpl i cpu =
  if cpu & p & negative & not
    then cpu & branch i
    else cpu

overflowing :: CPU -> Bool
overflowing cpu = cpu & p & overflow

bvc :: Int8 -> CPU -> CPU
bvc i cpu =
  if cpu & overflowing & not
    then cpu & branch i
    else cpu

bvs :: Int8 -> CPU -> CPU
bvs i cpu =
  if cpu & overflowing
    then cpu & branch i
    else cpu
