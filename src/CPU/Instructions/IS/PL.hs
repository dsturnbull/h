{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.PL
  ( pla
  , plp
  ) where

import CPU

import Control.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable

pla :: CPU -> CPU
pla cpu = cpu & field @"rA" .~ v
              & field @"s" %~ (flip (+) 1)
  where addr = fromIntegral (cpu & s) + stack + 1
        v    = (cpu & mem) ! fromIntegral addr

plp :: CPU -> CPU
plp cpu = cpu & wordToFlags v
              & field @"s" %~ (flip (+) 1)
  where addr = fromIntegral (cpu & s) + stack + 1
        v    = (cpu & mem) ! fromIntegral addr
