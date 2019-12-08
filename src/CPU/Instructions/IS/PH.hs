{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.PH
  ( pha
  , php
  , push
  ) where

import CPU

import Control.Lens
import Data.Generics.Product.Fields
import Data.Word

push :: Word8 -> CPU -> CPU
push w cpu = cpu & st addr w
                 & field @"s" %~ (flip (-) 1)
  where addr = fromIntegral (cpu & s) + stack

pha :: CPU -> CPU
pha cpu = cpu & push (cpu & rA)

php :: CPU -> CPU
php cpu = cpu & push (cpu & p & flagsToWord)
