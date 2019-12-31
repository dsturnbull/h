{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.T
  ( tax
  , txa
  , tay
  , tya
  , tsx
  , txs
  ) where

import CPU

import Control.Lens
import Data.Generics.Product.Fields
import Data.Word

setNegative :: Word8 -> CPU -> CPU
setNegative w cpu = cpu & field @"p" . field @"negative" .~ (w < 0)

setZero :: Word8 -> CPU -> CPU
setZero v cpu = cpu & field @"p" . field @"zero" .~ (v == 0)

ta_ :: (CPU -> Word8) -> ASetter' CPU Word8 -> CPU -> CPU
ta_ f w cpu =
  cpu & setNegative v
      & setZero v
      & w .~ v
  where v = f cpu

tax :: CPU -> CPU
tax = ta_ rA (field @"rX")

txa :: CPU -> CPU
txa = ta_ rX (field @"rA")

tay :: CPU -> CPU
tay = ta_ rA (field @"rY")

tya :: CPU -> CPU
tya = ta_ rY (field @"rA")

tsx :: CPU -> CPU
tsx = ta_ s (field @"rX")

txs :: CPU -> CPU
txs = ta_ rX (field @"s")
