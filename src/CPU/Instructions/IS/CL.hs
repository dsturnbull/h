{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.CL
  ( clc
  , cld
  , cli
  , clv
  ) where

import CPU

import Control.Lens
import Data.Generics.Product.Fields

clc :: CPU -> CPU
clc cpu = cpu & field @"p" . field @"carry" .~ False

cld :: CPU -> CPU
cld cpu = cpu & field @"p" . field @"decimal" .~ False

cli :: CPU -> CPU
cli cpu = cpu & field @"p" . field @"interrupt" .~ False

clv :: CPU -> CPU
clv cpu = cpu & field @"p" . field @"overflow" .~ False
