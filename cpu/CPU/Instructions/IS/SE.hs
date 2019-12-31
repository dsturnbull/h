{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module CPU.Instructions.IS.SE
  ( sec
  , sed
  , sei
  ) where

import CPU

import Control.Lens
import Data.Generics.Product.Fields

sec :: CPU -> CPU
sec cpu = cpu & field @"p" . field @"carry" .~ True

sed :: CPU -> CPU
sed cpu = cpu & field @"p" . field @"decimal" .~ True

sei :: CPU -> CPU
sei cpu = cpu & field @"p" . field @"interrupt" .~ True
