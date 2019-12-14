{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Hardware.Timer.JiffyTimer
  ( JiffyTimer(..)
  , mkJiffyTimer
  )
  where

import Data.Time.Clock
import GHC.Generics

data JiffyTimer = JiffyTimer
  { clock :: UTCTime
  , dt    :: Double
  } deriving (Eq, Generic)

mkJiffyTimer :: UTCTime -> JiffyTimer
mkJiffyTimer t0 = JiffyTimer t0 0.0
