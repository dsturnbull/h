{-# LANGUAGE DeriveGeneric #-}

module CPU.Debugger.Mode
  (DebugMode(..))
  where

import GHC.Generics

data DebugMode = Status | Debug deriving (Generic, Eq)

instance Show DebugMode where
  show Status = "stat"
  show Debug  = "dbug"
