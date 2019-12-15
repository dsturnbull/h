{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CPU.Debugger
  ( debugger
  , debuggerInput
  , DebugState(..)
  ) where

import CPU
import CPU.Hardware.Terminal
import CPU.Instructions.Impl (rti)

import Control.Applicative
import Control.Concurrent.STM
import Control.Lens                 hiding (elements)
import Control.Monad
import Data.Generics.Product.Fields
import Data.Maybe
import Data.Word
import GHC.Generics
import Prelude                      hiding (break)
import System.Posix.Types           (Fd)

-- import Foreign.C.Types
-- import Foreign.Marshal.Alloc
-- import Foreign.Storable

data DebugState a = Broken a | Step a | Continue a
  deriving Generic

debugger :: Maybe Word8 -> CPU -> IO (DebugState CPU)
debugger mc cpu =
  case mc of
    Just 17 -> return $ Continue $ cpu & continue
    Just 24 -> return $ Step cpu
    Just c  -> return $ Broken $ cpu & processInput c
    Nothing -> return $ Broken cpu

continue :: CPU -> CPU
continue cpu =
  cpu & rti
      & field @"p" . field @"break" .~ False
      & field @"p" . field @"interrupt" .~ False
      & field @"pc" %~ flip (-) 2

debuggerInput :: TMVar Word8 -> Fd -> CPU -> IO (DebugState CPU)
debuggerInput wS tty cpu = do
  mc <- getInput tty
  k <- atomically $ tryTakeTMVar wS
  cpu & debugger (mc <|> k)
