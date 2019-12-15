{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CPU.Debugger
  ( debugger
  , debuggerInput
  , initDebugger
  , updateDebugger
  , DebugState(..)
  ) where

import CPU
import CPU.Debugger.Mode
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
import System.Console.ANSI
import System.IO
import System.IO.Echo.Internal
import System.Posix.Types           (Fd)

import qualified CPU.Debugger.Debug  as Debug
import qualified CPU.Debugger.Status as Status

data DebugState a = Broken a | Step a | Continue a
  deriving Generic

debugger :: Maybe Word8 -> CPU -> IO (DebugState CPU)
debugger mc cpu =
  case mc of
    Just 17 -> return $ Continue $ cpu & continue                  -- ^Q
    Just 24 -> return $ Step cpu                                   -- ^S
    Just 7  -> return $ Broken $ cpu & field @"debugMode" .~ Debug -- ^G
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

initDebugger :: CPU -> IO ()
initDebugger cpu = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  void $ sttyRaw "-ixon"
  clearScreen
  cpu & Status.drawScreen
  hFlush stdout

updateDebugger :: CPU -> IO ()
updateDebugger cpu = do
  case cpu & debugMode of
    Status -> cpu & Status.updateScreen
    Debug  -> cpu & Debug.updateScreen
  hFlush stdout
  hFlush stdout
