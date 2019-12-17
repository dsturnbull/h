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

data DebugState a = Broken a | Step a | Continue a | Overwrite a
  deriving Generic

debugger :: [Word8] -> CPU -> IO (DebugState CPU)
debugger mc cpu =
  case mc of
    [0x11]             -> return $ Continue $ cpu & continue                  -- ^Q
    [0x18]             -> return $ Step cpu                                   -- ^S
    [0x07]             -> return $ Broken $ cpu & field @"debugMode" .~ Debug -- ^G
    [0x5b]             -> return $ Broken $ cpu & field @"pc" %~ flip (-) 1   -- [
    [0x7b]             -> return $ Broken $ cpu & field @"pc" %~ flip (-) 16  -- {
    [0x5d]             -> return $ Broken $ cpu & field @"pc" %~ flip (+) 1   -- ]
    [0x7d]             -> return $ Broken $ cpu & field @"pc" %~ flip (+) 16  -- }
    [0x1b, 0x5b, 0x41] -> return $ Broken $ cpu & field @"pc" %~ flip (-) 32  -- <up>
    [0x1b, 0x5b, 0x42] -> return $ Broken $ cpu & field @"pc" %~ flip (+) 32  -- <down>
    [0x1b, 0x5b, 0x43] -> return $ Broken $ cpu & field @"pc" %~ flip (+) 1   -- <right>
    [0x1b, 0x5b, 0x44] -> return $ Broken $ cpu & field @"pc" %~ flip (-) 1   -- <left>
    [0x14]             -> return $ Overwrite cpu                              -- ^T
    [c]                -> return $ Broken $ cpu & processInput c
    _                  -> return $ Broken cpu

continue :: CPU -> CPU
continue cpu =
  cpu & rti
      & field @"p" . field @"break" .~ False
      & field @"p" . field @"interrupt" .~ False
      & field @"pc" %~ flip (-) 2

debuggerInput :: Bool -> TMVar Word8 -> Fd -> CPU -> IO (DebugState CPU)
debuggerInput ttyMode wS tty cpu =
    -- ttyMode <- atomically (isEmptyTMVar wS)
    go [] [0x1b, 0x5b]
  where
    go :: [Word8] -> [Word8] -> IO (DebugState CPU)
    go keys (n:ns) = do
      mc <- getKey
      case mc of
        Just c | c == n -> go (keys ++ [c]) ns
        Just c          -> go (keys ++ [c]) []
        _               -> go keys (n:ns)
    go [c] [] = cpu & debugger [c]
    go keys [] = do
      mc <- getKey
      case mc of
        Just c -> cpu & debugger (keys ++ [c])
        _      -> cpu & debugger keys
    getKey =
      if ttyMode
        then getInput tty
        else atomically $ Just <$> takeTMVar wS

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
