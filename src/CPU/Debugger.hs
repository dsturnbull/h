{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CPU.Debugger
  ( debugger
  , debuggerInput
  , initDebugger
  , updateDebugger
  , mf
  , DebugState(..)
  ) where

import CPU
import CPU.Debugger.Mode
import CPU.Instructions.Impl (rti)

import Control.Lens                 hiding (elements)
import Control.Monad
import Data.Char
import Data.Generics.Product.Fields
import Data.Word
import GHC.Generics
import Prelude                      hiding (break)
import System.Console.ANSI
import System.IO
import System.IO.Echo.Internal

import qualified CPU.Debugger.Debug  as Debug
import qualified CPU.Debugger.Status as Status

data DebugState a = Broken a | Step a | Continue a | Overwrite a | Goto a
  deriving Generic

debugger :: [Word8] -> CPU -> IO (DebugState CPU)
debugger mc cpu =
  case mc of
    [0x11]             -> return $ Continue $ cpu & continue                  -- ^Q
    [0x18]             -> return $ Step cpu                                   -- ^X
    [0x07]             -> return $ Goto cpu                                   -- ^G
    [0x1b, 0x5b, 0x41] -> return $ Broken $ cpu & field @"pc" %~ flip (-) 32  -- <up>
    [0x1b, 0x5b, 0x42] -> return $ Broken $ cpu & field @"pc" %~ flip (+) 32  -- <down>
    [0x1b, 0x5b, 0x43] -> return $ Broken $ cpu & field @"pc" %~ flip (+) 1   -- <right>
    [0x1b, 0x5b, 0x44] -> return $ Broken $ cpu & field @"pc" %~ flip (-) 1   -- <left>
    [0x14]             -> return $ Overwrite cpu                              -- ^T
    _                  -> return $ Broken cpu

continue :: CPU -> CPU
continue cpu =
  cpu & rti
      & field @"p" . field @"break" .~ False
      & field @"p" . field @"interrupt" .~ False
      & field @"pc" %~ flip (-) 2

debuggerInput :: CPU -> IO (DebugState CPU)
debuggerInput cpu =
    go [] [0x1b, 0x5b]
  where
    go :: [Word8] -> [Word8] -> IO (DebugState CPU)
    go keys (n:ns) = do
      c <- getKey
      if c == n
        then go (keys ++ [c]) ns
        else go (keys ++ [c]) []
    go [c] [] = cpu & debugger [c]
    go keys [] = do
      c <- getKey
      cpu & debugger (keys ++ [c])
    getKey = fromIntegral . ord <$> getChar

mf :: [Word8] -> ([Word8], [Word8], [Word8])
mf input =
    go input [] [0x1b, 0x5b]
  where
    go :: [Word8] -> [Word8] -> [Word8] -> ([Word8], [Word8], [Word8])
    go (c:cs) keys (n:ns) =
      if c == n
        then go cs (keys ++ [c]) ns
        else go cs (keys ++ [c]) []
    go (c:cs) [k] [] = ([k], [], cs)
    go (c:cs) keys [] = (keys ++ [c], [], cs)
    go [] keys n = (keys, n, [])

initDebugger :: CPU -> IO ()
initDebugger cpu = do
  void $ sttyRaw "-ixon"
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hideCursor
  clearScreen

  cpu & Status.drawScreen
  hFlush stdout

updateDebugger :: CPU -> IO ()
updateDebugger cpu = do
  case cpu & debugMode of
    Status -> cpu & Status.updateScreen
    Debug  -> cpu & Debug.updateScreen
  hFlush stdout
