module CPU.Hardware.Terminal
  ( readTermKbd
  ) where

import CPU
import CPU.Instructions.Impl

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Char
import Prelude                hiding (break)

readTermKbd :: TVar CPU -> IO ()
readTermKbd cpuSTM = do
  c <- ord <$> getChar
  atomically $ do
    cpu <- readTVar cpuSTM -- keep waiting for cpu to change
    when (cpu & p & interrupt & not) $ void $ tryPutTMVar (cpu & ready) () -- keep waiting for interrupt clear
    _   <- takeTMVar (cpu & ready) -- make non-ready
    when (c == 0x13) $
      writeTVar cpuSTM (cpu & brk) -- interrupts, so subsequently we will wait for interrupt to clear again

  readTermKbd cpuSTM
