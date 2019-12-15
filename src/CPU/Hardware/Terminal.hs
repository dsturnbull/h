{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Hardware.Terminal
  ( getInput
  , readKbd
  , readTermKbd
  , writeOutput
  , processInput
  ) where

import CPU
import CPU.Hardware.Interrupt
import CPU.Instructions.Impl

import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Char              (ord)
import Data.Vector.Storable   ((!))
import Data.Word
import Foreign                hiding (void)
import Prelude                hiding (break)
import System.Posix.IO
import System.Posix.Types     (Fd)

getInput :: Fd -> IO (Maybe Word8)
getInput tty =
  allocaBytes 1 $ \ptr -> do
    nr <- fdReadBuf tty ptr 1 `catch` (\(_ :: IOException) -> pure 0)
    if nr > 0
      then do c <- peek ptr
              return $ Just c
      else return Nothing

readKbd :: Fd -> CPU -> IO CPU
readKbd tty cpu = do
  mc <- getInput tty
  case mc of
    Just 19 -> return $ cpu & brk
    Just c  -> return $ cpu & processInput c
    Nothing -> return cpu

readTermKbd :: TMVar Word8 -> TVar CPU -> IO ()
readTermKbd wS cpuSTM = do
  c <- fromIntegral . ord <$> getChar
  atomically $ do
    cpu <- readTVar cpuSTM

    if cpu & p & break
      then
        -- only put chars for debugger when broken, and not immediately.
        putTMVar wS c
      else do
        -- otherwise, update as normal
        let cpu' = if | c == 19   -> cpu & brk
                      | otherwise -> cpu & processInput c
        writeTVar cpuSTM cpu'

  readTermKbd wS cpuSTM

processInput :: Word8 -> CPU -> CPU
processInput c cpu =
  cpu & st (fromIntegral kbd) c
      & st (fromIntegral (kbd + 2)) 1
      & intr

writeOutput :: Fd -> CPU -> IO CPU
writeOutput tty cpu =
  allocaBytes 1 $ \ptr -> do
    when (c > 0) $ do
      poke ptr c
      void $ fdWriteBuf tty ptr 1
    return $ cpu
           & st (fromIntegral (kbd + 1)) (0 :: Word8)
    where c = (cpu & mem) ! fromIntegral (kbd + 1)
