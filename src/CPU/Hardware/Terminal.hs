module CPU.Hardware.Terminal
  ( readKbd
  , writeOutput
  , processInput
  ) where

import CPU
import CPU.Hardware.Interrupt
import CPU.Instructions.Impl
import CPU.Terminal

import Control.Lens
import Control.Monad
import Data.Vector.Storable ((!))
import Data.Word
import Foreign              hiding (void)
import Prelude              hiding (break)
import System.Posix.IO
import System.Posix.Types   (Fd)

readKbd :: Fd -> CPU -> IO CPU
readKbd tty cpu = do
  mc <- getInput tty
  case mc of
    Just 19 -> return $ cpu & brk
    Just c  -> return $ cpu & processInput c
    Nothing -> return cpu

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
