{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Hardware.Terminal
  ( readTermKbd
  , updateScreen
  , processInput
  , checkForDebug
  ) where

import CPU
import CPU.Hardware.Interrupt
import CPU.Hardware.TTY
import CPU.Instructions.Impl

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Vector.Storable   (slice)
import Data.Word
import Prelude                hiding (break)
import System.Console.ANSI
import Text.Latin1

import qualified Data.Vector.Storable as DVS

readTermKbd :: TVar CPU -> IO ()
readTermKbd cpuSTM = do
  c <- fromIntegral . ord <$> getChar
  atomically $ do
    cpu <- readTVar cpuSTM -- keep waiting for cpu to change
    when (cpu & p & interrupt & not) $ void $ tryPutTMVar (cpu & ready) () -- keep waiting for interrupt clear
    _   <- takeTMVar (cpu & ready) -- make non-ready
    writeTVar cpuSTM (cpu & processInput c) -- interrupts, so we will now wait for interrupt to clear again

  readTermKbd cpuSTM

processInput :: Word8 -> CPU -> CPU
processInput c cpu =
  cpu & st (fromIntegral kbd) c
      & st (fromIntegral (kbd + 2)) 1
      & intr

screenRows :: Int
screenRows = 25

screenCols :: Int
screenCols = 40

updateScreen :: CPU -> IO ()
updateScreen cpu = traverse_ (uncurry printRow) rows
  where rows     = getRow <$> [0 .. screenRows - 1]
        printRow r cols =
          void . sequence $ zip [0..] (DVS.toList cols) <&>
            \(c, w) -> do
              setCursorPosition r c
              putChar . toPrintable . chr . fromIntegral $ w
        getRow r = (r, slice (fromIntegral screenV + r * screenCols) screenCols (cpu & mem))
        toPrintable c = if isPrintable c then c else ' '

checkForDebug :: CPU -> IO CPU
checkForDebug cpu = do
  mc <- getInput (cpu & tty & mfd)
  case mc of
    Just 19 -> return $ cpu & brk
    Just _  -> return cpu
    Nothing -> return cpu
