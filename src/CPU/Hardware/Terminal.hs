{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CPU.Hardware.Terminal
  ( readKbd
  , writeOutput
  ) where

import Control.Exception
import Control.Lens
import Control.Monad
import CPU
import CPU.Hardware.Interrupt
import Data.Generics.Product.Fields
import Data.Vector.Storable         ((!))
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.Posix.IO
import System.Posix.Types           (Fd)

import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM

readKbd :: Fd -> CPU -> IO CPU
readKbd tty cpu =
  if cpu & p & interrupt
    then return cpu
    else do
      allocaBytes 1 $ \ptr -> do
        nr <- fdReadBuf tty ptr 1 `catch` (\(_ :: IOException) -> pure 0)
        if (nr > 0)
          then do
            c <- peek ptr
            return $ cpu
                  & field @"mem" %~ DVS.modify (\vec -> DVSM.write vec (fromIntegral kbd) c)
                  & intr
          else return cpu

writeOutput :: Fd -> CPU -> IO CPU
writeOutput tty cpu =
  allocaBytes 1 $ \ptr -> do
    when (c > 0) $ do
      poke ptr c
      void $ fdWriteBuf tty ptr 1
    return $ cpu
           & field @"mem" %~ DVS.modify (\vec -> DVSM.write vec (fromIntegral (kbd + 1)) (0 :: Word8))
    where c = (cpu & mem) ! fromIntegral (kbd + 1)
