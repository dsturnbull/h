{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Hardware.Terminal
  ( readKbd
  , writeOutput
  , initTTY
  , updateTTY
  ) where

import CPU
import CPU.Hardware.Interrupt

import Control.Exception
import Control.Lens
import Control.Monad
import Data.ByteString.Internal (c2w)
import Data.Vector.Storable     (Vector, (!))
import Data.Word
import Foreign                  hiding (void)
import System.Posix.IO
import System.Posix.Types       (Fd)

-- import Foreign.C.Types
-- import Foreign.Marshal.Alloc
-- import Foreign.Storable

import qualified Data.Vector.Storable          as DVS
import qualified Data.Vector.Storable.Internal as DVSI

readKbd :: Fd -> CPU -> IO CPU
readKbd tty cpu =
  if cpu & p & interrupt
    then return cpu
    else allocaBytes 1 $ \ptr -> do
      nr <- fdReadBuf tty ptr 1 `catch` (\(_ :: IOException) -> pure 0)
      if nr > 0
        then do
          c <- peek ptr
          return $ cpu
                 & st (fromIntegral kbd) c
                 & st (fromIntegral (kbd + 2)) 1
                 & intr
        else return cpu

writeOutput :: Fd -> CPU -> IO CPU
writeOutput tty cpu =
  allocaBytes 1 $ \ptr -> do
    when (c > 0) $ do
      poke ptr c
      void $ fdWriteBuf tty ptr 1
    return $ cpu
           & st (fromIntegral (kbd + 1)) (0 :: Word8)
    where c = (cpu & mem) ! fromIntegral (kbd + 1)

ttyE :: Vector Word8 -> Fd -> IO ()
ttyE vec tty =
  let (ptr, l) = vectorToPtr0 vec
  in void $ fdWriteBuf tty ptr (fromIntegral l)

vectorToPtr0 :: DVS.Storable a => DVS.Vector a -> (Ptr a, Int)
vectorToPtr0 vector =
  let (foreignPtr, size) = DVS.unsafeToForeignPtr0 vector
  in (DVSI.getPtr foreignPtr, size)

initTTY :: Fd -> IO ()
initTTY tty =
  ttyE (convert "A: ") tty

updateTTY :: CPU -> Fd -> IO ()
updateTTY cpu tty = do
  -- let ls = lines $ show cpu
  -- ttyE (convert (head ls)) tty
  return ()
  -- print one last time, while broken

convert :: String -> Vector Word8
convert str = DVS.fromList (c2w <$> str)
