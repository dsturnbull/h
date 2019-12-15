{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CPU.Debugger
  ( initTTY
  , updateTTY
  , debugger
  , DebugState(..)
  ) where

import CPU
import CPU.Hardware.Terminal
import CPU.Instructions.Impl (rti)
import CPU.Terminal

import Control.Lens
import Control.Monad
import Data.ByteString.Internal     (c2w)
import Data.Generics.Product.Fields
import Data.Vector.Storable         (Vector, (!))
import Data.Word
import Foreign                      hiding (void)
import GHC.Generics
import Prelude                      hiding (break)
import System.Posix.IO
import System.Posix.Types           (Fd)

-- import Foreign.C.Types
-- import Foreign.Marshal.Alloc
-- import Foreign.Storable

import qualified Data.Vector.Storable          as DVS
import qualified Data.Vector.Storable.Internal as DVSI

data DebugState a = Broken a | Step a | Continue a
  deriving Generic

debugger :: Fd -> CPU -> IO (DebugState CPU)
debugger tty cpu = do
  mc <- getInput tty
  case mc of
    Just 17 -> return $ Continue $ cpu & continue
    Just 24 -> return $ Step cpu
    Just c  -> return $ Broken $ cpu & processInput c
    Nothing -> return $ Broken cpu

continue :: CPU -> CPU
continue cpu =
  cpu & rti
      & field @"p" . field @"break" .~ False
      & field @"pc" %~ flip (-) 2

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

updateTTY :: Fd -> CPU -> IO ()
updateTTY tty cpu = do
  -- let ls = lines $ show cpu
  -- ttyE (convert (head ls)) tty
  return ()
  -- print one last time, while broken

convert :: String -> Vector Word8
convert str = DVS.fromList (c2w <$> str)
