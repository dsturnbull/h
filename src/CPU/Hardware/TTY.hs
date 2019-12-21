{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Hardware.TTY
  ( TTY(..)
  , mkTTY
  , connected
  , out
  , getInput
  , getInputBlocking
  )
  where

import Control.Exception
import Control.Monad
import Foreign            hiding (void)
import Foreign.C.String
import GHC.Generics
import Prelude            hiding (break)
import System.Posix.IO
import System.Posix.Types (Fd)

data TTY = TTY
  { mfd  :: Fd
  , name :: String
  } deriving (Generic, Eq)

mkTTY :: Fd -> String -> TTY
mkTTY = TTY

-- XXX
connected :: TTY -> Bool
connected _ = True

out :: TTY -> String -> IO ()
out (TTY fd _) s =
  withCString s $ \ptr ->
    void $ fdWriteBuf fd (castPtr ptr) (fromIntegral l)
  where l = length s

getInput :: Fd -> IO (Maybe Word8)
getInput t =
  allocaBytes 1 $ \ptr -> do
    nr <- fdReadBuf t ptr 1 `catch` (\(_ :: IOException) -> pure 0)
    if nr > 0
      then do c <- peek ptr
              return $ Just c
      else return Nothing

getInputBlocking :: Fd -> IO Word8
getInputBlocking t = do
  mc <- getInput t
  case mc of
    Just c  -> return c
    Nothing -> getInputBlocking t
