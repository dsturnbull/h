{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Terminal
  ( getInput
  ) where

import Control.Exception
import Data.Word
import Foreign
import System.Posix.IO
import System.Posix.Types (Fd)

getInput :: Fd -> IO (Maybe Word8)
getInput tty =
    allocaBytes 1 $ \ptr -> do
      nr <- fdReadBuf tty ptr 1 `catch` (\(_ :: IOException) -> pure 0)
      if nr > 0
        then do c <- peek ptr
                return $ Just c
        else return Nothing
