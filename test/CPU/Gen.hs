module CPU.Gen
  ( genCPU
  ) where

import CPU
import CPU.Hardware.TTY

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Time.Clock
import Data.Word

import qualified Data.Vector.Storable as DVS

genCPU :: MonadIO m => Word16 -> m CPU
genCPU memSize = do
  now <- liftIO getCurrentTime
  tm <- liftIO $ newTMVarIO ()
  return $ mkCPU now (mkTTY 0 "") 0 (DVS.replicate (fromIntegral memSize) 0) 0x0000 tm
