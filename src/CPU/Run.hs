{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module CPU.Run
  ( load
  , step
  , stepCPU
  , stepSound
  ) where

import CPU
import CPU.Debugger
import CPU.Hardware.Sound
import CPU.Hardware.Terminal
import CPU.Hardware.Timer
import CPU.Instructions.Decodes
import CPU.Instructions.Execute
import CPU.Instructions.Jumps
import CPU.Instructions.Length
import CPU.Instructions.Opcode
import CPU.Instructions.Timing
import CPU.Program

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Fixed
import Data.Generics.Product.Any
import Data.Generics.Product.Fields
import Data.Time.Clock
import Data.Vector.Storable         ((//))
import GHC.IO                       (evaluate)
import Prelude                      hiding (break)
import System.Posix.Types           (Fd)

import qualified Data.Vector.Storable as DVS

load :: Int -> Program -> CPU -> CPU
load o (Program bin) cpu = do
  let w = zip [o..] (DVS.toList bin)
  cpu & field @"mem" %~ (// w)

step :: CPU -> CPU
step cpu =
  cpu & execute ins & updatePC & setTim
  where ins      = decode @Opcode mem'
        mem'     = DVS.drop (fromIntegral (pc cpu)) (mem cpu)
        len      = fromIntegral $ insLength ins
        updatePC = if jumps ins then id else field @"pc" %~ flip (+) len
        setTim   = field @"tim" .~ t
        t        = cycles ins

stepCPU :: Fd -> TVar CPU -> IO ()
stepCPU tty cpuSTM = do
  cpu' <- readTVarIO cpuSTM >>= \cpu ->
    if cpu & p & break
      then cpu & debugger tty
                  >>= debugged (evaluate . step)
                  >>= debugged (writeOutput tty)
                  >>= debugged updateClock
                  >>= debugged updateTimers
                  >>= (\d -> return $ d ^. the @1)
      else step cpu & readKbd tty
                  >>= writeOutput tty
                  >>= updateClock
                  >>= updateTimers
  atomically $ writeTVar cpuSTM cpu'
  threadDelay (cpu' & tim)

debugged :: (CPU -> IO CPU) -> DebugState CPU -> IO (DebugState CPU)
debugged f (Step cpu)     = Step <$> f cpu
debugged _ (Broken cpu)   = Broken <$> return cpu
debugged _ (Continue cpu) = Continue <$> return cpu

stepSound :: TVar CPU -> IO ()
stepSound cpuSTM = do
  cpu <- readTVarIO cpuSTM
  cpu' <- cpu & updateSIDClock >>= tickSound
  atomically $ writeTVar cpuSTM cpu'
  -- (atomically $ modifyTVar cpuSTM tickSound)

updateClock :: CPU -> IO CPU
updateClock cpu = do
  (now, diff) <- timeDelta (cpu  ^. field @"clock")
  return $ cpu & field @"clock" .~ now
               & field @"dt"    .~ diff

updateSIDClock :: CPU -> IO CPU
updateSIDClock cpu = do
  (now, diff) <- timeDelta (sid' ^. field @"clock")
  return $ cpu & field @"sid" . field @"clock" .~ now
               & field @"sid" . field @"dt"    .~ diff
  where sid' = cpu ^. field @"sid"

timeDelta :: UTCTime -> IO (UTCTime, Double)
timeDelta before = do
  now <- getCurrentTime
  let diff :: Double = fromInteger . fromPico . nominalDiffTimeToSeconds $ diffUTCTime now before
  return (now, diff)

  where fromPico :: Pico -> Integer
        fromPico (MkFixed i) = i
