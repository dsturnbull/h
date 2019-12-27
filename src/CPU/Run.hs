{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module CPU.Run
  ( readProgram
  , load
  , runShowCPU
  , runCPU
  , runSound
  , step
  ) where

import CPU
import CPU.Debugger
import CPU.Hardware.Sound
import CPU.Hardware.Sound.SID
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
import Control.Concurrent.Suspend
import Control.Concurrent.Timer
import Control.Lens
import Control.Monad
import Data.Binary                  hiding (decode)
import Data.Binary.Get
import Data.Fixed
import Data.Generics.Product.Any
import Data.Generics.Product.Fields
import Data.Time.Clock
import Data.Vector.Storable         ((//))
import GHC.Generics
import GHC.IO                       (evaluate)
import Options.Applicative
import Prelude                      hiding (break)
import System.Posix.Unistd

import qualified Data.Vector.Storable            as DVS
import qualified Data.Vector.Storable.ByteString as DVSB

-- XXX figure it out
runCPU :: Integer -> TVar CPU -> IO ()
runCPU h' cpuSTM =
  forever $ do
    sl <- stepCPU cpuSTM
    let delay = ceiling $ nanos h'
    nanosleep (fromIntegral $ delay * sl)
    yield

runShowCPU :: Integer -> TVar CPU -> IO ()
runShowCPU d cpuSTM = void $ flip repeatedTimer (msDelay $ ceiling (((1 :: Double) / fromInteger d) * 1000)) $
  readTVarIO cpuSTM >>= updateDebugger

runSound :: TVar CPU -> IO ()
runSound cpuSTM =
  void $ flip repeatedTimer (usDelay (fromInteger (ceiling CPU.Hardware.Sound.SID.µs))) $
    stepSound cpuSTM

readProgram :: Get (Word16, Program)
readProgram = do
  cloc <- loc
  clen <- len
  cdat <- getByteString (fromIntegral clen)
  dloc <- loc
  dlen <- len
  ddat <- getByteString (fromIntegral dlen)
  return (cloc, Program (cloc, DVSB.byteStringToVector cdat) (dloc, DVSB.byteStringToVector ddat))

  where
    loc :: Get Word16
    loc = getWord16be

    len :: Get Word16
    len = getWord16be

load :: Program -> CPU -> CPU
load (Program (coff, cdat) (doff, ddat)) cpu = do
  let c = zip [fromIntegral coff..] (DVS.toList cdat)
  let d = zip [fromIntegral doff..] (DVS.toList ddat)
  cpu & field @"mem" %~ (// (c ++ d))

stepCPU :: TVar CPU -> IO Int
stepCPU cpuSTM = do
  cpu' <- readTVarIO cpuSTM >>= \cpu ->
    if cpu & p & break
      then cpu & debuggerInput
                  >>= debugged (evaluate . step)
                  >>= debugged updateTimers
                  >>= (\d -> return $ d ^. the @1)
      else step cpu & updateTimers
  atomically $ writeTVar cpuSTM cpu'
  return $ cpu' & tim

step :: CPU -> CPU
step cpu =
  cpu & execute ins & updatePC & setTim
  where ins      = decode @Opcode mem'
        mem'     = DVS.drop (fromIntegral (pc cpu)) (mem cpu)
        len      = fromIntegral $ insLength ins
        updatePC = if jumps ins then id else field @"pc" %~ flip (+) len
        setTim   = field @"tim" .~ t
        t        = cycles ins

debugged :: (CPU -> IO CPU) -> DebugState CPU -> IO (DebugState CPU)
debugged f (Step cpu)      = Step <$> f cpu
debugged _ (Broken cpu)    = Broken <$> return cpu
debugged _ (Continue cpu)  = Continue <$> return cpu
debugged _ (Overwrite cpu) = do
  a <- getChar
  b <- getChar
  let val = read $ "0x" <> [a, b]
  return $ Broken (cpu & st (cpu & pc) val & field @"pc" %~ flip (+) 1)
debugged _ (Goto cpu) = do
  a <- getChar
  b <- getChar
  c <- getChar
  d <- getChar
  let val = read $ "0x" <> [a, b, c, d]
  return $ Broken $ cpu & field @"pc" .~ val

stepSound :: TVar CPU -> IO ()
stepSound cpuSTM = do
  cpu <- readTVarIO cpuSTM
  cpu' <- cpu & updateSIDClock >>= tickSound
  atomically $ writeTVar cpuSTM cpu'
  -- (atomically $ modifyTVar cpuSTM tickSound)

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
