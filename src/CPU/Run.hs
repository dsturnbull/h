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
  ) where

import CPU
import CPU.Debugger
import CPU.Hardware.Sound
import CPU.Hardware.Sound.SID
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
import Control.Concurrent.Suspend
import Control.Concurrent.Timer
import Control.Lens
import Control.Monad
import Data.Binary                  hiding (decode)
import Data.Binary.Get
import Data.Char
import Data.Fixed
import Data.Generics.Product.Any
import Data.Generics.Product.Fields
import Data.Time.Clock
import Data.Vector.Storable         ((//))
import GHC.Generics
import GHC.IO                       (evaluate)
import Options.Applicative
import Prelude                      hiding (break)
import System.Posix.Types           (Fd)

import qualified Data.Vector.Storable            as DVS
import qualified Data.Vector.Storable.ByteString as DVSB

runCPU :: TMVar Word8 -> Integer -> Fd -> TVar CPU -> IO ()
runCPU wS h' tty cpuSTM =
  forever $ do
  -- void $ flip repeatedTimer (usDelay (ceiling (CPU.µs h))) $
    sl <- stepCPU wS tty cpuSTM
    let delay = ceiling $ CPU.µs h'
    threadDelay (sl * delay)

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

stepCPU :: TMVar Word8 -> Fd -> TVar CPU -> IO Int
stepCPU wS tty cpuSTM = do
  cpu' <- readTVarIO cpuSTM >>= \cpu ->
    if cpu & p & break
      -- TODO: pass in a real ttyMode
      then cpu & debuggerInput False wS tty
                  >>= debugged wS (evaluate . step)
                  >>= debugged wS (writeOutput tty)
                  >>= debugged wS updateClock
                  >>= debugged wS updateTimers
                  >>= (\d -> return $ d ^. the @1)
      else step cpu & readKbd tty
                  >>= writeOutput tty
                  >>= updateClock
                  >>= updateTimers
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

debugged :: TMVar Word8 -> (CPU -> IO CPU) -> DebugState CPU -> IO (DebugState CPU)
debugged _  f (Step cpu)      = Step <$> f cpu
debugged _  _ (Broken cpu)    = Broken <$> return cpu
debugged _  _ (Continue cpu)  = Continue <$> return cpu
debugged wS _ (Overwrite cpu) = do
  a <- atomically $ takeTMVar wS
  b <- atomically $ takeTMVar wS
  let val = read $ "0x" <> [chr (fromIntegral a), chr (fromIntegral b)]
  return $ Broken (cpu & st (cpu & pc) val & field @"pc" %~ flip (+) 1)
debugged wS _ (Goto cpu) = do
  a <- atomically $ takeTMVar wS
  b <- atomically $ takeTMVar wS
  c <- atomically $ takeTMVar wS
  d <- atomically $ takeTMVar wS
  let val = read $ "0x" <> (chr . fromIntegral <$> [a, b, c, d])
  return $ Broken $ cpu & field @"pc" .~ val

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
