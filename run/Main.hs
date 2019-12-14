{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

import CPU
import CPU.Hardware.Sound
import CPU.Hardware.Sound.SID
import CPU.Hardware.Terminal
import CPU.Hardware.Timer
import CPU.Program
import CPU.Run

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Suspend
import Control.Concurrent.Timer
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Fixed
import Data.Generics.Product.Fields
import Data.Int
import Data.Time.Clock
import GHC.Generics
import Options.Applicative
import Prelude                      hiding (break, cycle, init)
import System.Posix.IO
import System.Posix.Terminal
import System.Posix.Types           (Fd)

import qualified Data.ByteString                 as BS
import qualified Data.Vector.Storable            as DVS
import qualified Data.Vector.Storable.ByteString as DVSB

data Run = Run
  { inputFile :: FilePath
  , memory    :: Int
  , hz        :: Integer
  , interval  :: Integer
  , debug     :: Bool
  , graphics  :: Bool
  } deriving Generic

main :: IO ()
main = do
  opt <- execParser sasmInfo
  let verbose = opt ^. field @"debug"

  b <- BS.readFile (opt ^. field @"inputFile")
  let prog = Program (DVSB.byteStringToVector b)
  when verbose $ print prog

  --  $0000 zpg
  --  $0100 stack
  --  $0200 unused
  -- *$0300 kbd in
  -- *$0301 kbd out
  --  $0314-$0315 irq vector    -- XXX $FFFE-$FFFF irq+brk (https://www.c64-wiki.com/wiki/Interrupt#Interrupt_Request_.28IRQ.29)
  --  $0316-$0317 break vector
  --  $0318-$0319 nmi vector
  -- *$0380 timer a control
  -- *$0381-$0382 timer a
  --  $0400 - $0500 audio

  --  $0300 - 0x11a0 bitmap
  --  ????? - colour

  (mfd, _) <- liftIO openPseudoTerminal
  liftIO $ setFdOption mfd NonBlockingRead True

  tty <- getSlaveTerminalName mfd
  putStrLn tty

  now <- getCurrentTime
  let init = load 0 prog $ mkCPU now (DVS.replicate (opt ^. field @"memory") 0)
                         & field @"ttyName" ?~ tty
  cpuSTM <- initSound init >>= newTVarIO

  stopping <- newEmptyTMVarIO
  _ <- forkIO $ runCPU stopping (opt ^. field @"hz") mfd cpuSTM
  _ <- forkIO $ runSound cpuSTM
  when verbose $ void . forkIO $ runShowCPU stopping (opt ^. field @"interval") cpuSTM

  forever $ threadDelay 10000000

runShowCPU :: TMVar Bool -> Integer -> TVar CPU -> IO ()
runShowCPU stop d cpuSTM = void $ flip repeatedTimer (msDelay $ ceiling (((1 :: Double) / fromInteger d) * 1000)) $ do
  cpu <- readTVarIO cpuSTM
  print cpu
  -- print one last time, while broken
  when (cpu & p & break) $ atomically $ void $ putTMVar stop True

runCPU :: TMVar Bool -> Integer -> Fd -> TVar CPU -> IO ()
runCPU stop h tty cpuSTM =
  void $ flip repeatedTimer (usDelay (ceiling (CPU.µs h))) $ do
    cpu' <- readTVarIO cpuSTM >>= \cpu ->
      step cpu & readKbd tty
             >>= writeOutput tty
             >>= updateClock
             >>= updateTimers
    atomically $ writeTVar cpuSTM cpu'
    threadDelay (cpu' & tim)
    when (cpu' & p & break) $ atomically $ putTMVar stop True

runSound :: TVar CPU -> IO ()
runSound cpuSTM =
  void $ flip repeatedTimer (usDelay (fromInteger (ceiling CPU.Hardware.Sound.SID.µs))) $ do
    cpu <- readTVarIO cpuSTM
    unless (cpu & p & break) $ do
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

sasmInfo :: ParserInfo Run
sasmInfo = info (sasmOpts <**> helper) (fullDesc <> progDesc "compile 6502 program" <> header "sasm")

sasmOpts :: Parser Run
sasmOpts = Run
  <$> strOption   (long "input-file"                  <> short 'f'       <> metavar "FILE"  <> help "file to assemble")
  <*> option auto (long "memory-size"                 <> short 'm'       <> metavar "BYTES" <> help "mem size")
  <*> option auto (long "hz"          <> value 985248 <> short 'h'       <> metavar "Hz"    <> help "Hz")
  <*> option auto (long "interval"                    <> short 'i'       <> metavar "Hz"    <> help "Hz (show)")
  <*> switch      (long "debug"                       <> short 'd'                          <> help "debug")
  <*> switch      (long "graphics"                    <> short 'g'                          <> help "graphics")
