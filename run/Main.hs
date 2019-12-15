{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

import CPU
import CPU.Debugger
import CPU.Hardware.Sound
import CPU.Hardware.Sound.SID
import CPU.Program
import CPU.Run

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Suspend
import Control.Concurrent.Timer
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Generics.Product.Fields
import Data.Time.Clock
import GHC.Generics
import Options.Applicative
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

  --  $0000 zpg
  --  $0100 stack
  --  $0200 unused
  -- *$0300 kbd in
  -- *$0301 kbd out
  -- *$0302 kbd irq
  --  $0314-$0315 irq vector    -- XXX $FFFE-$FFFF irq+brk (https://www.c64-wiki.com/wiki/Interrupt#Interrupt_Request_.28IRQ.29)
  --  $0316-$0317 break vector
  --  $0318-$0319 nmi vector
  -- *$0320 timer a control
  -- *$0321-$0382 timer a
  -- *$0323 timer irq
  --  $0400 - $0500 audio

  --  $0300 - 0x11a0 bitmap
  --  ????? - colour

  (mfd, _) <- liftIO openPseudoTerminal
  liftIO $ setFdOption mfd NonBlockingRead True

  tty <- getSlaveTerminalName mfd
  putStrLn tty

  now <- getCurrentTime
  let initial = load 0 prog $ mkCPU now (DVS.replicate (opt ^. field @"memory") 0)
                            & field @"ttyName" ?~ tty
  cpuSTM <- initSound initial >>= newTVarIO

  -- initTTY mfd
  _ <- forkIO $ runCPU (opt ^. field @"hz") mfd cpuSTM
  _ <- forkIO $ runSound cpuSTM
  when verbose $ void . forkIO $ runShowCPU mfd (opt ^. field @"interval") cpuSTM

  forever $ threadDelay 10000000

runShowCPU :: Fd -> Integer -> TVar CPU -> IO ()
runShowCPU tty d cpuSTM = void $ flip repeatedTimer (msDelay $ ceiling (((1 :: Double) / fromInteger d) * 1000)) $ do
  cpu <- readTVarIO cpuSTM
  print cpu
  cpu & updateTTY tty

runCPU :: Integer -> Fd -> TVar CPU -> IO ()
runCPU h tty cpuSTM =
  void $ flip repeatedTimer (usDelay (ceiling (CPU.µs h))) $
    stepCPU tty cpuSTM

runSound :: TVar CPU -> IO ()
runSound cpuSTM =
  void $ flip repeatedTimer (usDelay (fromInteger (ceiling CPU.Hardware.Sound.SID.µs))) $
    stepSound cpuSTM

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
