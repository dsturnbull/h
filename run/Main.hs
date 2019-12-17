{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

import CPU
import CPU.Debugger
import CPU.Hardware.Sound
import CPU.Hardware.Terminal
import CPU.Program
import CPU.Run

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Generics.Product.Fields
import Data.Time.Clock
import Data.Word
import GHC.Generics
import Options.Applicative
import System.Posix.IO
import System.Posix.Terminal

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
  , location  :: Word16
  } deriving Generic

main :: IO ()
main = do
  opt <- execParser sasmInfo

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
  --  $0400 - $041c audio
  --  $04fc-$04fd reset vector address
  --  $04fe-$04ff reset routine address

  --  $0300 - 0x11a0 bitmap
  --  ????? - colour

  (mfd, _) <- liftIO openPseudoTerminal
  liftIO $ setFdOption mfd NonBlockingRead True

  tty <- getSlaveTerminalName mfd

  let loc  = opt ^. field @"location"
  b <- BS.readFile (opt ^. field @"inputFile")
  let prog = Program (DVSB.byteStringToVector b)

  now <- getCurrentTime
  let initial = load prog $ mkCPU now (opt ^. field @"hz") (DVS.replicate (opt ^. field @"memory") 0) loc
                          & field @"ttyName" ?~ tty
                          & field @"pc" .~ fromIntegral loc
  initial & initDebugger

  cpuSTM <- initSound initial >>= newTVarIO
  wS <- newEmptyTMVarIO
  _ <- forkIO $ readTermKbd wS cpuSTM
  _ <- forkIO $ runCPU wS (opt ^. field @"hz") mfd cpuSTM
  _ <- forkIO $ runSound cpuSTM
  _ <- forkIO $ runShowCPU (opt ^. field @"interval") cpuSTM

  forever $ threadDelay 10000000

sasmInfo :: ParserInfo Run
sasmInfo = info (sasmOpts <**> helper) (fullDesc <> progDesc "compile 6502 program" <> header "sasm")

sasmOpts :: Parser Run
sasmOpts = Run
  <$> strOption   (long "input-file"                  <> short 'f'       <> metavar "FILE"   <> help "file to assemble")
  <*> option auto (long "memory-size" <> value 0x500  <> short 'm'       <> metavar "BYTES"  <> help "mem size")
  <*> option auto (long "hz"          <> value 985248 <> short 'h'       <> metavar "Hz"     <> help "Hz")
  <*> option auto (long "interval"    <> value 50     <> short 'i'       <> metavar "Hz"     <> help "Hz (show)")
  <*> switch      (long "debug"                       <> short 'd'                           <> help "debug")
  <*> switch      (long "graphics"                    <> short 'g'                           <> help "graphics")
  <*> option auto (long "location"    <> value 0x200  <> short 'l'       <> metavar "OFFSET" <> help "target location")
