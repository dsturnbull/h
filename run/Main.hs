{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

import CPU
import CPU.Debugger
import CPU.Hardware.Sound
import CPU.Hardware.Terminal
import CPU.Hardware.TTY
import CPU.Hardware.Video
import CPU.Run

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.IO.Class
import Data.Binary.Get
import Data.Generics.Product.Fields
import Data.Time.Clock
import GHC.Generics
import Options.Applicative
import System.Posix.IO
import System.Posix.Terminal

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector.Storable as DVS

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

  --  $0000 zpg
  --  $0100 stack
  --  $0200 unused
  -- *$0300 kbd in
  -- *$0301 kbd out
  -- *$0302 kbd irq
  --  $fffe-ffff irq vector    -- (https://www.c64-wiki.com/wiki/Interrupt#Interrupt_Request_.28IRQ.29)
  --  $0316-$0317 break vector
  --  $0318-$0319 nmi vector
  -- *$0320 timer a control
  -- *$0321-$0382 timer a
  -- *$0323 timer irq
  --  $0400-$07e7 -- screen ram
  --  $2000-$3fff - bitmap memory
  --  $d400-$d41c audio
  --  $d4fc-$d4fd reset vector address
  --  $d4fe-$d4ff reset routine address
  --  $d800-$dbe7 -- colour ram

  (fd, _) <- liftIO openPseudoTerminal
  liftIO $ setFdOption fd NonBlockingRead True
  ttyN <- getSlaveTerminalName fd

  lbs <- LBS.readFile (opt ^. field @"inputFile")

  let (cloc, prog) = runGet readProgram lbs

  tm <- newEmptyTMVarIO
  now <- getCurrentTime
  let initial = load prog $ mkCPU now (mkTTY fd ttyN) (opt ^. field @"hz") (DVS.replicate (opt ^. field @"memory") 0) cloc tm
                          & field @"pc" .~ cloc
  initial & initDebugger

  cpuSTM <- initSound initial >>= newTVarIO
  _ <- forkIO $ readTermKbd cpuSTM
  _ <- forkIO $ runCPU (opt ^. field @"hz") cpuSTM
  _ <- forkIO $ runSound cpuSTM
  _ <- forkIO $ runShowCPU (opt ^. field @"interval") cpuSTM

  runVideo cpuSTM

sasmInfo :: ParserInfo Run
sasmInfo = info (sasmOpts <**> helper) (fullDesc <> progDesc "compile 6502 program" <> header "sasm")

sasmOpts :: Parser Run
sasmOpts = Run
  <$> strOption   (long "input-file"                   <> short 'f'       <> metavar "FILE"   <> help "file to assemble")
  <*> option auto (long "memory-size" <> value 0x10000 <> short 'm'       <> metavar "BYTES"  <> help "mem size")
  <*> option auto (long "hz"          <> value 985248  <> short 'h'       <> metavar "Hz"     <> help "Hz")
  <*> option auto (long "interval"    <> value 30      <> short 'i'       <> metavar "Hz"     <> help "Hz (show)")
  <*> switch      (long "debug"                        <> short 'd'                           <> help "debug")
  <*> switch      (long "graphics"                     <> short 'g'                           <> help "graphics")
