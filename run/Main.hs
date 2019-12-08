{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

import CPU
import CPU.Graphics
import CPU.Hardware.Terminal
import CPU.Hardware.Timer
import CPU.Program
import CPU.Run

import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.Generics.Product.Fields
import GHC.Generics
import Graphics.Proc
import Options.Applicative
import Prelude                      hiding (break)
import System.Posix.IO
import System.Posix.Terminal
import System.Posix.Types           (Fd)

import qualified Data.ByteString                 as BS
import qualified Data.Vector.Storable            as DVS
import qualified Data.Vector.Storable.ByteString as DVSB

data Run = Run
  { inputFile :: FilePath
  , memory    :: Int
  , hz        :: Int
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
  -- *$0380 -- timer a control
  -- *$0381-$0382 -- timer a
  --  $0300 - 0x11a0 bitmap
  --  ????? - colour

  let cpu = load 0 prog $ mkCPU (DVS.replicate (opt ^. field @"memory") 0)

  (mfd, _) <- liftIO openPseudoTerminal
  liftIO $ setFdOption mfd NonBlockingRead True

  tty <- getSlaveTerminalName mfd
  putStrLn tty

  return (cpu & field @"ttyName" ?~ tty) >>= \cpu' -> do
    if opt ^. field @"graphics"
      then
        runProc $ def
          { procSetup      = setup cpu'
          , procDraw       = draw
          , procUpdateTime = update verbose
          , procKeyPressed = keyPressed
          }
      else
          void $ runShow verbose (opt ^. field @"hz") mfd cpu'

runShow :: Bool -> Int -> Fd -> CPU -> IO CPU
runShow verbose h tty cpu =
  if cpu & p & break
    then return cpu
    else do
      simulateTime h (step cpu)
        >>= (\cpu' -> when verbose (print cpu') >> return cpu')
        >>= readKbd tty
        >>= writeOutput tty
        >>= updateTimers
        >>= runShow verbose h tty

simulateTime :: Int -> CPU -> IO CPU
simulateTime h cpu = do
  threadDelay (ceiling (fromIntegral (cpu & tim) * cyf h))
  return cpu

cyf :: Int -> Double
cyf h = 1e6 / fromIntegral h

width :: Int
width = virtWidth * fst virtScale

height :: Int
height = virtHeight * snd virtScale

virtWidth :: Int
virtWidth  = 200

virtHeight :: Int
virtHeight = 160

virtScale :: (Int, Int)
virtScale = (4, 4)

setup :: CPU -> Pio CPU
setup cpu = do
              size (fromIntegral width, fromIntegral height)
              return cpu

draw :: CPU -> Pio ()
draw cpu = do
  -- scale virtScale
  background (grey 0)
  let c = orange
  stroke c
  let pixels = pixelData virtHeight 0x200 0xfa0 cpu
  -- n <- millis
  -- let pixels = [(x, y) | x <- [n `mod` virtHeight .. ], y <- [0..]]
  void $
    traverse ((\(x,y) ->
                rect (fromIntegral x * fromIntegral (fst virtScale), fromIntegral y * fromIntegral (snd virtScale))
                (fromIntegral (fst virtScale), fromIntegral (snd virtScale)) >> fill c))
      -- (take virtWidth pixels)
      pixels

update :: Bool -> TimeInterval -> CPU -> Pio CPU
update verbose t cpu = do
  liftIO $ print t
  let cpu' = step cpu
  when verbose $
    liftIO $ print cpu'
  return $ cpu'

keyPressed :: CPU -> Pio CPU
keyPressed cpu = do
  k <- key
  liftIO . print $ k
  return cpu

sasmInfo :: ParserInfo Run
sasmInfo = info (sasmOpts <**> helper) (fullDesc <> progDesc "compile 6502 program" <> header "sasm")

sasmOpts :: Parser Run
sasmOpts = Run
  <$> strOption   (long "input-file"                  <> short 'f'       <> metavar "FILE"  <> help "file to assemble")
  <*> option auto (long "memory-size"                 <> short 'm'       <> metavar "BYTES" <> help "mem size")
  <*> option auto (long "hz"          <> value 985248 <> short 'h'       <> metavar "Hz"    <> help "Hz")
  <*> switch      (long "debug"                       <> short 'd'                          <> help "debug")
  <*> switch      (long "graphics"                    <> short 'g'                          <> help "graphics")
