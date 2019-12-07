{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

import ASM.Assembler
import CPU
import CPU.Run

import Control.Exception
import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import GHC.Generics
import Options.Applicative

import qualified Data.ByteString                 as BS
import qualified Data.Vector.Storable            as DVS
import qualified Data.Vector.Storable.ByteString as DVSB

data Run = Run
  { inputFile :: FilePath
  , memory    :: Int
  , delay     :: Int
  , debug     :: Bool
  } deriving Generic

main :: IO ()
main = do
  opts <- execParser sasmInfo
  let verbose = opts ^. the @"debug"
  let memSize = opts ^. the @"memory"
  let sleep   = opts ^. the @"delay"

  b <- BS.readFile (opts ^. the @"inputFile")
  let prog = Program (DVSB.byteStringToVector b)
  when verbose $ print prog

  let cpu = load 0 prog $ mkCPU (DVS.replicate memSize 0)
  _ <- if verbose
         then runShow sleep cpu
         else evaluate $ run cpu

  return ()

sasmInfo :: ParserInfo Run
sasmInfo = info (sasmOpts <**> helper) (fullDesc <> progDesc "compile 6502 program" <> header "sasm")

sasmOpts :: Parser Run
sasmOpts = Run
  <$> strOption   (long "input-file"  <> short 'f' <> metavar "FILE"  <> help "file to assemble")
  <*> option auto (long "memory-size" <> short 'm' <> metavar "BYTES" <> help "mem size")
  <*> option auto (long "delay"       <> short 's' <> metavar "µsec"  <> help "sleep between steps")
  <*> switch      (long "debug"       <> short 'd' <> help "debug")
