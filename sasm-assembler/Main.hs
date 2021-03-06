{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

import ASM.Assembler
import CPU.Disassembler

import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import GHC.Generics
import Options.Applicative
import System.IO
import Text.Printf

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T

data SASM = SASM
  { inputFile  :: FilePath
  , outputFile :: FilePath
  , codeLoc    :: Int
  , dataLoc    :: Int
  , debug      :: Bool
  } deriving Generic

main :: IO ()
main = do
  opt <- execParser sasmInfo
  let verbose = opt ^. the @"debug"
  let out     = opt ^. the @"outputFile"

  t <- readFile (opt ^. the @"inputFile")

  prog <- assemble
            (T.pack t)
            (fromIntegral $ opt ^. the @"codeLoc")
            (fromIntegral $ opt ^. the @"dataLoc")

  handle <- openBinaryFile out WriteMode
  LBS.hPut handle $ writeProgram prog
  hClose handle

  when verbose $ print prog
  let !(cdat, ddat) = disasm prog
  when verbose $ putStrLn $ foldMap (++ "\n") ((\(o, s) -> printf "%04x: " o <> T.unpack s) <$> cdat)
  when verbose $ putStrLn $ foldMap (++ "\n") ((\(o, s) -> printf "%04x: " o <> T.unpack s) <$> ddat)

sasmInfo :: ParserInfo SASM
sasmInfo = info (sasmOpts <**> helper) (fullDesc <> progDesc "compile 6502 program" <> header "sasm")

sasmOpts :: Parser SASM
sasmOpts = SASM
  <$> strOption   (long "input-file"  <> short 'f' <> metavar "FILE"                    <> help "file to assemble")
  <*> strOption   (long "output-file" <> short 'o' <> metavar "FILE"   <> value "a.out" <> help "target file")
  <*> option auto (long "code"        <> short 'c' <> metavar "OFFSET" <> value 0x200   <> help "code location")
  <*> option auto (long "data"        <> short 'd' <> metavar "OFFSET" <> value 0       <> help "data location")
  <*> switch      (long "debug"       <> short 'v' <> help "debug")
