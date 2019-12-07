{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

import ASM.Assembler
import ASM.Parser

import Control.Lens
import Control.Monad
import Data.Either
import Data.Generics.Product.Any
import Data.Word
import GHC.Generics
import Options.Applicative
import System.IO

import qualified Data.ByteString                 as BS
import qualified Data.Vector.Storable            as DVS
import qualified Data.Vector.Storable.ByteString as DVSB

data SASM = SASM
  { inputFile  :: FilePath
  , outputFile :: FilePath
  , debug      :: Bool
  } deriving Generic

main :: IO ()
main = do
  opts <- execParser sasmInfo
  let verbose = opts ^. the @"debug"
  let out     = opts ^. the @"outputFile"

  t <- readFile (opts ^. the @"inputFile")
  let instructions = parseAssembly t

  let ws :: [Word8] = join $ asm <$> fromRight [] instructions
  let prog = Program (DVS.fromList ws)

  h <- openBinaryFile out WriteMode
  let bs = DVSB.vectorToByteString (prog ^. the @"program")
  BS.hPut h bs
  hClose h

  when verbose $ print instructions
  when verbose $ print prog
  putStrLn $ "wrote " <> show (BS.length bs) <> " bytes to " <> out

  return ()

sasmInfo :: ParserInfo SASM
sasmInfo = info (sasmOpts <**> helper) (fullDesc <> progDesc "compile 6502 program" <> header "sasm")

sasmOpts :: Parser SASM
sasmOpts = SASM
  <$> strOption   (long "input-file"  <> short 'f' <> metavar "FILE" <> help "file to assemble")
  <*> strOption   (long "output-file" <> short 'o' <> metavar "FILE" <> value "a.out" <> help "target file")
  <*> switch      (long "debug"       <> short 'd' <> help "debug")
