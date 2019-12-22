{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Hardware.Video.Chars
  ( loadFont
  ) where

import Data.Bifunctor
import Data.Char
import Data.Word

import qualified Data.ByteString                 as BS
import qualified Data.Map.Strict                 as M
import qualified Data.Vector.Storable            as DVS
import qualified Data.Vector.Storable.ByteString as DVSB

loadFont :: FilePath -> IO (M.Map Char [Word8])
loadFont fp = do
  bs <- BS.readFile fp
  return $ parseFont bs

-- https://asawicki.info/Download/Misc/Atari%20Font%20FNT%20File%20Format.txt
-- 0x000 == 0x20
-- 0x100 == 0x40
-- 0x200 == 0x60
parseFont :: BS.ByteString -> M.Map Char [Word8]
parseFont bs = M.fromList (first chr <$> zip [0x20..] bmps)
  where
    ws :: DVS.Vector Word8 = DVSB.byteStringToVector bs
    bmps = DVS.toList . (\i -> DVS.slice (i * 8) 8 ws) <$> [0..127]
