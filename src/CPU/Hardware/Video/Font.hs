{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Hardware.Video.Font
  ( Font(..)
  , mkFont
  )
  where

import Data.Bifunctor
import Data.Char
import Data.Word
import SDL

import qualified Data.ByteString                 as BS
import qualified Data.Map.Strict                 as M
import qualified Data.Vector.Storable            as DVS
import qualified Data.Vector.Storable.ByteString as DVSB

newtype Font = Font
  { chars :: M.Map Char (Texture, [Word8])
  }

mkFont :: Renderer -> IO Font
mkFont r = do
    font <- loadFont "fonts/ISKRA1.FNT"
    ts <- traverse charTexture (M.toList font)
    return $ Font $ M.fromList ts
  where
    charTexture :: (Char, [Word8]) -> IO (Char, (Texture, [Word8]))
    charTexture (c, bin) = do
      t <- createTexture r ARGB8888 TextureAccessTarget (V2 8 8)
      return (c, (t, bin))

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
