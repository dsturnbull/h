{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Hardware.Video.Chars
  ( loadFont
  , mkFont
  , updateCharColour
  , Font(..)
  , mkARGB8888
  ) where

import Control.Lens
import Control.Monad
import Data.Bifunctor
import Data.Bits
import Data.Bits.Lens
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

mkSprite :: Word8 -> [Word8] -> BS.ByteString
mkSprite c ws = BS.pack (bs =<< ws)
  where bs w = mkARGB8888 (lo c) (hi c) =<< reverse (toListOf bits w)
        lo n = n .&. 0b00001111
        hi n = n `shiftR` 4

-- little endian so bgra
mkARGB8888 :: Word8 -> Word8 -> Bool -> [Word8]
mkARGB8888 fg _ True  = colour fg
mkARGB8888 _ bg False = colour bg

colour :: Word8 -> [Word8]
colour 0b0000 = [0x00, 0x00, 0x00, 0x00] -- black
colour 0b0001 = [0xff, 0xff, 0xff, 0x00] -- white
colour 0b0010 = [0x0d, 0x16, 0x7d, 0x00] -- red
colour 0b0011 = [0xee, 0xfd, 0xbd, 0x00] -- cyan
colour 0b0100 = [0xc6, 0x51, 0xbd, 0x00] -- violet
colour 0b0101 = [0x64, 0xc8, 0x5c, 0x00] -- green
colour 0b0110 = [0xa3, 0x10, 0x00, 0x00] -- blue
colour 0b0111 = [0x88, 0xed, 0xee, 0x00] -- yellow
colour 0b1000 = [0x53, 0x8b, 0xd2, 0x00] -- orange
colour 0b1001 = [0x13, 0x45, 0x61, 0x00] -- brown
colour 0b1010 = [0x7b, 0x7f, 0xef, 0x00] -- light red
colour 0b1011 = [0x1e, 0x1e, 0x1e, 0x00] -- grey 1
colour 0b1100 = [0x77, 0x77, 0x77, 0x00] -- grey 2
colour 0b1101 = [0x7c, 0xfc, 0xbd, 0x00] -- light green
colour 0b1110 = [0xf7, 0x88, 0x37, 0x00] -- light blue
colour 0b1111 = [0xbb, 0xbb, 0xbb, 0x00] -- grey 3
colour _      = [0x00, 0x00, 0x00, 0x00] -- ???

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

updateCharColour :: (Texture, [Word8]) -> Word8 -> IO ()
updateCharColour (t, bin) c =
  void $ updateTexture t Nothing (mkSprite c bin) (8 * 4)
