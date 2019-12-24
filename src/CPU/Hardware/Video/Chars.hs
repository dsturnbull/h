{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Hardware.Video.Chars
  ( loadFont
  , mkFont
  , updateCharColour
  , mkARGB8888
  , drawChars
  ) where

import CPU
import CPU.Hardware.Video.Font
import CPU.Hardware.Video.VIC

import Control.Lens
import Control.Monad
import Data.Bifunctor
import Data.Bits
import Data.Bits.Lens
import Data.Char
import Data.Foldable
import Data.Vector.Storable ((!))
import Data.Word
import Foreign.C.Types
import SDL

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
colour 0b0010 = [0x43, 0x4e, 0x89, 0x00] -- red
colour 0b0011 = [0xcb, 0xc3, 0x92, 0x00] -- cyan
colour 0b0100 = [0xb1, 0x57, 0x8a, 0x00] -- violet
colour 0b0101 = [0x59, 0xae, 0x80, 0x00] -- green
colour 0b0110 = [0xa4, 0x3e, 0x45, 0x00] -- blue
colour 0b0111 = [0x89, 0xde, 0xd7, 0x00] -- yellow
colour 0b1000 = [0x38, 0x6a, 0x92, 0x00] -- orange
colour 0b1001 = [0x17, 0x52, 0x64, 0x00] -- brown
colour 0b1010 = [0x7a, 0x84, 0xb8, 0x00] -- light red
colour 0b1011 = [0x60, 0x60, 0x60, 0x00] -- grey 1
colour 0b1100 = [0x8a, 0x8a, 0x8a, 0x00] -- grey 2
colour 0b1101 = [0x99, 0xe6, 0xbd, 0x00] -- light green
colour 0b1110 = [0xd8, 0x7c, 0x83, 0x00] -- light blue
colour 0b1111 = [0xb3, 0xb3, 0xb3, 0x00] -- grey 3
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

drawChars :: VIC -> CPU -> IO ()
drawChars VIC {..} cpu = traverse_ (uncurry printRow) rows
  where rows     = getRow <$> [0 .. screenRows - 1]
        printRow :: CInt -> DVS.Vector Word8 -> IO ()
        printRow r cols =
          void . sequence $ zip [0..] (DVS.toList cols) <&>
            \(c, w) ->
              case (font & chars) M.!? chr (fromIntegral w) of
                Just f@(t, _)  -> do
                  updateCharColour f (getColour r c)
                  copy renderer t Nothing (pure $ Rectangle (P (V2 ((c + 4) * 8 * scale) ((r + 4) * 8 * scale))) (V2 (8 * scale) (8 * scale)))
                Nothing -> return ()
        getRow :: Int -> (CInt, DVS.Vector Word8)
        getRow r = (fromIntegral r, DVS.slice (fromIntegral screenV + r * screenCols) screenCols (cpu & mem))
        getColour :: CInt -> CInt -> Word8
        getColour r c = (cpu & mem) ! (fromIntegral colourV + fromIntegral r * screenCols + fromIntegral c)
