{-# LANGUAGE BinaryLiterals  #-}
{-# LANGUAGE RecordWildCards #-}

module CPU.Hardware.Video.Sprites
  ( drawSprites
  , mkSprite
  , colour
  , updateSpriteColour
  , mkARGB8888
  , BackgroundMode(..)
  ) where

import CPU
import CPU.Hardware.Video.VIC

import Control.Lens
import Control.Monad
import Data.Bits
import Data.Bits.Lens
import Data.Foldable
import Data.Vector.Storable ((!))
import Data.Word
import SDL

import qualified Data.ByteString      as BS
import qualified Data.Map.Strict      as M
import qualified Data.Vector.Storable as DVS

data BackgroundMode = Transparent | Colour deriving (Eq)

mkSprite :: Word8 -> [Word8] -> BackgroundMode -> [Word8]
mkSprite c ws bgmode = bs =<< ws
  where bs w = mkARGB8888 (lo c) (hi c) bgmode =<< reverse (toListOf bits w)
        lo n = n .&. 0b00001111
        hi n = if bgmode == Colour then n `shiftR` 4 else 0xff

updateSpriteColour :: (Texture, [Word8]) -> Int -> Word8 -> BackgroundMode -> IO ()
updateSpriteColour (t, bin) pitch' c bgmode =
  let spr = BS.pack $ mkSprite c bin bgmode
  in void $ updateTexture t Nothing spr (fromIntegral pitch' * 4)

mkSpriteExtended :: [Word8] -> [Word8] -> BackgroundMode -> [Word8]
mkSpriteExtended cs ws bgmode = uncurry bs =<< zip cs ws
  where bs c w = mkARGB8888 (lo c) (hi c) bgmode =<< reverse (toListOf bits w)
        lo n   = n .&. 0b00001111
        hi n   = if bgmode == Colour then n `shiftR` 4 else 0xff

updateSpriteColourExtended :: (Texture, [Word8]) -> Int -> [Word8] -> BackgroundMode -> IO ()
updateSpriteColourExtended (t, bin) pitch' cs bgmode =
  let spr = BS.pack $ mkSpriteExtended cs bin bgmode
  in void $ updateTexture t Nothing spr (fromIntegral pitch' * 4)

-- little endian so bgra
mkARGB8888 :: Word8 -> Word8 -> BackgroundMode -> Bool -> [Word8]
mkARGB8888 fg _ _ True       = colour fg ++ [0xff]
mkARGB8888 _ bg bgmode False = colour bg ++ addBackground bgmode

addBackground :: BackgroundMode -> [Word8]
addBackground Transparent = [0x00]
addBackground Colour      = [0xff]

colour :: Word8 -> [Word8]
colour 0b0000 = [0x00, 0x00, 0x00] -- black
colour 0b0001 = [0xff, 0xff, 0xff] -- white
colour 0b0010 = [0x43, 0x4e, 0x89] -- red
colour 0b0011 = [0xcb, 0xc3, 0x92] -- cyan
colour 0b0100 = [0xb1, 0x57, 0x8a] -- violet
colour 0b0101 = [0x59, 0xae, 0x80] -- green
colour 0b0110 = [0xa4, 0x3e, 0x45] -- blue
colour 0b0111 = [0x89, 0xde, 0xd7] -- yellow
colour 0b1000 = [0x38, 0x6a, 0x92] -- orange
colour 0b1001 = [0x17, 0x52, 0x64] -- brown
colour 0b1010 = [0x7a, 0x84, 0xb8] -- light red
colour 0b1011 = [0x60, 0x60, 0x60] -- grey 1
colour 0b1100 = [0x8a, 0x8a, 0x8a] -- grey 2
colour 0b1101 = [0x99, 0xe6, 0xbd] -- light green
colour 0b1110 = [0xd8, 0x7c, 0x83] -- light blue
colour 0b1111 = [0xb3, 0xb3, 0xb3] -- grey 3
colour _      = [0x00, 0x00, 0x00] -- ???

drawSprites :: VIC -> CPU -> IO ()
drawSprites vic cpu =
  traverse_ (drawSprite vic cpu) [0..7]

drawSprite :: VIC -> CPU -> Word16 -> IO ()
drawSprite VIC {..} cpu spr = do
  let sper = (cpu & mem) ! fromIntegral (vicV + 0x15)
  let on = sper ^. bitAt (fromIntegral spr)
  when on $ do
    let sprp = (*64) $ fromIntegral $ (cpu & mem) ! fromIntegral (spritePointerV + spr)
    let sprc = DVS.toList $ DVS.slice (fromIntegral (vicExtendedV + spr * 64)) 64 (cpu & mem)
    let sprx = fromIntegral $ (cpu & mem) ! fromIntegral (vicV + spr * 2 + 0)
    let spry = fromIntegral $ (cpu & mem) ! fromIntegral (vicV + spr * 2 + 1)
    case sprites M.!? fromIntegral spr of
      Just (t, _) -> do
        let bin = loadSprite sprp cpu
        updateSpriteColourExtended (t, bin) pitch sprc Transparent
        copy renderer t Nothing (pure $ Rectangle (P (V2 (sprx * scale) (spry * scale))) (V2 (24 * scale) (21 * scale)))
      Nothing -> return ()

loadSprite :: Word16 -> CPU -> [Word8]
loadSprite addr cpu = DVS.toList $ DVS.slice (fromIntegral addr) 63 (cpu & mem)

pitch :: Int
pitch = 24
