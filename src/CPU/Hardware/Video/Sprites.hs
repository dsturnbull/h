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
drawSprites VIC {..} cpu = do
  let spr0p = (*64) $ w16 $ DVS.slice (fromIntegral (spritePointerV + 0)) 2 (cpu & mem)
  let spr0c = (cpu & mem) ! fromIntegral (vicV + 0x27 + 0)
  let spr0x = fromIntegral $ (cpu & mem) ! fromIntegral (vicV + 0)
  let spr0y = fromIntegral $ (cpu & mem) ! fromIntegral (vicV + 1)
  case sprites M.!? 0 of
    Just (t, _) -> do
      let bin = loadSprite spr0p cpu
      updateSpriteColour (t, bin) pitch spr0c Transparent
      copy renderer t Nothing (pure $ Rectangle (P (V2 (spr0x * scale) (spr0y * scale))) (V2 (24 * scale) (21 * scale)))
    Nothing -> return ()

loadSprite :: Word16 -> CPU -> [Word8]
loadSprite addr cpu = DVS.toList $ DVS.slice (fromIntegral addr) 63 (cpu & mem)

w16 :: DVS.Vector Word8 -> Word16
w16 v = (addrH `shiftL` 8) .|. addrL
  where addrL = fromIntegral (v ! 0)
        addrH = fromIntegral (v ! 1)

pitch :: Int
pitch = 24
