{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Hardware.Video.Chars
  ( drawChars
  ) where

import CPU
import CPU.Hardware.Video.Font
import CPU.Hardware.Video.Sprites
import CPU.Hardware.Video.VIC

import Control.Lens
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Vector.Storable ((!))
import Data.Word
import Foreign.C.Types
import SDL

import qualified Data.Map.Strict      as M
import qualified Data.Vector.Storable as DVS

drawChars :: VIC -> CPU -> IO ()
drawChars vic@VIC {..} cpu = traverse_ (uncurry printRow) rows
  where rows     = getRow <$> [0 .. screenRows - 1]
        printRow :: CInt -> DVS.Vector Word8 -> IO ()
        printRow r cols =
          void . sequence $ zip [0..] (DVS.toList cols) <&>
            \(c, w) -> vic & drawChar r c w (getColour r c)
        getRow :: Int -> (CInt, DVS.Vector Word8)
        getRow r = (fromIntegral r, DVS.slice (fromIntegral screenV + r * screenCols) screenCols (cpu & mem))
        getColour :: CInt -> CInt -> Word8
        getColour r c = (cpu & mem) ! (fromIntegral colourV + fromIntegral r * screenCols + fromIntegral c)

drawChar :: CInt -> CInt -> Word8 -> Word8 -> VIC -> IO ()
drawChar r c w col VIC {..} =
  case (font & chars) M.!? chr (fromIntegral w) of
    Just f@(t, _)  -> do
      updateSpriteColour f pitch col Colour
      copy renderer t Nothing (pure $ Rectangle (P (V2 ((c + 4) * 8 * scale) ((r + 4) * 8 * scale))) (V2 (8 * scale) (8 * scale)))
    Nothing -> return ()

pitch :: Int
pitch = 8
