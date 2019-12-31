{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module CPU.Hardware.Video.VIC
  ( VIC(..)
  , mkVIC
  , screenRows
  , screenCols
  , vicBankV
  , border
  ) where

import CPU.Hardware.Video.Font
import CPU.Hardware.Video.Sprite

import Control.Concurrent.STM
import Data.Word
import Foreign.C.Types
import SDL

import qualified Data.Map.Strict as M

data VIC = VIC
  { renderer :: Renderer
  , scale    :: CInt
  , width    :: CInt
  , height   :: CInt
  , font     :: Font
  , sprites  :: M.Map Word8 (Texture, [Word8])
  , held     :: TMVar ()
  }

mkVIC :: IO VIC
mkVIC = do
  let w'             = (fromIntegral screenCols + 8) * 8
  let h'             = (fromIntegral screenRows + 8) * 8
  let scale          = 2
  window            <- createWindow "h" (defaultWindow { windowInitialSize = V2 (w' * scale) (h' * scale) })
  renderer          <- createRenderer window (-1) defaultRenderer
  (V2 width height) <- glGetDrawableSize window
  font              <- mkFont renderer
  held              <- newTMVarIO ()
  sprites           <- M.fromList <$> traverse (mkEmptySprite renderer) [0..7]
  return $ VIC {..}

screenRows :: Int
screenRows = 25

screenCols :: Int
screenCols = 40

border :: Int
border = 4

vicBankV :: Word16
vicBankV = 0x2000
