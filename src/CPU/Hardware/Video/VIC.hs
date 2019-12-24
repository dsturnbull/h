module CPU.Hardware.Video.VIC
  ( VIC(..)
  , screenRows
  , screenCols
  ) where

import CPU
import CPU.Hardware.Video.Font

import Control.Concurrent.STM
import Foreign.C.Types
import SDL

data VIC = VIC
  { renderer :: Renderer
  , scale    :: CInt
  , width    :: CInt
  , height   :: CInt
  , font     :: Font
  , cpuSTM   :: TVar CPU
  , held     :: TMVar ()
  }

screenRows :: Int
screenRows = 25

screenCols :: Int
screenCols = 40
