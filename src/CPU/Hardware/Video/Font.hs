module CPU.Hardware.Video.Font
  ( Font(..)
  )
  where

import Data.Word
import SDL

import qualified Data.Map.Strict as M

newtype Font = Font
  { chars :: M.Map Char (Texture, [Word8])
  }
