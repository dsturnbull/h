{-# LANGUAGE BinaryLiterals #-}

module CPU.Hardware.Video.Chars
  ( charMap
  ) where

import Data.Map.Strict
import Data.Word

charMap :: Map Char [Word8]
charMap = fromList [ ('a', spriteA)
                   ]

spriteA :: [Word8]
spriteA = [ 0b00000000
          , 0b00000000
          , 0b00000000
          , 0b00111110
          , 0b00000011
          , 0b00111111
          , 0b01000011
          , 0b00111111 ]
