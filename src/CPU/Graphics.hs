module CPU.Graphics where

import CPU

import Control.Lens
import Data.Bits
import Data.Vector.Storable
import Data.Word

import qualified Data.Vector.Storable as DVS

pixelData :: Int -> Int -> Int -> CPU -> [(Int, Int)]
pixelData rows start len cpu = pixelData' rows gfx []
  where gfx = DVS.slice start len (cpu & mem)

pixelData' :: Int -> Vector Word8 -> [(Int, Int)] -> [(Int, Int)]
pixelData' r m = go 0
  where go n = if n < DVS.length m
          then word8ToPixels r n (m ! n) . go (n + 1)
          else id

word8ToPixels :: Int -> Int -> Word8 -> [(Int, Int)] -> [(Int, Int)]
word8ToPixels _ _ 0 = id
word8ToPixels r o w = (v:) . word8ToPixels r o (w .&. complement b)
  where pos  = countTrailingZeros w
        hi   = o `shiftL` 3
        v    = (i `div` r, i `mod` r)
        i    = (pos .|. hi)
        b    = 1 `shiftL` fromIntegral pos
