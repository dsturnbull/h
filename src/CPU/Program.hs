{-# LANGUAGE DeriveGeneric #-}

module CPU.Program
  ( Program(..)
  ) where

import Control.Lens
import Control.Monad
import Data.Char
import Data.Word
import GHC.Generics
import Text.Printf

import qualified Data.Vector.Storable as DVS

newtype Program = Program
  { program :: DVS.Vector Word8
  } deriving (Eq, Generic)

instance Show Program where
  show (Program prog)       = header ++ foldMap (++ "\n") (showRow <$> rows)
    where header            = printf "    : 00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f\n"
          showRow (o, row)  = printf "%04x: %s |%s|" o (showEles o row) (ascii row)
          showEles o eles   = pad o (foldMap (++ " ") (printf "%02x" <$> DVS.toList eles))
          pad i s           = s ++ if i + rowLength <= progLen then "" else join $ replicate (rowLength - progLen `mod` rowLength) "   "
          ascii eles        = foldMap (++ "") (printf "%c" . toPrintable . chr . fromIntegral <$> DVS.toList eles)
          toPrintable c     = if isPrint c then c else '.'
          rows              = (\i -> (i, crimp i)) <$> rowStarts
          rowStarts         = (* rowLength) <$> [0 .. progLen `div` rowLength]
          rowLength         = 16
          crimp i           = if i + rowLength <= progLen then sl i rowLength else sl i (progLen `mod` rowLength)
          sl i len          = prog & DVS.slice i len
          progLen           = DVS.length prog
