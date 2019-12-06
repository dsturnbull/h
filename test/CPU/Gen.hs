module CPU.Gen
  ( genCPU
  , genCPURandom
  ) where

import CPU

import Data.Word
import Hedgehog
import Hedgehog.Gen   as G
import Hedgehog.Range as R

import qualified Data.Vector.Storable as DVS

w8 :: MonadGen m => m Word8
w8 = word8 R.linearBounded

genCPU :: MonadGen m => Word16 -> m CPU
genCPU memSize = return $ mkCPU (DVS.replicate (fromIntegral memSize) 0)

genCPURandom :: MonadGen m => Word16 -> m CPU
genCPURandom memSize = do
  bs <- G.list (R.constant (fromIntegral memSize) (fromIntegral memSize)) w8
  return $ mkCPU (DVS.fromList bs)
