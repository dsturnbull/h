{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CPU.Hardware.Timer
  ( updateTimers
  )
  where

import CPU
import CPU.Hardware.Interrupt
import CPU.Instructions.Assembles
import CPU.Instructions.Decodes

import Control.Lens
import Data.Bits.Lens
import Data.Vector.Storable (slice, (!))

import qualified Data.Vector.Storable.Mutable as DVSM

{-
                   control
                  00000011
                        ||
                        |+- enable
                        +-- enabled
-}

updateTimers :: CPU -> IO CPU
updateTimers cpu = do
  if | ctrl & enable  ->                    return $ cpu & st timerA (ctrl & clearEnable & setEnabled)
     | ctrl & enabled -> if | timer == 0 -> return $ cpu & st timerA (ctrl & clearEnabled)
                                                         & intr
                            | otherwise  -> return $ cpu & st (timerA + 1) (l timer')
                                                         & st (timerA + 2) (h timer')
     | otherwise      -> return cpu
  where ctrl           = (cpu & mem) ! fromIntegral timerA
        enable       c = c ^. bitAt 0
        enabled      c = c ^. bitAt 1
        clearEnable  c = c & bitAt 0 .~ False
        clearEnabled c = c & bitAt 1 .~ False
        setEnabled   c = c & bitAt 1 .~ True
        timer          = w16 $ slice (fromIntegral (timerA + 1)) 2 (cpu & mem)
        timer'         = timer - 1
  -- allocaBytes 1 $ \ptr -> do
  --   when (c > 0) $ do
  --     poke ptr c
  --     -- void $ fdWriteBuf tty ptr 1
  --   return $ cpu
  --          & field @"mem" %~ DVS.modify (\vec -> DVSM.write vec (fromIntegral (kbd + 1)) (0 :: Word8))
  --   where c = (cpu & mem) ! fromIntegral (kbd + 1)
