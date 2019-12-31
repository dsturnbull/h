{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CPU.Hardware.Timer
  ( updateTimers
  )
  where

import CPU                           (CPU (mem), h, l, st, timerAV)
import CPU.Hardware.Interrupt
import CPU.Hardware.Timer.JiffyTimer

import Control.Lens                 hiding (set)
import Data.Bits
import Data.Bits.Lens
import Data.Fixed
import Data.Generics.Product.Fields
import Data.Time.Clock
import Data.Vector.Storable         (slice, (!))
import Data.Word

import qualified Data.Vector.Storable as DVS

{-
                   control
                  00000011
                        ||
                        |+- enable
                        +-- enabled
-}

updateTimers :: CPU -> IO CPU
updateTimers =
  updateTimer timerAV (field @"timerA") (field @"timerA")

updateTimer :: Word16 -> ASetter' CPU JiffyTimer -> Getting JiffyTimer CPU JiffyTimer -> CPU -> IO CPU
updateTimer v set get cpu = do
  (now, dt') <- timeDelta $ cpu ^. get & clock
  if | ctrl & enable  ->                    return $ cpu & st v (ctrl & clearEnable & setEnabled) & updateClock now set
     | ctrl & enabled -> if | timer == 0 -> return $ cpu & st v (ctrl & clearEnabled)
                                                         & st (fromIntegral (v + 3)) 1
                                                         & intr
                            | otherwise  -> return $ cpu & st (v + 1) (l $ timer' dt')
                                                         & st (v + 2) (h $ timer' dt')
                                                         & if update dt' then updateClock now set else id
     | otherwise      -> return cpu
  where ctrl           = (cpu & mem) ! fromIntegral v
        enable       c = c ^. bitAt 0
        enabled      c = c ^. bitAt 1
        clearEnable  c = c & bitAt 0 .~ False
        clearEnabled c = c & bitAt 1 .~ False
        setEnabled   c = c & bitAt 1 .~ True
        timer          = w16 $ slice (fromIntegral (v + 1)) 2 (cpu & mem)
        timer' dt'     = if update dt' then timer - 1 else timer
        update dt'     = dt' >= jiffy
        jiffy          = 1 / 60

updateClock :: UTCTime -> ASetter' CPU JiffyTimer -> CPU -> CPU
updateClock now set cpu =
   cpu & set . field @"clock" .~ now

timeDelta :: UTCTime -> IO (UTCTime, Double)
timeDelta before = do
  now <- getCurrentTime
  let diff :: Double = fromInteger . fromPico . nominalDiffTimeToSeconds $ diffUTCTime now before
  return (now, diff / 1000 / 1000 / 1000 / 1000)

  where fromPico :: Pico -> Integer
        fromPico (MkFixed i) = i

w16 :: DVS.Vector Word8 -> Word16
w16 v = (addrH `shiftL` 8) .|. addrL
  where addrL = fromIntegral (v ! 0)
        addrH = fromIntegral (v ! 1)
