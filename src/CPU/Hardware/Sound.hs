{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CPU.Hardware.Sound
  ( initSound
  , updateSound
  )
  where

import CPU

import Bindings.PortAudio
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Generics.Product.Fields
import Foreign                      hiding (void)
import Foreign.C.Types
import Linear                       (V2 (..))
import System.PortAudio

import qualified Data.Vector.Storable          as DVS
import qualified Data.Vector.Storable.Internal as DVSI

{-
                   control
                  00000011
                        ||
                        |+- enable
                        +-- enabled
-}

  -- if | ctrl & enable  ->                    return $ cpu & st timerA (ctrl & clearEnable & setEnabled)
  --    | ctrl & enabled -> if | timer == 0 -> return $ cpu & st timerA (ctrl & clearEnabled)
  --                                                        & intr
  --                           | otherwise  -> return $ cpu & st (timerA + 1) (l timer')
  --                                                        & st (timerA + 2) (h timer')
  --    | otherwise      -> return cpu
  -- where ctrl           = (cpu & mem) ! fromIntegral timerA
  --       enable       c = c ^. bitAt 0
  --       enabled      c = c ^. bitAt 1
  --       clearEnable  c = c & bitAt 0 .~ False
  --       clearEnabled c = c & bitAt 1 .~ False
  --       setEnabled   c = c & bitAt 1 .~ True
  --       timer          = w16 $ slice (fromIntegral (timerA + 1)) 2 (cpu & mem)
  --       timer'         = timer - 1

initSound :: CPU -> IO CPU
initSound cpu = do
  ps <- malloc
  w c'Pa_Initialize
  (_, _ : dev : []) <- getDevices
  let output :: Maybe (StreamParameters Output (V2 Float)) = streamParameters dev 0

  withMaybe noConnection $ \pin -> withMaybe output $ \pout -> do
    w $ c'Pa_OpenStream ps
        (castPtr pin)
        (castPtr pout)
        (CDouble rate)
        framesPerBuffer
        0
        nullFunPtr
        nullPtr

  stream <- peek ps
  return $ cpu & field @"audio" ?~ stream

updateSound :: CPU -> IO CPU
updateSound cpu =
  case cpu & audio of
    Just str -> do
      w $ c'Pa_StartStream str
      let per = 32
      let t = table per
      print t
      let t' = castPtr . fst $ vectorToPtr0 t

      forM_ [0 .. rate / fromIntegral framesPerBuffer] $
        const . w $ c'Pa_WriteStream str t' framesPerBuffer

      w $ c'Pa_StopStream str

      return $ cpu & field @"audio" .~ Nothing

    Nothing -> return cpu

rate :: Double
rate = 48000

framesPerBuffer :: CULong
framesPerBuffer = 100

table :: Int -> DVS.Vector Float
table per = DVS.fromList [sin t | i <- [0..per - 1], let t = fromIntegral i / fromIntegral per * 2 * pi]
-- table per = DVS.fromList [fromIntegral (i `mod` 2) | i <- [0.. per - 1]]

vectorToPtr0 :: DVS.Storable a => DVS.Vector a -> (Ptr a,Int)
vectorToPtr0 vector =
 let (foreignPtr,size) = DVS.unsafeToForeignPtr0 vector
 in (DVSI.getPtr foreignPtr,size)

w :: IO CInt -> IO ()
w n = do
  r <- n
  unless (r == 0) $ throwIO $ fromErrorCode r

fromErrorCode :: CInt -> Error
fromErrorCode n = toEnum (fromIntegral n + 10000)

withMaybe :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybe Nothing c  = c nullPtr
withMaybe (Just a) c = with a c
