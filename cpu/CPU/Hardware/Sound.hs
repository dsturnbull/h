{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

  -- ( initSound
  -- , tickSound
  -- )

module CPU.Hardware.Sound
  where

import CPU                      (CPU (audio, mem, sid), soundV)
import CPU.Hardware.Sound.SID
import CPU.Hardware.Sound.Voice
import CPU.Instructions.Decodes
import CPU.Instructions.Impl

import Bindings.PortAudio
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Suspend
import Control.Concurrent.Timer
import Control.Exception
import Control.Lens                 hiding (set)
import Control.Monad
import Data.Bits.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable         ((!))
import Foreign                      hiding (void)
import Foreign.C.Types
import Linear                       (V2 (..))
import System.IO
import System.PortAudio
import Text.Printf

import qualified Data.Vector                   as V
import qualified Data.Vector.Storable          as DVS
import qualified Data.Vector.Storable.Internal as DVSI

import qualified Synthesizer.Storable.Oscillator as Osci
import qualified Synthesizer.Storable.Signal     as SigSt

import qualified Algebra.Additive       as Additive
import qualified Algebra.RealRing       as RealRing
import qualified Algebra.Transcendental as Trans

import qualified Data.Foldable                as F
import qualified Data.Vector.Storable.Mutable as MV

{-
$0400 frequency voice 1 low byte
$0401 frequency voice 1 high byte
$0402 pulse wave duty cycle voice 1 low byte
$0403 pulse wave duty cycle voice 1 high byte (3..0)
$0404 control register voice 1
          7      6         5         4     3                             2                         1     0
      noise  pulse  sawtooth  triangle  test  ring modulation with voice 3  synchronize with voice 3  gate
$0405 attack duration (7..4) decay duration voice 1 (3..0)
$0406 sustain level (7..4) release duration voice 1 (3..0)
$0407 frequency voice 2 low byte
$0408 frequency voice 2 high byte
$0409 pulse wave duty cycle voice 2 low byte
$040a pulse wave duty cycle voice 2 high byte (3..0)
$040b control register voice 2
          7      6         5         4     3                             2                         1     0
      noise  pulse  sawtooth  triangle  test  ring modulation with voice 1  synchronize with voice 1  gate
$040c attack duration (7..4) decay duration voice 2 (3..0)
$040d sustain level (7..4) release duration voice 2 (3..0)
$040e frequency voice 3 low byte
$040f frequency voice 3 low byte
$0410 pulse wave duty cycle voice 1 low byte
$0411 pulse wave duty cycle voice 1 high byte (3..0)
$0412 control register voice 1
          7      6         5         4     3                             2                         1     0
      noise  pulse  sawtooth  triangle  test  ring modulation with voice 2  synchronize with voice 2  gate
$0413 attack duration (7..4) decay duration voice 1 (3..0)
$0414 sustain level (7..4) release duration voice 1 (3..0)
$0415 filter cutoff frequency low byte (3..0)
$0416 filter cutoff frequency high byte
$0417 filter resonance and routing
                  7..4                3        2       1       0
      filter resonance  external input  voice 3  voice 2  voice 1
$0418 filter mode and main volume control
                 7          6          5         4         3..0
      mute voice 3  high pass  band pass  low pass  main volume
$0419 paddle x value (read only)
$041a paddle y value (read only)
$041b oscillator voice 3 (read only)
$041c envelope voice 3 (read only)
$0500..$07ff sid registers mirrored
-}

period :: Int
period = 200

table :: V.Vector Float
table = V.fromList [sin t | i <- [0..period - 1], let t = fromIntegral i / fromIntegral period * 2 * pi]

callback :: MVar Int -> Status -> input -> MV.IOVector (V2 Float) -> IO StreamCallbackResult
callback phase _ _ o = do
  i0 <- takeMVar phase
  go i0 0
  putMVar phase $ i0 + n
  return Continue
  where
    n = MV.length o
    go :: Int -> Int -> IO ()
    go i0 i
      | i == n = return ()
      | otherwise = do
        let v = table V.! ((i0 + i) `mod` period)
        MV.write o i (V2 v v)
        go i0 (i + 1)

initSound :: CPU -> IO CPU
initSound cpu = do
  ps <- malloc
  we c'Pa_Initialize
  (_, dev : _) <- getDevices

  let output :: Maybe (StreamParameters Output (V2 Float)) = streamParameters dev 0

  -- phase <- newMVar 0
  -- withStream 44100 1024 noConnection output mempty (callback phase) $
  --   threadDelay 10000000

  withMaybe noConnection $ \pin -> withMaybe output $ \pout ->
    we $ c'Pa_OpenStream ps
      (castPtr pin)
      (castPtr pout)
      (CDouble (fromInteger . fromIntegral $ rate))
      0
      0
      nullFunPtr
      nullPtr

  stream <- peek ps
  we $ c'Pa_StartStream stream
  return $ cpu & field @"audio" ?~ stream
  -- return cpu

tickSound :: CPU -> IO CPU
tickSound cpu =
  case cpu & audio of
    Just str -> do
      let cpu' = cpu & field @"sid" . field @"volume" .~ vol
      cpu' &   updateVoice v1 (field @"voice1") (field @"voice1")
           >>= updateVoice v2 (field @"voice2") (field @"voice2")
           >>= updateVoice v3 (field @"voice3") (field @"voice3")

    Nothing -> return cpu

  where vol = (cpu & mem) ! fromIntegral (soundV & volumeControl) .&. 0b00000111

-- gate on -> start attack
-- gate on -> attack finished -> decay
-- gate on -> decay finished -> sustain
-- gate off -> start release
-- gate off -> release finished

updateVoice :: (Word16 -> Word16) -> ASetter' SID Voice -> Getting Voice SID Voice -> CPU -> IO CPU
updateVoice v set get cpu = do
  let voice   = cpu ^. field @"sid" . get -- previous state
  let rising  = not (gate voice) && gate' vctrl -- gate off -> gate on
  let high    = gate' vctrl
  let falling = not $ gate' vctrl -- newly off

  let attacking = high
  let decaying  = high && voice ^. field @"attack" == 0
  let releasing = not high

  let cpu' = cpu & field @"sid" . set %~ \vc ->
                vc & field @"gate"      .~ gate'    vctrl
                   & field @"wave"      .~ wave'    vctrl
                   & field @"freqW"     .~ freqW'
                   & field @"freq"      .~ freq'
                   & field @"attackW"   .~ attack'  vad
                   & field @"decayW"    .~ decay'   vad
                   & field @"sustainW"  .~ sustain' vsr
                   & field @"releaseW"  .~ release' vsr
                   & field @"attack"    %~ (\a -> if rising then attackTable  (attack'  vad) else if attacking then max 0 (a - dt') else a)
                   & field @"decay"     %~ (\a -> if rising then drTable      (decay'   vad) else if decaying  then max 0 (a - dt') else a)
                   & field @"release"   %~ (\a -> if rising then drTable      (release' vsr) else if releasing then max 0 (a - dt') else a)
                   & field @"sustain"   %~ (\a -> fromIntegral (sustain' vsr)) -- can it be changed?

  {-
  sample rate: 4000/sec
  loop freq: 4000/sec (maybe)
  frames per buffer: 0 (variable)
  mono
  -}

  -- let Just str = cpu & audio
  -- av <- c'Pa_GetStreamWriteAvailable str

  -- let voice' = cpu' ^. field @"sid" . get
  -- let attackMultiplier = 1 - ((voice' & attack) / (voice' & attackW & attackTable))
  -- let vfreq = voice' & freq
  -- let dur = ceiling $ (voice' & attack) * (fromInteger . fromIntegral $ rate) -- * fromInteger ts
  -- if dur > 0
  --   then do
  --     let w = SigSt.take dur
  --               (Osci.staticSine (SigSt.chunkSize dur) Additive.zero vfreq)
  --     let vc = DVS.fromList . SigSt.toList $ w
  --     let t = castPtr . fst $ vectorToPtr0 vc
  --     we $ c'Pa_WriteStream str t (fromIntegral av)
  --   else do

  -- let t = castPtr . fst $ vectorToPtr0 silence
  -- we $ c'Pa_WriteStream str t (fromIntegral av)

  return cpu'

  where dt'     = (cpu & sid & dt) / 1000 / 1000 / 1000 / 1000
        vctrl   = (cpu & mem) ! fromIntegral (soundV & v & voiceControl)
        vad     = (cpu & mem) ! fromIntegral (soundV & v & voiceAD)
        vsr     = (cpu & mem) ! fromIntegral (soundV & v & voiceSR)
        freqW'  = w16 $ DVS.slice (fromIntegral (soundV & v & voiceFreqH)) 2 (cpu & mem)
        freq'   = (fromInteger . fromIntegral $ freqW') / sidFreq
        gate' b = b ^. bitAt 0
        wave' b = if | b ^. bitAt 7 -> Noise
                     | b ^. bitAt 6 -> Pulse
                     | b ^. bitAt 5 -> Sawtooth
                     | b ^. bitAt 4 -> Triangle
                     | otherwise    -> (cpu ^. field @"sid" . get) & wave
        attack'  w = (w .&. 0b11110000) `shiftR` 4
        decay'   w =  w .&. 0b00001111
        sustain' w = (w .&. 0b11110000) `shiftR` 4
        release' w =  w .&. 0b00001111

silence :: DVS.Vector Double
silence = DVS.replicate 4000 0

sine :: Int -> DVS.Vector Float
sine per = DVS.fromList [sin t | i <- [0 .. per - 1], let t = fromIntegral i / fromIntegral per * 2 * pi]

vectorToPtr0 :: DVS.Storable a => DVS.Vector a -> (Ptr a, Int)
vectorToPtr0 vector =
 let (foreignPtr, size) = DVS.unsafeToForeignPtr0 vector
 in (DVSI.getPtr foreignPtr, size)

we :: IO CInt -> IO ()
we n = do
  r <- n
  unless (r == 0) $ throwIO $ fromErrorCode r

fromErrorCode :: CInt -> Error
fromErrorCode n = toEnum (fromIntegral n + 10000)

withMaybe :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybe Nothing c  = c nullPtr
withMaybe (Just a) c = with a c