{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module CPU.Hardware.Sound
  ( Voices
  , initSound
  , tickSound
  )
  where

import CPU                      (CPU (audio, mem, sid), soundV)
import CPU.Hardware.Sound.SID
import CPU.Hardware.Sound.Voice
import CPU.Instructions.Impl

import Bindings.PortAudio
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Suspend
import Control.Concurrent.Timer
import Control.Exception
import Control.Lens                 hiding (op, set)
import Control.Monad
import Data.Binary                  (encode)
import Data.Bits.Lens
import Data.Generics.Product.Fields
import Data.Vector.Storable         ((!))
import Foreign                      hiding (void)
import Foreign.C.Types
import Linear                       (V2 (..))
import System.IO
import System.PortAudio
import Text.Printf

import qualified CPU.Hardware.Sound.Signal as R

import qualified Data.Vector                   as V
import qualified Data.Vector.Storable          as DVS
import qualified Data.Vector.Storable.Internal as DVSI

import qualified Synthesizer.Storable.Oscillator as Osci
import qualified Synthesizer.Storable.Signal     as SigSt

import qualified Algebra.Additive       as Additive
import qualified Algebra.RealRing       as RealRing
import qualified Algebra.Transcendental as Trans

import qualified Data.ByteString.Lazy         as BS (concat, hPutStr)
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

data Voices = Voices
  { v1T :: TVar Voice
  , v2T :: TVar Voice
  , v3T :: TVar Voice
  }

callback :: (Storable a, Storable b) => (Status -> DVS.Vector a -> MV.IOVector b -> IO StreamCallbackResult) -> Ptr () -> Ptr () -> CULong -> Ptr C'PaStreamCallbackTimeInfo -> CULong -> z -> IO CUInt
callback f (castPtr -> pin) (castPtr -> pout) (fromIntegral -> n) pinfo flags _ = do
  ip <- newForeignPtr_ pin
  op <- newForeignPtr_ pout
  info <- peek pinfo
  toEnum . fromEnum <$> f (Status (realToFrac $ c'PaStreamCallbackTimeInfo'currentTime info)
      (realToFrac $ c'PaStreamCallbackTimeInfo'inputBufferAdcTime info)
      (realToFrac $ c'PaStreamCallbackTimeInfo'outputBufferDacTime info)
      (testBit flags 0)
      (testBit flags 1)
      (testBit flags 2)
      (testBit flags 3)
      (testBit flags 4))
    (DVS.unsafeFromForeignPtr0 ip n)
    (MV.unsafeFromForeignPtr0 op n)

type Amplitude = Double
type Time = Double
type Signal = Time -> Amplitude

callback' :: V.Vector Int16 -> Voices -> MVar Int -> Status -> input -> MV.IOVector (V2 Int16) -> IO StreamCallbackResult
callback' rendered voices phase _ _ o = do
  i0 <- takeMVar phase
  putMVar phase $ i0 + n
  v <- readTVarIO $ voices & v1T
  let period = ceiling $ v & freq
  go v period i0 0
  return Continue
  where
    n = MV.length o
    go :: Voice -> Int -> Int -> Int -> IO ()
    go v period i0 i
      | i == n = return ()
      | otherwise = do

        if v & gate
          then do
            let value = rendered V.! ((i0 + i) `mod` period)
            -- let value = realToFrac (rendered !! (i0 + i))
            MV.write o i (V2 value value)
          else
            MV.write o i (V2 (-32768) (-32768))

        go v period i0 (i + 1)

    table :: Int -> V.Vector Float
    table period = V.fromList [sin t | i <- [0..period - 1], let t = fromIntegral i / fromIntegral period * 2 * pi]

chord :: [Int] -> Signal
chord notes = mixMany $ map (volume' 0.2 . sine . fromIntegral) notes

mix :: Signal -> Signal -> Signal
mix x y t = x t + y t

mixMany :: [Signal] -> Signal
mixMany = foldr mix silence

silence :: Signal
silence = const 0

sine :: Time -> Signal
sine freq t = sin $ freq * (2.0 * pi) * t

volume' :: Amplitude -> Signal -> Signal
volume' x s t = s t * x

clip :: Amplitude -> Amplitude -> Signal -> Signal
clip low high s = max low . min high . s

render :: Time -> Int -> Signal -> [Int16]
render startT sampleRate s =
    [ int16signal (sample * samplePeriod) | sample <- [0..] ]
    where
        int16signal = toInt16 . clipped -- a function of Time -> Int16
        toInt16 x = truncate (minSig + ((x + 1.0) / 2.0 * (maxSig - minSig)))
        minSig = fromIntegral (minBound :: Int16)
        maxSig = fromIntegral (maxBound :: Int16)
        clipped = clip (-1.0) 1.0 s -- the same signal clipped to stay within [-1; 1]
        -- totalSamples = (endT - startT) / samplePeriod -- total number of samples to render
        samplePeriod = 1.0 / fromIntegral sampleRate -- time interval between two sample points

initSound :: CPU -> IO (Voices, CPU)
initSound cpu = do
  ps <- malloc
  we c'Pa_Initialize
  (_, dev : _) <- getDevices

  phase <- newMVar 0
  let output :: Maybe (StreamParameters Output (V2 Int16)) = streamParameters dev 100

  vs <- Voices <$> newTVarIO (cpu & sid & voice1)
               <*> newTVarIO (cpu & sid & voice2)
               <*> newTVarIO (cpu & sid & voice3)

  let r = V.fromList . take 80000 $ render 0.0 4000 (chord [440])
  let f :: Status -> DVS.Vector (V2 Int16) -> MV.IOVector (V2 Int16) -> IO StreamCallbackResult = callback' r vs phase
  cb <- mk'PaStreamCallback $ callback f

  withMaybe noConnection $ \pin -> withMaybe output $ \pout ->
    we $ c'Pa_OpenStream ps
      (castPtr pin)
      (castPtr pout)
      (CDouble (fromInteger . fromIntegral $ rate))
      0
      0
      cb
      nullPtr

  stream <- peek ps
  we $ c'Pa_StartStream stream
  return (vs, cpu & field @"audio" ?~ stream)

tickSound :: Voices -> CPU -> IO CPU
tickSound vs cpu =
  case cpu & audio of
    Just str -> do
      let cpu' = cpu & field @"sid" . field @"volume" .~ vol
      cpu' &   updateVoice v1 (field @"voice1") (field @"voice1") (vs & v1T)
           >>= updateVoice v2 (field @"voice2") (field @"voice2") (vs & v2T)
           >>= updateVoice v3 (field @"voice3") (field @"voice3") (vs & v3T)

    Nothing -> return cpu

  where vol = (cpu & mem) ! fromIntegral (soundV & volumeControl) .&. 0b00000111

-- gate on -> start attack                 | inject waveform for attack+decay+sustain
-- gate on -> attack finished -> decay
-- gate on -> decay finished -> sustain
-- gate off -> start release               | inject waveform for release
-- gate off -> release finished

updateVoice :: (Word16 -> Word16) -> ASetter' SID Voice -> Getting Voice SID Voice -> TVar Voice -> CPU -> IO CPU
updateVoice v set get vt cpu = do
  let voice   = cpu ^. field @"sid" . get -- previous state
  let rising  = not (gate voice) && gate' vctrl -- gate off -> gate on
  let high    = gate' vctrl
  let falling = not (gate' vctrl) && gate voice -- newly off

  let attacking = high
  let decaying  = high && voice ^. field @"attack" == 0
  let releasing = not high

  when rising $ do
    h <- openFile "log" AppendMode
    hPutStrLn h $ printf "rising. w: %s f: %f with attack %f, decay: %f, sustain level: %i" (show $ wave' vctrl) freq' (attackTable $ attack' vad) (drTable $ decay' vad) (sustain' vsr)
    let vol = cpu & sid & volume
    let bs = BS.concat $ map encode (fst $ R.ss (fromIntegral vol) (fromIntegral rate) (wave' vctrl) freq' (attackTable $ attack' vad) (drTable $ decay' vad) (fromIntegral $ sustain' vsr))
    o <- openBinaryFile "wav" WriteMode
    BS.hPutStr o bs
    hClose o
    hClose h

  when falling $ do
    h <- openFile "log" AppendMode
    hPutStrLn h $ printf "falling, release: %f" (drTable $ release' vsr)
    hClose h

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

  atomically $ writeTVar vt (cpu' ^. field @"sid" . get)
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


-- silence :: DVS.Vector Double
-- silence = DVS.replicate 4000 0

-- sine :: Int -> DVS.Vector Float
-- sine per = DVS.fromList [sin t | i <- [0 .. per - 1], let t = fromIntegral i / fromIntegral per * 2 * pi]

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

w16 :: DVS.Vector Word8 -> Word16
w16 v = (addrH `shiftL` 8) .|. addrL
  where addrL = fromIntegral (v ! 0)
        addrH = fromIntegral (v ! 1)
