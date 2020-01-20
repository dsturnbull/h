{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Hardware.Sound.Signal
  where

-- import CPU.Hardware.Sound.Voice

-- import Control.Monad
import Data.Binary   (encode)
import Data.Int
import System.IO

-- import qualified Synthesizer.Basic.Binary                     as BinSmp
-- import qualified Synthesizer.Basic.Wave                       as Wave
-- import qualified Synthesizer.Plain.Control                    as Ctrl
-- import qualified Synthesizer.Plain.File                       as File
-- import qualified Synthesizer.Plain.Filter.NonRecursive        as Filt
-- import qualified Synthesizer.Plain.Filter.Recursive           as FiltRec
-- import qualified Synthesizer.Plain.Filter.Recursive.Universal as UniFilter
-- import qualified Synthesizer.Plain.Oscillator                 as Osci
-- import qualified Synthesizer.Plain.Play                       as Play
-- import qualified Synthesizer.Storable.Signal                  as SigSt

import qualified Data.ByteString.Lazy     as BS (concat, hPutStr)
-- import qualified Data.StorableVector.Lazy as DSVL
import qualified Data.Vector.Storable     as DVS

type Amplitude = Double
type Time = Double
type Signal = Time -> Amplitude

-- |A sound from outer space.
silence :: Signal
silence = const 0

-- |Oscillate in a form of a sine wave at 'freq' Hz.
sine :: Time -> Signal
sine f t = sin $ f * (2.0 * pi) * t

-- |Square wave at 'freq' Hz.
square :: Time -> Signal
square f t = if odd i then 1.0 else -1.0
  where (i :: Int, _) = properFraction (t * f)

-- |Multiplies the signal by a fixed value.
volume :: Amplitude -> Signal -> Signal
volume x s t = s t * x

-- |Mixes two signals together by adding amplitudes.
mix :: Signal -> Signal -> Signal
mix x y t = x t + y t

-- |Mixes several signals together by adding amplitudes.
mixMany :: [Signal] -> Signal
mixMany = foldr mix silence

-- |Calculates an oscillation frequency for a MIDI note number, in an equally tempered scale.
midiNoteToFreq :: (Floating a) => Int -> a
midiNoteToFreq n =
  f0 * (a ** (fromIntegral n - midiA4))
  where
    a = 2 ** (1.0 / 12.0)
    f0 = 440.0 -- A-4 in an ETS is 440 Hz.
    midiA4 = 69 -- A-4 in MIDI is 69.

-- |Mixes given notes into a single chord signal.
chord :: [Int] -> Signal
chord notes = mixMany $ map (volume 0.2 . sine . midiNoteToFreq) notes

-- |Limits the signal's amplitude to not leave specified range.
clip :: Amplitude -> Amplitude -> Signal -> Signal
clip low high s = max low . min high . s

-- |Controls the amplitude of one signal by value of another signal.
amp :: Signal -> Signal -> Signal
amp x y t = x t * y t

-- |Emits a control signal for an exponential fade out.
fade :: Double -> Double -> Double
fade speed a = exp $ (* speed) $ (* (-1.0)) $ a

attack :: Double -> Double -> Amplitude
attack tr b = do
  let f = b * (exp $ log ((1 + tr) / tr) / 4000)
  f
-- exp(-log((1 + targetRatio) / targetRatio) / time);

-- |Samples the signal over a specified time range with given sample rate.
render :: Time -> Time -> Int -> Signal -> DVS.Vector Int16
render startT endT sampleRate s =
  DVS.fromList $ [ int16signal (sample * samplePeriod) | sample <- [0 .. totalSamples - 1] ]
  where
    int16signal = toInt16 . clipped -- a function of Time -> Int16
    toInt16 x = truncate (minSig + ((x + 1.0) / 2.0 * (maxSig - minSig)))
    minSig = fromIntegral (minBound :: Int16)
    maxSig = fromIntegral (maxBound :: Int16)
    clipped = clip (-1.0) 1.0 s -- the same signal clipped to stay within [-1; 1]
    totalSamples = (endT - startT) / samplePeriod -- total number of samples to render
    samplePeriod = 1.0 / fromIntegral sampleRate -- time interval between two sample points

q :: [Int16]
q = do
  let f = 440
  let s = volume 0.01 . sine $ f
  DVS.toList $ render 0.0 0.10 4000 s

r :: IO ()
r = do
  o <- openBinaryFile "wav" WriteMode
  let w = BS.concat $ map encode q
  BS.hPutStr o w
  hClose o

-- ss :: Int -> Int -> Waveform -> Double -> Double -> Double -> Int -> ([Int16], [Int16])
-- ss vol rate w freq' a d s = do
--   let al = ceiling $ realToFrac rate * a
--   let dl = ceiling $ realToFrac rate * d
--   let a = Ctrl.line al (0.0, fromInteger (fromIntegral vol) :: Double)
--   let d = Ctrl.line dl (fromInteger (fromIntegral vol) :: Double, fromInteger (fromIntegral s) :: Double)
--   let s = Ctrl.line 13230 (0.25, 0.25)
--   let ad = a ++ d
--   let siw = Filt.envelope ad (Osci.static Wave.saw 0 freq')
--   let ad = map BinSmp.int16FromCanonical (take 4000 siw)
--   let sustain = Filt.envelope s (Osci.static Wave.saw 0 freq')
--   let s' = map BinSmp.int16FromCanonical (take 4000 sustain)
--   (ad, s')

-- sf :: Int -> Int -> Waveform -> Double -> Double -> Double -> Double -> IO ()
-- sf vol rate w freq' a d s = do
--   let al = ceiling $ realToFrac rate * a
--   let dl = ceiling $ realToFrac rate * d
--   let ae = Ctrl.line al (0.0, fromInteger (fromIntegral vol) :: Double)
--   let de = Ctrl.line dl (fromInteger (fromIntegral vol) :: Double, s)
--   let se = Ctrl.line 13230 (0.25, 0.25)
--   let ads = ae ++ de ++ se
--   let w' = Osci.static Wave.saw 0 freq'
--   let siw = Filt.envelope ads w'
--   void $ File.writeToInt16 "sine.aiff" (realToFrac rate :: Double) (take (rate * ceiling a) siw)
--   void $ Play.monoToInt16 (realToFrac rate :: Double) (take (rate * ceiling a) siw)

-- test :: Int -> Int -> IO ()
-- test rate attack = do
--   let a = Ctrl.line 22050 (0, 0.5)
--   let d = Ctrl.line 13230 (0.5, 0.25)
--   let s = Ctrl.line 13230 (0.25, 0.25)
--   let r = Ctrl.exponential 33075 0.25
--   let ad = a ++ d ++ s ++ r
--   let siw :: [Double] = Filt.envelope ad (Osci.static Wave.saw 0 (0.01::Double))
--   -- let laser = Osci.freqMod Wave.saw 0 $ map (\f -> 0.12+0.11*f) $ Osci.static Wave.saw 0 (0.0011::Double)
--   -- let ping = Filt.envelope (Ctrl.exponential 5000 1) (Osci.static Wave.sine 0 (0.01::Double))
--   -- let fmping = Osci.phaseMod Wave.sine (0.01::Double) $ map (2*) ping
--   -- let fs = map UniFilter.lowpass $ UniFilter.run (map (\f -> UniFilter.parameter $ FiltRec.Pole 10 (0.04+0.02*f)) $ Osci.static Wave.sine 0 (0.00001::Double)) $ Osci.static Wave.saw 0 (0.002::Double)
--   void $ File.writeToInt16 "sine.aiff" (realToFrac rate :: Double) (take (rate * attack) siw)
--   void $ Play.monoToInt16 (realToFrac rate :: Double) (take (rate * attack) siw)
