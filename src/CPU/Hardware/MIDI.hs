module CPU.Hardware.MIDI
  ( jackMain
  ) where

import CPU
import CPU.Instructions.Decodes

import Control.Concurrent.STM
import Control.Lens

import qualified Data.Vector.Storable as DVS

import qualified Sound.JACK                         as Jack
import qualified Sound.JACK.MIDI                    as JackMidi
import qualified Sound.MIDI.Message                 as MIDI
import qualified Sound.MIDI.Message.Channel         as Channel
import qualified Sound.MIDI.Message.Channel.Voice   as Voice
import qualified Sound.MIDI.Message.Class.Construct as MidiCons

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class           as Trans

import qualified Data.EventList.Absolute.TimeBody  as AbsEventList
import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import           Data.IORef                        (IORef, newIORef, readIORef, writeIORef)
import qualified Numeric.NonNegative.Wrapper       as NonNegW

import qualified Foreign.C.Error as E

import System.Environment (getProgName)


eventLoop :: [Channel.Pitch] -> EventList.T NonNegW.Double MIDI.T
eventLoop scale' =
  EventList.fromPairList $
    concatMap
      (\p' ->
          let note on =
                  MidiCons.note (Channel.toChannel 0)
                      (Voice.normalVelocity, p', on)
              note' on =
                  MidiCons.note (Channel.toChannel 1)
                      (Voice.normalVelocity, p', on)
          in  [ (0, note True)
              , (0.1, note False)
              , (0.1, note' True)
              , (0, note' False)
              ])
      scale'

jackMain :: TVar CPU -> IO ()
jackMain cpuSTM = do
  name <- getProgName
  Jack.handleExceptions $
    Jack.withClientDefault name $ \client -> do
      Jack.withPort client "output" $ \output -> do
        rate <- fmap fromIntegral $ Trans.lift $ Jack.getSampleRate client
        stateRef <-
          Trans.lift $
            newIORef (EventList.resample rate $ (EventList.fromPairList [] :: EventList.T NonNegW.Double MIDI.T))
        Jack.withProcess client (process cpuSTM rate stateRef output) $
          Jack.withActivation client $ Trans.lift $ do
            Jack.waitForBreak

process ::
  TVar CPU ->
  NonNegW.Double ->
  IORef (EventList.T NonNegW.Int MIDI.T) ->
  JackMidi.Port Jack.Output ->
  Jack.NFrames ->
  Sync.ExceptionalT E.Errno IO ()
process cpuSTM rate stateRef output nframes@(Jack.NFrames nframesInt) = do
  events <- Trans.lift $ readIORef stateRef
  (_, notes) <- Trans.lift $ atomically $ do
    cpu <- readTVar cpuSTM
    let ct = w16 $ DVS.slice 0x0381 2 (cpu & mem)
    -- let ct = head . DVS.toList $ DVS.slice 0x0300 1 (cpu & mem)
    if (ct == 0x01)
      then do
        -- let ns = DVS.toList $ DVS.slice 0x0300 8 (cpu & mem)
        -- return $ EventList.resample rate $ eventLoop (Channel.toPitch . fromIntegral <$> ns)
        let ns = map Channel.toPitch [60, 62, 64, 65, 67, 69, 71, 72]
        return $ (ct, EventList.resample rate $ eventLoop ns)
      else return (ct, events)
  let (currentEvents, _) = EventListTM.splitAtTime (fromIntegral nframesInt) events
  Trans.lift $ writeIORef stateRef notes
  JackMidi.writeEventsToPort output nframes $
    AbsEventList.mapTime (Jack.NFrames . NonNegW.toNumber . fromIntegral) $
      EventList.toAbsoluteEventList 0 $
        fst $ EventListTM.viewTimeR currentEvents
