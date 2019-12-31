{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Hardware.Video
  ( runVideo
  ) where

import CPU
import CPU.Hardware.Interrupt
import CPU.Hardware.Video.Chars
import CPU.Hardware.Video.Sprites
import CPU.Hardware.Video.VIC

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Char
import Data.Word
import SDL

import qualified Data.Text     as T
import qualified SDL.Raw.Event as Raw

runVideo :: TVar CPU -> IO ()
runVideo cpuSTM = do
  initializeAll
  Raw.startTextInput
  vic <- mkVIC
  appLoop vic cpuSTM

appLoop :: VIC -> TVar CPU -> IO ()
appLoop vic@VIC {..} cpuSTM = do
  events <- pollEvents

  let text = mconcat $ events <&> \event ->
        case eventPayload event of
          TextInputEvent textInputEvent ->
            textInputEventText textInputEvent
          _ -> mempty

  unless (T.null text) $ atomically $ do
    cpu <- readTVar cpuSTM
    when (cpu & p & interrupt & not) $ void $ tryPutTMVar held ()
    _ <- takeTMVar held
    let cpu' = foldl (\cp c -> cp & processInput (fromIntegral . ord $ c)) cpu (T.unpack text)
    writeTVar cpuSTM (cpu' & intr)

  rendererDrawColor renderer $= V4 0x83 0x7c 0xd8 0
  clear renderer

  cpu <- readTVarIO cpuSTM
  drawChars vic cpu
  drawSprites vic cpu

  present renderer
  appLoop vic cpuSTM

processInput :: Word8 -> CPU -> CPU
processInput c cpu =
  cpu & st (fromIntegral kbd) c
      & st (fromIntegral (kbd + 2)) 1
