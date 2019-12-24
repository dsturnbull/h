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
import CPU.Hardware.Video.VIC

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Char
import Data.Word
import Foreign.C.Types
import SDL

import qualified Data.Text     as T
import qualified SDL.Raw.Event as Raw

runVideo :: TVar CPU -> IO ()
runVideo cpuSTM = do
  initializeAll
  let w' = (40 + 8) * 8
  let h' = (25 + 8) * 8
  let scale :: CInt = 4
  window   <- createWindow "h" (defaultWindow { windowInitialSize = V2 (w' * scale) (h' * scale) })
  renderer <- createRenderer window (-1) defaultRenderer
  (V2 width height) <- glGetDrawableSize window
  font <- mkFont renderer
  held <- newTMVarIO ()

  Raw.startTextInput

  appLoop $ VIC {..}

appLoop :: VIC -> IO ()
appLoop vic@VIC {..} = do
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

  present renderer
  appLoop vic

processInput :: Word8 -> CPU -> CPU
processInput c cpu =
  cpu & st (fromIntegral kbd) c
      & st (fromIntegral (kbd + 2)) 1
