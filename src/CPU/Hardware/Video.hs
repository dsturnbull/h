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

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Vector.Storable   ((!))
import Data.Word
import Foreign.C.Types
import SDL

import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Data.Vector.Storable as DVS
import qualified SDL.Raw.Event        as Raw

runVideo :: TVar CPU -> IO ()
runVideo cpuSTM = do
  initializeAll
  let w' = 320
  let h' = 200
  let scale :: CInt = 2
  window   <- createWindow "h" (defaultWindow { windowInitialSize = V2 (w' * scale) (h' * scale) })
  renderer <- createRenderer window (-1) defaultRenderer
  (V2 width height) <- glGetDrawableSize window
  font <- mkFont renderer
  held <- newTMVarIO ()

  Raw.startTextInput

  appLoop $ VIC {..}

data VIC = VIC
  { renderer :: Renderer
  , scale    :: CInt
  , width    :: CInt
  , height   :: CInt
  , font     :: Font
  , cpuSTM   :: TVar CPU
  , held     :: TMVar ()
  }

screenRows :: Int
screenRows = 25

screenCols :: Int
screenCols = 40

appLoop :: VIC -> IO ()
appLoop vic@VIC {..} = do
  events <- pollEvents
  -- let keysPressed = events <&> \event ->
  --       case eventPayload event of
  --         KeyboardEvent keyboardEvent -> if keyboardEventKeyMotion keyboardEvent == Pressed
  --           then Just $ keysymKeycode (keyboardEventKeysym keyboardEvent)
  --           else Nothing
  --         _ -> Nothing
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

  rendererDrawColor renderer $= V4 131 124 216 0
  clear renderer

  cpu <- readTVarIO cpuSTM
  drawChars vic cpu

  present renderer
  appLoop vic

drawChars :: VIC -> CPU -> IO ()
drawChars VIC {..} cpu = traverse_ (uncurry printRow) rows
  where rows     = getRow <$> [0 .. screenRows - 1]
        printRow :: CInt -> DVS.Vector Word8 -> IO ()
        printRow r cols =
          void . sequence $ zip [0..] (DVS.toList cols) <&>
            \(c, w) ->
              case (font & chars) M.!? chr (fromIntegral w) of
                Just f@(t, _)  -> do
                  updateCharColour f (getColour r c)
                  copy renderer t Nothing (pure $ Rectangle (P (V2 (c * 8 * scale) (r * 8 * scale))) (V2 (8 * scale) (8 * scale)))
                Nothing -> return ()
        getRow :: Int -> (CInt, DVS.Vector Word8)
        getRow r = (fromIntegral r, DVS.slice (fromIntegral screenV + r * screenCols) screenCols (cpu & mem))
        getColour :: CInt -> CInt -> Word8
        getColour r c = (cpu & mem) ! (fromIntegral colourV + fromIntegral r * screenCols + fromIntegral c)

processInput :: Word8 -> CPU -> CPU
processInput c cpu =
  cpu & st (fromIntegral kbd) c
      & st (fromIntegral (kbd + 2)) 1
