{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Hardware.Video
  ( runVideo
  , mkSprite
  , mkRGB332
  , chars
  ) where

import CPU
import CPU.Hardware.Video.Chars

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Bits.Lens
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Word
import Foreign.C.Types
import SDL

import qualified Data.ByteString      as BS
import qualified Data.Map             as M
import qualified Data.Vector.Storable as DVS

mkSprite :: Word8 -> [Word8] -> BS.ByteString
mkSprite c ws = BS.pack (bs =<< ws)
  where bs w  = mkRGB332 c <$> reverse (toListOf bits w)

mkRGB332 :: Word8 -> Bool -> Word8
mkRGB332 _ True  = 0b11100000
mkRGB332 _ False = 0x00

-- rgb
-- mkColour _ = [0x00, 0xff, 0x00]

mkChars :: Renderer -> IO (M.Map Char Texture)
mkChars r = do
  ts <- traverse charTexture (M.toList charMap)
  return $ M.fromList ts
  where charTexture :: (Char, [Word8]) -> IO (Char, Texture)
        charTexture (c, bin) = do
          t <- createTexture r RGB332 TextureAccessStatic (V2 8 8)
          void $ updateTexture t Nothing (mkSprite 0 bin) 8
          return (c, t)

runVideo :: TVar CPU -> IO ()
runVideo cpuSTM = do
  initializeAll
  let w' = 320
  let h' = 200
  let scale :: CInt = 4
  window   <- createWindow "h" (defaultWindow { windowInitialSize = V2 (w' * scale) (h' * scale) })
  renderer <- createRenderer window (-1) defaultRenderer
  (V2 width height) <- glGetDrawableSize window
  chars <- mkChars renderer
  let i = 0

  appLoop $ VIC {..}

data VIC = VIC
  { renderer :: Renderer
  , scale    :: CInt
  , width    :: CInt
  , height   :: CInt
  , chars    :: M.Map Char Texture
  , cpuSTM   :: TVar CPU
  , i        :: CInt
  }

screenRows :: Int
screenRows = 25

screenCols :: Int
screenCols = 40

appLoop :: VIC -> IO ()
appLoop vic@VIC {..} = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events


  let keysPressed = events <&> \event ->
        case eventPayload event of
          KeyboardEvent keyboardEvent -> if keyboardEventKeyMotion keyboardEvent == Pressed
            then Just $ keysymKeycode (keyboardEventKeysym keyboardEvent)
            else Nothing
          _ -> Nothing

  -- XXX
  let key = listToMaybe . catMaybes $ keysPressed
  print key
  rendererDrawColor renderer $= V4 0 0 0 0
  clear renderer

  cpu <- readTVarIO cpuSTM
  drawChars vic cpu

  present renderer
  unless qPressed (appLoop vic)

drawChars :: VIC -> CPU -> IO ()
drawChars VIC {..} cpu = traverse_ (uncurry printRow) rows
  where rows     = getRow <$> [0 .. screenRows - 1]
        printRow :: CInt -> DVS.Vector Word8 -> IO ()
        printRow r cols =
          void . sequence $ zip [0..] (DVS.toList cols) <&>
            \(c, w) ->
              case chars M.!? chr (fromIntegral w) of
                Just t  -> copy renderer t Nothing (pure $ Rectangle (P (V2 (c * 8 * scale) (r * 8 * scale))) (V2 (8 * scale) (8 * scale)))
                Nothing -> return ()
        getRow :: Int -> (CInt, DVS.Vector Word8)
        getRow r = (fromIntegral r, DVS.slice (fromIntegral screenV + r * screenCols) screenCols (cpu & mem))
