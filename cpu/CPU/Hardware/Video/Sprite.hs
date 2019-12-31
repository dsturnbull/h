{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Hardware.Video.Sprite
  ( mkEmptySprite
  )
  where

import Data.Word
import SDL

mkEmptySprite :: Renderer -> Word8 -> IO (Word8, (Texture, [Word8]))
mkEmptySprite r i = do
  t <- createTexture r ARGB8888 TextureAccessTarget (V2 24 21)
  textureBlendMode t $= BlendAlphaBlend
  return (i, (t, replicate (24 * 21) 0))
