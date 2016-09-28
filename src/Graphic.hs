{-# LANGUAGE OverloadedStrings #-}

-- | Video & Audio support

module Graphic where

import           Control.Monad
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word                   (Word8)
import           Foreign.Ptr
import           Foreign.Storable
import           Linear                      (V2 (..), V4 (..))
import           SDL
import           Utils                       (toBin)

constWindowSize = 256 * 224

initDisplay :: IO (Renderer, Texture)
initDisplay = do
  initializeAll
  window <- createWindow "Space Invaders" defaultWindow { windowInitialSize = V2 256 224 }
  renderer <- createRenderer window (-1) defaultRenderer
  texture <- createTexture renderer ARGB8888 TextureAccessStreaming (V2 256 224)
  rendererDrawColor renderer $=  V4 0 0 0 255
  return (renderer, texture)

renderFrame :: Renderer -> Texture -> VUM.IOVector Word8 -> IO ()
renderFrame rend text vec = do
  (ptr, _) <- lockTexture text Nothing
  writeBuffer (castPtr ptr) vec
  clear rend
  unlockTexture text
  copy rend text Nothing Nothing
  present rend

writePixel :: [Word8] -> Ptr Word8 -> Int -> IO ()
writePixel (r:g:b:a:[]) ptr off = do
  pokeElemOff ptr (off + 0) r
  pokeElemOff ptr (off + 1) g
  pokeElemOff ptr (off + 2) b
  pokeElemOff ptr (off + 3) a
writePixel _ _ _ = fail "color format is not RGBA"

writePixels :: [[Word8]] -> Ptr Word8 -> Int -> IO ()
writePixels (pixel:pixels) ptr off = do
  writePixel pixel ptr off
  writePixels pixels ptr (off + 4)
writePixels [] _ _ = return ()

writeBuffer :: Ptr Word8 -> VUM.IOVector Word8 -> IO ()
writeBuffer ptr vec = do
  -- we assume the vector is long enough here.
  forM_ [0,1 .. constWindowSize `div` 8 - 1] $ \x -> do
    -- one byte represents 8 pixels
    byte <- VUM.read vec x
    let pixels = map fromBW . (map (*255)) . toBin $ byte
    writePixels pixels ptr (x * 32)

fromBW :: Word8 -> [Word8]
fromBW w = [w,w,w,255]
