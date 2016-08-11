{-# LANGUAGE OverloadedStrings #-}

-- | Video & Audio support

module Graphic where

import           Control.Monad
import qualified Data.Vector      as V
import           Data.Word        (Word8)
import           Foreign.Ptr
import           Foreign.Storable
import           Linear           (V2 (..), V4 (..))
import           SDL
import           Utils            (mask)

initializeDisplay :: IO (Renderer, Texture)
initializeDisplay = do
  initializeAll
  window <- createWindow "Space Invaders" defaultWindow { windowInitialSize = V2 256 224 }
  renderer <- createRenderer window (-1) defaultRenderer
  texture <- createTexture renderer RGB888 TextureAccessStreaming (V2 256 224)
  rendererDrawColor renderer $=  V4 0 0 0 255
  return (renderer, texture)

renderFrame :: Renderer -> Texture -> V.Vector Word8 -> IO ()
renderFrame rend text vec = do
  (ptr, _) <- lockTexture text Nothing
  writeBuffer (castPtr ptr) . expand $ vec
  clear rend
  unlockTexture text
  copy rend text Nothing Nothing
  present rend

writeBGRA :: Word8 -> Ptr Word8 -> Int -> IO ()
writeBGRA bit ptr off = forM_ [0..3] $ \x -> pokeElemOff ptr (off + x) bit

writeBuffer :: Ptr Word8 -> V.Vector Word8 -> IO ()
writeBuffer ptr vec = forM_ [0,4 .. V.length vec - 1] $ \x -> writeBGRA (vec V.! x) ptr x

expand :: V.Vector Word8 -> V.Vector Word8
expand = V.concatMap (V.fromList . map (*255) . mask)
