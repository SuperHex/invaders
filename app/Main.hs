{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Lens
import           Control.Monad               (forever)
import           Control.Monad.State
import           Data.IORef
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Graphic
import           MonadCPU                    (CPUState (..), initCPU, runCPU)

makeLenses ''CPUState

main :: IO ()
main = do
  cpu               <- initCPU
  (render, texture) <- initDisplay
  -- flip evalStateT cpu $ do
  --   forever $ do
  --     replicateM_ 100 (runCPU False)
  --     cpu' <- get
  --     let videoRAM = ((VUM.splitAt 0x2400 (cpu' ^. memory)) ^. _2)
  --     liftIO $ renderFrame render texture videoRAM
  ref               <- newIORef cpu
  forever $ do
    initState <- readIORef ref
    newState  <- execStateT (replicateM_ 500 $ runCPU False) initState
    isr       <- VUM.read (cpu ^. memory) 0x20c0
    VUM.write (cpu ^. memory) 0x20c0 (isr - 1)
    writeIORef ref newState
    let video = VUM.splitAt 0x2400 (newState ^. memory) ^. _2
    renderFrame render texture video
