module Timer where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class

type Timer = MVar TimerState

data TimerState = Ready | Timeout

newTimer :: IO Timer
newTimer = do
  m <- newEmptyMVar
  putMVar m Ready
  return m

timeout :: Timer -> Int -> IO ()
timeout t n = void . forkIO . forever $ do
  threadDelay n
  swapMVar t Timeout

withTimer :: (MonadIO m) => Int -> m a -> m a -> m a
withTimer n action interrupt = do
  t <- liftIO newTimer
  liftIO $ t `timeout` n
  loop t
  where loop timer = do
          state <- liftIO $ readMVar timer
          void $ case state of
                   Ready   -> action
                   Timeout -> do
                     liftIO . void $ swapMVar timer Ready
                     interrupt
          loop timer
