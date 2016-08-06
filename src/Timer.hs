module Timer where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class

type Timer = MVar TimerState

data TimerState = Ready | Timeout

newTimer :: (MonadIO m) => m Timer
newTimer = liftIO $ do
  m <- newEmptyMVar
  putMVar m Ready
  return m

delay :: (MonadIO m) => Int -> m ()
delay = liftIO . threadDelay

startTimer :: (MonadIO m) => Timer -> Int -> m ()
startTimer timer n = liftIO . void . forkIO . forever $ do
  threadDelay n
  swapMVar timer Timeout

checkTimer :: (MonadIO m) => Timer -> m Bool
checkTimer timer = do
  stat <- liftIO . readMVar $ timer
  case stat of
    Ready   -> return False
    Timeout -> return True

resetTimer :: (MonadIO m) => Timer -> m ()
resetTimer timer = liftIO . void $ swapMVar timer Ready

registerTimer :: (MonadIO m) => Int -> m Timer
registerTimer n = do
  t <- newTimer
  startTimer t n
  return t
