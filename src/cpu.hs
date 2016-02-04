module CPU where

import Control.Concurrent
import Control.Monad (forever)

timer :: MVar () -> Int -> IO ()
timer m i = forever $ do 
              putMVar m ()
              threadDelay (10^6 * i)

tick :: IO ()
tick = do
  m <- newEmptyMVar
  forkIO $ timer m 10
  takeMVar m
  print "hey!"
