module Helpers where

import System.Timeout

seconds :: Double -> Int
seconds = truncate . (* 1e6)

timeLimit :: (Show a) => Double -> IO a -> IO a
timeLimit secs action = timeout (seconds secs) action >>=
  maybe (error "Timeout") return
