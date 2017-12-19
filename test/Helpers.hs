{-# LANGUAGE FlexibleInstances #-}
module Helpers where

import Test.Hspec (shouldSatisfy)
import Data.Maybe (isJust, fromJust)
import System.Timeout


seconds = truncate . (* 1e6)

timeLimit :: (Show a) => Double -> IO a -> IO a
timeLimit secs action = (timeout (seconds secs) action) >>= \r -> do
    r `shouldSatisfy` isJust
    return (fromJust r)
