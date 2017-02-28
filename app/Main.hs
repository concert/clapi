{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import System.Posix.Signals
import Network.Socket.ByteString (send)
import Network.Simple.TCP hiding (send)

import Server


main :: IO ()
main =
  mask $ \restore -> do
    a <- async $ withListen HostAny "1234" $ \(lsock, _) ->
        serve' lsock $ \(hsock, _) -> send hsock "hello\n" >> threadDelay 10000000
    installHandler keyboardSignal (Catch $ cancel a) Nothing
    restore $ wait a
