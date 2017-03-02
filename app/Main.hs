{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import System.Posix.Signals
import Network.Socket.ByteString (send)
import qualified Network.Socket as NS
import Network.Simple.TCP hiding (send)

import Server

main :: IO ()
main =
  do
    tid <- myThreadId
    installHandler keyboardSignal (Catch $ killThread tid) Nothing
    withListen HostAny "1234" $ \(lsock, _) -> serve' lsock $
        \(hsock, _) -> send hsock "hello\n" >> threadDelay 10000000
