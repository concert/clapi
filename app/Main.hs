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

-- thing :: ((Socket, SockAddr) -> IO r) -> IO r
thing :: (Socket -> IO r) -> IO r
thing serve = mask $ \restore -> do
    (lsock, _) <- bindSock HostAny "1234"
    NS.listen lsock 2048
    addr <- NS.getSocketName lsock
    -- a <- async $ serve (lsock, addr)
    a <- async $ serve lsock
    restore $ doubleCatch
       (NS.close lsock >> wait a)
       (cancel a >> putStrLn "success")
       (wait a >>= \r -> NS.close lsock >> return r)

main :: IO ()
main =
  do
    tid <- myThreadId
    installHandler keyboardSignal (Catch $ killThread tid) Nothing
    thing $
        flip serve' $ \(hsock, _) -> send hsock "hello\n" >> threadDelay 10000000
