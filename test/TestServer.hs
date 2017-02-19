{-# LANGUAGE OverloadedStrings #-}
module TestServer where

import Test.HUnit (assertEqual, assertBool)
import Test.Framework.Providers.HUnit (testCase)

import Data.Either (isRight)
import System.Timeout
import Control.Exception (AsyncException(ThreadKilled))
import qualified Control.Exception as E
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (
    async, withAsync, wait, cancel, asyncThreadId, mapConcurrently,
    replicateConcurrently)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Network.Socket as NS
import Network.Socket.ByteString (send, recv)
import Network.Simple.TCP (HostPreference(HostAny), connect)

import Pipes (runEffect, cat)
import Pipes.Core (Client, request, respond, (>>~))
import qualified Pipes.Prelude as PP
import Pipes.Safe (runSafeT)

import Server (selfAwareAsync, withListen, serve', socketServer)


tests = [
    testCase "zero listen" testListenZeroGivesPort,
    testCase "self-aware async" testSelfAwareAsync,
    testCase "server kills children" testKillServeKillsHandlers,
    testCase "handler term closes socket" testHandlerTerminationClosesSocket,
    testCase "handler error closes socket" testHandlerErrorClosesSocket,
    testCase "multiple connections" $ testMultipleConnections 42,
    testCase "socketServer echo" testSocketServerBasicEcho,
    testCase "socketServer graceful interrupt" testSocketServerClosesGracefully
    ]

seconds n = truncate $ n * 1e6

getPort :: NS.SockAddr -> NS.PortNumber
getPort (NS.SockAddrInet port _) = port
getPort (NS.SockAddrInet6 port _ _ _) = port

withListen' = withListen HostAny "0"
withServe lsock handler = E.bracket (serve' lsock handler) cancel
withServe' handler io =
    withListen' $ \(lsock, laddr) ->
        withServe lsock handler $ \_ ->
            io (show . getPort $ laddr)

testListenZeroGivesPort =
  do
    port <- withListen' (return . getPort . snd)
    assertBool "port == 0" $ port /= 0


testSelfAwareAsync =
  do
    a <- selfAwareAsync (return . asyncThreadId)
    tid <- wait a
    assertEqual "async ids in and out" tid $ asyncThreadId a


testKillServeKillsHandlers = withListen' $ \(lsock, laddr) ->
  do
    v <- newEmptyMVar
    withServe lsock (handler v) $ \a ->
     do
        connect "localhost" (show . getPort $ laddr)
            (\(csock, _) -> recv csock 4096 >> cancel a)
        -- wait a -- FIXME: why does this hang the world?!
        res <- takeMVar v
        assertBool "timed out waiting for thread to be killed" $ isRight res
  where
    handler v (hsock, _) = E.catch
        (send hsock "hello" >>
         threadDelay (seconds 0.1) >>
         putMVar v (Left ()))
        (\ThreadKilled -> putMVar v (Right ()))


socketCloseTest handler = withServe' handler $ \port->
  do
    mbs <- timeout (seconds 0.1) $
        connect "localhost" port
            (\(csock, _) -> recv csock 4096)
    assertEqual "didn't get closed" (Just "") mbs

testHandlerTerminationClosesSocket = socketCloseTest return
testHandlerErrorClosesSocket = socketCloseTest $ error "part of test"


testMultipleConnections n = withServe' handler $ \port ->
  do
    res <- replicateConcurrently n $ timeout (seconds 0.1) $
        connect "localhost" port (\(csock, _) -> recv csock 4096)
    assertEqual "bad thread data" res $ replicate n (Just "hello")
  where
    handler (hsock, _) = send hsock "hello"

echoMap :: (Monad m) => (a -> b) -> a -> Client b a m r
echoMap f input = request (f input) >>= echoMap f

testSocketServerBasicEcho = withListen' $ \(lsock, laddr) ->
  let
    s = socketServer cat cat lsock
    client word = connect "localhost" (show $ getPort laddr) $ \(csock, _) ->
        send csock word >> recv csock 4096
  in
    withAsync (runSafeT $ runEffect $ s >>~ echoMap pure) $ \_ ->
      do
        receivedWords <- mapConcurrently client words
        assertEqual "received words" words receivedWords
  where
    words = ["hello", "world", "llama", "train"]


testSocketServerClosesGracefully = withListen' $ \(lsock, laddr) -> do
    let s = socketServer cat cat lsock
    a <- async (runSafeT $ runEffect $ s >>~ echoMap pure)
    mbs <- timeout (seconds 0.1) $
        connect "localhost" (show $ getPort laddr) $ \(csock, _) ->
            cancel a >> recv csock 4096
    assertEqual "timed out waiting for close" (Just "") mbs
