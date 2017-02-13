{-# LANGUAGE OverloadedStrings #-}
module TestServer where

import Test.HUnit (assertEqual, assertBool)
import Test.Framework.Providers.HUnit (testCase)

import Data.Either (isRight)
import System.Timeout
import Control.Monad (replicateM)
import Control.Exception (AsyncException(ThreadKilled))
import qualified Control.Exception as E
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, wait, cancel, asyncThreadId)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Network.Socket as NS
import Network.Socket.ByteString (send, recv)
import Network.Simple.TCP (HostPreference(HostAny), connect)

import Server (selfAwareAsync, listen', serve')


tests = [
    testCase "zero listen" testListenZeroGivesPort,
    testCase "self-aware async" testSelfAwareAsync,
    testCase "server kills children" testKillServeKillsHandlers,
    testCase "handler term closes socket" testHandlerTerminationClosesSocket,
    testCase "handler error closes socket" testHandlerErrorClosesSocket,
    testCase "multiple connections" $ testMultipleConnections 42
    ]

seconds n = truncate $ n * 1e6

getPort :: NS.SockAddr -> NS.PortNumber
getPort (NS.SockAddrInet port _) = port
getPort (NS.SockAddrInet6 port _ _ _) = port

withListen = E.bracket (listen' HostAny "0") (NS.close . fst)
withServe lsock handler = E.bracket (serve' lsock handler) cancel
withServe' handler io =
    withListen $ \(lsock, laddr) ->
        withServe lsock handler $ \_ ->
            io (show . getPort $ laddr)

testListenZeroGivesPort =
  do
    port <- withListen (return . getPort . snd)
    assertBool "port == 0" $ port /= 0


testSelfAwareAsync =
  do
    a <- selfAwareAsync (return . asyncThreadId)
    tid <- wait a
    assertEqual "async ids in and out" tid $ asyncThreadId a


testKillServeKillsHandlers = withListen $ \(lsock, laddr) ->
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
    res <- replicateM n $ timeout (seconds 0.1) $ connect "localhost"
        (show . getPort $ laddr)
        (\(csock, _) -> recv csock 4096)
    assertEqual "bad thread data" res $ replicate n (Just "hello")
  where
    handler (hsock, _) = send hsock "hello"
