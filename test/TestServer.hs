{-# LANGUAGE OverloadedStrings #-}
module TestServer where

import Test.HUnit (assertEqual, assertBool)
import Test.Framework.Providers.HUnit (testCase)

import Data.Either (isRight)
import System.Timeout
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
    testCase "handler term closes socket" testHandlerTerminationClosesSocket
    ]

seconds n = truncate $ n * 1e6

getPort :: NS.SockAddr -> NS.PortNumber
getPort (NS.SockAddrInet port _) = port
getPort (NS.SockAddrInet6 port _ _ _) = port

testListenZeroGivesPort =
  do
    p <- port
    assertBool "port == 0" $ p /= 0
  where
    port = E.bracket
        (listen' HostAny "0")
        (NS.close . fst)
        (return . getPort . snd)


testSelfAwareAsync =
  do
    a <- selfAwareAsync (return . asyncThreadId)
    tid <- wait a
    assertEqual "async ids in and out" tid $ asyncThreadId a


testKillServeKillsHandlers =
  do
    (lsock, laddr) <- listen' HostAny "0"
    v <- newEmptyMVar
    a <- serve' lsock (handler v)
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


testHandlerTerminationClosesSocket =
  do
    (lsock, laddr) <- listen' HostAny "0"
    a <- serve' lsock return
    mbs <- timeout (seconds 0.1) $
        connect "localhost" (show . getPort $ laddr)
            (\(csock, _) -> recv csock 4096)
    assertEqual "didn't get closed" (Just "") mbs
        `E.finally` (cancel a)
