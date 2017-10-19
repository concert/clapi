{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module TestServer where

import Test.HUnit (assertEqual, assertBool)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad (forever)
import Data.Either (isRight)
import Data.Maybe (isJust, fromJust)
import Data.Void
import System.Timeout
import Control.Exception (AsyncException(ThreadKilled))
import qualified Control.Exception as E
import Control.Concurrent (threadDelay, killThread)
import Control.Concurrent.Async (
    async, withAsync, wait, cancel, asyncThreadId, mapConcurrently,
    replicateConcurrently)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Network.Socket as NS
import Network.Socket.ByteString (send, recv)
import Network.Simple.TCP (HostPreference(HostAny), connect)

import Server (
  ClientEvent(..), ClientEvent', ServerEvent(..), ServerEvent', doubleCatch,
  swallowExc, withListen, serve', protocolServer)
import Protocol (Protocol, waitThen, sendFwd, sendRev)

import Helpers (seconds, timeLimit)


tests = [
    testCase "zero listen" testListenZeroGivesPort,
    testCase "double kill" testDoubleCatchKills,
    testCase "doubleCatch return" testDoubleCatchReturn,
    testCase "doubleCatch soft kill" testDoubleCatchSoftKill,
    testCase "server waits children" testKillServeWaitsHandlers,
    testCase "server can kill children" testDoubleKillServerKillsHandlers,
    testCase "handler term closes socket" testHandlerTerminationClosesSocket,
    testCase "handler error closes socket" testHandlerErrorClosesSocket,
    testCase "multiple connections" $ testMultipleConnections 42,
    testCase "protocolServer echo" testProtocolServerBasicEcho,
    testCase "protocolServer graceful interrupt"
        testProtocolServerClosesGracefully
    ]

getPort :: NS.SockAddr -> NS.PortNumber
getPort (NS.SockAddrInet port _) = port
getPort (NS.SockAddrInet6 port _ _ _) = port

withListen' = withListen HostAny "0"
withServe lsock handler = E.bracket (async $ serve' lsock handler) cancel
withServe' handler io =
    withListen' $ \(lsock, laddr) ->
        withServe lsock handler $ \_ ->
            io (show . getPort $ laddr)

testListenZeroGivesPort =
  do
    port <- withListen' (return . getPort . snd)
    assertBool "port == 0" $ port /= 0

assertAsyncKilled a =
    timeout (seconds 0.1) (E.try $ wait a) >>=
    assertEqual "thread wasn't killed" (Just $ Left E.ThreadKilled)

testDoubleCatchKills =
  do
    body <- newEmptyMVar
    soft <- newEmptyMVar
    hard <- newEmptyMVar
    a <- async $ doubleCatch
        (swallowExc $ putMVar soft () >> threadDelay 500000)
        (putMVar hard ())
        (putMVar body () >> threadDelay 500000)
    let die = killThread $ asyncThreadId a
    taken body >>= assertBool "body not executed"
    (not <$> taken body) >>= assertBool "soft handler executed early"
    die
    taken soft >>= assertBool "soft handler not executed"
    (not <$> taken hard) >>= assertBool "hard handler executed early"
    die
    taken hard >>= assertBool "hard handler not executed"
    assertAsyncKilled a
  where
    taken mvar =
      do
        called <- timeout (seconds 0.1) $ takeMVar mvar
        return $ called /= Nothing

testDoubleCatchReturn =
    doubleCatch (swallowExc undefined) undefined (return 42) >>=
    assertEqual "bad return value" 42

testDoubleCatchSoftKill =
    doubleCatch (swallowExc $ return 42) undefined undefined >>=
    assertEqual "bad return value" 42


killServerHelper connector handler = withListen' $ \(lsock, laddr) ->
  do
    v <- newEmptyMVar
    withServe lsock (\(hsock, _) -> handler v hsock) $ \a ->
     do
        connect "127.0.0.1" (show . getPort $ laddr) $
            \(csock, _) -> connector a csock
        timeLimit 0.1 (takeMVar v)
        assertAsyncKilled a


testKillServeWaitsHandlers = killServerHelper connector handler
  where
    connector a csock = recv csock 4096 >> killThread (asyncThreadId a) >> send csock "bye"
    handler v hsock = send hsock "hello" >> recv hsock 4096 >> putMVar v ()


testDoubleKillServerKillsHandlers = killServerHelper connector handler
  where
    connector a csock =
        let kill = killThread (asyncThreadId a) in
        recv csock 4096 >> kill >> kill
    handler v hsock = E.catch
        (send hsock "hello" >> threadDelay (seconds 1))
        (\E.ThreadKilled -> putMVar v ())


socketCloseTest handler = withServe' handler $ \port->
  do
    mbs <- timeout (seconds 0.1) $
        connect "127.0.0.1" port
            (\(csock, _) -> recv csock 4096)
    assertEqual "didn't get closed" (Just "") mbs

testHandlerTerminationClosesSocket = socketCloseTest return
testHandlerErrorClosesSocket = socketCloseTest $ error "part of test"


testMultipleConnections n = withServe' handler $ \port ->
  do
    res <- replicateConcurrently n $ timeout (seconds 0.1) $
        connect "127.0.0.1" port (\(csock, _) -> recv csock 4096)
    assertEqual "bad thread data" res $ replicate n (Just "hello")
  where
    handler (hsock, _) = send hsock "hello"

cat :: (Monad m) => Protocol a a b b m ()
cat = forever $ waitThen sendFwd sendRev

echo :: (Monad m) =>
  Protocol
    (ClientEvent' a ())
    Void
    (ServerEvent' a)
    Void m ()
echo = forever $ waitThen boing undefined
  where
    boing (ClientData addr a) = sendRev (ServerData addr a)
    boing _ = return ()

testProtocolServerBasicEcho = withListen' $ \(lsock, laddr) ->
  let
     client word = connect "127.0.0.1" (show $ getPort laddr) $ \(csock, _) ->
       send csock word >> recv csock 4096
  in
    withAsync (protocolServer lsock cat echo) $ \_ ->
      do
        receivedWords <- mapConcurrently client words
        assertEqual "received words" words receivedWords
  where
    words = ["hello", "world", "llama", "train"]


testProtocolServerClosesGracefully =
  do
    addrV <- newEmptyMVar
    a <- async $ withListen' $ \(lsock, laddr) -> E.mask $ \restore -> do
        putMVar addrV laddr
        protocolServer lsock cat echo
    let kill = killThread (asyncThreadId a)
    port <- show . getPort <$> takeMVar addrV
    timeLimit 0.1 $ connect "127.0.0.1" port $ \(csock, _) -> do
        let chat = send csock "hello" >> recv csock 4096
        -- We have to do some initial chatting to ensure the connection has
        -- been established before we kill the server, otherwise recv can get a
        -- "connection reset by peer":
        chat
        kill
        -- killing once should just have stopped us listening, but not chatting
        connect "127.0.0.1" port undefined
            `E.catch` (\(e :: E.IOException) -> return ())
        chat
        kill
        bs <- recv csock 4096
        assertEqual "Connection not closed cleanly" "" bs
