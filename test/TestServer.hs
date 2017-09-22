{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module TestServer where

import Test.HUnit (assertEqual, assertBool)
import Test.Framework.Providers.HUnit (testCase)

import Data.Either (isRight)
import Data.Maybe (isJust, fromJust)
import System.Timeout
import Control.Exception (AsyncException(ThreadKilled))
import qualified Control.Exception as E
import Control.Concurrent (threadDelay, killThread)
import Control.Concurrent.Async (
    async, withAsync, wait, cancel, asyncThreadId, mapConcurrently,
    replicateConcurrently)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever)
import qualified Network.Socket as NS
import Network.Socket.ByteString (send, recv)
import Network.Simple.TCP (HostPreference(HostAny), connect)

import Pipes (runEffect, cat)
import Pipes.Core (Client, request, respond, (>>~))
import qualified Pipes.Prelude as PP
import Pipes.Safe (runSafeT)

import qualified Control.Concurrent.Chan.Unagi as Q

import Server (doubleCatch, swallowExc, withListen, serve', socketServer)


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
    testCase "socketServer echo" testSocketServerBasicEcho,
    testCase "socketServer graceful interrupt" testSocketServerClosesGracefully
    ]

seconds n = truncate $ n * 1e6

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

timeLimit :: IO a -> IO a
timeLimit action = (timeout (seconds 0.1) action) >>= \r ->
    assertBool "timed out" (isJust r) >> return (fromJust r)

testDoubleCatchKills = do
    (i, o) <- Q.newChan
    dieLock <- newEmptyMVar
    a <- async $ doubleCatch
      (swallowExc $
        Q.writeChan i "soft" >> putMVar dieLock () >> waitAges >> return ())
      (Q.writeChan i "hard")
      (Q.writeChan i "body" >> putMVar dieLock () >> waitAges :: IO ())
    withAsync (killLoop dieLock i $ asyncThreadId a) $ \_ ->
      do
        result <- readToList o "hard" []
        assertEqual "bloop" ["body", "die", "soft", "die", "hard"] result
        assertAsyncKilled a
  where
    waitAges = threadDelay (seconds 100)
    readToList o stopAt l = do
      item <- Q.readChan o
      if item == stopAt
        then return . reverse $ item : l
        else readToList o stopAt $ item : l
    killLoop m i tId = forever $
      takeMVar m >> Q.writeChan i "die" >> killThread tId

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
        timeLimit (takeMVar v)
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
    mbs <- timeout (seconds 1) $
        connect "127.0.0.1" port
            (\(csock, _) -> recv csock 4096)
    assertEqual "didn't get closed" (Just "") mbs

testHandlerTerminationClosesSocket = socketCloseTest return
testHandlerErrorClosesSocket = socketCloseTest $ error "part of test"


testMultipleConnections n = withServe' handler $ \port ->
  do
    res <- replicateConcurrently n $ timeout (seconds 1) $
        connect "127.0.0.1" port (\(csock, _) -> recv csock 4096)
    assertEqual "bad thread data" res $ replicate n (Just "hello")
  where
    handler (hsock, _) = send hsock "hello"

echoMap :: (Monad m) => (a -> b) -> a -> Client b a m r
echoMap f input = request (f input) >>= echoMap f

testSocketServerBasicEcho = withListen' $ \(lsock, laddr) ->
  let
    s = socketServer cat cat lsock
    client word = connect "127.0.0.1" (show $ getPort laddr) $ \(csock, _) ->
        send csock word >> recv csock 4096
  in
    withAsync (runSafeT $ runEffect $ s >>~ echoMap pure) $ \_ ->
      do
        receivedWords <- mapConcurrently client words
        assertEqual "received words" words receivedWords
  where
    words = ["hello", "world", "llama", "train"]


testSocketServerClosesGracefully =
  do
    addrV <- newEmptyMVar
    a <- async $ withListen' $ \(lsock, laddr) -> E.mask $ \restore -> do
        let s = socketServer cat cat lsock
        putMVar addrV laddr
        restore $ runSafeT $ runEffect $ s >>~ echoMap pure
    let kill = killThread (asyncThreadId a)
    port <- show . getPort <$> takeMVar addrV
    timeLimit $ connect "127.0.0.1" port $ \(csock, _) -> do
        let chat = send csock "hello" >> recv csock 4096
        -- We have to do some initial chatting to ensure the connection has
        -- been established before we kill the server, otherwise recv can get a
        -- "connection reset by peer":
        chat
        kill
        -- killing once should just have stopped us listening
        chat
        connect "127.0.0.1" port undefined
            `E.catch` (\(e :: E.IOException) -> return ())
        kill
        bs <- recv csock 4096
        assertEqual "Connection not closed cleanly" "" bs
