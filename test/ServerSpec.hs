{-# LANGUAGE
    LambdaCase
  , OverloadedStrings
#-}

module ServerSpec where

import Prelude hiding (words)

import Test.Hspec
import Test.Hspec.Expectations (Selector)

import Control.Monad (forever, void)
import Data.Void
import System.Timeout
import qualified Control.Exception as E
import Control.Concurrent (threadDelay, killThread)
import Control.Concurrent.Async
  ( Async, async, withAsync, wait, poll, cancel, asyncThreadId, mapConcurrently
  , replicateConcurrently)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Network.Socket as NS
import Network.Socket.ByteString (send, recv)
import Network.Simple.TCP (HostPreference(HostAny), connect)

import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Server
  ( ClientEvent', ServerEvent', throwAfter, doubleCatch, swallowExc, withListen
  , serve', protocolServer)
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Helpers (seconds, timeLimit)
import qualified Control.Concurrent.Chan.Unagi as Q

data TestException = TestException Int deriving (Show, Eq)
instance E.Exception TestException

assertAsyncRunning :: Async a -> IO ()
assertAsyncRunning a = poll a >>= check
  where
    check Nothing = return ()
    check _ = error "Shouldn't have finished yet!"

throwAfterCheck :: E.Exception a => IO () -> Selector a -> IO ()
throwAfterCheck threadAction excSelector = do
  trigger <- newEmptyMVar
  resp <- newEmptyMVar
  a <- async $ throwAfter
    (E.toException $ TestException 0)
    (putMVar resp 'a' >> takeMVar trigger >> threadAction)
  _ <- takeMVar resp
  assertAsyncRunning a
  putMVar trigger 'b'
  wait a `shouldThrow` excSelector

spec :: Spec
spec = do
    describe "Listen" $ do
        it "Zero gives port" $ do
            port <- withListen' (return . getPort . snd)
            port `shouldSatisfy` (/= 0)

    describe "throwAfter" $ do
        it "should only throw its given exc after completing the action" $ do
          throwAfterCheck (return ()) (\(TestException _) -> True)

        it "should not throw its given exc if the action throws" $ do
          throwAfterCheck (E.throwIO $ E.toException $ TestException 0)
            (\(TestException n) -> n == 0)

    describe "doubleCatch" $ do
        it "Thread terminated on second kill" $ do
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
                result `shouldBe` ["body", "die", "soft", "die", "hard"]
                assertAsyncKilled a
        it "Returns value on success" $ do
            rv <- doubleCatch (swallowExc undefined) undefined (return 42)
            rv `shouldBe` 42
        it "Triggers soft kill first time" $ do
            rv <- doubleCatch (swallowExc $ return 42) undefined undefined
            rv `shouldBe` 42
    describe "Server" $ do
        it "Waits for children if socket is closed" $
          killServerHelper connector0 handler0 assertAsyncRunning
        it "Kills handlers reveives kill" $
          killServerHelper connector1 handler1 assertAsyncKilled
    describe "Socket closes" $ do
        it "On termination" $ socketCloseTest return
        it "On error" $ socketCloseTest $ error "part of test"
    it "Handles multiple connections" $ testMultipleConnections 42
    describe "protocolServer" $ do
        it "Echoes" $ withListen' $ \(lsock, laddr) ->
          let
             client word = connect "127.0.0.1" (show $ getPort laddr) $ \(csock, _) ->
               send csock word >> recv csock 4096
          in
            withAsync (protocolServer lsock getCat echo (return ())) $ \_ ->
              do
                receivedWords <- mapConcurrently client words
                receivedWords `shouldBe` words
        it "Closes gracefully" $ do
            addrV <- newEmptyMVar
            a <- async $ withListen' $ \(lsock, laddr) -> do
                putMVar addrV laddr
                protocolServer lsock getCat echo (putMVar addrV laddr)
            let kill = killThread (asyncThreadId a)
            port <- show . getPort <$> takeMVar addrV
            timeLimit 0.2 $ connect "127.0.0.1" port $ \(csock, _) -> do
                let chat = void $ send csock "hello" >> recv csock 4096
                -- We have to do some initial chatting to ensure the connection has
                -- been established before we kill the server, otherwise recv can get a
                -- "connection reset by peer":
                chat
                kill
                _ <- takeMVar addrV
                -- killing once should just have stopped us listening, but not chatting
                connect "127.0.0.1" port undefined
                    `E.catch` (\(_e :: E.IOException) -> return ())
                chat
                kill
                bs <- recv csock 4096
                bs `shouldBe` ""
  where
    waitAges = threadDelay (seconds 100)
    readToList o stopAt l = do
      item <- Q.readChan o
      if item == stopAt
        then return . reverse $ item : l
        else readToList o stopAt $ item : l
    killLoop m i tId = forever $
      takeMVar m >> Q.writeChan i "die" >> killThread tId
    assertAsyncKilled a = do
        rv <- timeout (seconds 0.1) (E.try $ wait a)
        rv `shouldBe` (Just $ Left E.ThreadKilled)
    killServerHelper connector handler runCheck =
      withListen' $ \(lsock, laddr) -> do
        v <- newEmptyMVar
        withServe lsock (\(hsock, _) -> handler v hsock) $ \a -> do
          _ <- connect "127.0.0.1" (show . getPort $ laddr) $
            \(csock, _) -> connector a csock
          _ <- timeLimit 0.1 (takeMVar v)
          runCheck a
    connector0 _ csock = recv csock 4096 >> send csock "bye"
    handler0 v hsock =
        send hsock "hello" >> recv hsock 4096 >> NS.close hsock >> putMVar v ()
    connector1 a csock = recv csock 4096 >> killThread (asyncThreadId a)
    handler1 v hsock = E.catch
        (send hsock "hello" >> threadDelay (seconds 1))
        (\E.ThreadKilled -> putMVar v ())
    socketCloseTest handler = withServe' handler $ \port-> do
        mbs <- timeout (seconds 2) $
            connect "127.0.0.1" port
                (\(csock, _) -> recv csock 4096)
        mbs `shouldBe` (Just "")
    testMultipleConnections n = withServe'
        (\(hsock, _) -> send hsock "hello") $
        \port -> do
            res <- replicateConcurrently n $ timeout (seconds 2) $
                connect "127.0.0.1" port (\(csock, _) -> recv csock 4096)
            replicate n (Just "hello") `shouldBe` res
    words = ["hello", "world", "llama", "train"]

getPort :: NS.SockAddr -> NS.PortNumber
getPort (NS.SockAddrInet port _) = port
getPort (NS.SockAddrInet6 port _ _ _) = port
getPort _ = error "Cannot get port for given addr"

withListen' :: ((NS.Socket, NS.SockAddr) -> IO r) -> IO r
withListen' = withListen (pure ()) (pure ()) HostAny "0"

withServe
  :: NS.Socket
  -> ((NS.Socket, NS.SockAddr) -> IO r)
  -> (Async () -> IO c)
  -> IO c
withServe lsock handler = E.bracket (async $ serve' lsock handler (return ())) cancel

withServe' :: ((NS.Socket, NS.SockAddr) -> IO r) -> (String -> IO a) -> IO a
withServe' handler io =
    withListen' $ \(lsock, laddr) ->
        withServe lsock handler $ \_ ->
            io (show . getPort $ laddr)

cat :: (Monad m) => Protocol a a b b m ()
cat = forever $ waitThen sendFwd sendRev

echo :: (Monad m) =>
  Protocol
    (ClientEvent' a)
    Void
    (ServerEvent' a)
    Void m ()
echo = forever $ waitThen boing undefined
  where
    boing (ClientData addr a) = sendRev (ServerData addr a)
    boing _ = return ()

getCat :: Monad m => a -> (String, a, Protocol x x y y m ())
getCat addr = ("For Test", addr, cat)
