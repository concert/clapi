{-# LANGUAGE OverloadedStrings #-}
module Server where

import Control.Monad (forever, when, filterM, liftM, (>=>))
import Control.Monad.IO.Class (MonadIO)
import Control.Concurrent (ThreadId, forkIO, forkFinally, killThread, threadDelay)
import Control.Concurrent.Async (Async, race_, async, wait, cancel, poll, link, withAsync)
import Control.Concurrent.STM (STM, atomically)
import qualified Control.Exception as E
import Control.Concurrent.Chan.Unagi (
    InChan, OutChan, newChan, writeChan, readChan, tryReadChan, getChanContents)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Traversable (forM)
import Data.Maybe (isNothing)
import Network.Socket (
    Socket, SockAddr, PortNumber--, Family(AF_INET), SocketType(Stream),
    -- SocketOption(ReuseAddr), SockAddr(SockAddrInet), socket, setSocketOption,
    -- bind, listen, accept, close, iNADDR_ANY
    )
import qualified Network.Socket as NS
import Network.Socket.ByteString (send, recv)
import Network.Simple.TCP (HostPreference(HostAny), bindSock)

import Pipes (
  runEffect, lift, liftIO, Proxy, Producer, yield, Consumer, await, Pipe, (>->), cat)
import qualified Pipes.Prelude as PP
import Pipes.Core (Server, respond, Client, request, (>>~))
import Pipes.Concurrent (
  spawn', withSpawn, unbounded, Input, Output, fromInput, toOutput, performGC)
import qualified Pipes.Concurrent as PC
import Pipes.Safe (SafeT, MonadSafe, runSafeT)
import qualified Pipes.Safe as PS
import Pipes.Network.TCP (fromSocket, toSocket)

import Data.Foldable (toList)
import qualified Data.Map as Map

import Text.Printf (printf)
import qualified Data.ByteString as B
import Blaze.ByteString.Builder (toByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

data User = Alice | Bob | Charlie deriving (Eq, Ord, Show)

swallowExc :: a -> E.SomeException -> a
swallowExc = const


withListen :: HostPreference -> NS.ServiceName ->
    ((Socket, SockAddr) -> IO r) -> IO r
withListen hp port action = E.mask $ \restore -> do
    -- The addr returned by bindSock is useless when we bind "0":
    (lsock, _) <- bindSock hp port
    NS.listen lsock $ max 2048 NS.maxListenQueue
    addr <- NS.getSocketName lsock
    a <- async $ action (lsock, addr)
    restore $ doubleCatch
       (swallowExc $ NS.close lsock >> wait a)
       (cancel a)
       (wait a >>= \r -> NS.close lsock >> return r)

doubleCatch :: (E.Exception e) => (e -> IO a) -> IO b -> IO a -> IO a
doubleCatch softHandle hardHandle action =
    action `E.catch` (\e -> (softHandle e) `E.onException` hardHandle)


throwAfter :: IO a -> E.SomeException -> IO b
throwAfter action e = action >> E.throwIO e


serve' :: Socket -> ((Socket, SockAddr) -> IO r) -> IO r
serve' listenSock handler = E.mask_ $ loop []
  where
    loop as =
      do
        as' <- doubleCatch
            (throwAfter $ putStrLn softMsg >> mapM wait as)
            (mapM cancel as) (do
                x@(sock, addr) <- NS.accept listenSock
                a <- async (handler x `E.finally` NS.close sock)
                filterM (poll >=> return . isNothing) (a:as))
        loop as'
    softMsg = "Waiting for handlers to exit cleanly. Press Ctrl+C to terminate forcefully"


myServe :: NS.ServiceName -> IO ()
myServe port = withListen HostAny port (\(listenSock, _) ->
    let s = socketServer authentication (PP.take 3) listenSock in
    runSafeT $ runEffect $
    s >>~ subscriptionRegistrar >>~ examplePipesProxy >>~ stripper >>~ echoServer)


socketServer ::
  Pipe (SockAddr, B.ByteString) a IO () ->
  Pipe b B.ByteString IO () ->
  Socket ->
  Server [(SockAddr, b)] a (SafeT IO) ()
socketServer inboundPipe outboundPipe listenSock =
  do
    connectedR <- liftIO $ newIORef mempty
    (relayOutWrite, relayOutRead, sealOut) <- liftIO $ spawn' unbounded
    (relayInWrite, relayInRead, sealIn) <- liftIO $ spawn' unbounded
    as1 <- liftIO $ async $
        serve' listenSock $ socketHandler relayInWrite connectedR
    liftIO $ link as1
    as2 <- liftIO $ async $
        runEffect $ fromInput relayOutRead >-> dispatch connectedR
    pairToServer relayOutWrite relayInRead
      `PS.finally` (liftIO $
         cancel as1 >> atomically sealOut >> atomically sealIn >> wait as2)
  where
    socketHandler relayInWrite connectedR (sock, addr) = do
        (outboundWrite, outboundRead, seal) <- spawn' unbounded
        E.bracket_
            (atomicUpdate connectedR $ Map.insert addr outboundWrite)
            (atomicUpdate connectedR $ Map.delete addr)
            -- We use withAsync here to make sure that if our outbound pipeline
            -- terminates or is killed we will also terminate our inbound thread
            (withAsync
                (do
                    runEffect $
                        fromSocket sock 4096 >->
                        PP.map ((,) addr) >->
                        inboundPipe >->
                        toOutput relayInWrite
                    -- We seal so that if the client closes their connection we
                    -- correctly terminate the outbound pipeline too
                    atomically seal)
                (const $
                    runEffect $
                        fromInput outboundRead >->
                        outboundPipe >->
                        toSocket sock))
    dispatch ::
        IORef (Map.Map SockAddr (Output a)) ->
        Consumer [(SockAddr, a)] IO ()
    dispatch connectedR = do
        msgs <- await
        connected <- liftIO $ readIORef connectedR
        forM msgs $ \(addr, bs) ->
            case Map.lookup addr connected of
                Just out -> liftIO $ atomically $ PC.send out bs
                Nothing -> return True
        dispatch connectedR


authentication ::
    Pipe (SockAddr, B.ByteString) (SockAddr, User, B.ByteString) IO ()
authentication = forever $ do
    (addr, bs) <- await
    yield (addr, Alice, bs)


subscriptionRegistrar ::
    (Monad m) =>
    (SockAddr, User, B.ByteString) ->
    Proxy [(SockAddr, B.ByteString)] (SockAddr, User, B.ByteString)
    B.ByteString (User, B.ByteString) m ()
subscriptionRegistrar = loop mempty
  where
    loop registered (a, u, bs) =
      do
        let registered' = case bs of
              "subscribe\n" -> Map.insert a () registered
              "unsubscribe\n" -> Map.delete a registered
              _ -> registered
        bs' <- respond (u, bs)
        req <- request $ Map.toList $ fmap (const $ bs') registered'
        loop registered' req

atomicUpdate :: IORef a -> (a -> a) -> IO ()
atomicUpdate r f = atomicModifyIORef' r $ flip (,) () . f

examplePipesProxy :: (Show a, Show b, MonadIO m) => b -> Proxy a b a b m ()
examplePipesProxy input =
  do
    liftIO $ putStrLn $ "Proxy saw input " ++ (show input)
    req <- respond input
    liftIO $ putStrLn $ "Proxy saw response " ++ (show req)
    nextInput <- request req
    examplePipesProxy nextInput

stripper :: (Monad m) => (User, B.ByteString) ->
    Proxy B.ByteString (User, B.ByteString) B.ByteString B.ByteString m ()
stripper (u, bs) =
  do
    bs' <- respond bs
    next <- request $ bs'
    stripper next

echoServer :: (Monad m) => B.ByteString -> Client B.ByteString B.ByteString m ()
echoServer input =
  do
    nextInput <- request input
    echoServer nextInput


pairToClient :: Input a -> Output b -> Client a b IO ()
pairToClient input output = loop
  where
    loop = do
      ma <- liftIO $ atomically $ PC.recv input
      case ma of
          Nothing -> return ()
          Just a -> do
              resp <- request a
              alive <- liftIO $ atomically $ PC.send output resp
              when alive loop


pairToServer :: (MonadIO m) => Output a -> Input b -> Server a b m ()
pairToServer output input = loop
  where
    loop = do
        mb <- liftIO $ atomically $ PC.recv input
        case mb of
            Nothing -> return ()
            Just b -> do
                a <- respond b
                alive <- liftIO $ atomically $ PC.send output a
                when alive loop
