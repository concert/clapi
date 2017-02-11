{-# LANGUAGE OverloadedStrings #-}
module Server where

import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Concurrent (ThreadId, forkIO, forkFinally, killThread, threadDelay)
import Control.Concurrent.Async (race_, async, wait, cancel, link, withAsync)
import Control.Concurrent.STM (STM, atomically)
import qualified Control.Exception as E
import Control.Concurrent.Chan.Unagi (
    InChan, OutChan, newChan, writeChan, readChan, tryReadChan, getChanContents)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Traversable (forM)
import Network.Socket (
    Socket, SockAddr, PortNumber--, Family(AF_INET), SocketType(Stream),
    -- SocketOption(ReuseAddr), SockAddr(SockAddrInet), socket, setSocketOption,
    -- bind, listen, accept, close, iNADDR_ANY
    )
import qualified Network.Socket as NS
import Network.Socket.ByteString (send, recv)
import Network.Simple.TCP (HostPreference(HostAny), serve, listen)

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

serve' ::
    HostPreference -> NS.ServiceName -> ((Socket, SockAddr) -> IO ()) -> IO ()
serve' hp port f =
  do
    v <- newEmptyMVar
    listen hp port $ \(listenSock,_) -> do
      a <- async $ do
        a <- takeMVar v
        forever $ do
            (sock, addr) <- NS.accept listenSock
            forkFinally
                (link a >> f (sock, addr))
                (const $ NS.close sock)
      putMVar v a
      wait a


myServe :: IO ()
myServe = runSafeT $ runEffect $
    let s = socketServer authentication (PP.take 3) HostAny "1234" in
    s >>~ subscriptionRegistrar >>~ examplePipesProxy >>~ stripper >>~ echoServer


socketServer ::
  Pipe (SockAddr, B.ByteString) a IO () ->
  Pipe b B.ByteString IO () ->
  HostPreference -> NS.ServiceName ->
  Server [(SockAddr, b)] a (SafeT IO) ()
socketServer inboundPipe outboundPipe hp port =
  do
    connectedR <- liftIO $ newIORef mempty
    (relayOutWrite, relayOutRead, sealOut) <- liftIO $ spawn' unbounded
    (relayInWrite, relayInRead, sealIn) <- liftIO $ spawn' unbounded
    as1 <- liftIO $ async $
        serve' hp port $ socketHandler relayInWrite connectedR
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
