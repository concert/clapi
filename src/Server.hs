{-# LANGUAGE OverloadedStrings #-}
module Server where

import Control.Concurrent.Async (async, cancel, link, poll, wait, withAsync)
import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Monad (filterM, forever, forM, when, (>=>))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.IORef (atomicModifyIORef', IORef, newIORef, readIORef)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import qualified Network.Socket as NS
import Network.Simple.TCP (HostPreference(HostAny), bindSock)

import Pipes (await, Consumer, Pipe, runEffect, yield, (>->))
import Pipes.Core (Client, Proxy, request, respond, Server, (>>~))
import qualified Pipes.Prelude as PP
import qualified Pipes.Safe as PS
import Pipes.Concurrent (fromInput, Input, Output, spawn', toOutput, unbounded)
import qualified Pipes.Concurrent as PC
import Pipes.Network.TCP (fromSocket, toSocket)


import qualified Data.ByteString as B

data User = Alice | Bob | Charlie deriving (Eq, Ord, Show)

swallowExc :: a -> E.SomeException -> a
swallowExc = const


withListen :: HostPreference -> NS.ServiceName ->
    ((NS.Socket, NS.SockAddr) -> IO r) -> IO r
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


serve' :: NS.Socket -> ((NS.Socket, NS.SockAddr) -> IO r) -> IO r
serve' listenSock handler = E.mask_ $ loop []
  where
    loop as =
      do
        as' <- doubleCatch
            (throwAfter $ mapM wait as)
            (mapM cancel as) (do
                x@(sock, addr) <- NS.accept listenSock
                a <- async (handler x `E.finally` NS.close sock)
                filterM (poll >=> return . isNothing) (a:as))
        loop as'


myServe :: NS.ServiceName -> IO ()
myServe port = withListen HostAny port (\(listenSock, _) ->
    let s = socketServer authentication (PP.take 3) listenSock in
    PS.runSafeT $ runEffect $
    s >>~ subscriptionRegistrar >>~ examplePipesProxy >>~ stripper >>~ echoServer)


socketServer ::
  Pipe (NS.SockAddr, B.ByteString) a IO () ->
  Pipe b B.ByteString IO () ->
  NS.Socket ->
  Server [(NS.SockAddr, b)] a (PS.SafeT IO) ()
socketServer inboundPipe outboundPipe listenSock = PS.mask $ \restore ->
  do
    connectedR <- liftIO $ newIORef mempty
    (relayOutWrite, relayOutRead, sealOut) <- liftIO $ spawn' unbounded
    (relayInWrite, relayInRead, sealIn) <- liftIO $ spawn' unbounded
    as1 <- liftIO $ async $
        serve' listenSock $ socketHandler relayInWrite connectedR
    liftIO $ link as1
    as2 <- liftIO $ async $
        runEffect $ fromInput relayOutRead >-> dispatch connectedR
    restore $ pairToServer relayOutWrite relayInRead
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
        IORef (Map.Map NS.SockAddr (Output a)) ->
        Consumer [(NS.SockAddr, a)] IO ()
    dispatch connectedR = do
        msgs <- await
        connected <- liftIO $ readIORef connectedR
        forM msgs $ \(addr, bs) ->
            case Map.lookup addr connected of
                Just out -> liftIO $ atomically $ PC.send out bs
                Nothing -> return True
        dispatch connectedR


authentication ::
    Pipe (NS.SockAddr, B.ByteString) (NS.SockAddr, User, B.ByteString) IO ()
authentication = forever $ do
    (addr, bs) <- await
    yield (addr, Alice, bs)


subscriptionRegistrar ::
    (Monad m) =>
    (NS.SockAddr, User, B.ByteString) ->
    Proxy [(NS.SockAddr, B.ByteString)] (NS.SockAddr, User, B.ByteString)
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
