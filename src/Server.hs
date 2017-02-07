{-# LANGUAGE OverloadedStrings #-}
module Server where

import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Concurrent (ThreadId, forkIO, forkFinally, killThread)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (atomically)
-- import Control.Exception (bracket)
import Control.Concurrent.Chan.Unagi (
    InChan, OutChan, newChan, writeChan, readChan, tryReadChan, getChanContents)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Network.Socket (
    Socket, SockAddr, PortNumber, Family(AF_INET), SocketType(Stream),
    SocketOption(ReuseAddr), SockAddr(SockAddrInet), socket, setSocketOption,
    bind, listen, accept, close, iNADDR_ANY)
import Network.Socket.ByteString (send, recv)
import Network.Simple.TCP (HostPreference(HostAny), serve)

import Pipes (
  runEffect, lift, liftIO, Proxy, Producer, yield, Consumer, await, Pipe, (>->))
import qualified Pipes.Prelude as P
import Pipes.Core (Server, respond, Client, request, (<<+))
import Pipes.Concurrent (spawn, unbounded, Input, Output, fromInput, toOutput)
import qualified Pipes.Concurrent as PC
import Pipes.Safe (SafeT, runSafeT, bracket)
import Pipes.Network.TCP (fromSocket, toSocket)

import Data.Foldable (toList)
import qualified Data.Map as Map

import Text.Printf (printf)
import qualified Data.ByteString as B
import Blaze.ByteString.Builder (toByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

data User = Alice | Bob | Charlie deriving (Eq, Ord, Show)

myServe :: IO ()
myServe = serve HostAny "1234" thing
  where
    thing (sock, addr) = do
        (inboundWrite, inboundRead) <- spawn unbounded
        (outboundWrite, outboundRead) <- spawn unbounded
        forkIO $ runEffect $ fromSocket sock 4096 >-> toOutput inboundWrite
        forkIO $ runEffect $ fromInput outboundRead >-> toSocket sock
        runEffect $ fromInput inboundRead >-> toOutput outboundWrite



-- type WriteChan a = InChan a
-- type ReadChan a = OutChan a
-- FIXME: Pipe isn't really the right name for this, as we're not using it to
-- represent the connection between things, we're using to represent the two
-- ends of a component...
-- data Pipe a b = Pipe (ReadChan a) (WriteChan b)

-- writePipe :: Pipe a b -> b -> IO ()
-- writePipe (Pipe _ w) b = writeChan w b

-- readPipe :: Pipe a b -> IO a
-- readPipe (Pipe r _) = readChan r

-- type Action a b = SockAddr -> User -> Socket -> WriteChan a -> ReadChan b -> IO ()
-- type Worker a b = Pipe a b -> IO ()
-- type ClientMap a = Map.Map SockAddr (WriteChan a)

-- serve :: Worker (User, B.ByteString) B.ByteString ->
--     Action (SockAddr, User, Maybe B.ByteString) B.ByteString -> PortNumber -> IO ()
-- serve worker action port =
--   do
--     (workerInWrite, workerInRead) <- newChan
--     (workerOutWrite, workerOutRead) <- newChan
--     clientChansRef <- newIORef mempty :: IO (IORef (ClientMap a))
--     (subsInWrite, subsInRead) <- newChan
--     (dispatcherOutWrite, dispatcherOutRead) <- newChan
--     forkIO $ subscriptionDude (Pipe subsInRead workerInWrite) (Pipe workerOutRead dispatcherOutWrite)
--     forkIO $ dispatcher dispatcherOutRead clientChansRef
--     forkIO $ worker (Pipe workerInRead workerOutWrite) -- FIXME: how do we clean up the worker?
--     bracket
--         startListening
--         stopListening
--         (handleConnections (cycle [Alice, Bob, Charlie]) clientChansRef subsInWrite)
--   where
--     startListening = do
--         sock <- socket AF_INET Stream 0
--         setSocketOption sock ReuseAddr 1  -- make socket immediately reusable - eases debugging.
--         bind sock (SockAddrInet port iNADDR_ANY)
--         listen sock 2  -- set a max of 2 queued connections  see maxListenQueue
--         return sock
--     stopListening = close
--     handleConnections (u:us) clientChansRef subsInWrite sock = do
--         (sock', addr) <- accept sock
--         putStrLn $ show addr ++ " connected"
--         (clientWrite, clientRead) <- newChan
--         registerClient addr clientWrite
--         forkFinally
--             (action addr u sock' subsInWrite clientRead)
--             (const $ unregisterClient addr)
--         handleConnections us clientChansRef subsInWrite sock
--       where
--         updateCCs = atomicUpdate clientChansRef
--         registerClient i clientWrite = updateCCs (Map.insert i clientWrite)
--         unregisterClient i = updateCCs (Map.delete i)
--     dispatcher dispatcherOutRead clientChansRef = forever $ do
--         messages <- readChan dispatcherOutRead :: IO [(SockAddr, B.ByteString)]
--         clientChans <- readIORef clientChansRef
--         putStrLn $ show $ fmap fst $ Map.toList clientChans
--         mapM (dispatch clientChans) messages
--       where
--         dispatch clientChans (u, msg) = case Map.lookup u clientChans of
--             Just chan -> writeChan chan msg
--             Nothing -> return ()

-- action :: (Show b) => Action (SockAddr, User, Maybe B.ByteString) b
-- action addr u sock inWrite outRead =
--   do
--     -- Here is where our authentication dialogue will go
--     send sock $ bytes $ printf "hello %s\n" $ show u
--     race_ (shuffleOut 0) shuffleIn
--   where
--     bytes = toByteString . fromString
--     shuffleOut i = do
--         value <- readChan outRead
--         send sock $ bytes $ show value
--         if i == 2
--             then (send sock $ bytes "bye\n") >> close sock
--             else shuffleOut (i + 1)
--     shuffleIn = do
--         byteString <- recv sock 4096
--         if B.null byteString
--             -- Client closed connection:
--             then writeChan inWrite (addr, u, Nothing) >> close sock
--             else writeChan inWrite (addr, u, Just byteString) >> shuffleIn


-- relayWorker :: Worker (User, B.ByteString) B.ByteString
-- relayWorker p = forever $ do
--     (u, value) <- readPipe p
--     writePipe p value


-- subscriptionDude ::
--     Pipe (SockAddr, User, Maybe B.ByteString) (User, B.ByteString) ->
--     Pipe B.ByteString [(SockAddr, B.ByteString)] -> IO ()
-- subscriptionDude inboundPipe outboundPipe =
--   do
--     clientChansRef <- newIORef mempty :: IO (IORef (Map.Map SockAddr ()))
--     forkIO $ inProc clientChansRef
--     outProc clientChansRef
--   where
--     inProc ccr = forever $ do
--         (addr, u, mv) <- readPipe inboundPipe
--         putStrLn $ show mv
--         case mv of
--             Just "register\n" -> atomicUpdate ccr (Map.insert addr ())
--             Just "unregister\n" -> atomicUpdate ccr (Map.delete addr)
--             Just v -> writePipe inboundPipe (u, v)
--             Nothing -> atomicUpdate ccr (Map.delete addr)
--     outProc ccr = forever $ do
--         v <- readPipe outboundPipe
--         ccs <- readIORef ccr
--         writePipe outboundPipe $ fmap (\a -> (a, v)) (Map.keys ccs)


-- atomicUpdate :: IORef a -> (a -> a) -> IO ()
-- atomicUpdate r f = atomicModifyIORef' r $ flip (,) () . f

-----------------------------------------------

-- examplePipesProxy :: (Show a) => a -> Proxy a a a a IO ()
-- examplePipesProxy input =
--   do
--     lift $ putStrLn $ "Proxy saw input " ++ (show input)
--     response <- request input
--     lift $ putStrLn $ "Proxy saw response " ++ (show response)
--     nextInput <- respond response
--     examplePipesProxy nextInput

-- stripper :: (SockAddr, User, B.ByteString) ->
--     Proxy B.ByteString B.ByteString (SockAddr, User, B.ByteString) B.ByteString
--     IO ()
-- stripper (a, u, bs) =
--   do
--     bs' <- request bs
--     next <- respond bs'
--     stripper next

-- echoServer :: B.ByteString -> Server B.ByteString B.ByteString IO ()
-- echoServer input =
--   do
--     nextInput <- respond input
--     echoServer nextInput

-- data ConnStatus = Connected (Socket, SockAddr) | Disconnected (Socket, SockAddr)

-- socketProducer :: PortNumber -> Producer (Socket, SockAddr) (SafeT IO) ()
-- socketProducer port =
--   do
--     (output, input) <- liftIO $ spawn unbounded
--     forkIO $ runEffect $
--         bracket setup stopListening acceptLoop >-> toOutput output
--     forever $ fromInput input
--   where
--     setup = do
--         sock <- socket AF_INET Stream 0
--         setSocketOption sock ReuseAddr 1  -- make socket immediately reusable - eases debugging.
--         bind sock (SockAddrInet port iNADDR_ANY)
--         listen sock 2  -- set a max of 2 queued connections  see maxListenQueue
--         return sock
--     acceptLoop sock = do
--         (sock', addr) <- liftIO $ accept sock
--         yield $ Connected (sock', addr)
--         acceptLoop sock
--     stopListening sock = liftIO $ close sock

-- socketConsumer :: (MonadIO m) => Output (SockAddr, User, B.ByteString) ->
--     Consumer (Socket, SockAddr) m ()
-- socketConsumer toRelay = loop mempty $ cycle [Alice, Bob, Charlie]
--   where
--     loop activeSocks (u:us) =
--       do
--         (sock, addr) <- await
--         let activeSocks' = Map.insert addr () activeSocks
--         liftIO $ putStrLn $ show addr ++ " connected"
--         liftIO $ putStrLn $ show activeSocks'
--         -- liftM2 race_ fromSock toSock
--         -- FIXME: It's in the pipline below that we'll actually parse incoming msgs:
--         liftIO $ forkIO $ runEffect $
--             fromSock (\bs -> (addr, u, bs)) sock >-> toOutput toRelay
--         -- lift $ putStrLn $ "I is closing " ++ (show addr)
--         -- lift $ close sock
--         loop activeSocks' us
--     fromSock wrap sock =
--       do
--         byteString <- liftIO $ recv sock 4096
--         if B.null byteString
--           then liftIO $ (close sock >> putStrLn "diconnect")
--           else do
--             yield $ wrap byteString
--             fromSock wrap sock
--     toSock sock =
--       forever $ do
--         byteString <- await
--         liftIO $ send sock byteString

-- bsToString :: Pipe B.ByteString String IO ()
-- bsToString =
--   forever $ do
--     bs <- await
--     yield $ show bs


-- pairToClient :: Input a -> Output b -> Client a b IO ()
-- pairToClient input output = loop
--   where
--     loop = do
--       ma <- liftIO $ atomically $ PC.recv input
--       case ma of
--           Nothing -> return ()
--           Just a -> do
--               resp <- request a
--               alive <- liftIO $ atomically $ PC.send output resp
--               when alive loop


-- exampleSocketPipeline :: IO ()
-- exampleSocketPipeline =
--   do
--     (inboundWrite, inboundRead) <- spawn unbounded
--     (outboundWrite, outboundRead) <- spawn unbounded
--     forkIO $ runEffect $ relayPipeline inboundRead outboundWrite
--     runSafeT $ runEffect $ listenPipeline inboundWrite
--   where
--     relayPipeline i o =
--         pairToClient i o <<+
--         stripper <<+
--         examplePipesProxy <<+
--         echoServer
--     listenPipeline o = socketProducer 1234 >-> socketConsumer o
