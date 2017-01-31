{-# LANGUAGE OverloadedStrings #-}
module Server where

import Control.Monad (forever)
import Control.Concurrent (ThreadId, forkIO, forkFinally, killThread)
import Control.Concurrent.Async (race_)
import Control.Exception (bracket)
import Control.Concurrent.Chan.Unagi (
    InChan, OutChan, newChan, writeChan, readChan, tryReadChan, getChanContents)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Network.Socket (
    Socket, SockAddr, PortNumber, Family(AF_INET), SocketType(Stream),
    SocketOption(ReuseAddr), SockAddr(SockAddrInet), socket, setSocketOption,
    bind, listen, accept, close, iNADDR_ANY)
import Network.Socket.ByteString (send, recv)

import Data.Foldable (toList)
import qualified Data.Map as Map

import Text.Printf (printf)
import qualified Data.ByteString as B
import Blaze.ByteString.Builder (toByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

type WriteChan a = InChan a
type ReadChan a = OutChan a
-- FIXME: Pipe isn't really the right name for this, as we're not using it to
-- represent the connection between things, we're using to represent the two
-- ends of a component...
data Pipe a b = Pipe (ReadChan a) (WriteChan b)

writePipe :: Pipe a b -> b -> IO ()
writePipe (Pipe _ w) b = writeChan w b

readPipe :: Pipe a b -> IO a
readPipe (Pipe r _) = readChan r

data User = Alice | Bob | Charlie deriving (Eq, Ord, Show)

type Action a b = SockAddr -> User -> Socket -> WriteChan a -> ReadChan b -> IO ()
type Worker a b = Pipe a b -> IO ()
type ClientMap a = Map.Map SockAddr (WriteChan a)

serve :: Worker (User, B.ByteString) B.ByteString ->
    Action (SockAddr, User, Maybe B.ByteString) B.ByteString -> PortNumber -> IO ()
serve worker action port =
  do
    (workerInWrite, workerInRead) <- newChan
    (workerOutWrite, workerOutRead) <- newChan
    clientChansRef <- newIORef mempty :: IO (IORef (ClientMap a))
    (subsInWrite, subsInRead) <- newChan
    (dispatcherOutWrite, dispatcherOutRead) <- newChan
    forkIO $ subscriptionDude (Pipe subsInRead workerInWrite) (Pipe workerOutRead dispatcherOutWrite)
    forkIO $ dispatcher dispatcherOutRead clientChansRef
    forkIO $ worker (Pipe workerInRead workerOutWrite) -- FIXME: how do we clean up the worker?
    bracket
        startListening
        stopListening
        (handleConnections (cycle [Alice, Bob, Charlie]) clientChansRef subsInWrite)
  where
    startListening = do
        sock <- socket AF_INET Stream 0
        setSocketOption sock ReuseAddr 1  -- make socket immediately reusable - eases debugging.
        bind sock (SockAddrInet port iNADDR_ANY)
        listen sock 2  -- set a max of 2 queued connections  see maxListenQueue
        return sock
    stopListening = close
    handleConnections (u:us) clientChansRef subsInWrite sock = do
        (sock', addr) <- accept sock
        putStrLn $ show addr ++ " connected"
        (clientWrite, clientRead) <- newChan
        registerClient addr clientWrite
        forkFinally
            (action addr u sock' subsInWrite clientRead)
            (const $ unregisterClient addr)
        handleConnections us clientChansRef subsInWrite sock
      where
        updateCCs = atomicUpdate clientChansRef
        registerClient i clientWrite = updateCCs (Map.insert i clientWrite)
        unregisterClient i = updateCCs (Map.delete i)
    dispatcher dispatcherOutRead clientChansRef = forever $ do
        messages <- readChan dispatcherOutRead :: IO [(SockAddr, B.ByteString)]
        clientChans <- readIORef clientChansRef
        putStrLn $ show $ fmap fst $ Map.toList clientChans
        mapM (dispatch clientChans) messages
      where
        dispatch clientChans (u, msg) = case Map.lookup u clientChans of
            Just chan -> writeChan chan msg
            Nothing -> return ()

action :: (Show b) => Action (SockAddr, User, Maybe B.ByteString) b
action addr u sock inWrite outRead =
  do
    -- Here is where our authentication dialogue will go
    send sock $ bytes $ printf "hello %s\n" $ show u
    race_ (shuffleOut 0) shuffleIn
  where
    bytes = toByteString . fromString
    shuffleOut i = do
        value <- readChan outRead
        send sock $ bytes $ show value
        if i == 2
            then (send sock $ bytes "bye\n") >> close sock
            else shuffleOut (i + 1)
    shuffleIn = do
        byteString <- recv sock 4096
        if B.null byteString
            -- Client closed connection:
            then writeChan inWrite (addr, u, Nothing) >> close sock
            else writeChan inWrite (addr, u, Just byteString) >> shuffleIn


relayWorker :: Worker (User, B.ByteString) B.ByteString
relayWorker p = forever $ do
    (u, value) <- readPipe p
    writePipe p value


subscriptionDude ::
    Pipe (SockAddr, User, Maybe B.ByteString) (User, B.ByteString) ->
    Pipe B.ByteString [(SockAddr, B.ByteString)] -> IO ()
subscriptionDude inboundPipe outboundPipe =
  do
    clientChansRef <- newIORef mempty :: IO (IORef (Map.Map SockAddr ()))
    forkIO $ inProc clientChansRef
    outProc clientChansRef
  where
    inProc ccr = forever $ do
        (addr, u, mv) <- readPipe inboundPipe
        putStrLn $ show mv
        case mv of
            Just "register\n" -> atomicUpdate ccr (Map.insert addr ())
            Just "unregister\n" -> atomicUpdate ccr (Map.delete addr)
            Just v -> writePipe inboundPipe (u, v)
            Nothing -> atomicUpdate ccr (Map.delete addr)
    outProc ccr = forever $ do
        v <- readPipe outboundPipe
        ccs <- readIORef ccr
        writePipe outboundPipe $ fmap (\a -> (a, v)) (Map.keys ccs)


atomicUpdate :: IORef a -> (a -> a) -> IO ()
atomicUpdate r f = atomicModifyIORef' r $ flip (,) () . f
