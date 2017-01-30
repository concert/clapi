{-# LANGUAGE OverloadedStrings #-}
module Server where

import Control.Monad (forever)
import Control.Concurrent (ThreadId, forkIO, forkFinally, killThread)
import Control.Exception (bracket)
import Control.Concurrent.Chan.Unagi (
    InChan, OutChan, newChan, writeChan, readChan, tryReadChan, getChanContents)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Network.Socket (
    Socket, PortNumber, Family(AF_INET), SocketType(Stream),
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

type Action a b = Int -> Socket -> WriteChan a -> ReadChan b -> IO ()
type Worker a b = Pipe a b -> IO ()
type ClientMap a = Map.Map Int (WriteChan a)

serve :: Worker (Int, B.ByteString) B.ByteString ->
    Action (Int, Maybe B.ByteString) B.ByteString -> PortNumber -> IO ()
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
        (handleConnections [1..] clientChansRef subsInWrite)
  where
    startListening = do
        sock <- socket AF_INET Stream 0
        setSocketOption sock ReuseAddr 1  -- make socket immediately reusable - eases debugging.
        bind sock (SockAddrInet port iNADDR_ANY)
        listen sock 2  -- set a max of 2 queued connections  see maxListenQueue
        return sock
    stopListening = close
    handleConnections (i:is) clientChansRef subsInWrite sock = do
        (sock', addr) <- accept sock
        putStrLn $ show addr ++ " connected"
        (clientWrite, clientRead) <- newChan
        registerClient i clientWrite
        forkFinally
            (action i sock' subsInWrite clientRead)
            (const $ unregisterClient i)
        handleConnections is clientChansRef subsInWrite sock
      where
        updateCCs = atomicUpdate clientChansRef
        registerClient i clientWrite = updateCCs (Map.insert i clientWrite)
        unregisterClient i = updateCCs (Map.delete i)
    dispatcher dispatcherOutRead clientChansRef = forever $ do
        messages <- readChan dispatcherOutRead :: IO [(Int, B.ByteString)]
        clientChans <- readIORef clientChansRef
        putStrLn $ show $ fmap fst $ Map.toList clientChans
        mapM (dispatch clientChans) messages
      where
        dispatch clientChans (i, msg) = case Map.lookup i clientChans of
            Just chan -> writeChan chan msg
            Nothing -> return ()

action :: (Show b) => Action (Int, Maybe B.ByteString) b
action i sock inWrite outRead =
  do
    -- Here is where our authentication dialogue will go
    send sock $ bytes $ printf "hello %v\n" i
    (outId, outMVar) <- forkMVar $ shuffleOut 0
    (_, inMVar) <- forkMVar $ shuffleIn outId
    joinMVars [outMVar, inMVar]
  where
    bytes = toByteString . fromString
    shuffleOut i = do
        value <- readChan outRead
        send sock $ bytes $ show value
        if i == 2
            then (send sock $ bytes "bye\n") >> close sock
            else shuffleOut (i + 1)
    shuffleIn outThreadId = do
        byteString <- recv sock 4096
        if B.null byteString
            -- Client closed connection:
            then writeChan inWrite (i, Nothing) >> close sock >>  killThread outThreadId
            else writeChan inWrite (i, Just byteString) >> shuffleIn outThreadId


subscriptionDude :: Pipe (Int, Maybe B.ByteString) (Int, B.ByteString) ->
    Pipe B.ByteString [(Int, B.ByteString)] -> IO ()
subscriptionDude inboundPipe outboundPipe =
  do
    clientChansRef <- newIORef mempty :: IO (IORef (Map.Map Int ()))
    forkIO $ inProc clientChansRef
    outProc clientChansRef
  where
    inProc ccr = forever $ do
        (i, mv) <- readPipe inboundPipe
        putStrLn $ show mv
        case mv of
            Just "register\n" -> atomicUpdate ccr (Map.insert i ())
            Just "unregister\n" -> atomicUpdate ccr (Map.delete i)
            Just v -> writePipe inboundPipe (i, v)
            Nothing -> atomicUpdate ccr (Map.delete i)
    outProc ccr = forever $ do
        v <- readPipe outboundPipe
        ccs <- readIORef ccr
        writePipe outboundPipe $ fmap (\i -> (i, v)) (Map.keys ccs)


forkMVar :: IO () -> IO (ThreadId, MVar ())
forkMVar action = do
    mvar <- newEmptyMVar
    threadId <- forkFinally action (const $ putMVar mvar ())
    return (threadId, mvar)

joinMVars :: [MVar a] -> IO ()
joinMVars mvars = (mapM takeMVar mvars) >> return ()

forkAndJoin :: [IO ()] -> IO ()
forkAndJoin actions =
  do
    mvars <- (fmap . fmap) snd $ mapM forkMVar actions
    joinMVars mvars


atomicUpdate :: IORef a -> (a -> a) -> IO ()
atomicUpdate r f = atomicModifyIORef' r $ flip (,) () . f
