module Server where

import Control.Monad (forever)
import Control.Concurrent (ThreadId, forkIO, forkFinally)
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

type Action a b = Int -> Socket -> WriteChan a -> ReadChan b -> IO ()

serve :: Action (Int, B.ByteString) B.ByteString -> PortNumber -> IO ()
serve action port =
  do
    (workerInWrite, workerInRead) <- newChan
    clientChansRef <- newIORef mempty :: IO (IORef (Map.Map Int (WriteChan a)))
    (dispatcherInWrite, dispatcherInRead) <- newChan
    forkIO $ dispatcher dispatcherInRead clientChansRef
    forkIO $ worker clientChansRef workerInRead dispatcherInWrite -- FIXME: how do we clean up the worker?
    bracket
        startListening
        stopListening
        (handleConnections [1..] clientChansRef workerInWrite)
  where
    startListening = do
        sock <- socket AF_INET Stream 0
        setSocketOption sock ReuseAddr 1  -- make socket immediately reusable - eases debugging.
        bind sock (SockAddrInet port iNADDR_ANY)
        listen sock 2  -- set a max of 2 queued connections  see maxListenQueue
        return sock
    stopListening = close
    handleConnections (i:is) clientChansRef workerInWrite sock = do
        (sock', _) <- accept sock
        (clientWrite, clientRead) <- newChan
        registerClient i clientWrite
        forkFinally
            (action i sock' workerInWrite clientRead)
            (const $ unregisterClient i)
        handleConnections is clientChansRef workerInWrite sock
      where
        updateCCs = atomicUpdate clientChansRef
        registerClient i clientWrite = updateCCs (Map.insert i clientWrite)
        unregisterClient i = updateCCs (Map.delete i)
    worker clientChansRef workerInRead dispatcherInWrite = forever $ do
        (i, value) <- readChan workerInRead
        clientChans <- readIORef clientChansRef  -- FIXME: should become subsriptions
        writeChan dispatcherInWrite $ zip (Map.keys clientChans) (repeat value)
    dispatcher dispatcherInRead clientChansRef = forever $ do
        messages <- readChan dispatcherInRead :: IO [(Int, B.ByteString)]
        clientChans <- readIORef clientChansRef
        putStrLn $ show $ fmap fst $ Map.toList clientChans
        sequence $ fmap (dispatch clientChans) messages
      where
        dispatch clientChans (i, msg) = case Map.lookup i clientChans of
            Just chan -> writeChan chan msg
            Nothing -> return ()

action :: (Show b) => Action (Int, B.ByteString) b
action i sock inWrite outRead =
  do
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
            then killThread outThreadId  -- Client closed connection
            else writeChan inWrite (i, byteString) >> shuffleIn outThreadId


forkMVar :: IO () -> IO (ThreadId, MVar ())
forkMVar action = do
    mvar <- newEmptyMVar
    threadId <- forkFinally action (const $ putMVar mvar ())
    return (threadId, mvar)

joinMVars :: [MVar a] -> IO ()
joinMVars mvars = (sequence $ fmap takeMVar mvars) >> return ()

forkAndJoin :: [IO ()] -> IO ()
forkAndJoin actions =
  do
    mvars <- (fmap . fmap) snd $ sequence $ fmap forkMVar actions
    joinMVars mvars


atomicUpdate :: IORef a -> (a -> a) -> IO ()
atomicUpdate r f = atomicModifyIORef' r $ flip (,) () . f
