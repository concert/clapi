module Server where

import Control.Monad (forever)
import Control.Concurrent (forkIO, forkFinally)
import Control.Exception (bracket)
import Control.Concurrent.Chan.Unagi (
    InChan, OutChan, newChan, writeChan, readChan, tryReadChan, getChanContents)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Network.Socket (
    Socket, PortNumber, Family(AF_INET), SocketType(Stream),
    SocketOption(ReuseAddr), SockAddr(SockAddrInet), socket, setSocketOption,
    bind, listen, accept, close, iNADDR_ANY)
import Network.Socket.ByteString (send, recv)

import Data.Foldable (toList)
import qualified Data.Map as Map

import Text.Printf (printf)
import Data.ByteString (ByteString)
import Blaze.ByteString.Builder (toByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

type WriteChan a = InChan a
type ReadChan a = OutChan a

type Action a b = Int -> Socket -> WriteChan a -> ReadChan b -> IO ()

serve :: (Show a) => Action a String -> PortNumber -> IO ()
serve action port =
  do
    (workerInWrite, workerInRead) <- newChan
    clientChansRef <- newIORef mempty :: IO (IORef (Map.Map Int (WriteChan a)))
    forkIO $ worker clientChansRef workerInRead -- FIXME: how do we clean up the worker?
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
    worker clientChansRef workerInRead = forever $ do
        value <- readChan workerInRead
        clientChans <- readIORef clientChansRef
        putStrLn $ show $ fmap fst $ Map.toList clientChans
        sequence $ fmap (flip writeChan $ show value) $ toList $ clientChans

action :: (Show b) => Action ByteString b
action i sock inWrite outRead =
  do
    send sock $ bytes $ printf "hello %v\n" i
    forkAndJoin [shuffleOut 0, shuffleIn]
    -- close sock  -- Never get here! Also need to handle client disconnect
  where
    bytes = toByteString . fromString
    shuffleOut i = do
        value <- readChan outRead
        send sock $ bytes $ show value
        if i == 2
            then (send sock $ bytes "bye\n") >> close sock
            else shuffleOut (i + 1)
    shuffleIn = forever $ do
        byteString <- recv sock 4096
        writeChan inWrite byteString


forkAndJoin :: [IO ()] -> IO ()
forkAndJoin actions =
  do
    mvars <- sequence $ fmap (const newEmptyMVar) actions
    sequence $ zipWith fork actions mvars
    sequence $ fmap takeMVar mvars
    return ()
  where
    fork action mvar = forkFinally action (const $ putMVar mvar ())


atomicUpdate :: IORef a -> (a -> a) -> IO ()
atomicUpdate r f = atomicModifyIORef' r $ flip (,) () . f
