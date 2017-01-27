module Server where

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Concurrent.Chan.Unagi (
    InChan, OutChan, newChan, writeChan, readChan, tryReadChan, getChanContents)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Network.Socket (
    Socket, PortNumber, Family(AF_INET), SocketType(Stream),
    SocketOption(ReuseAddr), SockAddr(SockAddrInet), socket, setSocketOption,
    bind, listen, accept, close, iNADDR_ANY)
import Network.Socket.ByteString (send)

import Data.Foldable (toList)
import qualified Data.Map as Map

import Text.Printf (printf)
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
        atomicUpdate clientChansRef (Map.insert i clientWrite)
        forkIO (action i sock' workerInWrite clientRead)
        handleConnections is clientChansRef workerInWrite sock
    worker clientChansRef workerInRead = forever $ do
        value <- readChan workerInRead
        clientChans <- readIORef clientChansRef
        putStrLn $ show $ fmap fst $ Map.toList clientChans
        sequence $ fmap (flip writeChan "planet!") $ toList $ clientChans

action :: (Show b) => Action Int b
action i sock inWrite outRead =
  do
    send sock $ bytes $ printf "hello %v\n" i
    writeChan inWrite i
    forever $ do
        value <- readChan outRead
        send sock $ bytes $ show value
    close sock  -- Never get here! Also need to handle client disconnect
  where
    bytes = toByteString . fromString


atomicUpdate :: IORef a -> (a -> a) -> IO ()
atomicUpdate r f = atomicModifyIORef' r $ flip (,) () . f
