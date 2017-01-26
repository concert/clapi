module Server where

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Concurrent.Chan.Unagi (
    InChan, OutChan, newChan, dupChan, writeChan, readChan)
import Network.Socket (
    Socket, PortNumber, Family(AF_INET), SocketType(Stream),
    SocketOption(ReuseAddr), SockAddr(SockAddrInet), socket, setSocketOption,
    bind, listen, accept, close, iNADDR_ANY)
import Network.Socket.ByteString (send)

import Blaze.ByteString.Builder (toByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

type WriteChan a = InChan a
type ReadChan a = OutChan a

serve :: (Show a) => (Socket -> WriteChan a -> ReadChan String -> IO ()) ->
    PortNumber -> IO ()
serve action port =
  do
    (inWrite, inRead) <- newChan
    (outWrite, outRead) <- newChan
    forkIO $ worker inRead outWrite -- FIXME: how do we clean up the worker?
    bracket startListening stopListening (handleConnections inWrite outWrite)
  where
    startListening = do
        sock <- socket AF_INET Stream 0
        setSocketOption sock ReuseAddr 1  -- make socket immediately reusable - eases debugging.
        bind sock (SockAddrInet port iNADDR_ANY)
        listen sock 2  -- set a max of 2 queued connections  see maxListenQueue
        return sock
    stopListening = close
    handleConnections inWrite outWrite sock = forever $ do
        (sock', _) <- accept sock
        -- FIXME: dupChan outWrite should be drawing from our lsit of outReads :-/
        c <- dupChan outWrite
        forkIO (action sock' inWrite c)
    worker inRead outWrite = forever $ do
        value <- readChan inRead
        putStrLn $ show value
        writeChan outWrite "planet!"

action :: (Show b) => Socket -> WriteChan String -> ReadChan b -> IO ()
action sock inWrite outRead =
  do
    send sock $ bytes "hello\n"
    writeChan inWrite "world"
    forever $ do
        value <- readChan outRead
        send sock $ bytes $ show value
    close sock  -- Never get here!
  where
    bytes = toByteString . fromString
