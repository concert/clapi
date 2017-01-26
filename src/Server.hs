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

serve :: (Show a) => (Socket -> WriteChan a -> IO ()) -> PortNumber -> IO ()
serve action port =
  do
    (inWrite, inRead) <- newChan
    forkIO $ worker inRead  -- FIXME: how do we clean up the worker?
    bracket startListening stopListening (handleConnections inWrite)
  where
    startListening = do
        sock <- socket AF_INET Stream 0
        setSocketOption sock ReuseAddr 1  -- make socket immediately reusable - eases debugging.
        bind sock (SockAddrInet port iNADDR_ANY)
        listen sock 2  -- set a max of 2 queued connections  see maxListenQueue
        return sock
    stopListening = close
    handleConnections inWrite sock = forever $ do
        (sock', _) <- accept sock
        forkIO (action sock' inWrite)
    worker inRead = forever $ do
        value <- readChan inRead
        putStrLn $ show value

action :: Socket -> WriteChan String -> IO ()
action sock inWrite =
  do
    send sock (toByteString . fromString $ "hello\n")
    writeChan inWrite "world"
    close sock
