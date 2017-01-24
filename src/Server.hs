module Server where

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Network.Socket (
    Socket, PortNumber, Family(AF_INET), SocketType(Stream),
    SocketOption(ReuseAddr), SockAddr(SockAddrInet), socket, setSocketOption,
    bind, listen, accept, close, iNADDR_ANY)
import Network.Socket.ByteString (send)

import Blaze.ByteString.Builder (toByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

serve :: (Socket -> IO ()) -> PortNumber -> IO ()
serve action port = bracket startListening stopListening handleConnections
  where
    startListening = do
        sock <- socket AF_INET Stream 0
        setSocketOption sock ReuseAddr 1  -- make socket immediately reusable - eases debugging.
        bind sock (SockAddrInet port iNADDR_ANY)
        listen sock 2  -- set a max of 2 queued connections  see maxListenQueue
        return sock
    stopListening = close
    handleConnections sock = forever $ do
        (sock', _) <- accept sock
        forkIO (action sock')

action :: Socket -> IO ()
action sock =
  do
    send sock (toByteString . fromString $ "hello\n")
    close sock
