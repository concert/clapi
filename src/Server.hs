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

import Text.Printf (printf)
import Blaze.ByteString.Builder (toByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

type WriteChan a = InChan a
type ReadChan a = OutChan a

type Action a b = Int -> Socket -> WriteChan a -> ReadChan b -> IO ()

serve :: (Show a) => Action a String -> PortNumber -> IO ()
serve action port =
  do
    (inWrite, inRead) <- newChan
    (outWrite, outRead) <- newChan
    forkIO $ worker inRead outWrite -- FIXME: how do we clean up the worker?
    bracket startListening stopListening (handleConnections [1..] inWrite outWrite)
  where
    startListening = do
        sock <- socket AF_INET Stream 0
        setSocketOption sock ReuseAddr 1  -- make socket immediately reusable - eases debugging.
        bind sock (SockAddrInet port iNADDR_ANY)
        listen sock 2  -- set a max of 2 queued connections  see maxListenQueue
        return sock
    stopListening = close
    handleConnections (i:is) inWrite outWrite sock = do
        (sock', _) <- accept sock
        -- FIXME: dupChan outWrite should be drawing from our lsit of outReads :-/
        c <- dupChan outWrite
        forkIO (action i sock' inWrite c)
        handleConnections is inWrite outWrite sock
    worker inRead outWrite = forever $ do
        value <- readChan inRead
        putStrLn $ printf "got %v" (show value)
        writeChan outWrite $ printf "planet %v!" (show value)

action :: (Show b) => Action Int b
action i sock inWrite outRead =
  do
    send sock $ bytes $ printf "hello %v\n" i
    writeChan inWrite i
    forever $ do
        value <- readChan outRead
        send sock $ bytes $ show value
    close sock  -- Never get here!
  where
    bytes = toByteString . fromString
