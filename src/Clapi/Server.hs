{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Clapi.Server where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, poll, wait, withAsync)
import Control.Concurrent.MVar
import qualified Control.Concurrent.Chan.Unagi as Q
import qualified Control.Concurrent.Chan.Unagi.Bounded as BQ
import qualified Control.Exception as E
import Control.Monad (filterM, forever, (>=>))
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Void (Void)
import GHC.IO.Exception (IOException)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Network.Simple.TCP (HostPreference, bindSock)

import Clapi.Protocol (Protocol, runProtocolIO)
import Clapi.PerClientProto
  (ClientEvent(..), ServerEvent(..), liftToPerClientEvent, seIdent)

data User = Alice | Bob | Charlie deriving (Eq, Ord, Show)

swallowExc :: a -> E.SomeException -> a
swallowExc = const


withListen
  :: IO () -> IO () -> HostPreference -> NS.ServiceName
  -> ((NS.Socket, NS.SockAddr) -> IO r) -> IO r
withListen onDraining onTerminated hp port action = E.mask $ \restore -> do
    -- The addr returned by bindSock is useless when we bind "0":
    (lsock, _) <- bindSock hp port
    NS.listen lsock $ max 2048 NS.maxListenQueue
    addr <- NS.getSocketName lsock
    a <- async $ action (lsock, addr)
    restore $ doubleCatch
       (swallowExc $ NS.close lsock >> onDraining >> wait a)
       (cancel a >> onTerminated)
       (wait a >>= \r -> NS.close lsock >> return r)

doubleCatch :: (E.Exception e) => (e -> IO a) -> IO b -> IO a -> IO a
doubleCatch softHandle hardHandle action =
    action `E.catch` (\e -> softHandle e `E.onException` hardHandle)


throwAfter :: E.SomeException -> IO a -> IO b
throwAfter e action = action >> E.throwIO e


serve' :: NS.Socket -> ((NS.Socket, NS.SockAddr) -> IO r) -> IO () -> IO ()
serve' listenSock handler onShutdown = E.mask_ $ loop []
  where
    loop as =
      (do
        x@(sock, _addr) <- NS.accept listenSock
        a <- async (handler x `E.finally` NS.close sock)
        filterM (poll >=> return . isNothing) (a:as) >>= loop
      ) `E.catch`
          -- The socket was (almost certainly) closed on us:
          (\(_ :: IOException) -> onShutdown >> mapM_ wait as)
        `E.catch`
          -- Something else happened, like we were killed!
          (\(e :: E.SomeException) -> throwAfter e $ mapM_ cancel as)

type ClientAddr = NS.SockAddr

type ClientEvent' = ClientEvent ClientAddr
type ServerEvent' = ServerEvent ClientAddr

instance Show (Q.InChan a) where
    show _ = "<InChan>"

_handlePerClient ::
    i ->
    Protocol B.ByteString a B.ByteString b IO () ->
    ((ServerEvent i b -> IO ()) -> IO (ClientEvent i a -> IO ())) ->
    NS.Socket ->
    IO ()
_handlePerClient i proto toMainChan sock = do
    (clientChanIn, clientChanOut) <- Q.newChan
    mainChanIn <- toMainChan $ Q.writeChan clientChanIn
    runProtocolIO
        (NSB.recv sock 4096) mainChanIn
        (NSB.sendAll sock) (Q.readChan clientChanOut)
        (liftToPerClientEvent i proto)

neverDoAnything :: IO a
neverDoAnything = forever $ threadDelay maxBound

protocolServer
  :: (Ord i)
  => NS.Socket -> (NS.SockAddr
  -> (i, Protocol B.ByteString a B.ByteString b IO ()))
  -> Protocol
        (ClientEvent i a) Void
        (ServerEvent i b) Void
        IO ()
  -> IO () -> IO ()
protocolServer listenSock getClientProto mainProto onShutdown = do
    (mainI, mainO) <- BQ.newChan 4
    clientMap <- newMVar mempty
    withAsync (mainP mainO clientMap) (clientP mainI clientMap)
  where
    mainP mainChan clientMap = runProtocolIO
        (BQ.readChan mainChan) undefined
        (dispatch clientMap) neverDoAnything
        mainProto
    dispatch clientMap msg = withMVar clientMap (\m -> maybe
        (return ()) (\tc -> tc msg) (Map.lookup (seIdent msg) m))
    addReturnPath clientMap mainI i rp = do
        modifyMVar_ clientMap (return . Map.insert i rp)
        return (BQ.writeChan mainI)
    rmReturnPath clientMap i = modifyMVar_ clientMap (return . Map.delete i)
    clientP mainI clientMap _as = serve' listenSock (clientHandler clientMap mainI) onShutdown
    clientHandler clientMap mainI (sock, addr) = do
        let (i, cp) = getClientProto addr
        _handlePerClient i cp (addReturnPath clientMap mainI i) sock
        rmReturnPath clientMap i
