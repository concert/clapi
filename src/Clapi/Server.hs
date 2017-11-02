{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Clapi.Server where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, link, poll, wait, withAsync)
import qualified Control.Concurrent.Chan.Unagi as Q
import qualified Control.Concurrent.Chan.Unagi.Bounded as BQ
import qualified Control.Exception as E
import Control.Monad (filterM, forever, (>=>))
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.ByteString as B
import Data.Function (fix)
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromJust)
import Data.Void (Void)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Network.Simple.TCP (HostPreference(HostAny), bindSock)

import qualified Clapi.Protocol as Protocol
import Clapi.Protocol (
  Directed(..), Protocol, sendFwd, sendRev, (<->), waitThen, runProtocolIO)

data User = Alice | Bob | Charlie deriving (Eq, Ord, Show)

swallowExc :: a -> E.SomeException -> a
swallowExc = const


withListen :: HostPreference -> NS.ServiceName ->
    ((NS.Socket, NS.SockAddr) -> IO r) -> IO r
withListen hp port action = E.mask $ \restore -> do
    -- The addr returned by bindSock is useless when we bind "0":
    (lsock, _) <- bindSock hp port
    NS.listen lsock $ max 2048 NS.maxListenQueue
    addr <- NS.getSocketName lsock
    a <- async $ action (lsock, addr)
    restore $ doubleCatch
       (swallowExc $ NS.close lsock >> wait a)
       (cancel a)
       (wait a >>= \r -> NS.close lsock >> return r)

doubleCatch :: (E.Exception e) => (e -> IO a) -> IO b -> IO a -> IO a
doubleCatch softHandle hardHandle action =
    action `E.catch` (\e -> (softHandle e) `E.onException` hardHandle)


throwAfter :: IO a -> E.SomeException -> IO b
throwAfter action e = action >> E.throwIO e


serve' :: NS.Socket -> ((NS.Socket, NS.SockAddr) -> IO r) -> IO () -> IO r
serve' listenSock handler onShutdown = E.mask_ $ loop []
  where
    loop as =
      do
        as' <- doubleCatch
            (throwAfter $ onShutdown >> mapM wait as)
            (mapM cancel as) (do
                x@(sock, addr) <- NS.accept listenSock
                a <- async (handler x `E.finally` NS.close sock)
                filterM (poll >=> return . isNothing) (a:as))
        loop as'

type ClientAddr = NS.SockAddr

data ClientEvent ident a b
    = ClientConnect ident b
    | ClientDisconnect ident
    | ClientData ident a
    deriving (Eq, Show)
type ClientEvent' = ClientEvent ClientAddr

data ServerEvent ident a
    = ServerData ident a
    | ServerDisconnect ident
    deriving (Eq, Show)
type ServerEvent' = ServerEvent ClientAddr

instance Show (Q.InChan a) where
    show _ = "InChan"

_serveToChan ::
  Protocol
      (ClientEvent' B.ByteString (Q.InChan (ServerEvent i b))) a'
      (ServerEvent' B.ByteString) (ServerEvent i b)
      IO () ->
  IO () ->
  NS.Socket ->
  BQ.InChan a' ->
  IO ()
_serveToChan perClientProtocol onShutdown listenSock inChan =
    serve' listenSock handler onShutdown
  where
    handler (sock, addr) = do
      (returnChanIn, returnChanOut) <- Q.newChan
      runProtocolIO
          (NSB.recv sock 4096) (BQ.writeChan inChan)
          (NSB.sendAll sock) (Q.readChan returnChanOut)
          (blk addr returnChanIn <-> perClientProtocol)
    blk addr returnChanIn =
      do
        sendFwd $ ClientConnect addr returnChanIn
        inner
      where
        inner = do
            d <- Protocol.wait
            case d of
              Fwd "" -> sendFwd (ClientDisconnect addr) >> return ()
              Fwd bs -> sendFwd (ClientData addr bs) >> inner
              Rev (ServerData _ bs) -> sendRev bs >> inner
              Rev (ServerDisconnect _) -> return ()

neverDoAnything :: IO a
neverDoAnything = fix id

protocolServer ::
  (Ord i) =>
  NS.Socket ->
  Protocol
     (ClientEvent' B.ByteString (Q.InChan (ServerEvent i b)))
     (ClientEvent i a' (Q.InChan (ServerEvent i b)))
     (ServerEvent' B.ByteString)
     (ServerEvent i b)
     IO () ->
  Protocol
      (ClientEvent i a' ())
      Void
      (ServerEvent i b)
      Void IO () ->
  IO () ->
  IO ()
protocolServer listenSock perClientProtocol sharedProtocol onShutdown =
  do
    (i, o) <- BQ.newChan 4
    withAsync (bar o) (\as -> _serveToChan perClientProtocol onShutdown listenSock i)
  where
    bar o = runProtocolIO
        (BQ.readChan o) undefined
        (uncurry Q.writeChan) neverDoAnything
        (servBlk mempty <-> sharedProtocol)
    servBlk connectedMap = do
        d <- Protocol.wait
        case d of
          Fwd (ClientConnect addr q) ->
             sendFwd (ClientConnect addr ()) >> servBlk (Map.insert addr q connectedMap)
          Fwd (ClientDisconnect addr) ->
             sendFwd (ClientDisconnect addr) >> servBlk (Map.delete addr connectedMap)
          Fwd (ClientData addr bs) ->
             sendFwd (ClientData addr bs) >> servBlk connectedMap
          Rev se@(ServerDisconnect addr) ->
             toClient se connectedMap addr >>
             servBlk (Map.delete addr connectedMap)
          Rev se@(ServerData addr bs) ->
             toClient se connectedMap addr >> servBlk connectedMap
    toClient se m a = maybe
        (return ())
        (\chan -> sendRev (chan, se))
        (Map.lookup a m)

demoServer = withListen HostAny "1234" $ \(lsock, _) ->
    protocolServer lsock fakeAuth (subscriptionRegistrar <-> loggyEcho) (return ())

loggyCat :: (Show a) => Protocol (ClientEvent' a x) (ClientEvent' a x) b b IO ()
loggyCat = forever $ do
    d <- Protocol.wait
    liftIO $ case d of
      Fwd (ClientConnect addr q) -> putStrLn $ "conn " ++ (show addr)
      Fwd (ClientDisconnect addr) -> putStrLn $ "dis " ++ (show addr)
      Fwd (ClientData addr a) -> putStrLn $ "data " ++ (show addr) ++ " " ++ (show a)
      Rev _ -> return ()
    Protocol.send d

-- NB: Should make this an opaque type with accessors only, no constructor
-- pattern matching:
data AddrWithUser a u = AddrWithUser {
    awuAddr :: a,
    awuUser :: u
    } deriving (Eq, Ord)

instance (Show a, Show u) => Show (AddrWithUser a u) where
    show (AddrWithUser a u) = show u ++ ":" ++ show a

newAwu :: a -> u -> AddrWithUser a u
newAwu = AddrWithUser

type AddrWithUser' = AddrWithUser ClientAddr B.ByteString

fakeAuth ::
    Protocol
        (ClientEvent' B.ByteString x)
        (ClientEvent AddrWithUser' B.ByteString x)
        (ServerEvent' B.ByteString)
        (ServerEvent AddrWithUser' B.ByteString)
        IO ()
fakeAuth = awaitAuth Nothing
  where
    awaitAuth maybeQ = waitThen
      (
        \ce -> case ce of
          ClientConnect addr q -> (sendRev $ ServerData addr
              "Type a username to simulate auth, or 'die' to fail auth:\n") >>
              awaitAuth (Just q)
          ClientData addr "die\n" -> sendRev $
              ServerData addr "Oh noes, you dinnae auth\n"
          ClientData addr name ->
            let awu = newAwu addr name in
            do
              sendFwd $ ClientConnect awu (fromJust maybeQ)
              sendRev $ ServerData addr "authed!\n"
              authed awu
          _ -> awaitAuth maybeQ
      )
      (const $ awaitAuth maybeQ)
    authed awu = forever $ Protocol.map
        (\ce -> case ce of
            ClientData _ bs -> ClientData awu bs
            ClientDisconnect _ -> ClientDisconnect awu
        )
        (\se -> case se of
            ServerData _ bs -> ServerData (awuAddr awu) bs
            ServerDisconnect _ -> ServerDisconnect (awuAddr awu)
        )

loggyEcho :: (Show a) =>
    Protocol (ClientEvent AddrWithUser' a x) Void a Void IO ()
loggyEcho = forever $ do
    d <- Protocol.wait
    case d of
      Fwd (ClientData awu a) ->
          (liftIO . putStrLn $ (show $ awuUser awu) ++ ": bounce " ++ show a) >>
          sendRev a
      Fwd (ClientDisconnect awu) ->
          liftIO $ putStrLn $ (show $ awuUser awu) ++ " disconnected"
      Fwd _ -> return ()

subscriptionRegistrar ::
    forall i b x.
    (Ord i, Show i) =>
    Protocol
        (ClientEvent i B.ByteString x)
        (ClientEvent i B.ByteString x)
        (ServerEvent i b) b IO ()
subscriptionRegistrar = loop (mempty :: Map.Map i ())
  where
    loop subscribed =
      do
        waitThen
          (
            \ce -> case ce of
              ClientData addr "subscribe\n" -> do
                  liftIO $ putStrLn $ (show addr) ++ " subscribed"
                  loop $ Map.insert addr () subscribed
              ClientData addr "unsubscribe\n" -> do
                  liftIO $ putStrLn $ (show addr) ++ " unsubscribed"
                  loop $ Map.delete addr subscribed
              _ -> sendFwd ce >> loop subscribed
          )
          (
            \bs -> do
              mapM_
                (sendRev . uncurry ServerData)
                (Map.toList $ fmap (const bs) subscribed)
              loop subscribed
          )
