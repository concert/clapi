{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import System.Posix.Signals
import Network.Simple.TCP hiding (send)
import Network.Socket (SockAddr(SockAddrCan))
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Text as T
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString.Base16 as B16
import Control.Concurrent.MVar

import Clapi.Server (protocolServer, withListen)
import Clapi.SerialisationProtocol (serialiser)
import Clapi.NamespaceTracker (namespaceTrackerProtocol, Owners(..))
import Clapi.Relay (relay)
import Clapi.Attributor (attributor)
import Clapi.Valuespace (baseValuespace)
import Clapi.RelayApi (relayApiProto, PathSegable(..))
import Clapi.Protocol ((<<->), Protocol, waitThen, sendFwd, sendRev)
import Clapi.Path (mkSeg)
import Clapi.PathQ (segq)

shower :: (Show a, Show b) => String -> Protocol a a b b IO ()
shower tag = forever $ waitThen (s " -> " sendFwd) (s " <- " sendRev)
  where
    s d act ms = liftIO (putStrLn $ tag ++ d ++ (show ms)) >> act ms

-- FIXME: This is owned by something unsendable and we should reflect that
internalAddr = SockAddrCan 12
apiClaimed :: Owners SockAddr
apiClaimed = Map.singleton [segq|api|] internalAddr

instance PathSegable SockAddr where
    pathNameFor (SockAddrCan _) = [segq|relay|]
    -- NOTE: Do not persist this as it depends on the form of show
    pathNameFor clientAddr = fromJust $ mkSeg $ T.pack $ take 8 $ UTF8.toString $ B16.encode $ hash $ UTF8.fromString $ show clientAddr

main :: IO ()
main =
  do
    ownershipMvar <- newMVar apiClaimed
    tid <- myThreadId
    installHandler keyboardSignal (Catch $ killThread tid) Nothing
    withListen HostAny "1234" $ \(lsock, _) ->
        protocolServer lsock perClientProto (totalProto ownershipMvar) (return ())
  where
    perClientProto addr = (addr, serialiser <<-> attributor "someone")
    totalProto ownershipMvar = shower "total" <<-> relayApiProto ownershipMvar internalAddr <<-> shower "nt" <<-> namespaceTrackerProtocol (void . swapMVar ownershipMvar) apiClaimed mempty <<-> relay baseValuespace
