{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import System.Posix.Signals
import Network.Socket.ByteString (send)
import qualified Network.Socket as NS
import Network.Simple.TCP hiding (send)

import Clapi.Server (protocolServer, withListen)
import Clapi.SerialisationProtocol (eventSerialiser)
import Clapi.NamespaceTracker (namespaceTrackerProtocol)
import Clapi.Relay (relay)
import Clapi.Protocol ((<->))
import Clapi.Auth (noAuth)
import Clapi.Attributor (attributor)

import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)

shower :: (Show a, Show b) => String -> Protocol a a b b IO ()
shower tag = forever $ waitThen (s " -> " sendFwd) (s " <- " sendRev)
  where
    s d act ms = liftIO (putStrLn $ tag ++ d ++ (show ms)) >> act ms

main :: IO ()
main =
  do
    tid <- myThreadId
    installHandler keyboardSignal (Catch $ killThread tid) Nothing
    withListen HostAny "1234" $ \(lsock, _) ->
        protocolServer lsock perClientProto totalProto (return ())
  where
    perClientProto = shower "network" <-> noAuth <-> eventSerialiser
    totalProto = shower "total" <-> namespaceTrackerProtocol mempty mempty <-> attributor <-> shower "relay" <-> relay mempty
