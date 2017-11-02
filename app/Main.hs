{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import System.Posix.Signals
import Network.Socket.ByteString (send)
import qualified Network.Socket as NS
import Network.Simple.TCP hiding (send)

import Clapi.Server (protocolServer, withListen)
import Clapi.SerialisationProtocol (serialiser)
import Clapi.NamespaceTracker (namespaceTrackerProtocol)
import Clapi.Relay (relay)
import Clapi.Protocol ((<->))
import Clapi.Auth (noAuth)

main :: IO ()
main =
  do
    tid <- myThreadId
    installHandler keyboardSignal (Catch $ killThread tid) Nothing
    withListen HostAny "1234" $ \(lsock, _) ->
        protocolServer lsock perClientProto totalProto (return ())
  where
    perClientProto = noAuth <-> serialiser
    totalProto = namespaceTrackerProtocol mempty mempty <-> relay mempty
