{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import System.Posix.Signals
import Network.Simple.TCP hiding (send)
import Network.Socket (SockAddr(SockAddrCan))

import Clapi.Server (protocolServer, withListen)
import Clapi.SerialisationProtocol (serialiser)
import Clapi.NamespaceTracker (namespaceTrackerProtocol, Owners(..))
import Clapi.Relay (relay)
import Clapi.Protocol ((<<->))
import Clapi.Attributor (attributor)
import Clapi.Valuespace (baseValuespace)

import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)

shower :: (Show a, Show b) => String -> Protocol a a b b IO ()
shower tag = forever $ waitThen (s " -> " sendFwd) (s " <- " sendRev)
  where
    s d act ms = liftIO (putStrLn $ tag ++ d ++ (show ms)) >> act ms

-- FIXME: This is owned by something unsendable and we should reflect that
apiClaimed :: Owners SockAddr
apiClaimed = Map.singleton "api" $ SockAddrCan 12

main :: IO ()
main =
  do
    tid <- myThreadId
    installHandler keyboardSignal (Catch $ killThread tid) Nothing
    withListen HostAny "1234" $ \(lsock, _) ->
        protocolServer lsock perClientProto totalProto (return ())
  where
    perClientProto addr = (addr, serialiser <<-> attributor "someone")
    totalProto = shower "total" <<-> namespaceTrackerProtocol (void . return) apiClaimed mempty <<-> relay baseValuespace
