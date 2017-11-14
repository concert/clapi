module Clapi.SerialisationProtocol (serialiser, mapProtocol, eventSerialiser) where

import Control.Monad.Trans.Free
import Data.Attoparsec.ByteString (parse, Result, IResult(..))
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)

import Clapi.Serialisation (encode, parser, Serialisable)
import Clapi.Protocol (
    Protocol, wait, waitThen, sendFwd, sendRev, ProtocolF(..), Directed(..))
import Clapi.Server (ClientEvent(..), ServerEvent(..))

eventSerialiser :: (Serialisable a, Serialisable b, Monad m) => Protocol
    (ClientEvent i ByteString c)
    (ClientEvent i a c)
    (ServerEvent i ByteString)
    (ServerEvent i b)
    m ()
eventSerialiser = serialiser' $ parse parser
  where
    serialiser' p = waitThen (fwd p) (rev p)
    fwd parseNext (ClientData i bs) = case parseNext bs of
        Fail _ ctxs err -> sendRev $ ServerData i $ fromString err
        Partial cont -> serialiser' cont
        Done unconsumed msgs -> sendFwd (ClientData i msgs) >> fwd (parse parser) (ClientData i unconsumed)
    fwd p (ClientConnect i b) = sendFwd (ClientConnect i b) >> serialiser' p
    fwd p (ClientDisconnect i) = sendFwd (ClientDisconnect i) >> serialiser' p
    rev p (ServerData i msgs) = either
        (const $ error "encode failed")
        (\bs -> sendRev (ServerData i bs) >> serialiser' p)
        (encode msgs)
    rev p (ServerDisconnect i) = sendRev (ServerDisconnect i) >> serialiser' p

mapProtocol ::
    Monad m
    => (c -> a)
    -> (a' -> c')
    -> (b' -> d')
    -> (d -> b)
    -> Protocol a a' b' b m ()
    -> Protocol c c' d' d m ()
mapProtocol toA fromA fromB toB p = FreeT $ go <$> runFreeT p
  where
    go (Free (Wait f)) = Free (Wait $ wn . f . mapDirected)
    go (Free (SendFwd a next)) = Free (SendFwd (fromA a) (wn next))
    go (Free (SendRev b next)) = Free (SendRev (fromB b) (wn next))
    mapDirected (Fwd a) = Fwd $ toA a
    mapDirected (Rev b) = Rev $ toB b
    wn next = mapProtocol toA fromA fromB toB next

serialiser :: (Serialisable a, Serialisable b, Monad m) => Protocol ByteString a ByteString b m ()
serialiser = mapProtocol (ClientData 0) unpackClientData unpackServerData (ServerData 0) eventSerialiser
  where
    unpackClientData (ClientData _ bs) = bs
    unpackServerData (ServerData _ bs) = bs
