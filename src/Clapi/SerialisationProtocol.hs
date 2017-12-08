{-# LANGUAGE OverloadedStrings #-}

module Clapi.SerialisationProtocol (serialiser, mapProtocol, eventSerialiser) where

import Control.Monad.Trans.Free
import Data.Attoparsec.ByteString (parse, Result, IResult(..))
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)

import Clapi.Serialisation (encode, parser, Serialisable)
import Clapi.Protocol (
    Protocol, wait, waitThen, sendFwd, sendRev, ProtocolF(..), Directed(..))
import Clapi.Server (ClientEvent(..), ServerEvent(..))

eventSerialiser :: (Serialisable a, Serialisable b, Monad m) =>
    (i -> j) ->  -- Not sold on this
    Protocol
        (ClientEvent i ByteString c)
        (ClientEvent i a c)
        (ServerEvent j ByteString)
        (ServerEvent j b)
        m ()
eventSerialiser idAdapt = serialiser' $ parse parser
  where
    serialiser' p = waitThen (fwd p) (rev p)
    fwd parseNext (ClientData i bs) = case parseNext bs of
        Fail _ ctxs err -> sendRev $ ServerData (idAdapt i) $ fromString err
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
serialiser = serialiser' $ parse parser
  where
    serialiser' p = waitThen (fwd p) (rev p)
    fwd parseNext bs = case parseNext bs of
        Fail _ ctxs err -> sendRev $ fromString err
        Partial cont -> serialiser' cont
        Done unconsumed a -> sendFwd a >> fwd (parse parser) unconsumed
    rev p b = either
        (const $ error "encode failed")
        (\bs -> sendRev bs >> serialiser' p)
        (encode b)

pceSerialiser i = liftToClientEvent i serialiser

data PerClientInboundEvent i a
  = PcieConnected i | PcieDisconnected i | PcieData i a

data PerClientOutboundEvent a = PcoeDisconnected | PcoeData a

liftToClientEvent ::
    Monad m
    => i
    -> Protocol ByteString a ByteString b m ()
    -> Protocol
         ByteString (PerClientInboundEvent i a)
         ByteString (PerClientOutboundEvent b)
         m ()
liftToClientEvent i p = do
    sendFwd $ PcieConnected i
    FreeT $ go <$> runFreeT p
  where
    go (Free (Wait f)) = Free (Wait $ g f)
    go (Free (SendFwd a next)) = Free (SendFwd (PcieData i a) (liftToClientEvent i next))
    go (Free (SendRev bs next)) = Free (SendRev bs (liftToClientEvent i next))
    go (Pure x) = Free (SendFwd (PcieDisconnected i) (return x))
    g _ (Fwd "") = sendFwd $ PcieDisconnected i
    g f (Fwd bs) = liftToClientEvent i $ f $ Fwd bs
    g _ (Rev PcoeDisconnected) = return ()
    g f (Rev (PcoeData b)) = liftToClientEvent i $ f $ Rev b
