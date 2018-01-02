{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module Clapi.SerialisationProtocol (serialiser, mapProtocol) where

import Control.Monad.Trans.Free
import Data.Attoparsec.ByteString (parse, IResult(..))
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)

import Clapi.Serialisation (encode, parser, Serialisable)
import Clapi.Protocol
  (Protocol, waitThen, sendFwd, sendRev, ProtocolF(..), Directed(..))

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
    go (Pure ()) = Pure ()
    mapDirected (Fwd a) = Fwd $ toA a
    mapDirected (Rev b) = Rev $ toB b
    wn next = mapProtocol toA fromA fromB toB next

serialiser
  :: (Serialisable a, Serialisable b, Monad m)
  => Protocol ByteString a ByteString b m ()
serialiser = serialiser' $ parse parser
  where
    serialiser' p = waitThen (fwd p) (rev p)
    fwd parseNext bs = case parseNext bs of
        Fail _ _ctxs err -> sendRev $ fromString err
        Partial cont -> serialiser' cont
        Done unconsumed a -> sendFwd a >> fwd (parse parser) unconsumed
    rev p b = either
        (const $ error "encode failed")
        (\bs -> sendRev bs >> serialiser' p)
        (encode b)
