{-# LANGUAGE ScopedTypeVariables #-}

module Clapi.SerialisationProtocol (serialiser, eventWrapper) where

import Control.Monad.Trans.Free
import Data.Attoparsec.ByteString (parse, Result, IResult(..))
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)

import Clapi.Serialisation (encode, parser)
import Clapi.Protocol (
    Protocol, wait, waitThen, sendFwd, sendRev, ProtocolF(..), Directed(..))
import Clapi.Server (ClientEvent(..), ServerEvent(..))
import Clapi.Types (Message)

serialiser :: Monad m => Protocol ByteString [Message] ByteString [Message] m ()
serialiser = serialiser' $ parse parser
  where
    serialiser' p = waitThen (fwd p) (rev p)
    fwd parseNext bs = case parseNext bs of
        Fail _ ctxs err -> sendRev $ fromString err
        Partial cont -> serialiser' cont
        Done unconsumed msgs -> sendFwd msgs >> fwd (parse parser) unconsumed
    rev p msgs = either
        (const $ error "encode failed")
        (\bs -> sendRev bs >> serialiser' p)
        (encode msgs)


eventWrapper ::
  forall x x' y' y m i b.
  Monad m
  => Protocol x x' y' y m ()
  -> Protocol
       (ClientEvent i x b) (ClientEvent i x' b)
       (ServerEvent i y') (ServerEvent i y)
       m ()
eventWrapper p = do
    d <- wait
    case d of
      Fwd (ClientConnect i b) -> sendFwd $ ClientConnect i b
      Fwd (ClientDisconnect i) -> sendFwd $ ClientDisconnect i
      Fwd (ClientData i x) -> FreeT $ runFreeT p >>= goFree i (Fwd x)
      Rev (ServerData i y) -> FreeT $ runFreeT p >>= goFree i (Rev y)
      Rev (ServerDisconnect i) -> sendRev $ ServerDisconnect i
  where
    goFree
      :: i
      -> Directed x y
      -> FreeF (ProtocolF x x' y' y) () (Protocol x x' y' y m ())
      -> m (FreeF
             (ProtocolF
               (ClientEvent i x b) (ClientEvent i x' b)
               (ServerEvent i y') (ServerEvent i y)
             )
             ()
             (Protocol
               (ClientEvent i x b) (ClientEvent i x' b)
               (ServerEvent i y') (ServerEvent i y)
               m ()
             ))
    goFree i d' (Free (Wait f)) = runFreeT $ eventWrapper $ f d'
    goFree i d' (Free (SendFwd x' next)) = do
        runFreeT $ sendFwd $ ClientData i x'
        runFreeT next >>= goFree i d'
    goFree i d' (Free (SendRev y' next)) = do
        runFreeT $ sendRev $ ServerData i y'
        runFreeT next >>= goFree i d'
