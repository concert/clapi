{-# LANGUAGE OverloadedStrings #-}
module Clapi.PerClientProto where
import Control.Monad.Trans.Free

import Data.ByteString (ByteString)
import Clapi.Protocol (Protocol, sendFwd, sendRev, ProtocolF(..), Directed(..))

data PerClientInboundEvent i a
  = PcieConnected i | PcieDisconnected i | PcieData i a

data PerClientOutboundEvent a = PcoeDisconnected | PcoeData a

liftToPerClientEvent ::
    Monad m
    => i
    -> Protocol ByteString a ByteString b m ()
    -> Protocol
         ByteString (PerClientInboundEvent i a)
         ByteString (PerClientOutboundEvent b)
         m ()
liftToPerClientEvent i p = do
    sendFwd $ PcieConnected i
    FreeT $ go <$> runFreeT p
  where
    go (Free (Wait f)) = Free (Wait $ g f)
    go (Free (SendFwd a next)) = Free (SendFwd (PcieData i a) (liftToPerClientEvent i next))
    go (Free (SendRev bs next)) = Free (SendRev bs (liftToPerClientEvent i next))
    go (Pure x) = Free (SendFwd (PcieDisconnected i) (return x))
    g _ (Fwd "") = sendFwd $ PcieDisconnected i
    g f (Fwd bs) = liftToPerClientEvent i $ f $ Fwd bs
    g _ (Rev PcoeDisconnected) = return ()
    g f (Rev (PcoeData b)) = liftToPerClientEvent i $ f $ Rev b
