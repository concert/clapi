{-# LANGUAGE OverloadedStrings #-}
module Clapi.PerClientProto where
import Control.Monad.Trans.Free

import Data.ByteString (ByteString)
import Clapi.Protocol (Protocol, sendFwd, sendRev, ProtocolF(..), Directed(..))

data ClientEvent ident a
    = ClientConnect ident
    | ClientDisconnect ident
    | ClientData ident a
    deriving (Eq, Show)

data ServerEvent ident a
    = ServerData ident a
    | ServerDisconnect ident
    deriving (Eq, Show)

seIdent :: ServerEvent i a -> i
seIdent (ServerData i _) = i
seIdent (ServerDisconnect i) = i

liftToPerClientEvent ::
    Monad m
    => i
    -> Protocol ByteString a ByteString b m ()
    -> Protocol
         ByteString (ClientEvent i a)
         ByteString (ServerEvent i b)
         m ()
liftToPerClientEvent i p = do
    sendFwd $ ClientConnect i
    FreeT $ go <$> runFreeT p
  where
    go (Free (Wait f)) = Free (Wait $ g f)
    go (Free (SendFwd a next)) = Free (SendFwd (ClientData i a) (liftToPerClientEvent i next))
    go (Free (SendRev bs next)) = Free (SendRev bs (liftToPerClientEvent i next))
    go (Pure x) = Free (SendFwd (ClientDisconnect i) (return x))
    g _ (Fwd "") = sendFwd $ ClientDisconnect i
    g f (Fwd bs) = liftToPerClientEvent i $ f $ Fwd bs
    g _ (Rev (ServerDisconnect _)) = return ()
    g f (Rev (ServerData _ b)) = liftToPerClientEvent i $ f $ Rev b
