module Clapi.Attributor where

import Control.Monad (forever)
import Data.Text (Text)

import Clapi.PerClientProto (ClientEvent(..))
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.Types
  (Attributee, ToRelayBundle(..), RequestBundle(..), DataUpdateMessage(..))

attributeDataUpdateMsg :: Text -> DataUpdateMessage -> DataUpdateMessage
attributeDataUpdateMsg u m = m{duMsgAttributee = Just u}

attributor :: (Monad m, Functor f) => Attributee -> Protocol (f ToRelayBundle) (f ToRelayBundle) a a m ()
attributor u = forever $ waitThen (sendFwd . fmap attributeClient) sendRev
  where
    attributeClient (TRBClient (RequestBundle gets dums)) =
        TRBClient $ RequestBundle gets (fmap (attributeDataUpdateMsg u) dums)
    attributeClient (TRBOwner b) = TRBOwner b
