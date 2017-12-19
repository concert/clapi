module Clapi.Attributor where

import Control.Monad (forever)
import Data.Text (Text)

import Clapi.PerClientProto (ClientEvent(..))
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.Types
  (Attributee, ToRelayBundle(..), RequestBundle(..), DataUpdateMessage(..))

attributeDataUpdateMsg :: Text -> DataUpdateMessage -> DataUpdateMessage
attributeDataUpdateMsg u m = m{duMsgAttributee = Just u}

attributor :: Monad m => Attributee -> Protocol ToRelayBundle ToRelayBundle a a m ()
attributor u = forever $ waitThen fwdAttributed sendRev
  where
    fwdAttributed (TRBClient (RequestBundle gets dums)) =
        sendFwd $ TRBClient $ RequestBundle gets (fmap (attributeDataUpdateMsg u) dums)
    fwdAttributed (TRBOwner b) = sendFwd $ TRBOwner b
