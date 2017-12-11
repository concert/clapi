module Clapi.Attributor where
import Control.Monad (forever)

import Clapi.Server (ClientEvent(..), AddrWithUser(..))
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.Types (ToRelayBundle(..), RequestBundle(..), DataUpdateMessage(..))

-- The attributee stuff needs to be lining up through everything less hokily
attributeDataUpdateMsg :: String -> DataUpdateMessage -> DataUpdateMessage
attributeDataUpdateMsg u m = m{duMsgAttributee = Just u}

attributor :: Monad m => String -> Protocol ToRelayBundle ToRelayBundle a a m ()
attributor u = forever $ waitThen fwdAttributed sendRev
  where
    fwdAttributed (TRBClient (RequestBundle gets dums)) =
        sendFwd $ TRBClient $ RequestBundle gets (fmap (attributeDataUpdateMsg u) dums)
    fwdAttributed (TRBOwner b) = sendFwd $ TRBOwner b
