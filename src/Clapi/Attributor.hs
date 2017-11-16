module Clapi.Attributor where
import Control.Monad (forever)

import Clapi.Server (ClientEvent(..), AddrWithUser(..))
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.Types (ToRelayBundle(..), RequestBundle(..), DataUpdateMessage(..))

-- The attributee stuff needs to be lining up through everything less hokily
attributeDataUpdateMsg :: String -> DataUpdateMessage -> DataUpdateMessage
attributeDataUpdateMsg u m = m{duMsgAttributee = Just u}

attributor :: Monad m => Protocol (ClientEvent (AddrWithUser i String) ToRelayBundle x) (ClientEvent i ToRelayBundle x) a a m ()
attributor = forever $ waitThen fwdAttributed sendRev
  where
    fwdAttributed (ClientData (AddrWithUser i u) (TRBClient (RequestBundle gets dums))) =
        sendFwd $ ClientData i $ TRBClient $ RequestBundle gets (fmap (attributeDataUpdateMsg u) dums)
    fwdAttributed (ClientData awu b) = sendFwd $ ClientData (awuAddr awu) b
    fwdAttributed (ClientConnect awu x) = sendFwd $ ClientConnect (awuAddr awu) x
    fwdAttributed (ClientDisconnect awu) = sendFwd $ ClientDisconnect (awuAddr awu)
