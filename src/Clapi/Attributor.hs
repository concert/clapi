module Clapi.Attributor where
import Control.Monad (forever)

import Clapi.Server (ClientEvent(..), AddrWithUser(..))
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.Types (Bundle(..), RequestBundle(..), DataUpdateMessage(..))

-- The attributee stuff needs to be lining up through everything less hokily
attributeDataUpdateMsg :: String -> DataUpdateMessage -> DataUpdateMessage
attributeDataUpdateMsg u m = m{duMsgAttributee = Just u}

attributor :: Monad m => Protocol (ClientEvent (AddrWithUser i String) Bundle x) (ClientEvent i Bundle x) a a m ()
attributor = forever $ waitThen fwdAttributed sendRev
  where
    fwdAttributed (ClientData (AddrWithUser i u) (Right (RequestBundle gets dums))) = sendFwd $ ClientData i $ Right $ RequestBundle gets (fmap (attributeDataUpdateMsg u) dums)
    fwdAttributed (ClientData awu b) = sendFwd $ ClientData (awuAddr awu) b
    fwdAttributed (ClientConnect awu x) = sendFwd $ ClientConnect (awuAddr awu) x
    fwdAttributed (ClientDisconnect awu) = sendFwd $ ClientDisconnect (awuAddr awu)
