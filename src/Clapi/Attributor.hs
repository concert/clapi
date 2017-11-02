module Clapi.Attributor where
import Control.Monad (forever)

import Clapi.Server (ClientEvent(ClientData), AddrWithUser(..))
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.Types (Message(..))
import Clapi.NamespaceTracker (Om, Ownership(Client))

-- The attributee stuff needs to be lining up through everything less hokily
attributeClientMsgs :: String -> [Om] -> [Om]
attributeClientMsgs u oms = map attributeClientMsg oms
  where
    -- Should this always apply or only if missing?
    attributeClientMsg (Client, MsgAdd p t v i _ s) = (Client, MsgAdd p t v i a s)
    attributeClientMsg (Client, MsgSet p t v i _ s) = (Client, MsgSet p t v i a s)
    attributeClientMsg (Client, MsgRemove p t _ s) = (Client, MsgRemove p t a s)
    attributeClientMsg (Client, MsgClear p t _ s) = (Client, MsgClear p t a s)
    attributeClientMsg m = m
    a = Just u

-- Not sure this really makes sense as a protocol, the above function could just go in the NSTracker
attributor :: Monad m => Protocol (ClientEvent (AddrWithUser i String) [Om] ()) (ClientEvent (AddrWithUser i String) [Om] ()) a a m ()
attributor = forever $ waitThen fwdAttributed sendRev
  where
    fwdAttributed (ClientData (AddrWithUser i u) ms) = sendFwd $ ClientData (AddrWithUser i u) (attributeClientMsgs u ms)
    fwdAttributed _ = return ()
