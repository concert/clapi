module Clapi.Attributor where
import Control.Monad (forever)

import Clapi.Server (ClientEvent(ClientData), AddrWithUser(..))
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.Types (Message(..))
import Clapi.NamespaceTracker (Om, Ownership(Client))

-- The attributee stuff needs to be lining up through everything less hokily
attributeClientMsg u om = let a = Just u in case om of
    (Client, MsgAdd p t v i _ s) -> (Client, MsgAdd p t v i a s)
    (Client, MsgSet p t v i _ s) -> (Client, MsgSet p t v i a s)
    (Client, MsgRemove p t _ s) -> (Client, MsgRemove p t a s)
    (Client, MsgClear p t _ s) -> (Client, MsgClear p t a s)
    m -> m

-- Not sure this really makes sense as a protocol, the above function could just go in the NSTracker
attributor :: Monad m => Protocol (ClientEvent (AddrWithUser i String) [Om] ()) (ClientEvent (AddrWithUser i String) [Om] ()) a a m ()
attributor = forever $ waitThen fwdAttributed sendRev
  where
    fwdAttributed (ClientData (AddrWithUser i u) ms) = sendFwd $ ClientData (AddrWithUser i u) (fmap (attributeClientMsg u) ms)
    fwdAttributed _ = return ()
