module Clapi.Auth where
import Control.Monad (forever)
import qualified Data.ByteString as B

import Clapi.Server (ClientEvent(..), ServerEvent(..), AddrWithUser(..))
import Clapi.Protocol (Protocol, sendFwd, sendRev, waitThen)

noAuth :: Monad m => Protocol
    (ClientEvent i B.ByteString b)
    (ClientEvent (AddrWithUser i String) B.ByteString b)
    (ServerEvent i B.ByteString)
    (ServerEvent i B.ByteString)
    m ()
noAuth = forever $ waitThen fwd sendRev
  where
    fwd (ClientConnect i b) = sendFwd $ ClientConnect (AddrWithUser i "someone") b
    fwd (ClientDisconnect i) = sendFwd $ ClientDisconnect (AddrWithUser i "someone")
    fwd (ClientData i d) = sendFwd $ ClientData (AddrWithUser i "someone") d
