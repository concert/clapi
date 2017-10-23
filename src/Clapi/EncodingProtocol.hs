module Clapi.EncodingProtocol (serialiser) where
import Data.Attoparsec.ByteString (parse, Result, IResult(..))
import Clapi.Serialisation (encode, parser)
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.Types (Message)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)

serialiser :: Monad m => Protocol ByteString [Message] ByteString [Message] m ()
serialiser = serialiser' $ parse parser
  where
    serialiser' p = waitThen (fwd p) (rev p)
    fwd parseNext bs = case parseNext bs of
        Fail _ ctxs err -> sendRev $ fromString err
        Partial cont -> serialiser' cont
        Done unconsumed msgs -> sendFwd msgs >> fwd (parse parser) unconsumed
    rev p msgs = either (error "encode failed") (\bs -> sendRev bs >> serialiser' p) (encode msgs)
