{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module Clapi.SerialisationProtocol (serialiser) where

import Data.Attoparsec.ByteString (parse, IResult(..))
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)

import Blaze.ByteString.Builder (toByteString)

import Clapi.Types ()
import Clapi.Serialisation.Base (Encodable(..), Decodable(..))
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)

serialiser
  :: (Decodable a, Encodable b, Monad m)
  => Protocol ByteString a ByteString b m ()
serialiser = serialiser' $ parse parser
  where
    serialiser' p = waitThen (fwd p) (rev p)
    fwd parseNext bs = case parseNext bs of
        Fail _ _ctxs err -> sendRev $ fromString err
        Partial cont -> serialiser' cont
        Done unconsumed a -> sendFwd a >> fwd (parse parser) unconsumed
    rev p b = either
        (\s -> error $ "builder failed: " ++ s)
        (\bs -> sendRev bs >> serialiser' p)
        (toByteString <$> builder b)
