{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Clapi.Serialisation
  ( encode'
  , decode'
  , module X
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.ByteString (ByteString)

import Blaze.ByteString.Builder (toByteString)
import Data.Attoparsec.ByteString (parseOnly, endOfInput)

import Clapi.Serialisation.Base (Encodable(..))
import Clapi.Serialisation.Base as X
import Clapi.Serialisation.Messages as X
import Clapi.Serialisation.Wire as X

-- FIXME: maybe these should be in test utils, because we don't want to use
-- them straight off the wire because of chunking.
encode' :: (MonadFail m, Encodable a) => a -> m ByteString
encode' x = toByteString <$> encode x

decode' :: (MonadFail m, Encodable a) => ByteString -> m a
decode' = either fail return . parseOnly (decode <* endOfInput)
