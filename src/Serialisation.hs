module Serialisation
    (
      fromClapiString
    ) where

import Data.Monoid ((<>))
import Data.ByteString (ByteString, length)
import Blaze.ByteString.Builder (Builder, toByteString, fromInt32le)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

length' = Data.ByteString.length

{- Perhaps using a Builder here is overkill, because we have to encode the
entire string before we can prepend it's resulting length in bytes anyway. -}
fromClapiString :: String -> Builder
fromClapiString s =
  byteSize bytes <> fromByteString bytes where
    bytes = toByteString . fromString $ s
    {- FIXME: what do we do when the encoded string is more than 2^32 bytes
    long? -}
    byteSize = fromInt32le . fromIntegral . length'
