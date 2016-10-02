{-# LANGUAGE FlexibleInstances #-}
module Serialisation
    (
      ClapiValue(..),
      ClapiMessage(..),
      encode
    ) where

import Data.Monoid ((<>), Sum)
import qualified Data.ByteString as B
import Data.Int (Int32, Int64)
import Blaze.ByteString.Builder (
  Builder, toByteString, fromInt32be, fromInt64be)
import Data.ByteString.Builder(floatBE, doubleBE)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString, fromChar)

import Path (Path, toOsc)

prefixLength :: Builder -> Builder
prefixLength b = byteSize bs <> fromByteString bs where
    bs = toByteString b
    {- FIXME: what do we do when the encoded string is more than 2^32 bytes
    long? -}
    byteSize = fromInt32be . fromIntegral . B.length

class Serialisable a where
    encode :: a -> Builder

instance Serialisable String where
    encode = prefixLength . fromString

data ClapiValue = CNil | CBool Bool | CTimeTag Int32 Int32 |
    CInt32 Int32 | CInt64 Int64 | CFloat Float | CDouble Double |
    CString String | CList [ClapiValue] deriving (Eq, Show)

instance Serialisable ClapiValue where
    encode CNil = mempty
    encode (CBool True) = mempty
    encode (CBool False) = mempty
    encode (CTimeTag x y) = fromInt32be x <> fromInt32be y
    encode (CInt32 x) = fromInt32be x
    encode (CInt64 x) = fromInt64be x
    encode (CFloat x) = floatBE x
    encode (CDouble x) = doubleBE x
    encode (CString x) = encode x
    encode (CList vs) = encode vs

typeTag :: ClapiValue -> Char
typeTag CNil = 'N'
typeTag (CBool _) = 'F'
typeTag (CTimeTag _ _) = 't'
typeTag (CInt32 _) = 'i'
typeTag (CInt64 _) = 'h'
typeTag (CFloat _) = 'f'
typeTag (CDouble _) = 'd'
typeTag (CString _) = 's'
typeTag (CList _) = 'l'


taggedEncode :: (a -> (String, Builder)) -> [a] -> Builder
taggedEncode getPair as =
    encode typeTagString <> builder where
        (typeTagString, builder) = foldl add mempty as
        add acc a = acc <> getPair a

instance Serialisable [ClapiValue] where
    encode = taggedEncode getPair where
        getPair cv = ([typeTag cv], encode cv)


type MsgTag = (String, ClapiValue)

-- FIXME: not sure this instance flexibility is a good thing or not!
instance Serialisable [MsgTag] where
    encode = taggedEncode getPair where
        getPair (name, cv) = ([typeTag cv], encode name <> encode cv)

instance Serialisable Path where
    encode = encode . toOsc


data ClapiMessage = CMessage {
    msgPath :: Path,
    msgArgs :: [ClapiValue],
    msgTags :: [MsgTag]
}

instance Serialisable ClapiMessage where
    encode m =
        (encode . msgPath $ m) <>
        (encode . msgArgs $ m) <>
        (encode . msgTags $ m)


type ClapiPacket = [ClapiMessage]

instance Serialisable ClapiPacket where
    encode ms = fromInt32be nMsgs <> builder where
        (nMsgs, builder) = foldl addMsg (0, mempty) ms
        addMsg (nMsgs, builder) m = (nMsgs + 1, builder <> encode m)
