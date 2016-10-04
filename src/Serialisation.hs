{-# LANGUAGE FlexibleInstances #-}
module Serialisation
    (
      ClapiValue(..),
      ClapiMessage(..),
      encode
    ) where

import Data.Monoid ((<>), mconcat, Sum(..))
import qualified Data.ByteString as B
import Data.Int (Int32, Int64)
import Data.Word (Word16, Word32, Word64)
import Blaze.ByteString.Builder (
  Builder, toByteString, fromInt32be, fromInt64be, fromWord16be, fromWord32be,
  fromWord64be)
import Data.ByteString.Builder(floatBE, doubleBE)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString, fromChar)

import Path (BasePath(..), components, Method(..))
import Util (uncamel)

class Serialisable a where
    encode :: a -> Builder

-- FIXME: is there a way to generalise this Int handling?
instance Serialisable Int where
    encode = fromWord16be . fromIntegral

instance Serialisable (Sum Int) where
    encode (Sum i) = encode i

instance Serialisable BasePath where
    encode p = encode . mconcat . map ("/" <>) $ components $ p

instance Serialisable Method where
    encode = encode . uncamel . show


prefixLength :: Builder -> Builder
prefixLength b = byteSize bs <> fromByteString bs where
    bs = toByteString b
    {- FIXME: what do we do when the encoded string is more than 2^16 bytes
    long? -}
    byteSize = encode . B.length

instance Serialisable String where
    encode = prefixLength . fromString

data ClapiValue = CNil | CBool Bool | CTime Word64 Word32 |
    CWord32 Word32 | CWord64 Word64 |
    CInt32 Int32 | CInt64 Int64 |
    CFloat Float | CDouble Double |
    CString String | CList [ClapiValue] deriving (Eq, Show)

instance Serialisable ClapiValue where
    encode CNil = mempty
    encode (CBool True) = mempty
    encode (CBool False) = mempty
    encode (CTime x y) = fromWord64be x <> fromWord32be y
    encode (CWord32 x) = fromWord32be x
    encode (CWord64 x) = fromWord64be x
    encode (CInt32 x) = fromInt32be x
    encode (CInt64 x) = fromInt64be x
    encode (CFloat x) = floatBE x
    encode (CDouble x) = doubleBE x
    encode (CString x) = encode x
    encode (CList vs) = encode vs

typeTag :: ClapiValue -> Char
typeTag CNil = 'N'
typeTag (CBool _) = 'F'
typeTag (CTime _ _) = 't'
typeTag (CWord32 _) = 'u'
typeTag (CWord64 _) = 'U'
typeTag (CInt32 _) = 'i'
typeTag (CInt64 _) = 'I'
typeTag (CFloat _) = 'd'
typeTag (CDouble _) = 'D'
typeTag (CString _) = 's'
typeTag (CList _) = 'l'


taggedEncode :: (Monoid b, Serialisable b) =>
    (a -> (b, Builder)) -> [a] -> Builder
taggedEncode getPair as =
    encode typeTagString <> builder where
        (typeTagString, builder) = mconcat $ map getPair as

instance Serialisable [ClapiValue] where
    encode = taggedEncode getPair where
        getPair cv = ([typeTag cv], encode cv)


type MsgTag = (String, ClapiValue)

-- FIXME: not sure this instance flexibility is a good thing or not!
instance Serialisable [MsgTag] where
    encode = taggedEncode getPair where
        getPair (name, cv) = ([typeTag cv], encode name <> encode cv)


data ClapiMessage = CMessage {
    msgPath :: BasePath,
    msgMethod :: Method,
    msgArgs :: [ClapiValue],
    msgTags :: [MsgTag]
}

instance Serialisable ClapiMessage where
    encode m =
        (encode . msgPath $ m) <>
        (encode . msgMethod $ m) <>
        (encode . msgArgs $ m) <>
        (encode . msgTags $ m)


type ClapiBundle = [ClapiMessage]

instance Serialisable ClapiBundle where
    encode = taggedEncode getPair where
        getPair msg = (1 :: Sum Int, encode msg)
