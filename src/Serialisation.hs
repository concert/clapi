{-# LANGUAGE FlexibleInstances #-}
module Serialisation
    (
      ClapiValue(..),
      ClapiMessage(..),
      encode,
      decode,
      someBytes
    ) where

import Data.Monoid ((<>), mconcat, Sum(..))
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString as B
import Data.Int (Int32, Int64)
import Data.Word (Word16, Word32, Word64)
import Blaze.ByteString.Builder (
  Builder, toByteString, fromInt32be, fromInt64be, fromWord16be, fromWord32be,
  fromWord64be)
import Data.ByteString.Builder(floatBE, doubleBE)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Data.ByteString.UTF8 (toString)

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as Ap
import Data.Attoparsec.Binary

import Path (BasePath(..), components, Method(..))
import Util (uncamel)

class Serialisable a where
    encode :: a -> B.ByteString
    encode x = toByteString $ builder x
    builder :: a -> Builder
    decode :: B.ByteString -> Either String a
    decode = parseOnly parser
    parser :: Parser a

-- FIXME: is there a way to generalise this Int handling?
instance Serialisable Int where
    builder = fromWord16be . fromIntegral
    parser = fromIntegral <$> anyWord16be

instance Serialisable Word16 where
    builder = fromWord16be
    parser = anyWord16be

-- FIXME: I kinda wanna generalise this to any functor?
instance Serialisable a => Serialisable (Sum a) where
    builder (Sum i) = builder i
    parser = Sum <$> parser


prefixLength :: Builder -> Builder
prefixLength b = byteSize bs <> fromByteString bs where
    bs = toByteString b
    {- FIXME: what do we do when the encoded string is more than 2^16 bytes
    long? -}
    byteSize = builder . B.length

instance Serialisable String where
    builder = prefixLength . fromString
    parser = do
        len <- parser :: Parser Word16
        bytes <- Ap.take $ fromIntegral len
        return $ toString bytes

instance Serialisable BasePath where
    builder p = builder . mconcat . map ("/" <>) $ components $ p
    parser = return (BasePath ["hello", "world"])

instance Serialisable Method where
    builder = builder . uncamel . show
    parser = return Error

data ClapiValue = CNil | CBool Bool | CTime Word64 Word32 |
    CWord32 Word32 | CWord64 Word64 |
    CInt32 Int32 | CInt64 Int64 |
    CFloat Float | CDouble Double |
    CString String | CList [ClapiValue] deriving (Eq, Show)

instance Serialisable ClapiValue where
    builder CNil = mempty
    builder (CBool True) = mempty
    builder (CBool False) = mempty
    builder (CTime x y) = fromWord64be x <> fromWord32be y
    builder (CWord32 x) = fromWord32be x
    builder (CWord64 x) = fromWord64be x
    builder (CInt32 x) = fromInt32be x
    builder (CInt64 x) = fromInt64be x
    builder (CFloat x) = floatBE x
    builder (CDouble x) = doubleBE x
    builder (CString x) = builder x
    builder (CList vs) = builder vs

    parser = return CNil

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
    builder derived <> dataBuilder where
        (derived, dataBuilder) = mconcat $ map getPair as

instance Serialisable [ClapiValue] where
    builder = taggedEncode getPair where
        getPair cv = ([typeTag cv], builder cv)
    parser = return [CNil]


type MsgTag = (String, ClapiValue)

-- FIXME: not sure this instance flexibility is a good thing or not!
instance Serialisable [MsgTag] where
    builder = taggedEncode getPair where
        getPair (name, cv) = ([typeTag cv], builder name <> builder cv)
    parser = return [("nope", CNil)]


data ClapiMessage = CMessage {
    msgPath :: BasePath,
    msgMethod :: Method,
    msgArgs :: [ClapiValue],
    msgTags :: [MsgTag]
} deriving (Eq, Show)

instance Serialisable ClapiMessage where
    builder m =
        (builder . msgPath $ m) <>
        (builder . msgMethod $ m) <>
        (builder . msgArgs $ m) <>
        (builder . msgTags $ m)
    parser = return (CMessage (BasePath ["hello"]) Error [] [])


type ClapiBundle = [ClapiMessage]

instance Serialisable ClapiBundle where
    builder = taggedEncode getPair where
        getPair msg = (1 :: Sum Word16, builder msg)
    parser = return []


-- Parsing stuff for the time being:
someBytes :: B.ByteString
someBytes = toByteString . fromWord16be $ 255
