{-# LANGUAGE FlexibleInstances #-}
module Serialisation
    (
      encode,
      decode,
    ) where

import Data.Monoid ((<>), mconcat, Sum(..))
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString as B
import Data.Int (Int32, Int64)
import Data.Word (Word16, Word32, Word64)
import Data.List.Split (split, oneOf, dropDelims)
import Blaze.ByteString.Builder (
  Builder, toByteString, fromInt32be, fromInt64be, fromWord16be, fromWord32be,
  fromWord64be)
import Data.ByteString.Builder(floatBE, doubleBE)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Data.ByteString.UTF8 (toString)
import Data.Binary.IEEE754 (wordToFloat, wordToDouble)

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as Ap
import Data.Attoparsec.Binary

import Types(
    ClapiValue(..), ClapiMessage(..), ClapiMessageTag, ClapiBundle, ClapiPath,
    ClapiMethod(..)
    )
import Util (uncamel, camel)

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

instance Serialisable ClapiPath where
    builder p = builder . mconcat . map ("/" <>) $ p
    parser = do
        pathString <- parser :: Parser String
        -- FIXME: this should be a proper parser
        return $ split (dropDelims $ oneOf "/") pathString

instance Serialisable ClapiMethod where
    builder = builder . uncamel . show
    parser = do
        methodString <- parser :: Parser String
        return $ read . camel $ methodString


typeTag :: ClapiValue -> Char
typeTag CNil = 'N'
typeTag (CBool False) = 'F'
typeTag (CBool True) = 'T'
typeTag (CTime _ _) = 't'
typeTag (CWord32 _) = 'u'
typeTag (CWord64 _) = 'U'
typeTag (CInt32 _) = 'i'
typeTag (CInt64 _) = 'I'
typeTag (CFloat _) = 'd'
typeTag (CDouble _) = 'D'
typeTag (CString _) = 's'
typeTag (CList _) = 'l'

cvBuilder :: ClapiValue -> Builder
cvBuilder CNil = mempty
cvBuilder (CBool _) = mempty
cvBuilder (CTime x y) = fromWord64be x <> fromWord32be y
cvBuilder (CWord32 x) = fromWord32be x
cvBuilder (CWord64 x) = fromWord64be x
cvBuilder (CInt32 x) = fromInt32be x
cvBuilder (CInt64 x) = fromInt64be x
cvBuilder (CFloat x) = floatBE x
cvBuilder (CDouble x) = doubleBE x
cvBuilder (CString x) = builder x
cvBuilder (CList vs) = builder vs

cvParser :: Char -> Parser ClapiValue
cvParser 'N' = return CNil
cvParser 'F' = return $ CBool False
cvParser 'T' = return $ CBool True
cvParser 't' =
    CTime <$> (fromIntegral <$> anyWord64be) <*> (fromIntegral <$> anyWord32be)
cvParser 'u' = CWord32 <$> anyWord32be
cvParser 'U' = CWord64 <$> anyWord64be
cvParser 'i' = CInt32 <$> fromIntegral <$> anyWord32be
cvParser 'I' = CInt64 <$> fromIntegral <$> anyWord64be
cvParser 'd' = CFloat <$> wordToFloat <$> anyWord32be
cvParser 'D' = CDouble <$> wordToDouble <$> anyWord64be
cvParser 's' = CString <$> (parser :: Parser String)
cvParser 'l' = CList <$> (parser :: Parser [ClapiValue])


taggedEncode :: (Monoid b, Serialisable b) =>
    (a -> (b, Builder)) -> [a] -> Builder
taggedEncode getPair as =
    builder derived <> dataBuilder where
        (derived, dataBuilder) = mconcat $ map getPair as

sequenceParsers :: [Parser a] -> Parser [a]
sequenceParsers ps = foldl (>>=) (return []) $ map accumulatingParser ps where
    accumulatingParser :: Parser a -> [a] -> Parser [a]
    accumulatingParser p acc = do
        result <- p
        return $ acc ++ [result]

instance Serialisable [ClapiValue] where
    builder = taggedEncode getPair where
        getPair cv = ([typeTag cv], cvBuilder cv)
    parser = do
        typeTags <- parser :: Parser String
        sequenceParsers $ map cvParser typeTags

-- FIXME: not sure this instance flexibility is a good thing or not!
instance Serialisable [ClapiMessageTag] where
    builder = taggedEncode getPair where
        getPair (name, cv) = ([typeTag cv], builder name <> cvBuilder cv)
    parser = return [("nope", CNil)]


instance Serialisable ClapiMessage where
    builder m =
        (builder . msgPath $ m) <>
        (builder . msgMethod $ m) <>
        (builder . msgArgs $ m) <>
        (builder . msgTags $ m)
    parser = CMessage <$> parser <*> parser <*> parser <*> parser

instance Serialisable ClapiBundle where
    builder = taggedEncode getPair where
        getPair msg = (1 :: Sum Word16, builder msg)
    parser = do
        len <- parser :: Parser Word16
        messages <- count (fromIntegral len) (parser :: Parser ClapiMessage)
        return messages
