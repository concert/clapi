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
import Data.Word (Word8, Word16, Word32, Word64)
import Data.List.Split (split, oneOf, dropDelims, dropInitBlank)
import Blaze.ByteString.Builder (
  Builder, toByteString, fromInt32be, fromInt64be, fromWord16be, fromWord32be,
  fromWord64be)
import Data.ByteString.Builder(floatBE, doubleBE)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromText, fromString)
import Data.ByteString.UTF8 (toString)
import Data.Binary.IEEE754 (wordToFloat, wordToDouble)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)

import Data.Attoparsec.ByteString (Parser, parseOnly, count)
import qualified Data.Attoparsec.ByteString as Ap
import Data.Attoparsec.Binary (anyWord16be, anyWord32be, anyWord64be)
import qualified Data.Attoparsec.Text as APT

import Types(
    ClapiValue(..), ClapiMessage(..), ClapiMessageTag, ClapiBundle, ClapiPath,
    ClapiMethod(..)
    )
import Parsing (pathToString, pathParser, methodToString, methodParser)
import Util (composeParsers)

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

decodeLengthPrefixedBytes :: (B.ByteString -> b) -> Parser b
decodeLengthPrefixedBytes decoder = do
    len <- parser :: Parser Word16
    bytes <- Ap.take $ fromIntegral len
    return $ decoder bytes

instance Serialisable String where
    builder = prefixLength . fromString
    parser = decodeLengthPrefixedBytes toString

instance Serialisable T.Text where
    builder = prefixLength . fromText
    parser = decodeLengthPrefixedBytes $ decodeUtf8With onError
      where
        onError :: String -> Maybe Word8 -> Maybe Char
        onError s Nothing = Nothing  -- End of input
        onError s (Just c) = Just '?'  -- Undecodable

instance Serialisable ClapiPath where
    builder = builder . pathToString
    parser = composeParsers parser pathParser

instance Serialisable ClapiMethod where
    builder = builder . methodToString
    parser = composeParsers parser methodParser


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
cvParser 's' = CString <$> (parser :: Parser T.Text)
cvParser 'l' = CList <$> (parser :: Parser [ClapiValue])


taggedEncode :: (Monoid b, Serialisable b) =>
    (a -> (b, Builder)) -> [a] -> Builder
taggedEncode getPair as =
    builder derived <> dataBuilder where
        (derived, dataBuilder) = mconcat $ map getPair as

sequenceParsers :: [Parser a] -> Parser [a]
sequenceParsers [] = return []
sequenceParsers (p:ps) = do
    result <- p
    rest <- sequenceParsers ps
    return $ result : rest

-- FIXME: Repitition of tag chars :-(
parseTags :: APT.Parser String
parseTags = APT.many' $ APT.satisfy (APT.inClass "NFTtuUiIdDsl")

instance Serialisable [ClapiValue] where
    builder = taggedEncode getPair where
        getPair cv = ([typeTag cv], cvBuilder cv)
    parser = do
        typeTags <- composeParsers parser parseTags
        sequenceParsers $ map cvParser typeTags

-- FIXME: not sure this instance flexibility is a good thing or not!
instance Serialisable [ClapiMessageTag] where
    builder = taggedEncode getPair where
        getPair (name, cv) = ([typeTag cv], builder name <> cvBuilder cv)
    parser = do
        -- FIXME: this feels really... proceedural :-/
        typeTags <- parser :: Parser T.Text
        sequenceParsers $ map getPairParser (T.unpack typeTags) where
            getPairParser typeTag = do
                name <- parser :: Parser T.Text
                value <- cvParser typeTag
                return ((T.unpack name), value)


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
