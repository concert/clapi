{-# LANGUAGE FlexibleInstances #-}
module Serialisation
    (
      encode,
      decode,
    ) where

import Data.Monoid ((<>), mconcat, Sum(..))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM2)
import qualified Data.ByteString as B
import Data.Word (Word8, Word16)
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
import Data.Attoparsec.Binary (anyWord16be, anyWord32be, anyWord64be)
import qualified Data.Attoparsec.ByteString as APBS
import qualified Data.Attoparsec.Text as APT

import Types(
    ClapiValue(..), ClapiMessage(..), ClapiMessageTag, ClapiBundle, ClapiPath,
    ClapiMethod(..)
    )
import Parsing (pathToString, pathParser, methodToString, methodParser)
import Util (composeParsers)

(<<>>) = liftM2 (<>)

encode :: Serialisable a => a -> Either String B.ByteString
encode x = toByteString <$> builder x

decode :: Serialisable a => B.ByteString -> Either String a
decode = parseOnly parser

class Serialisable a where
    builder :: a -> Either String Builder
    parser :: Parser a

-- FIXME: is there a way to generalise this Int handling?
instance Serialisable Int where
    builder = Right . fromWord16be . fromIntegral
    parser = fromIntegral <$> anyWord16be

instance Serialisable Word16 where
    builder = Right . fromWord16be
    parser = anyWord16be

-- FIXME: I kinda wanna generalise this to any functor?
instance Serialisable a => Serialisable (Sum a) where
    builder (Sum i) = builder i
    parser = Sum <$> parser


prefixLength :: Builder -> Either String Builder
prefixLength b = byteSize <<>> (return b) where
    byteSize = builder $ B.length $ toByteString b

decodeLengthPrefixedBytes :: (B.ByteString -> b) -> Parser b
decodeLengthPrefixedBytes decoder = do
    len <- parser :: Parser Word16
    bytes <- APBS.take $ fromIntegral len
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

cvBuilder :: ClapiValue -> Either String Builder
cvBuilder CNil = Right mempty
cvBuilder (CBool _) = Right mempty
cvBuilder (CTime x y) = Right $ fromWord64be x <> fromWord32be y
cvBuilder (CWord32 x) = Right $ fromWord32be x
cvBuilder (CWord64 x) = Right $ fromWord64be x
cvBuilder (CInt32 x) = Right $ fromInt32be x
cvBuilder (CInt64 x) = Right $ fromInt64be x
cvBuilder (CFloat x) = Right $ floatBE x
cvBuilder (CDouble x) = Right $ doubleBE x
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
    (a -> b) -> (a -> Either String Builder) -> [a] -> Either String Builder
taggedEncode derive build xs = do
    built <- foldl (<<>>) (return mempty) (map build xs)
    derived <- builder $ foldl (<>) mempty (map derive xs)
    return $ derived <> built

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
    builder = taggedEncode derive cvBuilder where
        derive cv = [typeTag cv]
    parser = do
        typeTags <- composeParsers parser parseTags
        sequenceParsers $ map cvParser typeTags

-- FIXME: not sure this instance flexibility is a good thing or not!
instance Serialisable [ClapiMessageTag] where
    builder = taggedEncode derive build where
        derive (_, cv) = [typeTag cv]
        build (name, cv) = builder name <<>> cvBuilder cv
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
        (builder . msgPath $ m) <<>>
        (builder . msgMethod $ m) <<>>
        (builder . msgArgs $ m) <<>>
        (builder . msgTags $ m)
    parser = CMessage <$> parser <*> parser <*> parser <*> parser

instance Serialisable ClapiBundle where
    builder = taggedEncode derive builder where
        derive _ = 1 :: Sum Word16
    parser = do
        len <- parser :: Parser Word16
        messages <- count (fromIntegral len) (parser :: Parser ClapiMessage)
        return messages
