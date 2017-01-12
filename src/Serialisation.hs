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
    Builder, toByteString, fromInt32be, fromInt64be, fromWord8, fromWord16be,
    fromWord32be, fromWord64be)
import Data.ByteString.Builder(floatBE, doubleBE)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromText, fromString)
import Data.ByteString.UTF8 (toString)
import Data.Binary.IEEE754 (wordToFloat, wordToDouble)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)

import Data.Attoparsec.ByteString (Parser, parseOnly, count, anyWord8)
import Data.Attoparsec.Binary (anyWord16be, anyWord32be, anyWord64be)
import qualified Data.Attoparsec.ByteString as APBS
import qualified Data.Attoparsec.Text as APT

import Types(
    CanFail, ClapiValue(..), ClapiMessage(..), ClapiMessageTag, ClapiBundle,
    ClapiMethod(..), Time(..)
    )
import qualified Path
import qualified Path.Parsing as Path
import Parsing (methodToString, methodParser)
import Util (composeParsers)

(<<>>) = liftM2 (<>)

encode :: Serialisable a => a -> CanFail B.ByteString
encode x = toByteString <$> builder x

decode :: Serialisable a => B.ByteString -> CanFail a
decode = parseOnly parser

class Serialisable a where
    builder :: a -> CanFail Builder
    parser :: Parser a

instance Serialisable Word16 where
    builder = return . fromWord16be
    parser = anyWord16be

instance Serialisable a => Serialisable (Sum a) where
    builder (Sum i) = builder i
    parser = Sum <$> parser

prefixLength :: Builder -> CanFail Builder
prefixLength b = (lenBuilder byteSize) <<>> (return b) where
    lenBuilder x
      | x <= fromIntegral (maxBound :: Word16) =
          builder $ (fromIntegral x :: Word16)
      | otherwise = Left "Too long"
    byteSize = B.length $ toByteString b

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

instance Serialisable Path.Path where
    builder = builder . Path.toString
    parser = composeParsers parser Path.pathP

instance Serialisable ClapiMethod where
    builder = builder . methodToString
    parser = composeParsers parser methodParser


typeTag :: ClapiValue -> Char
typeTag (CBool False) = 'F'
typeTag (CBool True) = 'T'
typeTag (CTime _) = 't'
typeTag (CEnum _) = 'e'
typeTag (CWord32 _) = 'u'
typeTag (CWord64 _) = 'U'
typeTag (CInt32 _) = 'i'
typeTag (CInt64 _) = 'I'
typeTag (CFloat _) = 'd'
typeTag (CDouble _) = 'D'
typeTag (CString _) = 's'
typeTag (CList _) = 'l'

cvBuilder :: ClapiValue -> CanFail Builder
cvBuilder (CBool _) = Right mempty
cvBuilder (CTime (Time x y)) = Right $ fromWord64be x <> fromWord32be y
cvBuilder (CEnum x) = Right $ fromWord8 x
cvBuilder (CWord32 x) = Right $ fromWord32be x
cvBuilder (CWord64 x) = Right $ fromWord64be x
cvBuilder (CInt32 x) = Right $ fromInt32be x
cvBuilder (CInt64 x) = Right $ fromInt64be x
cvBuilder (CFloat x) = Right $ floatBE x
cvBuilder (CDouble x) = Right $ doubleBE x
cvBuilder (CString x) = builder x
cvBuilder (CList vs) = builder vs

cvParser :: Char -> Parser ClapiValue
cvParser 'F' = return $ CBool False
cvParser 'T' = return $ CBool True
cvParser 't' =
    foo <$> (fromIntegral <$> anyWord64be) <*> (fromIntegral <$> anyWord32be)
  where
    foo x y = CTime $ Time x y
cvParser 'e' = CEnum <$> anyWord8
cvParser 'u' = CWord32 <$> anyWord32be
cvParser 'U' = CWord64 <$> anyWord64be
cvParser 'i' = CInt32 <$> fromIntegral <$> anyWord32be
cvParser 'I' = CInt64 <$> fromIntegral <$> anyWord64be
cvParser 'd' = CFloat <$> wordToFloat <$> anyWord32be
cvParser 'D' = CDouble <$> wordToDouble <$> anyWord64be
cvParser 's' = CString <$> (parser :: Parser T.Text)
cvParser 'l' = CList <$> (parser :: Parser [ClapiValue])


taggedEncode :: (Monoid b, Serialisable b) =>
    (a -> b) -> (a -> CanFail Builder) -> [a] -> CanFail Builder
taggedEncode derive build xs = derived <<>> built where
    built = foldl (<<>>) (return mempty) (map build xs)
    derived = builder $ foldl (<>) mempty (map derive xs)

-- FIXME: Repitition of tag chars :-(
parseTags :: APT.Parser String
parseTags = APT.many' $ APT.satisfy (APT.inClass "NFTteuUiIdDsl")

instance Serialisable [ClapiValue] where
    builder = taggedEncode derive cvBuilder where
        derive cv = [typeTag cv]
    parser = do
        typeTags <- composeParsers parser parseTags
        sequence $ map cvParser typeTags

-- FIXME: not sure this instance flexibility is a good thing or not!
instance Serialisable [ClapiMessageTag] where
    builder = taggedEncode derive build where
        derive (_, cv) = [typeTag cv]
        build (name, cv) = builder name <<>> cvBuilder cv
    parser = do
        -- FIXME: this feels really... proceedural :-/
        typeTags <- parser :: Parser T.Text
        sequence $ map getPairParser (T.unpack typeTags) where
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
