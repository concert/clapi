{-# LANGUAGE FlexibleInstances #-}
module Clapi.Serialisation
    (
      encode,
      decode,
      parser,
      typeTag,
      typeTags,
      valueTag
    ) where

import Data.Monoid ((<>), mconcat, Sum(..))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM2)
import Control.Monad.Fail (MonadFail)
import qualified Data.ByteString as B
import Data.Word (Word8, Word16, Word32, Word64)
import Blaze.ByteString.Builder (
    Builder, toByteString, fromInt32be, fromInt64be, fromWord8, fromWord16be,
    fromWord32be, fromWord64be)
import Data.ByteString.Builder(floatBE, doubleBE)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromChar, fromText, fromString)
import Data.ByteString.UTF8 (toString)
import Data.Binary.IEEE754 (wordToFloat, wordToDouble)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)

import Data.Attoparsec.ByteString (Parser, parseOnly, count, anyWord8)
import Data.Attoparsec.Binary (anyWord16be, anyWord32be, anyWord64be)
import qualified Data.Attoparsec.ByteString as APBS
import qualified Data.Attoparsec.Text as APT

import Clapi.Types(
    CanFail, ClapiTypeEnum(..), ClapiValue(..), clapiValueType, Bundle(..),
    Message(..), ClapiMethod(..), Time(..), Interpolation(..)
    )
import qualified Clapi.Path as Path
import qualified Path.Parsing as Path
import Clapi.Parsing (methodToString, methodParser)
import Clapi.Util (composeParsers)

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

instance Serialisable Word32 where
    builder = return . fromWord32be
    parser = anyWord32be

instance Serialisable Word64 where
    builder = return . fromWord64be
    parser = anyWord64be

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

decodeUtf8With' = decodeUtf8With onError
  where
    onError :: String -> Maybe Word8 -> Maybe Char
    onError s Nothing = Nothing  -- End of input
    onError s (Just c) = Just '?'  -- Undecodable

instance Serialisable T.Text where
    builder = prefixLength . fromText
    parser = decodeLengthPrefixedBytes decodeUtf8With'

instance Serialisable Char where
    builder = return . fromChar
    parser = T.head . decodeUtf8With' <$> APBS.take 1

instance Serialisable Time where
    builder (Time x y) = return (fromWord64be x <> fromWord32be y)
    parser = foo <$> (fromIntegral <$> anyWord64be) <*> (fromIntegral <$> anyWord32be)
      where
        foo x y = Time x y

instance Serialisable Path.Path where
    builder = builder . Path.toString
    parser = composeParsers parser Path.pathP

instance Serialisable ClapiMethod where
    builder = builder . methodToString
    parser = composeParsers parser methodParser


typeTag :: ClapiTypeEnum -> Char
typeTag ClTTime = 't'
typeTag ClTEnum = 'e'
typeTag ClTWord32 = 'u'
typeTag ClTWord64 = 'U'
typeTag ClTInt32 = 'i'
typeTag ClTInt64 = 'I'
typeTag ClTFloat = 'd'
typeTag ClTDouble = 'D'
typeTag ClTString = 's'
typeTag ClTList = 'l'

typeTags :: [Char]
typeTags = typeTag <$> [(minBound :: ClapiTypeEnum) ..]

valueTag :: ClapiValue -> Char
valueTag = typeTag . clapiValueType

typeFromTag :: (MonadFail m) => Char -> m ClapiTypeEnum
typeFromTag 't' = return ClTTime
typeFromTag 'e' = return ClTEnum
typeFromTag 'u' = return ClTWord32
typeFromTag 'U' = return ClTWord64
typeFromTag 'i' = return ClTInt32
typeFromTag 'I' = return ClTInt64
typeFromTag 'd' = return ClTFloat
typeFromTag 'D' = return ClTDouble
typeFromTag 's' = return ClTString
typeFromTag 'l' = return ClTList
typeFromTag c = fail $ "Unrecognised type tag: '" ++ [c] ++ "'"

cvBuilder :: ClapiValue -> CanFail Builder
cvBuilder (ClTime t) = builder t
cvBuilder (ClEnum x) = return $ fromWord8 x
cvBuilder (ClWord32 x) = return $ fromWord32be x
cvBuilder (ClWord64 x) = return $ fromWord64be x
cvBuilder (ClInt32 x) = return $ fromInt32be x
cvBuilder (ClInt64 x) = return $ fromInt64be x
cvBuilder (ClFloat x) = return $ floatBE x
cvBuilder (ClDouble x) = return $ doubleBE x
cvBuilder (ClString x) = builder x
cvBuilder (ClList vs) = builder vs


taggedEncode :: (Monoid b, Serialisable b) =>
    (a -> b) -> (a -> CanFail Builder) -> [a] -> CanFail Builder
taggedEncode derive build xs = derived <<>> built where
    built = foldl (<<>>) (return mempty) (map build xs)
    derived = builder $ foldl (<>) mempty (map derive xs)

parseTags :: APT.Parser String
parseTags = APT.many' $ APT.satisfy (APT.inClass typeTags)

instance Serialisable [ClapiValue] where
    builder = taggedEncode derive cvBuilder where
        derive cv = [valueTag cv]
    parser = do
        types <- composeParsers parser parseTags >>= mapM typeFromTag
        sequence $ map (cvParser) types
      where
        cvParser :: ClapiTypeEnum -> Parser ClapiValue
        cvParser ClTTime = ClTime <$> (parser :: Parser Time)
        cvParser ClTEnum = ClEnum <$> anyWord8
        cvParser ClTWord32 = ClWord32 <$> anyWord32be
        cvParser ClTWord64 = ClWord64 <$> anyWord64be
        cvParser ClTInt32 = ClInt32 <$> fromIntegral <$> anyWord32be
        cvParser ClTInt64 = ClInt64 <$> fromIntegral <$> anyWord64be
        cvParser ClTFloat = ClFloat <$> wordToFloat <$> anyWord32be
        cvParser ClTDouble = ClDouble <$> wordToDouble <$> anyWord64be
        cvParser ClTString = ClString <$> (parser :: Parser T.Text)
        cvParser ClTList = ClList <$> (parser :: Parser [ClapiValue])


encodeListN :: (Serialisable a) => [a] -> CanFail Builder
encodeListN = taggedEncode (const (1 :: Sum Word16)) builder

instance Serialisable [Message] where
    builder = encodeListN where
    parser = do
        len <- parser :: Parser Word16
        messages <- count (fromIntegral len) (parser :: Parser Message)
        return messages

instance Serialisable Bundle where
    builder = undefined
    parser = undefined

badTag :: (MonadFail m) => String -> Char ->  m a
badTag n c = fail $ "Bad " ++ n ++ " type tag '" ++ (show c) ++ "'"


instance Serialisable Message where
    builder (MsgError p m) = builder 'E' <<>> builder p <<>> builder m
    builder (MsgSet p time cvs i ma ms) =
        builder 's' <<>> builder p <<>> builder time <<>> builder cvs <<>>
        builder i <<>> builder ma <<>> builder ms
    builder (MsgAdd p time cvs i ma ms) =
        builder 'a' <<>> builder p <<>> builder time <<>> builder cvs <<>>
        builder i <<>> builder ma <<>> builder ms
    builder (MsgRemove p time ma ms) =
        builder 'r' <<>> builder p <<>> builder time
    builder (MsgClear p time ma ms) =
        builder 'c' <<>> builder p <<>> builder time
    builder (MsgSubscribe p) = builder 'S' <<>> builder p
    builder (MsgUnsubscribe p) = builder 'U' <<>> builder p
    builder (MsgAssignType np tp) = builder 'T' <<>> builder np <<>> builder tp
    builder (MsgDelete p) = builder 'D' <<>> builder p
    builder (MsgChildren p ns) = builder 'C' <<>> builder p <<>> encodeListN ns

    parser = parser >>= parseByTag
      where
        parseByTag 'E' = MsgError <$> parser <*> parser
        parseByTag 's' =
            MsgSet <$> parser <*> parser <*> parser <*> parser <*> parser <*>
            parser
        parseByTag 'a' =
            MsgAdd <$> parser <*> parser <*> parser <*> parser <*> parser <*>
            parser
        parseByTag 'r' = MsgRemove <$> parser <*> parser <*> parser <*> parser
        parseByTag 'c' = MsgClear <$> parser <*> parser <*> parser <*> parser
        parseByTag 'S' = MsgSubscribe <$> parser
        parseByTag 'U' = MsgUnsubscribe <$> parser
        parseByTag 'T' = MsgAssignType <$> parser <*> parser
        parseByTag 'D' = MsgDelete <$> parser
        parseByTag 'C' = MsgChildren <$> parser <*> parser
        parseByTag c = badTag "message" c


buildInterpolation :: Interpolation -> Builder
buildInterpolation IConstant = fromChar 'C'
buildInterpolation ILinear = fromChar 'L'
buildInterpolation (IBezier a b) = fromChar 'B' <> fromWord32be a <> fromWord32be b


instance Serialisable Interpolation where
    builder = return . buildInterpolation
    parser = do
        c <- parser
        case c of
          'C' -> return IConstant
          'L' -> return ILinear
          'B' -> IBezier <$> parser <*> parser
          c -> badTag "interpolation" c


instance (Serialisable a) => Serialisable (Maybe a) where
    builder (Just a) = builder 'J' <<>> builder a
    builder Nothing = builder 'N'
    parser = do
        c <- parser
        case c of
          'J' -> Just <$> parser
          'N' -> return Nothing
          c -> badTag "maybe" c
