{-# LANGUAGE FlexibleInstances #-}
module Serialisation
    (
      encode,
      decode,
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

import Types(
    CanFail, ClapiValue(..), ClapiMessage(..), Message(..), ClapiMessageTag,
    ClapiBundle, ClapiMethod(..), Time(..), Interpolation(..)
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
cvBuilder (CBool _) = return mempty
cvBuilder (CTime t) = builder t
cvBuilder (CEnum x) = return $ fromWord8 x
cvBuilder (CWord32 x) = return $ fromWord32be x
cvBuilder (CWord64 x) = return $ fromWord64be x
cvBuilder (CInt32 x) = return $ fromInt32be x
cvBuilder (CInt64 x) = return $ fromInt64be x
cvBuilder (CFloat x) = return $ floatBE x
cvBuilder (CDouble x) = return $ doubleBE x
cvBuilder (CString x) = builder x
cvBuilder (CList vs) = builder vs
-- cvBuilder (CList cvs) = aggregate <$> mapM build cvs
--   where
--     build cv = sequence (fromChar $ typeTag cv, cvBuilder cv)
--     aggregate bs = let (bs1, bs2) = unzip bs in mconcat bs1 <> mconcat bs2

cvParser :: Char -> Parser ClapiValue
cvParser 'F' = return $ CBool False
cvParser 'T' = return $ CBool True
cvParser 't' = CTime <$> (parser :: Parser Time)
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


encodeListN :: (Serialisable a) => [a] -> CanFail Builder
encodeListN = taggedEncode (const (1 :: Sum Word16)) builder

instance Serialisable ClapiBundle where
    builder = encodeListN where
    parser = do
        len <- parser :: Parser Word16
        messages <- count (fromIntegral len) (parser :: Parser ClapiMessage)
        return messages

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
