{-# LANGUAGE FlexibleInstances #-}
module Clapi.Serialisation
    (
      encode,
      decode,
      parser,
      cvTaggedData,
      DataUpdateMsgType(..),
      dumtTaggedData,
      interpolationTaggedData,
      Serialisable
    ) where

import Data.Char (chr)
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

import Data.Attoparsec.ByteString (Parser, parseOnly, count, anyWord8, satisfy, inClass)
import Data.Attoparsec.Binary (anyWord16be, anyWord32be, anyWord64be)
import qualified Data.Attoparsec.ByteString as APBS
import qualified Data.Attoparsec.Text as APT

import Clapi.Types(
    CanFail, ClapiTypeEnum(..), ClapiValue(..), clapiValueType,
    ToRelayBundle(..), FromRelayBundle(..), Time(..), Interpolation(..),
    InterpolationType(..), interpolationType, UMsgError(..), SubMessage(..),
    DataUpdateMessage(..), TreeUpdateMessage(..), OwnerUpdateMessage(..),
    RequestBundle(..), UpdateBundle(..), OwnerRequestBundle(..))
import qualified Clapi.Path as Path
import qualified Path.Parsing as Path
import Clapi.Util (composeParsers)
import Clapi.TaggedData

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

lenBuilder :: Int -> CanFail Builder
lenBuilder x
  | x <= fromIntegral (maxBound :: Word16) =
      builder $ (fromIntegral x :: Word16)
  | otherwise = Left "Too long"

prefixLength :: Builder -> CanFail Builder
prefixLength b = (lenBuilder byteSize) <<>> (return b) where
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

instance Serialisable [T.Text] where
    builder = encodeListN
    parser = parseListN

instance Serialisable Char where
    builder = return . fromChar
    parser = T.head . decodeUtf8With' <$> APBS.take 1

instance Serialisable Time where
    builder (Time x y) = return (fromWord64be x <> fromWord32be y)
    parser = Time <$> (fromIntegral <$> anyWord64be) <*> (fromIntegral <$> anyWord32be)

instance Serialisable Path.Path where
    builder = builder . Path.toText
    parser = parser >>= Path.fromString

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

cvTaggedData = taggedData typeTag clapiValueType

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

instance Serialisable ClapiValue where
    builder = tdTaggedBuilder cvTaggedData cvBuilder
    parser = tdTaggedParser cvTaggedData cvParser
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
encodeListN items = (lenBuilder $ length items) <<>> (foldl (<<>>) (return mempty) (map builder items))

parseListN :: (Serialisable a) => Parser [a]
parseListN = do
    len <- parser :: Parser Word16
    count (fromIntegral len) parser

instance Serialisable [ClapiValue] where
    builder = encodeListN
    parser = parseListN

tdTaggedParser :: TaggedData e a -> (e -> Parser a) -> Parser a
tdTaggedParser td p = do
    t <- satisfy (inClass $ tdAllTags td)
    p $ tdTagToEnum td $ chr $ fromIntegral t

tdTaggedBuilder :: TaggedData e a -> (a -> CanFail Builder) -> a -> CanFail Builder
tdTaggedBuilder td bdr a = builder (tdInstanceToTag td $ a) <<>> bdr a

instance Serialisable UMsgError where
    builder (UMsgError p s) = builder p <<>> builder s
    parser = UMsgError <$> parser <*> parser

data SubMsgType
  = SubMsgTSub
  | SubMsgTUnsub deriving (Enum, Bounded)

subMsgTaggedData = taggedData typeToTag msgToType
  where
    typeToTag (SubMsgTSub) = 'S'
    typeToTag (SubMsgTUnsub) = 'U'
    msgToType (UMsgSubscribe _) = SubMsgTSub
    msgToType (UMsgUnsubscribe _) = SubMsgTUnsub

instance Serialisable SubMessage where
    builder = tdTaggedBuilder subMsgTaggedData $ builder . subMsgPath
    parser = tdTaggedParser subMsgTaggedData $ \e -> case e of
        (SubMsgTSub) -> UMsgSubscribe <$> parser
        (SubMsgTUnsub) -> UMsgUnsubscribe <$> parser

data DataUpdateMsgType
  = DUMTAdd
  | DUMTSet
  | DUMTRemove
  | DUMTClear
  | DUMTSetChildren deriving (Enum, Bounded)

dumtTaggedData = taggedData typeToTag msgToType
  where
    typeToTag (DUMTAdd) = 'a'
    typeToTag (DUMTSet) = 's'
    typeToTag (DUMTRemove) = 'r'
    typeToTag (DUMTClear) = 'c'
    typeToTag (DUMTSetChildren) = 'C'
    msgToType (UMsgAdd _ _ _ _ _ _) = DUMTAdd
    msgToType (UMsgSet _ _ _ _ _ _) = DUMTSet
    msgToType (UMsgRemove _ _ _ _) = DUMTRemove
    msgToType (UMsgClear _ _ _ _) = DUMTClear
    msgToType (UMsgSetChildren _ _ _) = DUMTSetChildren

dumtParser e = case e of
    (DUMTAdd) -> sap UMsgAdd
    (DUMTSet) -> sap UMsgSet
    (DUMTRemove) -> rcp UMsgRemove
    (DUMTClear) -> rcp UMsgClear
    (DUMTSetChildren) -> UMsgSetChildren <$> parser <*> parseListN <*> parser
  where
    sap mt = mt <$> parser <*> parser <*> parser <*> parser <*> parser <*> parser
    rcp mt = mt <$> parser <*> parser <*> parser <*> parser

dumtBuilder m = case m of
    (UMsgAdd p t v i a s) -> builder p <<>> builder t <<>> builder v <<>> builder i <<>> builder a <<>> builder s
    (UMsgSet p t v i a s) -> builder p <<>> builder t <<>> builder v <<>> builder i <<>> builder a <<>> builder s
    (UMsgRemove p t a s) -> builder p <<>> builder t <<>> builder a <<>> builder s
    (UMsgClear p t a s) -> builder p <<>> builder t <<>> builder a <<>> builder s
    (UMsgSetChildren p ns a) -> builder p <<>> encodeListN ns <<>> builder a

instance Serialisable DataUpdateMessage where
    builder = tdTaggedBuilder dumtTaggedData dumtBuilder
    parser = tdTaggedParser dumtTaggedData dumtParser

data TreeUpdateMsgType
  = TUMTAssignType
  | TUMTDelete deriving (Enum, Bounded)

tumtTaggedData = taggedData typeToTag msgToType
  where
    typeToTag (TUMTAssignType) = 'A'
    typeToTag (TUMTDelete) = 'D'
    msgToType (UMsgAssignType _ _) = TUMTAssignType
    msgToType (UMsgDelete _) = TUMTDelete

tumtBuilder :: TreeUpdateMessage -> CanFail Builder
tumtBuilder (UMsgAssignType p tp) = builder p <<>> builder tp
tumtBuilder (UMsgDelete p) = builder p

tumtParser :: TreeUpdateMsgType -> Parser TreeUpdateMessage
tumtParser (TUMTAssignType) = UMsgAssignType <$> parser <*> parser
tumtParser (TUMTDelete) = UMsgDelete <$> parser

parseEither :: TaggedData (Either e f) (Either a b) -> (e -> Parser a) -> (f -> Parser b) -> Parser (Either a b)
parseEither td pa pb = tdTaggedParser td $ either (fmap Left . pa) (fmap Right . pb)

buildEither ::
    TaggedData (Either e f) (Either a b) ->
    (a -> CanFail Builder) ->
    (b -> CanFail Builder) ->
    Either a b ->
    CanFail Builder
buildEither td ba bb eab = (builder $ tdInstanceToTag td eab) <<>> either ba bb eab

oumTaggedData = eitherTagged tumtTaggedData dumtTaggedData

instance Serialisable OwnerUpdateMessage where
    builder = buildEither oumTaggedData tumtBuilder dumtBuilder
    parser = parseEither oumTaggedData tumtParser dumtParser

instance Serialisable RequestBundle where
    builder (RequestBundle subs dums) = encodeListN subs <<>> encodeListN dums
    parser = RequestBundle <$> parseListN <*> parseListN

instance Serialisable UpdateBundle where
    builder (UpdateBundle errs oums) = encodeListN errs <<>> encodeListN oums
    parser = UpdateBundle <$> parseListN <*> parseListN

instance Serialisable OwnerRequestBundle where
    builder (OwnerRequestBundle errs dums) = encodeListN errs <<>> encodeListN dums
    parser = OwnerRequestBundle <$> parseListN <*> parseListN

data ToRelayBundleTypeEnum
  = RequestToRelayBundleType
  | UpdateToRelayBundleType deriving (Enum, Bounded)

trbTaggedData = taggedData typeToTag bundleType
  where
    typeToTag (RequestToRelayBundleType) = 'r'
    typeToTag (UpdateToRelayBundleType) = 'u'
    bundleType (TRBOwner _) = UpdateToRelayBundleType
    bundleType (TRBClient _) = RequestToRelayBundleType

instance Serialisable ToRelayBundle where
    builder = tdTaggedBuilder trbTaggedData $ \b -> case b of
        (TRBClient rb) -> builder rb
        (TRBOwner ub) -> builder ub
    parser = tdTaggedParser trbTaggedData $ \e -> case e of
        (UpdateToRelayBundleType) -> TRBOwner <$> parser
        (RequestToRelayBundleType) -> TRBClient <$> parser

data FromRelayBundleTypeEnum
  = RequestFromRelayBundleType
  | UpdateFromRelayBundleType deriving (Enum, Bounded)

frbTaggedData = taggedData typeToTag bundleType
  where
    typeToTag (RequestFromRelayBundleType) = 'R'
    typeToTag (UpdateFromRelayBundleType) = 'U'
    bundleType (FRBOwner _) = RequestFromRelayBundleType
    bundleType (FRBClient _) = UpdateFromRelayBundleType

instance Serialisable FromRelayBundle where
    builder = tdTaggedBuilder frbTaggedData $ \b -> case b of
        (FRBClient rb) -> builder rb
        (FRBOwner ub) -> builder ub
    parser = tdTaggedParser frbTaggedData $ \e -> case e of
        (UpdateFromRelayBundleType) -> FRBClient <$> parser
        (RequestFromRelayBundleType) -> FRBOwner <$> parser


badTag :: (MonadFail m) => String -> Char ->  m a
badTag n c = fail $ "Bad " ++ n ++ " type tag '" ++ (show c) ++ "'"


interpolationTaggedData = taggedData toTag interpolationType
  where
    toTag (ITConstant) = 'C'
    toTag (ITLinear) = 'L'
    toTag (ITBezier) = 'B'

instance Serialisable Interpolation where
    builder = tdTaggedBuilder interpolationTaggedData $ \i -> return $ case i of
        (IBezier a b) -> fromWord32be a <> fromWord32be b
        _ -> mempty
    parser = tdTaggedParser interpolationTaggedData $ \e -> case e of
        (ITConstant) -> return IConstant
        (ITLinear) -> return ILinear
        (ITBezier) -> IBezier <$> parser <*> parser


instance (Serialisable a) => Serialisable (Maybe a) where
    builder (Just a) = builder 'J' <<>> builder a
    builder Nothing = builder 'N'
    parser = do
        c <- parser
        case c of
          'J' -> Just <$> parser
          'N' -> return Nothing
          c -> badTag "maybe" c
