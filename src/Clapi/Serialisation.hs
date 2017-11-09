{-# LANGUAGE FlexibleInstances #-}
module Clapi.Serialisation
    (
      encode,
      decode,
      parser,
      typeTag,
      typeTags,
      typeFromTag,
      valueTag,
      interpolationTaggedData,
      TaggedData(..),
      tdTotalBuilder,
      Serialisable
    ) where

import Data.List (intersect)
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
    CanFail, ClapiTypeEnum(..), ClapiValue(..), clapiValueType, Bundle(..),
    Message(..), ClapiMethod(..), Time(..), Interpolation(..), InterpolationType(..), interpolationType,
    UMsgError(..), SubMessage(..), DataUpdateMessage(..),
    TreeUpdateMessage(..), OwnerUpdateMessage(..),
    RequestBundle(..), UpdateBundle(..))
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

cvTaggedData = genTagged typeTag clapiValueType

typeTags :: [Char]
typeTags = tdAllTags cvTaggedData

valueTag :: ClapiValue -> Char
valueTag = tdEnumToTag cvTaggedData . tdTypeToEnum cvTaggedData

typeFromTag :: (MonadFail m) => Char -> m ClapiTypeEnum
typeFromTag t = either fail return $ tdTagToEnum cvTaggedData t

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

parseListN :: (Serialisable a) => Parser [a]
parseListN = do
    len <- parser :: Parser Word16
    count (fromIntegral len) parser

instance Serialisable [Message] where
    builder = encodeListN
    parser = parseListN

data TaggedData e a = TaggedData {
    tdEnumToTag :: e -> Char,
    -- Given the way this integrates with parsers, not sure the CanFail buys us
    -- anything:
    tdTagToEnum :: Char -> CanFail e,
    tdAllTags :: [Char],
    tdTypeToEnum :: a -> e}

tdTotalParser :: TaggedData e a -> (e -> Parser a) -> Parser a
tdTotalParser td p = do
    t <- satisfy (inClass $ tdAllTags td)
    let te = case tdTagToEnum td $ chr $ fromIntegral t of
            (Left m) -> error m  -- Should never get here!
            (Right te) -> te
    p te

tdTotalBuilder :: TaggedData e a -> (a -> CanFail Builder) -> a -> CanFail Builder
tdTotalBuilder td bdr a = builder (tdEnumToTag td $ (tdTypeToEnum td) a) <<>> bdr a

genTagged :: (Enum e, Bounded e) => (e -> Char) -> (a -> e) -> TaggedData e a
genTagged toTag typeToEnum = TaggedData toTag fromTag allTags typeToEnum
  where
    tagMap = (\ei -> (toTag ei, ei)) <$> [minBound ..]
    allTags = fst <$> tagMap
    fromTag t = case lookup t tagMap of
        Just e -> return e
        Nothing -> fail $ "Unrecognised tag: '" ++ [t] ++ "' expecting one of '" ++ allTags ++ "'"

instance Serialisable UMsgError where
    builder (UMsgError p s) = builder p <<>> builder s
    parser = do
        p <- parser
        s <- parser
        return $ UMsgError p s

data SubMsgType
  = SubMsgTSub
  | SubMsgTUnsub deriving (Enum, Bounded)

subMsgTaggedData = genTagged typeToTag msgToType
  where
    typeToTag (SubMsgTSub) = 'S'
    typeToTag (SubMsgTUnsub) = 'U'
    msgToType (UMsgSubscribe _) = SubMsgTSub
    msgToType (UMsgUnsubscribe _) = SubMsgTUnsub

instance Serialisable SubMessage where
    builder = tdTotalBuilder subMsgTaggedData $ builder . subMsgPath
    parser = tdTotalParser subMsgTaggedData $ \e -> case e of
        (SubMsgTSub) -> UMsgSubscribe <$> parser
        (SubMsgTUnsub) -> UMsgUnsubscribe <$> parser

data DataUpdateMsgType
  = DUMTAdd
  | DUMTSet
  | DUMTRemove
  | DUMTClear
  | DUMTSetChildren deriving (Enum, Bounded)

dumtTaggedData = genTagged typeToTag msgToType
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
    (DUMTSetChildren) -> do
        p <- parser
        ns <- parseListN
        a <- parser
        return $ UMsgSetChildren p ns a
  where
    sap mt = do
        p <- parser
        t <- parser
        v <- parser
        i <- parser
        a <- parser
        s <- parser
        return $ mt p t v i a s
    rcp mt = do
        p <- parser
        t <- parser
        a <- parser
        s <- parser
        return $ mt p t a s

dumtBuilder m = case m of
    (UMsgAdd p t v i a s) -> builder p <<>> builder t <<>> builder v <<>> builder i <<>> builder a <<>> builder s
    (UMsgSet p t v i a s) -> builder p <<>> builder t <<>> builder v <<>> builder i <<>> builder a <<>> builder s
    (UMsgRemove p t a s) -> builder p <<>> builder t <<>> builder a <<>> builder s
    (UMsgClear p t a s) -> builder p <<>> builder t <<>> builder a <<>> builder s
    (UMsgSetChildren p ns a) -> builder p <<>> encodeListN ns <<>> builder a

instance Serialisable DataUpdateMessage where
    builder = tdTotalBuilder dumtTaggedData dumtBuilder
    parser = tdTotalParser dumtTaggedData dumtParser

data TUMT
  = TUMTAssignType
  | TUMTDelete deriving (Enum, Bounded)

tumtTaggedData = genTagged typeToTag msgToType
  where
    typeToTag (TUMTAssignType) = 'A'
    typeToTag (TUMTDelete) = 'D'
    msgToType (UMsgAssignType _ _) = TUMTAssignType
    msgToType (UMsgDelete _) = TUMTDelete

tumtBuilder :: TreeUpdateMessage -> CanFail Builder
tumtBuilder (UMsgAssignType p tp) = builder p <<>> builder tp
tumtBuilder (UMsgDelete p) = builder p

tumtParser :: TUMT -> Parser TreeUpdateMessage
tumtParser (TUMTAssignType) = do
    p <- parser
    tp <- parser
    return $ UMsgAssignType p tp
tumtParser (TUMTDelete) = UMsgDelete <$> parser

eitherTagged :: TaggedData e a -> TaggedData f b -> TaggedData (Either e f) (Either a b)
eitherTagged a b = case intersect (tdAllTags a) (tdAllTags b) of
    [] -> TaggedData toTag fromTag allTags typeToEnum
    i -> error $ "Tags overlap: " ++ i
  where
    allTags = (tdAllTags a) ++ (tdAllTags b)
    isATag t = t `elem` tdAllTags a
    toTag = either (tdEnumToTag a) (tdEnumToTag b)
    dcf mf = case mf of
        Left m -> error "dave had trouble with nested monads so this error doesn't propagate"
        Right v -> v
    fromTag t = return $ case isATag t of
        True -> Left $ dcf $ tdTagToEnum a t
        False -> Right $ dcf $ tdTagToEnum b t
    typeToEnum = either (Left <$> tdTypeToEnum a) (Right <$> tdTypeToEnum b)

parseEither :: TaggedData (Either e f) (Either a b) -> (e -> Parser a) -> (f -> Parser b) -> Parser (Either a b)
parseEither td pa pb = tdTotalParser td subParse
  where
    subParse eorf = either (\e -> Left <$> pa e) (\f -> Right <$> pb f) eorf

buildEither ::
    TaggedData (Either e f) (Either a b) ->
    (a -> CanFail Builder) ->
    (b -> CanFail Builder) ->
    Either a b ->
    CanFail Builder
buildEither td ba bb eab = (builder $ tdEnumToTag td $ tdTypeToEnum td eab) <<>> either ba bb eab

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

data BundleTypeEnum
  = RequestBundleType
  | UpdateBundleType deriving (Enum, Bounded)

bundleTaggedData = genTagged typeToTag bundleType
  where
    typeToTag (RequestBundleType) = 'r'
    typeToTag (UpdateBundleType) = 'u'
    bundleType (Left (UpdateBundle _ _)) = UpdateBundleType
    bundleType (Right (RequestBundle _ _)) = RequestBundleType

instance Serialisable Bundle where
    builder = tdTotalBuilder bundleTaggedData $ either builder builder
    parser = tdTotalParser bundleTaggedData $ \e -> case e of
        (UpdateBundleType) -> Left <$> parser
        (RequestBundleType) -> Right <$> parser


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


interpolationTaggedData = genTagged toTag interpolationType
  where
    toTag (ITConstant) = 'C'
    toTag (ITLinear) = 'L'
    toTag (ITBezier) = 'B'

instance Serialisable Interpolation where
    builder = tdTotalBuilder interpolationTaggedData $ \i -> return $ case i of
        (IBezier a b) -> fromWord32be a <> fromWord32be b
        _ -> mempty
    parser = tdTotalParser interpolationTaggedData $ \e -> case e of
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
