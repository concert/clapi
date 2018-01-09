{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Clapi.Serialisation.Messages where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Monoid ((<>))

import Blaze.ByteString.Builder (Builder, fromWord32be)

import Data.Attoparsec.ByteString (Parser)

import qualified Clapi.Path as Path
import Clapi.Serialisation.Base (Encodable(..), (<<>>))
import Clapi.Types.Base (InterpolationType(..), Interpolation(..), interpolationType)
import Clapi.Types.ByteTagQ (btq)
import Clapi.Types.Messages
import Clapi.TaggedData (TaggedData, taggedData, tdInstanceToTag, eitherTagged, tdAllTags, tdTagToEnum)
import Clapi.Serialisation.Wire ()

interpolationTaggedData :: TaggedData InterpolationType Interpolation
interpolationTaggedData = taggedData toTag interpolationType
  where
    toTag (ITConstant) = [btq|C|]
    toTag (ITLinear) = [btq|L|]
    toTag (ITBezier) = [btq|B|]

instance Encodable Interpolation where
    encode = tdTaggedBuilder interpolationTaggedData $ \i -> return $ case i of
        (IBezier a b) -> fromWord32be a <> fromWord32be b
        _ -> mempty
    decode = tdTaggedParser interpolationTaggedData $ \e -> case e of
        (ITConstant) -> return IConstant
        (ITLinear) -> return ILinear
        (ITBezier) -> IBezier <$> decode <*> decode

instance Encodable Path.Seg where
    encode = encode . Path.unSeg
    decode = decode >>= Path.mkSeg

instance Encodable Path.Path where
    encode = encode . Path.toText
    decode = decode >>= Path.fromText

tdTaggedParser :: TaggedData e a -> (e -> Parser a) -> Parser a
tdTaggedParser td p = let at = tdAllTags td in do
    t <- decode
    if t `elem` at
      then p $ tdTagToEnum td t
      else fail $ "Invalid tag " ++ show t ++ " valid tags are " ++ show at

tdTaggedBuilder
  :: MonadFail m =>TaggedData e a -> (a -> m Builder) -> a -> m Builder
tdTaggedBuilder td bdr a = encode (tdInstanceToTag td $ a) <<>> bdr a

instance Encodable UMsgError where
    encode (UMsgError p s) = encode p <<>> encode s
    decode = UMsgError <$> decode <*> decode

data SubMsgType
  = SubMsgTSub
  | SubMsgTUnsub deriving (Enum, Bounded)

subMsgTaggedData :: TaggedData SubMsgType SubMessage
subMsgTaggedData = taggedData typeToTag msgToType
  where
    typeToTag (SubMsgTSub) = [btq|S|]
    typeToTag (SubMsgTUnsub) = [btq|U|]
    msgToType (UMsgSubscribe _) = SubMsgTSub
    msgToType (UMsgUnsubscribe _) = SubMsgTUnsub

instance Encodable SubMessage where
    encode = tdTaggedBuilder subMsgTaggedData $ encode . subMsgPath
    decode = tdTaggedParser subMsgTaggedData $ \e -> case e of
        (SubMsgTSub) -> UMsgSubscribe <$> decode
        (SubMsgTUnsub) -> UMsgUnsubscribe <$> decode

data DataUpdateMsgType
  = DUMTAdd
  | DUMTSet
  | DUMTRemove
  | DUMTClear
  | DUMTSetChildren deriving (Enum, Bounded)

dumtTaggedData :: TaggedData DataUpdateMsgType DataUpdateMessage
dumtTaggedData = taggedData typeToTag msgToType
  where
    typeToTag (DUMTAdd) = [btq|a|]
    typeToTag (DUMTSet) = [btq|s|]
    typeToTag (DUMTRemove) = [btq|r|]
    typeToTag (DUMTClear) = [btq|c|]
    typeToTag (DUMTSetChildren) = [btq|C|]
    msgToType (UMsgAdd _ _ _ _ _ _) = DUMTAdd
    msgToType (UMsgSet _ _ _ _ _ _) = DUMTSet
    msgToType (UMsgRemove _ _ _ _) = DUMTRemove
    msgToType (UMsgClear _ _ _ _) = DUMTClear
    msgToType (UMsgSetChildren _ _ _) = DUMTSetChildren

dumtParser :: DataUpdateMsgType -> Parser DataUpdateMessage
dumtParser e = case e of
    DUMTAdd -> sap UMsgAdd
    DUMTSet -> sap UMsgSet
    DUMTRemove -> rcp UMsgRemove
    DUMTClear -> rcp UMsgClear
    DUMTSetChildren -> UMsgSetChildren <$> decode <*> decode <*> decode
  where
    sap mt =
      mt <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode
    rcp mt = mt <$> decode <*> decode <*> decode <*> decode

dumtBuilder :: MonadFail m => DataUpdateMessage -> m Builder
dumtBuilder m = case m of
    (UMsgAdd p t v i a s) ->
      encode p <<>> encode t <<>> encode v <<>> encode i <<>> encode a <<>>
      encode s
    (UMsgSet p t v i a s) ->
      encode p <<>> encode t <<>> encode v <<>> encode i <<>> encode a <<>>
      encode s
    (UMsgRemove p t a s) ->
      encode p <<>> encode t <<>> encode a <<>> encode s
    (UMsgClear p t a s) ->
      encode p <<>> encode t <<>> encode a <<>> encode s
    (UMsgSetChildren p ns a) ->
      encode p <<>> encode ns <<>> encode a

instance Encodable DataUpdateMessage where
    encode = tdTaggedBuilder dumtTaggedData dumtBuilder
    decode = tdTaggedParser dumtTaggedData dumtParser

data TreeUpdateMsgType
  = TUMTAssignType
  | TUMTDelete deriving (Enum, Bounded)

tumtTaggedData :: TaggedData TreeUpdateMsgType TreeUpdateMessage
tumtTaggedData = taggedData typeToTag msgToType
  where
    typeToTag (TUMTAssignType) = [btq|A|]
    typeToTag (TUMTDelete) = [btq|D|]
    msgToType (UMsgAssignType _ _) = TUMTAssignType
    msgToType (UMsgDelete _) = TUMTDelete

tumtBuilder :: MonadFail m => TreeUpdateMessage -> m Builder
tumtBuilder (UMsgAssignType p tp) = encode p <<>> encode tp
tumtBuilder (UMsgDelete p) = encode p

tumtParser :: TreeUpdateMsgType -> Parser TreeUpdateMessage
tumtParser (TUMTAssignType) = UMsgAssignType <$> decode <*> decode
tumtParser (TUMTDelete) = UMsgDelete <$> decode

parseEither
  :: TaggedData (Either e f) (Either a b) -> (e -> Parser a) -> (f -> Parser b)
  -> Parser (Either a b)
parseEither td pa pb =
  tdTaggedParser td $ either (fmap Left . pa) (fmap Right . pb)

buildEither ::
    MonadFail m =>
    TaggedData (Either e f) (Either a b) ->
    (a -> m Builder) ->
    (b -> m Builder) ->
    Either a b ->
    m Builder
buildEither td ba bb eab =
  (encode $ tdInstanceToTag td eab) <<>> either ba bb eab

oumTaggedData
  :: TaggedData
      (Either TreeUpdateMsgType DataUpdateMsgType)
      (Either TreeUpdateMessage DataUpdateMessage)
oumTaggedData = eitherTagged tumtTaggedData dumtTaggedData

instance Encodable OwnerUpdateMessage where
    encode = buildEither oumTaggedData tumtBuilder dumtBuilder
    decode = parseEither oumTaggedData tumtParser dumtParser

instance Encodable RequestBundle where
    encode (RequestBundle subs dums) = encode subs <<>> encode dums
    decode = RequestBundle <$> decode <*> decode

instance Encodable UpdateBundle where
    encode (UpdateBundle errs oums) = encode errs <<>> encode oums
    decode = UpdateBundle <$> decode <*> decode

instance Encodable OwnerRequestBundle where
    encode (OwnerRequestBundle errs dums) = encode errs <<>> encode dums
    decode = OwnerRequestBundle <$> decode <*> decode

data ToRelayBundleTypeEnum
  = RequestToRelayBundleType
  | UpdateToRelayBundleType deriving (Enum, Bounded)

trbTaggedData :: TaggedData ToRelayBundleTypeEnum ToRelayBundle
trbTaggedData = taggedData typeToTag bundleType
  where
    typeToTag (RequestToRelayBundleType) = [btq|r|]
    typeToTag (UpdateToRelayBundleType) = [btq|u|]
    bundleType (TRBOwner _) = UpdateToRelayBundleType
    bundleType (TRBClient _) = RequestToRelayBundleType

instance Encodable ToRelayBundle where
    encode = tdTaggedBuilder trbTaggedData $ \b -> case b of
        (TRBClient rb) -> encode rb
        (TRBOwner ub) -> encode ub
    decode = tdTaggedParser trbTaggedData $ \e -> case e of
        (UpdateToRelayBundleType) -> TRBOwner <$> decode
        (RequestToRelayBundleType) -> TRBClient <$> decode

data FromRelayBundleTypeEnum
  = RequestFromRelayBundleType
  | UpdateFromRelayBundleType deriving (Enum, Bounded)

frbTaggedData :: TaggedData FromRelayBundleTypeEnum FromRelayBundle
frbTaggedData = taggedData typeToTag bundleType
  where
    typeToTag (RequestFromRelayBundleType) = [btq|R|]
    typeToTag (UpdateFromRelayBundleType) = [btq|U|]
    bundleType (FRBOwner _) = RequestFromRelayBundleType
    bundleType (FRBClient _) = UpdateFromRelayBundleType

instance Encodable FromRelayBundle where
    encode = tdTaggedBuilder frbTaggedData $ \b -> case b of
        (FRBClient rb) -> encode rb
        (FRBOwner ub) -> encode ub
    decode = tdTaggedParser frbTaggedData $ \e -> case e of
        (UpdateFromRelayBundleType) -> FRBClient <$> decode
        (RequestFromRelayBundleType) -> FRBOwner <$> decode
