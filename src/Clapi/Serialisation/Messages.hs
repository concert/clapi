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
    builder = tdTaggedBuilder interpolationTaggedData $ \i -> return $ case i of
        (IBezier a b) -> fromWord32be a <> fromWord32be b
        _ -> mempty
    parser = tdTaggedParser interpolationTaggedData $ \e -> case e of
        (ITConstant) -> return IConstant
        (ITLinear) -> return ILinear
        (ITBezier) -> IBezier <$> parser <*> parser

instance Encodable Path.Seg where
    builder = builder . Path.unSeg
    parser = parser >>= Path.mkSeg

instance Encodable Path.Path where
    builder = builder . Path.toText
    parser = parser >>= Path.fromText

tdTaggedParser :: TaggedData e a -> (e -> Parser a) -> Parser a
tdTaggedParser td p = let at = tdAllTags td in do
    t <- parser
    if t `elem` at
      then p $ tdTagToEnum td t
      else fail $ "Invalid tag " ++ show t ++ " valid tags are " ++ show at

tdTaggedBuilder
  :: MonadFail m =>TaggedData e a -> (a -> m Builder) -> a -> m Builder
tdTaggedBuilder td bdr a = builder (tdInstanceToTag td $ a) <<>> bdr a

instance Encodable UMsgError where
    builder (UMsgError p s) = builder p <<>> builder s
    parser = UMsgError <$> parser <*> parser

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
    DUMTSetChildren -> UMsgSetChildren <$> parser <*> parser <*> parser
  where
    sap mt =
      mt <$> parser <*> parser <*> parser <*> parser <*> parser <*> parser
    rcp mt = mt <$> parser <*> parser <*> parser <*> parser

dumtBuilder :: MonadFail m => DataUpdateMessage -> m Builder
dumtBuilder m = case m of
    (UMsgAdd p t v i a s) ->
      builder p <<>> builder t <<>> builder v <<>> builder i <<>> builder a <<>>
      builder s
    (UMsgSet p t v i a s) ->
      builder p <<>> builder t <<>> builder v <<>> builder i <<>> builder a <<>>
      builder s
    (UMsgRemove p t a s) ->
      builder p <<>> builder t <<>> builder a <<>> builder s
    (UMsgClear p t a s) ->
      builder p <<>> builder t <<>> builder a <<>> builder s
    (UMsgSetChildren p ns a) ->
      builder p <<>> builder ns <<>> builder a

instance Encodable DataUpdateMessage where
    builder = tdTaggedBuilder dumtTaggedData dumtBuilder
    parser = tdTaggedParser dumtTaggedData dumtParser

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
tumtBuilder (UMsgAssignType p tp) = builder p <<>> builder tp
tumtBuilder (UMsgDelete p) = builder p

tumtParser :: TreeUpdateMsgType -> Parser TreeUpdateMessage
tumtParser (TUMTAssignType) = UMsgAssignType <$> parser <*> parser
tumtParser (TUMTDelete) = UMsgDelete <$> parser

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
  (builder $ tdInstanceToTag td eab) <<>> either ba bb eab

oumTaggedData
  :: TaggedData
      (Either TreeUpdateMsgType DataUpdateMsgType)
      (Either TreeUpdateMessage DataUpdateMessage)
oumTaggedData = eitherTagged tumtTaggedData dumtTaggedData

instance Encodable OwnerUpdateMessage where
    builder = buildEither oumTaggedData tumtBuilder dumtBuilder
    parser = parseEither oumTaggedData tumtParser dumtParser

instance Encodable RequestBundle where
    builder (RequestBundle subs dums) = builder subs <<>> builder dums
    parser = RequestBundle <$> parser <*> parser

instance Encodable UpdateBundle where
    builder (UpdateBundle errs oums) = builder errs <<>> builder oums
    parser = UpdateBundle <$> parser <*> parser

instance Encodable OwnerRequestBundle where
    builder (OwnerRequestBundle errs dums) = builder errs <<>> builder dums
    parser = OwnerRequestBundle <$> parser <*> parser

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
    builder = tdTaggedBuilder trbTaggedData $ \b -> case b of
        (TRBClient rb) -> builder rb
        (TRBOwner ub) -> builder ub
    parser = tdTaggedParser trbTaggedData $ \e -> case e of
        (UpdateToRelayBundleType) -> TRBOwner <$> parser
        (RequestToRelayBundleType) -> TRBClient <$> parser

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
    builder = tdTaggedBuilder frbTaggedData $ \b -> case b of
        (FRBClient rb) -> builder rb
        (FRBOwner ub) -> builder ub
    parser = tdTaggedParser frbTaggedData $ \e -> case e of
        (UpdateFromRelayBundleType) -> FRBClient <$> parser
        (RequestFromRelayBundleType) -> FRBOwner <$> parser
