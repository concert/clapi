{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Clapi.Serialisation.Messages where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Monoid ((<>))

import Blaze.ByteString.Builder (Builder, fromWord32be)

import Data.Attoparsec.ByteString (Parser)

import Clapi.Serialisation.Base
  (Encodable(..), (<<>>), tdTaggedBuilder, tdTaggedParser)
import Clapi.Serialisation.Path ()
import Clapi.TH (btq)
import Clapi.Types.Messages
import Clapi.TaggedData
  ( TaggedData, taggedData, tdInstanceToTag, eitherTagged, tdAllTags
  , tdTagToEnum)
import Clapi.Serialisation.Wire ()

instance Encodable a => Encodable (ErrorIndex a) where
  builder = undefined
  parser = undefined

instance Encodable a => Encodable (MsgError a) where
    builder (MsgError ei s) = builder ei <<>> builder s
    parser = MsgError <$> parser <*> parser

instance Encodable a => Encodable (DefMessage a) where
    builder = undefined
    parser = undefined

data SubMsgType
  = SubMsgTSub
  | SubMsgTTypeSub
  | SubMsgTUnsub
  | SubMsgTTypeUnsub deriving (Enum, Bounded)

subMsgTaggedData :: TaggedData SubMsgType SubMessage
subMsgTaggedData = taggedData typeToTag msgToType
  where
    typeToTag (SubMsgTSub) = [btq|s|]
    typeToTag (SubMsgTTypeSub) = [btq|S|]
    typeToTag (SubMsgTUnsub) = [btq|u|]
    typeToTag (SubMsgTTypeUnsub) = [btq|U|]
    msgToType (MsgSubscribe _) = SubMsgTSub
    msgToType (MsgTypeSubscribe _) = SubMsgTTypeSub
    msgToType (MsgUnsubscribe _) = SubMsgTUnsub
    msgToType (MsgTypeUnsubscribe _) = SubMsgTTypeUnsub

instance Encodable SubMessage where
    builder = tdTaggedBuilder subMsgTaggedData $ builder . subMsgPath
    parser = tdTaggedParser subMsgTaggedData $ \e -> case e of
        (SubMsgTSub) -> MsgSubscribe <$> parser
        (SubMsgTTypeSub) -> MsgTypeSubscribe <$> parser
        (SubMsgTUnsub) -> MsgUnsubscribe <$> parser
        (SubMsgTTypeUnsub) -> MsgTypeUnsubscribe <$> parser

instance Encodable TypeMessage where
    builder = undefined
    parser = undefined

data DataUpdateMsgType
  = DUMTConstSet
  | DUMTSet
  | DUMTRemove
  | DUMTSetChildren deriving (Enum, Bounded)

dumtTaggedData :: TaggedData DataUpdateMsgType DataUpdateMessage
dumtTaggedData = taggedData typeToTag msgToType
  where
    typeToTag (DUMTConstSet) = [btq|S|]
    typeToTag (DUMTSet) = [btq|s|]
    typeToTag (DUMTRemove) = [btq|r|]
    typeToTag (DUMTSetChildren) = [btq|k|]
    msgToType (MsgConstSet _ _ _) = DUMTConstSet
    msgToType (MsgSet _ _ _ _ _ _) = DUMTSet
    msgToType (MsgRemove _ _ _) = DUMTRemove
    msgToType (MsgSetChildren _ _ _) = DUMTSetChildren

dumtParser :: DataUpdateMsgType -> Parser DataUpdateMessage
dumtParser e = case e of
    DUMTConstSet -> MsgConstSet <$> parser <*> parser <*> parser
    DUMTSet -> MsgSet <$> parser <*> parser <*> parser <*> parser <*> parser <*> parser
    DUMTRemove -> MsgRemove <$> parser <*> parser <*> parser
    DUMTSetChildren -> MsgSetChildren <$> parser <*> parser <*> parser

dumtBuilder :: MonadFail m => DataUpdateMessage -> m Builder
dumtBuilder m = case m of
    MsgConstSet p v a -> builder p <<>> builder v <<>> builder a
    MsgSet p ptId t v i a ->
      builder p <<>> builder ptId <<>> builder t <<>> builder v <<>> builder i
      <<>> builder a
    MsgRemove p t a ->
      builder p <<>> builder t <<>> builder a
    MsgSetChildren p ns a ->
      builder p <<>> builder ns <<>> builder a

instance Encodable DataUpdateMessage where
    builder = tdTaggedBuilder dumtTaggedData dumtBuilder
    parser = tdTaggedParser dumtTaggedData dumtParser

instance Encodable ToRelayBundle where
    builder = undefined
    parser = undefined

instance Encodable FromRelayBundle where
    builder = undefined
    parser = undefined

-- data TreeUpdateMsgType
--   = TUMTAssignType
--   | TUMTDelete deriving (Enum, Bounded)

-- tumtTaggedData :: TaggedData TreeUpdateMsgType TreeUpdateMessage
-- tumtTaggedData = taggedData typeToTag msgToType
--   where
--     typeToTag (TUMTAssignType) = [btq|A|]
--     typeToTag (TUMTDelete) = [btq|D|]
--     msgToType (MsgAssignType _ _) = TUMTAssignType
--     msgToType (MsgDelete _) = TUMTDelete

-- tumtBuilder :: MonadFail m => TreeUpdateMessage -> m Builder
-- tumtBuilder (MsgAssignType p tp) = builder p <<>> builder tp
-- tumtBuilder (MsgDelete p) = builder p

-- tumtParser :: TreeUpdateMsgType -> Parser TreeUpdateMessage
-- tumtParser (TUMTAssignType) = MsgAssignType <$> parser <*> parser
-- tumtParser (TUMTDelete) = MsgDelete <$> parser

-- parseEither
--   :: TaggedData (Either e f) (Either a b) -> (e -> Parser a) -> (f -> Parser b)
--   -> Parser (Either a b)
-- parseEither td pa pb =
--   tdTaggedParser td $ either (fmap Left . pa) (fmap Right . pb)

-- buildEither ::
--     MonadFail m =>
--     TaggedData (Either e f) (Either a b) ->
--     (a -> m Builder) ->
--     (b -> m Builder) ->
--     Either a b ->
--     m Builder
-- buildEither td ba bb eab =
--   (builder $ tdInstanceToTag td eab) <<>> either ba bb eab

-- oumTaggedData
--   :: TaggedData
--       (Either TreeUpdateMsgType DataUpdateMsgType)
--       (Either TreeUpdateMessage DataUpdateMessage)
-- oumTaggedData = eitherTagged tumtTaggedData dumtTaggedData

-- instance Encodable OwnerUpdateMessage where
--     builder = buildEither oumTaggedData tumtBuilder dumtBuilder
--     parser = parseEither oumTaggedData tumtParser dumtParser

-- instance Encodable RequestBundle where
--     builder (RequestBundle subs dums) = builder subs <<>> builder dums
--     parser = RequestBundle <$> parser <*> parser

-- instance Encodable UpdateBundle where
--     builder (UpdateBundle errs oums) = builder errs <<>> builder oums
--     parser = UpdateBundle <$> parser <*> parser

-- instance Encodable OwnerRequestBundle where
--     builder (OwnerRequestBundle errs dums) = builder errs <<>> builder dums
--     parser = OwnerRequestBundle <$> parser <*> parser

-- data ToRelayBundleTypeEnum
--   = RequestToRelayBundleType
--   | UpdateToRelayBundleType deriving (Enum, Bounded)

-- trbTaggedData :: TaggedData ToRelayBundleTypeEnum ToRelayBundle
-- trbTaggedData = taggedData typeToTag bundleType
--   where
--     typeToTag (RequestToRelayBundleType) = [btq|r|]
--     typeToTag (UpdateToRelayBundleType) = [btq|u|]
--     bundleType (TRBOwner _) = UpdateToRelayBundleType
--     bundleType (TRBClient _) = RequestToRelayBundleType

-- instance Encodable ToRelayBundle where
--     builder = tdTaggedBuilder trbTaggedData $ \b -> case b of
--         (TRBClient rb) -> builder rb
--         (TRBOwner ub) -> builder ub
--     parser = tdTaggedParser trbTaggedData $ \e -> case e of
--         (UpdateToRelayBundleType) -> TRBOwner <$> parser
--         (RequestToRelayBundleType) -> TRBClient <$> parser

-- data FromRelayBundleTypeEnum
--   = RequestFromRelayBundleType
--   | UpdateFromRelayBundleType deriving (Enum, Bounded)

-- frbTaggedData :: TaggedData FromRelayBundleTypeEnum FromRelayBundle
-- frbTaggedData = taggedData typeToTag bundleType
--   where
--     typeToTag (RequestFromRelayBundleType) = [btq|R|]
--     typeToTag (UpdateFromRelayBundleType) = [btq|U|]
--     bundleType (FRBOwner _) = RequestFromRelayBundleType
--     bundleType (FRBClient _) = UpdateFromRelayBundleType

-- instance Encodable FromRelayBundle where
--     builder = tdTaggedBuilder frbTaggedData $ \b -> case b of
--         (FRBClient rb) -> builder rb
--         (FRBOwner ub) -> builder ub
--     parser = tdTaggedParser frbTaggedData $ \e -> case e of
--         (UpdateFromRelayBundleType) -> FRBClient <$> parser
--         (RequestFromRelayBundleType) -> FRBOwner <$> parser
