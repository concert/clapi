{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Clapi.Serialisation.Messages where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))

import Blaze.ByteString.Builder (Builder)

import Data.Attoparsec.ByteString (Parser)

import Clapi.Serialisation.Base
  (Encodable(..), (<<>>), tdTaggedBuilder, tdTaggedParser)
import Clapi.Serialisation.Definitions ()
import Clapi.Serialisation.Path ()
import Clapi.TH (btq)
import Clapi.Types.Messages
import Clapi.TaggedData (TaggedData, taggedData)
import Clapi.Serialisation.Wire ()

data ErrIdxType
  = EitGlobal
  | EitPath
  | EitTimePoint
  | EitPostTypeName
  | EitTypeName
  deriving (Enum, Bounded)

errIdxTaggedData :: TaggedData ErrIdxType (ErrorIndex a)
errIdxTaggedData = taggedData typeToTag eiToType
  where
    typeToTag ty = case ty of
      EitGlobal -> [btq|g|]
      EitPath -> [btq|p|]
      EitTimePoint -> [btq|t|]
      EitPostTypeName -> [btq|c|]  -- "create"
      EitTypeName -> [btq|n|]
    eiToType ei = case ei of
      GlobalError -> EitGlobal
      PathError _ -> EitPath
      TimePointError _ _ -> EitTimePoint
      PostTypeError _ -> EitPostTypeName
      TypeError _ -> EitTypeName

instance Encodable a => Encodable (ErrorIndex a) where
  builder = tdTaggedBuilder errIdxTaggedData $ \ei -> case ei of
    GlobalError -> return mempty
    PathError p -> builder p
    TimePointError p tpid -> builder p <<>> builder tpid
    PostTypeError tn -> builder tn
    TypeError tn -> builder tn
  parser = tdTaggedParser errIdxTaggedData $ \eit -> case eit of
    EitGlobal -> return GlobalError
    EitPath -> PathError <$> parser
    EitTimePoint -> TimePointError <$> parser <*> parser
    EitPostTypeName -> PostTypeError <$> parser
    EitTypeName -> TypeError <$> parser


instance Encodable a => Encodable (MsgError a) where
    builder (MsgError ei s) = builder ei <<>> builder s
    parser = MsgError <$> parser <*> parser

data DefMsgType = DefMsgTDef | DefMsgTUndef deriving (Enum, Bounded)

defMsgTaggedData :: TaggedData DefMsgType (DefMessage a def)
defMsgTaggedData = taggedData typeToTag msgToType
  where
    typeToTag ty = case ty of
      DefMsgTDef -> [btq|d|]
      DefMsgTUndef -> [btq|u|]
    msgToType msg = case msg of
      MsgDefine _ _ -> DefMsgTDef
      MsgUndefine _ -> DefMsgTUndef

instance (Encodable ident, Encodable def)
  => Encodable (DefMessage ident def) where
    builder = tdTaggedBuilder defMsgTaggedData $ \msg -> case msg of
      MsgDefine tn def -> builder tn <<>> builder def
      MsgUndefine tn -> builder tn
    parser = tdTaggedParser defMsgTaggedData $ \defTy -> case defTy of
      DefMsgTDef -> MsgDefine <$> parser <*> parser
      DefMsgTUndef -> MsgUndefine <$> parser

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
    builder = tdTaggedBuilder subMsgTaggedData $ \m -> case m of
        MsgSubscribe p -> builder p
        MsgUnsubscribe p -> builder p
        MsgTypeSubscribe t -> builder t
        MsgTypeUnsubscribe t -> builder t
    parser = tdTaggedParser subMsgTaggedData $ \e -> case e of
        (SubMsgTSub) -> MsgSubscribe <$> parser
        (SubMsgTTypeSub) -> MsgTypeSubscribe <$> parser
        (SubMsgTUnsub) -> MsgUnsubscribe <$> parser
        (SubMsgTTypeUnsub) -> MsgTypeUnsubscribe <$> parser

instance Encodable TypeMessage where
    builder (MsgAssignType p tn l) = builder p <<>> builder tn <<>> builder l
    parser = MsgAssignType <$> parser <*> parser <*> parser

data DataUpdateMsgType
  = DUMTConstSet
  | DUMTSet
  | DUMTRemove
  deriving (Enum, Bounded)

dumtTaggedData :: TaggedData DataUpdateMsgType DataUpdateMessage
dumtTaggedData = taggedData typeToTag msgToType
  where
    typeToTag DUMTConstSet = [btq|S|]
    typeToTag DUMTSet = [btq|s|]
    typeToTag DUMTRemove = [btq|r|]
    msgToType (MsgConstSet {}) = DUMTConstSet
    msgToType (MsgSet {}) = DUMTSet
    msgToType (MsgRemove {}) = DUMTRemove

dumtParser :: DataUpdateMsgType -> Parser DataUpdateMessage
dumtParser e = case e of
    DUMTConstSet -> MsgConstSet <$> parser <*> parser <*> parser
    DUMTSet -> MsgSet <$> parser <*> parser <*> parser <*> parser <*> parser <*> parser
    DUMTRemove -> MsgRemove <$> parser <*> parser <*> parser

dumtBuilder :: MonadFail m => DataUpdateMessage -> m Builder
dumtBuilder m = case m of
    MsgConstSet p v a -> builder p <<>> builder v <<>> builder a
    MsgSet p ptId t v i a ->
      builder p <<>> builder ptId <<>> builder t <<>> builder v <<>> builder i
      <<>> builder a
    MsgRemove p t a ->
      builder p <<>> builder t <<>> builder a

instance Encodable DataUpdateMessage where
    builder = tdTaggedBuilder dumtTaggedData dumtBuilder
    parser = tdTaggedParser dumtTaggedData dumtParser


data ContainerUpdateMsgType
  = CUMTPresentAfter
  | CUMTAbsent
  deriving (Enum, Bounded)

cumtTaggedData :: TaggedData ContainerUpdateMsgType ContainerUpdateMessage
cumtTaggedData = taggedData typeToTag msgToType
  where
    typeToTag CUMTPresentAfter = [btq|>|]
    typeToTag CUMTAbsent = [btq|-|]
    msgToType (MsgPresentAfter {}) = CUMTPresentAfter
    msgToType (MsgAbsent {}) = CUMTAbsent

cumtParser :: ContainerUpdateMsgType -> Parser ContainerUpdateMessage
cumtParser e = case e of
  CUMTPresentAfter ->
    MsgPresentAfter <$> parser <*> parser <*> parser <*> parser
  CUMTAbsent -> MsgAbsent <$> parser <*> parser <*> parser

cumtBuilder :: MonadFail m => ContainerUpdateMessage -> m Builder
cumtBuilder msg = case msg of
  MsgPresentAfter p t r a ->
    builder p <<>> builder t <<>> builder r <<>> builder a
  MsgAbsent p t a ->
    builder p <<>> builder t <<>> builder a

instance Encodable ContainerUpdateMessage where
    parser = tdTaggedParser cumtTaggedData cumtParser
    builder = tdTaggedBuilder cumtTaggedData cumtBuilder

data TrBundleType
  = TrbtProvider | TrbtProviderRelinquish | TrbtClient deriving (Enum, Bounded)

trBundleTaggedData :: TaggedData TrBundleType ToRelayBundle
trBundleTaggedData = taggedData typeToTag bundleToType
  where
    typeToTag ty = case ty of
      TrbtProvider -> [btq|P|]
      TrbtProviderRelinquish -> [btq|R|]
      TrbtClient ->  [btq|C|]
    bundleToType bund = case bund of
      Trpb _ -> TrbtProvider
      Trpr _ -> TrbtProviderRelinquish
      Trcb _ -> TrbtClient

instance Encodable ToRelayBundle where
    builder = tdTaggedBuilder trBundleTaggedData $ \bund -> case bund of
      Trpb (ToRelayProviderBundle ns errs defs dat contMsgs) ->
        builder ns <<>> builder errs <<>> builder defs <<>> builder dat
        <<>> builder contMsgs
      Trpr (ToRelayProviderRelinquish ns) -> builder ns
      Trcb (ToRelayClientBundle subs dat contMsgs) ->
        builder subs <<>> builder dat <<>> builder contMsgs
    parser = tdTaggedParser trBundleTaggedData $ \ty -> case ty of
      TrbtProvider -> Trpb <$>
        (ToRelayProviderBundle <$> parser <*> parser <*> parser <*> parser
        <*> parser)
      TrbtProviderRelinquish -> Trpr . ToRelayProviderRelinquish <$> parser
      TrbtClient -> Trcb <$>
        (ToRelayClientBundle <$> parser <*> parser <*> parser)

data FrBundleType
  = FrbtProvider | FrbtProviderError | FrbtClient deriving (Enum, Bounded)

frBundleTaggedData :: TaggedData FrBundleType FromRelayBundle
frBundleTaggedData = taggedData typeToTag bundleToType
  where
    typeToTag ty = case ty of
      FrbtProvider -> [btq|P|]
      FrbtProviderError -> [btq|E|]
      FrbtClient ->  [btq|C|]
    bundleToType bund = case bund of
      Frpb _ -> FrbtProvider
      Frpeb _ -> FrbtProviderError
      Frcb _ -> FrbtClient

instance Encodable FromRelayBundle where
    builder = tdTaggedBuilder frBundleTaggedData $ \bund -> case bund of
      Frpb (FromRelayProviderBundle ns dat contMsgs) ->
        builder ns <<>> builder dat <<>> builder contMsgs
      Frpeb (FromRelayProviderErrorBundle errs) -> builder errs
      Frcb (FromRelayClientBundle tyUns datUns errs defs tas dat contMsgs) ->
        builder tyUns <<>> builder datUns <<>> builder errs <<>> builder defs
        <<>> builder tas <<>> builder dat <<>> builder contMsgs
    parser = tdTaggedParser frBundleTaggedData $ \ty -> case ty of
      FrbtProvider -> Frpb <$>
        (FromRelayProviderBundle <$> parser <*> parser <*> parser)
      FrbtProviderError ->
        Frpeb . FromRelayProviderErrorBundle <$> parser
      FrbtClient -> Frcb <$>
        (FromRelayClientBundle <$> parser <*> parser <*>  parser <*> parser
        <*> parser <*> parser <*> parser)
