{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleInstances
  , LambdaCase
#-}

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

data DataErrIdxType
  = DeitGlobal
  | DeitPath
  | DeitTimePoint
  deriving (Enum, Bounded)

dataErrIndexTaggedData :: TaggedData DataErrIdxType DataErrorIndex
dataErrIndexTaggedData = taggedData typeToTag eiToType
  where
    typeToTag ty = case ty of
      DeitGlobal -> [btq|g|]
      DeitPath -> [btq|p|]
      DeitTimePoint -> [btq|t|]
    eiToType ei = case ei of
      GlobalError -> DeitGlobal
      PathError _ -> DeitPath
      TimePointError _ _ -> DeitTimePoint

instance Encodable DataErrorIndex where
  builder = tdTaggedBuilder dataErrIndexTaggedData $ \ei -> case ei of
    GlobalError -> return mempty
    PathError p -> builder p
    TimePointError p tpid -> builder p <<>> builder tpid
  parser = tdTaggedParser dataErrIndexTaggedData $ \eit -> case eit of
    DeitGlobal -> return GlobalError
    DeitPath -> PathError <$> parser
    DeitTimePoint -> TimePointError <$> parser <*> parser

instance Encodable DataErrorMessage where
    builder (MsgDataError ei s) = builder ei <<>> builder s
    parser = MsgDataError <$> parser <*> parser

data SubErrIdxType
  = SeitNamespace
  | SeitPostTypeName
  | SeitTypeName
  | SeitPath
  deriving (Enum, Bounded)

subErrIndexTaggedData :: TaggedData SubErrIdxType SubErrorIndex
subErrIndexTaggedData = taggedData typeToTag eiToType
  where
    typeToTag ty = case ty of
      SeitNamespace -> [btq|n|]
      SeitPostTypeName -> [btq|c|] -- "create"
      SeitTypeName -> [btq|t|]
      SeitPath -> [btq|p|]
    eiToType ei = case ei of
      NamespaceSubError _ -> SeitNamespace
      PostTypeSubError _ _ -> SeitPostTypeName
      TypeSubError _ _ -> SeitTypeName
      PathSubError _ _ -> SeitPath

instance Encodable SubErrorIndex where
  builder = tdTaggedBuilder subErrIndexTaggedData $ \ei -> case ei of
    NamespaceSubError ns -> builder ns
    PostTypeSubError ns ttn -> builder ns <<>> builder ttn
    TypeSubError ns ttn -> builder ns <<>> builder ttn
    PathSubError ns p -> builder ns <<>> builder p
  parser = tdTaggedParser subErrIndexTaggedData $ \eit -> case eit of
    SeitNamespace -> NamespaceSubError <$> parser
    SeitPostTypeName -> PostTypeSubError <$> parser <*> parser
    SeitTypeName -> TypeSubError <$> parser <*> parser
    SeitPath -> PathSubError <$> parser <*> parser

instance Encodable SubErrorMessage where
    builder (MsgSubError idx txt) = builder idx <<>> builder txt
    parser = MsgSubError <$> parser <*> parser

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
  | SubMsgTPostTypeSub
  | SubMsgTTypeSub
  | SubMsgTUnsub
  | SubMsgTPostTypeUnsub
  | SubMsgTTypeUnsub
  deriving (Enum, Bounded)

subMsgTaggedData :: TaggedData SubMsgType SubMessage
subMsgTaggedData = taggedData typeToTag msgToType
  where
    typeToTag (SubMsgTSub) = [btq|S|]
    typeToTag (SubMsgTPostTypeSub) = [btq|P|]
    typeToTag (SubMsgTTypeSub) = [btq|T|]
    typeToTag (SubMsgTUnsub) = [btq|s|]
    typeToTag (SubMsgTPostTypeUnsub) = [btq|p|]
    typeToTag (SubMsgTTypeUnsub) = [btq|t|]
    msgToType (MsgSubscribe _) = SubMsgTSub
    msgToType (MsgPostTypeSubscribe _) = SubMsgTPostTypeSub
    msgToType (MsgTypeSubscribe _) = SubMsgTTypeSub
    msgToType (MsgUnsubscribe _) = SubMsgTUnsub
    msgToType (MsgPostTypeUnsubscribe _) = SubMsgTPostTypeUnsub
    msgToType (MsgTypeUnsubscribe _) = SubMsgTTypeUnsub

instance Encodable SubMessage where
    builder = tdTaggedBuilder subMsgTaggedData $ \m -> case m of
        MsgSubscribe p -> builder p
        MsgUnsubscribe p -> builder p
        MsgTypeSubscribe t -> builder t
        MsgTypeUnsubscribe t -> builder t
        MsgPostTypeSubscribe t -> builder t
        MsgPostTypeUnsubscribe t -> builder t
    parser = tdTaggedParser subMsgTaggedData $ \e -> case e of
        (SubMsgTSub) -> MsgSubscribe <$> parser
        (SubMsgTPostTypeSub) -> MsgPostTypeSubscribe <$> parser
        (SubMsgTTypeSub) -> MsgTypeSubscribe <$> parser
        (SubMsgTUnsub) -> MsgUnsubscribe <$> parser
        (SubMsgTPostTypeUnsub) -> MsgPostTypeUnsubscribe <$> parser
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

data TccumType
  = TccumtPresentAfter
  | TccumtAbsent
  deriving (Enum, Bounded)

tccumTaggedData :: TaggedData TccumType ToClientContainerUpdateMessage
tccumTaggedData = taggedData typeToTag msgToType
  where
    typeToTag TccumtPresentAfter = [btq|>|]
    typeToTag TccumtAbsent = [btq|-|]
    msgToType (TccumPresentAfter {}) = TccumtPresentAfter
    msgToType (TccumAbsent {}) = TccumtAbsent

instance Encodable ToClientContainerUpdateMessage where
  builder = tdTaggedBuilder tccumTaggedData $ \case
    TccumPresentAfter targ ref att ->
      builder targ <<>> builder ref <<>> builder att
    TccumAbsent targ att ->
      builder targ <<>> builder att
  parser = tdTaggedParser tccumTaggedData $ \case
    TccumtPresentAfter -> TccumPresentAfter <$> parser <*> parser <*> parser
    TccumtAbsent -> TccumAbsent <$> parser <*> parser


data TpcumType
  = TpcumtCreateAfter
  | TpcumtMoveAfter
  | TpcumtAbsent
  deriving (Enum, Bounded)

tpcumTaggedData :: TaggedData TpcumType ToProviderContainerUpdateMessage
tpcumTaggedData = taggedData typeToTag msgToType
  where
    typeToTag TpcumtCreateAfter = [btq|+|]
    typeToTag TpcumtMoveAfter = [btq|>|]
    typeToTag TpcumtAbsent = [btq|-|]
    msgToType (TpcumCreateAfter {}) = TpcumtCreateAfter
    msgToType (TpcumMoveAfter {}) = TpcumtMoveAfter
    msgToType (TpcumAbsent {}) = TpcumtAbsent

instance Encodable ToProviderContainerUpdateMessage where
  builder = tdTaggedBuilder tpcumTaggedData $ \case
    TpcumCreateAfter args ph ref att ->
      builder args <<>> builder ph <<>> builder ref <<>> builder att
    TpcumMoveAfter targ ref att ->
      builder targ <<>> builder ref <<>> builder att
    TpcumAbsent targ att ->
      builder targ <<>> builder att
  parser = tdTaggedParser tpcumTaggedData $ \case
    TpcumtCreateAfter -> TpcumCreateAfter
      <$> parser <*> parser <*> parser <*> parser
    TpcumtMoveAfter -> TpcumMoveAfter <$> parser <*> parser <*> parser
    TpcumtAbsent -> TpcumAbsent <$> parser <*> parser

data TrBundleType
  = TrbtProvider
  | TrbtProviderRelinquish
  | TrbtClientSub
  | TrbtClientUpdate
  deriving (Enum, Bounded)

trBundleTaggedData :: TaggedData TrBundleType ToRelayBundle
trBundleTaggedData = taggedData typeToTag bundleToType
  where
    typeToTag ty = case ty of
      TrbtProvider -> [btq|P|]
      TrbtProviderRelinquish -> [btq|R|]
      TrbtClientSub ->  [btq|S|]
      TrbtClientUpdate ->  [btq|U|]
    bundleToType bund = case bund of
      Trpb _ -> TrbtProvider
      Trpr _ -> TrbtProviderRelinquish
      Trcsb _ -> TrbtClientSub
      Trcub _ -> TrbtClientUpdate

instance Encodable ToRelayBundle where
    builder = tdTaggedBuilder trBundleTaggedData $ \bund -> case bund of
      Trpb (ToRelayProviderBundle ns errs postDefs defs dat contMsgs) ->
        builder ns <<>> builder errs <<>> builder postDefs <<>> builder defs
        <<>> builder dat <<>> builder contMsgs
      Trpr (ToRelayProviderRelinquish ns) -> builder ns
      Trcsb (ToRelayClientSubBundle subs) -> builder subs
      Trcub (ToRelayClientUpdateBundle ns dat contMsgs) ->
        builder ns <<>> builder dat <<>> builder contMsgs
    parser = tdTaggedParser trBundleTaggedData $ \ty -> case ty of
      TrbtProvider -> Trpb <$>
        (ToRelayProviderBundle <$> parser <*> parser <*> parser <*> parser
        <*> parser <*> parser)
      TrbtProviderRelinquish -> Trpr . ToRelayProviderRelinquish <$> parser
      TrbtClientSub -> Trcsb <$> (ToRelayClientSubBundle <$> parser)
      TrbtClientUpdate -> Trcub <$>
        (ToRelayClientUpdateBundle <$> parser <*> parser <*> parser)

data FrBundleType
  = FrbtProvider
  | FrbtProviderError
  | FrbtClientRoot
  | FrbtClientSub
  | FrbtClientUpdate
  deriving (Enum, Bounded)

frBundleTaggedData :: TaggedData FrBundleType FromRelayBundle
frBundleTaggedData = taggedData typeToTag bundleToType
  where
    typeToTag ty = case ty of
      FrbtProvider -> [btq|P|]
      FrbtProviderError -> [btq|E|]
      FrbtClientRoot -> [btq|R|]
      FrbtClientSub ->  [btq|S|]
      FrbtClientUpdate ->  [btq|U|]
    bundleToType bund = case bund of
      Frpb _ -> FrbtProvider
      Frpeb _ -> FrbtProviderError
      Frcrb _ -> FrbtClientRoot
      Frcsb _ -> FrbtClientSub
      Frcub _ -> FrbtClientUpdate

instance Encodable FromRelayBundle where
    builder = tdTaggedBuilder frBundleTaggedData $ \bund -> case bund of
      Frpb (FromRelayProviderBundle ns dat contMsgs) ->
        builder ns <<>> builder dat <<>> builder contMsgs
      Frpeb (FromRelayProviderErrorBundle errs) -> builder errs
      Frcrb (FromRelayClientRootBundle cops) -> builder cops
      Frcsb (FromRelayClientSubBundle subErrs ptUnsubs tUnsubs dUnsubs) ->
        builder subErrs <<>> builder ptUnsubs <<>> builder tUnsubs
        <<>> builder dUnsubs
      Frcub (
          FromRelayClientUpdateBundle ns errs postDefs defs tas dat contMsgs) ->
        builder ns <<>> builder errs <<>> builder postDefs <<>> builder defs
        <<>> builder tas <<>> builder dat <<>> builder contMsgs
    parser = tdTaggedParser frBundleTaggedData $ \ty -> case ty of
      FrbtProvider -> Frpb <$>
        (FromRelayProviderBundle <$> parser <*> parser <*> parser)
      FrbtProviderError ->
        Frpeb . FromRelayProviderErrorBundle <$> parser
      FrbtClientRoot -> Frcrb . FromRelayClientRootBundle <$> parser
      FrbtClientSub -> Frcsb <$>
        (FromRelayClientSubBundle <$> parser <*> parser <*> parser <*> parser)
      FrbtClientUpdate -> Frcub <$>
        (FromRelayClientUpdateBundle <$> parser <*> parser <*> parser
         <*> parser <*> parser <*> parser <*> parser)
