{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , StandaloneDeriving
#-}

module Clapi.Serialisation.Digests where

import Clapi.Serialisation.Base
  (Encodable(..), Decodable(..), (<<>>), tdTaggedBuilder, tdTaggedParser)
import Clapi.Serialisation.Definitions ()
import Clapi.Serialisation.Path ()
import Clapi.TH (btq)
import Clapi.Types.Base (TypeEnumOf(..), Tag)
import Clapi.Types.Digests
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.TaggedData (TaggedData, taggedData)
import Clapi.Serialisation.Wire ()

data DataErrIdxType
  = DeitGlobal
  | DeitNamespace
  | DeitPath
  | DeitTimePoint
  deriving (Enum, Bounded)

dataErrIndexTaggedData :: TaggedData DataErrIdxType DataErrorIndex
dataErrIndexTaggedData = taggedData typeToTag eiToType
  where
    typeToTag ty = case ty of
      DeitGlobal -> [btq|g|]
      DeitNamespace -> [btq|n|]
      DeitPath -> [btq|p|]
      DeitTimePoint -> [btq|t|]
    eiToType ei = case ei of
      GlobalError -> DeitGlobal
      NamespaceError _ -> DeitNamespace
      PathError _ -> DeitPath
      TimePointError _ _ -> DeitTimePoint

instance Encodable DataErrorIndex where
  builder = tdTaggedBuilder dataErrIndexTaggedData $ \ei -> case ei of
    GlobalError -> return mempty
    NamespaceError ns -> builder ns
    PathError p -> builder p
    TimePointError p tpid -> builder p <<>> builder tpid
instance Decodable DataErrorIndex where
  parser = tdTaggedParser dataErrIndexTaggedData $ \eit -> case eit of
    DeitGlobal -> return GlobalError
    DeitNamespace -> NamespaceError <$> parser
    DeitPath -> PathError <$> parser
    DeitTimePoint -> TimePointError <$> parser <*> parser

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
instance Decodable SubErrorIndex where
  parser = tdTaggedParser subErrIndexTaggedData $ \eit -> case eit of
    SeitNamespace -> NamespaceSubError <$> parser
    SeitPostTypeName -> PostTypeSubError <$> parser <*> parser
    SeitTypeName -> TypeSubError <$> parser <*> parser
    SeitPath -> PathSubError <$> parser <*> parser

subOpTaggedData :: TaggedData SubOp SubOp
subOpTaggedData = taggedData toTag id
  where
    toTag = \case
      OpSubscribe -> [btq|s|]
      OpUnsubscribe -> [btq|u|]

instance Encodable SubOp where
  builder = tdTaggedBuilder subOpTaggedData $ const $ return mempty
instance Decodable SubOp where
  parser = tdTaggedParser subOpTaggedData return


data SeqOpType = SoAfterT | SoAbsentT deriving (Enum, Bounded)

soTaggedData :: TaggedData SeqOpType (SequenceOp i)
soTaggedData = taggedData typeToTag soToType
  where
    typeToTag = \case
      SoAfterT -> [btq|>|]
      SoAbsentT -> [btq|x|]
    soToType = \case
      SoAfter _ -> SoAfterT
      SoAbsent -> SoAbsentT

instance Encodable a => Encodable (SequenceOp a) where
  builder = tdTaggedBuilder soTaggedData $ \case
    SoAfter mi -> builder mi
    SoAbsent -> return mempty
instance Decodable a => Decodable (SequenceOp a) where
  parser = tdTaggedParser soTaggedData $ \case
    SoAfterT -> SoAfter <$> parser
    SoAbsentT -> return SoAbsent


data TsDOpT = OpSetT | OpRemoveT deriving (Enum, Bounded)

tsDOpTaggedData :: TaggedData TsDOpT TimeSeriesDataOp
tsDOpTaggedData = taggedData typeToTag tsDOpToType
  where
    typeToTag = \case
      OpSetT -> [btq|=|]
      OpRemoveT -> [btq|-|]
    tsDOpToType = \case
      OpSet _ _ _ -> OpSetT
      OpRemove -> OpRemoveT

instance Encodable TimeSeriesDataOp where
  builder = tdTaggedBuilder tsDOpTaggedData $ \case
    OpSet t wvs i -> builder t <<>> builder wvs <<>> builder i
    OpRemove -> return mempty
instance Decodable TimeSeriesDataOp where
  parser = tdTaggedParser tsDOpTaggedData $ \case
    OpSetT -> OpSet <$> parser <*> parser <*> parser
    OpRemoveT -> return OpRemove

data DataChangeType = ConstChangeT | TimeChangeT deriving (Enum, Bounded)

dcTaggedData :: TaggedData DataChangeType DataChange
dcTaggedData = taggedData typeToTag dcToType
  where
    typeToTag = \case
      ConstChangeT -> [btq|c|]
      TimeChangeT -> [btq|t|]
    dcToType = \case
      ConstChange _ _ -> ConstChangeT
      TimeChange _ -> TimeChangeT

instance Encodable DataChange where
  builder = tdTaggedBuilder dcTaggedData $ \case
    ConstChange att wvs -> builder att <<>> builder wvs
    TimeChange m -> builder m
instance Decodable DataChange where
  parser = tdTaggedParser dcTaggedData $ \case
    ConstChangeT -> ConstChange <$> parser <*> parser
    TimeChangeT -> TimeChange <$> parser


data DefOpType = OpDefineT | OpUndefineT deriving (Enum, Bounded)

defOpTaggedData :: TaggedData DefOpType (DefOp a)
defOpTaggedData = taggedData typeToTag defOpToType
  where
    typeToTag = \case
      OpDefineT -> [btq|d|]
      OpUndefineT -> [btq|u|]
    defOpToType = \case
      OpDefine _ -> OpDefineT
      OpUndefine -> OpUndefineT

instance Encodable a => Encodable (DefOp a) where
  builder = tdTaggedBuilder defOpTaggedData $ \case
    OpDefine def -> builder def
    OpUndefine -> return mempty
instance Decodable a => Decodable (DefOp a) where
  parser = tdTaggedParser defOpTaggedData $ \case
    OpDefineT -> OpDefine <$> parser
    OpUndefineT -> return OpUndefine

instance Encodable CreateOp where
  builder (OpCreate args after) = builder args <<>> builder after
instance Decodable CreateOp where
  parser = OpCreate <$> parser <*> parser


data TrDigestType = TrpdT | TrprdT | TrcsdT | TrcudT deriving (Enum, Bounded)

instance TypeEnumOf (TrDigest o a) TrDigestType where
  typeEnumOf = \case
    Trpd {} -> TrpdT
    Trprd {} -> TrprdT
    Trcsd {} -> TrcsdT
    Trcud {} -> TrcudT

instance TypeEnumOf SomeTrDigest TrDigestType where
  typeEnumOf = withTrDigest typeEnumOf

trdTypeToTag :: TrDigestType -> Tag
trdTypeToTag = \case
  TrpdT -> [btq|U|]
  TrprdT -> [btq|R|]
  TrcsdT -> [btq|s|]
  TrcudT -> [btq|u|]

trTaggedData :: TaggedData TrDigestType (TrDigest o a)
trTaggedData = taggedData trdTypeToTag typeEnumOf

strTaggedData :: TaggedData TrDigestType SomeTrDigest
strTaggedData = taggedData trdTypeToTag typeEnumOf


instance Encodable (TrDigest o a) where
  builder = tdTaggedBuilder trTaggedData $ \case
    Trpd ns pds defs dat cops errs ->
      builder ns <<>> builder pds <<>> builder defs
      <<>> builder dat <<>> builder cops <<>> builder errs
    Trprd ns ->
      builder ns
    Trcsd pts tys dat ->
      builder pts <<>> builder tys <<>> builder dat
    Trcud ns dat crs cops ->
      builder ns <<>> builder dat <<>> builder crs <<>> builder cops

instance Encodable SomeTrDigest where
  builder = withTrDigest builder
instance Decodable SomeTrDigest where
  parser = tdTaggedParser strTaggedData $ \case
    TrpdT -> fmap SomeTrDigest $
      Trpd <$> parser <*> parser <*> parser <*> parser <*> parser <*> parser
    TrprdT -> fmap SomeTrDigest $
      Trprd <$> parser
    TrcsdT -> fmap SomeTrDigest $
      Trcsd <$> parser <*> parser <*> parser
    TrcudT -> fmap SomeTrDigest $
      Trcud <$> parser <*> parser <*> parser <*> parser


data FrDigestType
  = FrpdT | FrpedT | FrcrdT | FrcsdT | FrcudT deriving (Enum, Bounded)

instance TypeEnumOf (FrDigest o a) FrDigestType where
  typeEnumOf = \case
    Frpd {} -> FrpdT
    Frped {} -> FrpedT
    Frcrd {} -> FrcrdT
    Frcsd {} -> FrcsdT
    Frcud {} -> FrcudT

instance TypeEnumOf SomeFrDigest FrDigestType where
  typeEnumOf = withFrDigest typeEnumOf

frdTypeToTag :: FrDigestType -> Tag
frdTypeToTag = \case
  FrpdT -> [btq|U|]
  FrpedT -> [btq|E|]
  FrcrdT -> [btq|r|]
  FrcsdT -> [btq|s|]
  FrcudT -> [btq|u|]

frTaggedData :: TaggedData FrDigestType (FrDigest o a)
frTaggedData = taggedData frdTypeToTag typeEnumOf

sfrTaggedData :: TaggedData FrDigestType SomeFrDigest
sfrTaggedData = taggedData frdTypeToTag typeEnumOf

instance Encodable (FrDigest o a) where
  builder = tdTaggedBuilder frTaggedData $ \case
    Frpd ns dat crs cops ->
      builder ns <<>> builder dat <<>> builder crs <<>> builder cops
    Frped errs ->
      builder errs
    Frcrd cops ->
      builder cops
    Frcsd errs pt ty dat ->
      builder errs <<>> builder pt <<>> builder ty <<>> builder dat
    Frcud ns pds defs tyas dat cops errs ->
      builder ns <<>> builder pds <<>> builder defs <<>> builder tyas
      <<>> builder dat <<>> builder cops <<>> builder errs

instance Encodable SomeFrDigest where
  builder = withFrDigest builder
instance Decodable SomeFrDigest where
  parser = tdTaggedParser sfrTaggedData $ \case
    FrpdT -> fmap SomeFrDigest $
      Frpd <$> parser <*> parser <*> parser <*> parser
    FrpedT -> fmap SomeFrDigest $
      Frped <$> parser
    FrcrdT -> fmap SomeFrDigest $
      Frcrd <$> parser
    FrcsdT -> fmap SomeFrDigest $
      Frcsd <$> parser <*> parser <*> parser <*> parser
    FrcudT -> fmap SomeFrDigest $
      Frcud <$> parser <*> parser <*> parser <*> parser <*> parser <*> parser
      <*> parser
