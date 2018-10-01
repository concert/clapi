{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , StandaloneDeriving
#-}

module Clapi.Serialisation.Digests where

import Clapi.Serialisation.Base
  (Encodable(..), (<<>>), tdTaggedBuilder, tdTaggedParser)
import Clapi.Serialisation.Definitions ()
import Clapi.Serialisation.Path ()
import Clapi.TH (btq)
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
  parser = tdTaggedParser defOpTaggedData $ \case
    OpDefineT -> OpDefine <$> parser
    OpUndefineT -> return OpUndefine

instance Encodable CreateOp where
  builder (OpCreate args after) = builder args <<>> builder after
  parser = OpCreate <$> parser <*> parser


instance Encodable TrpDigest where
  builder (TrpDigest ns pds defs dat cops errs) =
    builder ns <<>> builder pds <<>> builder defs <<>> builder dat
    <<>> builder cops <<>> builder errs
  parser = TrpDigest <$> parser <*> parser <*> parser <*> parser <*> parser
    <*> parser

deriving instance Encodable TrprDigest

instance Encodable TrcSubDigest where
  builder (TrcSubDigest pts tys dat) =
    builder pts <<>> builder tys <<>> builder dat
  parser = TrcSubDigest <$> parser <*> parser <*> parser

instance Encodable TrcUpdateDigest where
  builder (TrcUpdateDigest ns dat crs cops) =
    builder ns <<>> builder dat <<>> builder crs <<>> builder cops
  parser = TrcUpdateDigest <$> parser <*> parser <*> parser <*> parser


instance Encodable FrpDigest where
  builder (FrpDigest ns dat crs cops) =
    builder ns <<>> builder dat <<>> builder crs <<>> builder cops
  parser = FrpDigest <$> parser <*> parser <*> parser <*> parser

deriving instance Encodable FrpErrorDigest
deriving instance Encodable FrcRootDigest

instance Encodable FrcSubDigest where
  builder (FrcSubDigest errs pt ty dat) =
    builder errs <<>> builder pt <<>> builder ty <<>> builder dat
  parser = FrcSubDigest <$> parser <*> parser <*> parser <*> parser

instance Encodable FrcUpdateDigest where
  builder (FrcUpdateDigest ns pds defs tyas dat cops errs) =
    builder ns <<>> builder pds <<>> builder defs <<>> builder tyas <<>>
    builder dat <<>> builder cops <<>> builder errs
  parser = FrcUpdateDigest <$> parser <*> parser <*> parser <*> parser
    <*> parser <*> parser <*> parser


data TrDigestType = TrpdT | TrprdT | TrcsdT | TrcudT deriving (Enum, Bounded)

trTaggedData :: TaggedData TrDigestType TrDigest
trTaggedData = taggedData typeToTag digestToType
  where
    typeToTag = \case
      TrpdT -> [btq|U|]
      TrprdT -> [btq|R|]
      TrcsdT -> [btq|s|]
      TrcudT -> [btq|u|]
    digestToType = \case
        Trpd _ -> TrpdT
        Trprd _ -> TrprdT
        Trcsd _ -> TrcsdT
        Trcud _ -> TrcudT

instance Encodable TrDigest where
  builder = tdTaggedBuilder trTaggedData $ \case
    Trpd trpd -> builder trpd
    Trprd trprd -> builder trprd
    Trcsd trcsd -> builder trcsd
    Trcud trcud -> builder trcud
  parser = tdTaggedParser trTaggedData $ \case
    TrpdT -> Trpd <$> parser
    TrprdT -> Trprd <$> parser
    TrcsdT -> Trcsd <$> parser
    TrcudT -> Trcud <$> parser


data FrDigestType
  = FrpdT | FrpedT | FrcrdT | FrcsdT | FrcudT deriving (Enum, Bounded)


frTaggedData :: TaggedData FrDigestType FrDigest
frTaggedData = taggedData typeToTag digestToType
  where
    typeToTag = \case
      FrpdT -> [btq|U|]
      FrpedT -> [btq|E|]
      FrcrdT -> [btq|r|]
      FrcsdT -> [btq|s|]
      FrcudT -> [btq|u|]
    digestToType = \case
      Frpd _ -> FrpdT
      Frped _ -> FrpedT
      Frcrd _ -> FrcrdT
      Frcsd _ -> FrcsdT
      Frcud _ -> FrcudT

instance Encodable FrDigest where
  builder = tdTaggedBuilder frTaggedData $ \case
    Frpd frpd -> builder frpd
    Frped frped -> builder frped
    Frcrd frcrd -> builder frcrd
    Frcsd frcsd -> builder frcsd
    Frcud frcud -> builder frcud
  parser = tdTaggedParser frTaggedData $ \case
    FrpdT -> Frpd <$> parser
    FrpedT -> Frped <$> parser
    FrcrdT -> Frcrd <$> parser
    FrcsdT -> Frcsd <$> parser
    FrcudT -> Frcud <$> parser
