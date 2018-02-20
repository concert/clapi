{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE PatternSynonyms #-}

module Clapi.Types.Digests where

import Data.Maybe (fromJust)
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid
import Data.Text (Text)
import Data.Word (Word32)

import Clapi.Types.AssocList
  (AssocList, alEmpty, alFromList, alFmapWithKey, alValues, alKeysSet)
import Clapi.Types.Base (Attributee, Time, Interpolation)
import Clapi.Types.Definitions (Definition, Liberty)
import Clapi.Types.Messages
import Clapi.Types.Path (Seg, Path, TypeName(..), pattern (:</))
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Types.Wire (WireValue)

data SubOp = OpSubscribe | OpUnsubscribe deriving (Show, Eq)
data DefOp = OpDefine {odDef :: Definition} | OpUndefine deriving (Show, Eq)

data TimeSeriesDataOp =
  OpSet Time [WireValue] Interpolation | OpRemove deriving (Show, Eq)
data DataChange
  = ConstChange (Maybe Attributee) [WireValue]
  | TimeChange (Map Word32 (Maybe Attributee, TimeSeriesDataOp))
  deriving (Show, Eq)
type DataDigest = AssocList Path DataChange

type ContainerOps = Map Path (Map Seg (Maybe Attributee, SequenceOp Seg))

data TrpDigest = TrpDigest
  { trpdNamespace :: Seg
  , trpdDefinitions :: Map Seg DefOp
  , trpdData :: DataDigest
  , trpdContainerOps :: ContainerOps
  , trpdErrors :: Map (ErrorIndex Seg) [Text]
  } deriving (Show, Eq)

trpDigest :: Seg -> TrpDigest
trpDigest ns = TrpDigest ns mempty alEmpty mempty mempty

data FrpDigest = FrpDigest
  { frpdNamespace :: Seg
  , frpdData :: DataDigest
  , frpdContainerOps :: ContainerOps
  } deriving (Show, Eq)

data FrpErrorDigest = FrpErrorDigest
  { frpedErrors :: Map (ErrorIndex TypeName) [Text]
  } deriving (Show, Eq)

data TrcDigest = TrcDigest
  { trcdTypeSubs :: Map TypeName SubOp
  , trcdDataSubs :: Map Path SubOp
  , trcdData :: DataDigest
  , trcdContainerOps :: ContainerOps
  } deriving (Show, Eq)

trcdEmpty :: TrcDigest
trcdEmpty = TrcDigest mempty mempty alEmpty mempty

data FrcDigest = FrcDigest
  { frcdTypeUnsubs :: Set TypeName
  , frcdDataUnsubs :: Set Path
  , frcdDefinitions :: Map TypeName DefOp
  , frcdTypeAssignments :: Map Path (TypeName, Liberty)
  , frcdData :: DataDigest
  , frcdContainerOps :: ContainerOps
  , frcdErrors :: Map (ErrorIndex TypeName) [Text]
  } deriving (Show, Eq)

frcdEmpty :: FrcDigest
frcdEmpty = FrcDigest mempty mempty mempty mempty alEmpty mempty mempty

newtype TrprDigest = TrprDigest {trprdNamespace :: Seg} deriving (Show, Eq)

data TrDigest
  = Trpd TrpDigest
  | Trprd TrprDigest
  | Trcd TrcDigest
  deriving (Show, Eq)

data FrDigest
  = Frpd FrpDigest
  | Frped FrpErrorDigest
  | Frcd FrcDigest
  deriving (Show, Eq)

trcdNamespaces :: TrcDigest -> Set Seg
trcdNamespaces (TrcDigest ts ds dd co) =
    (Set.map tnNamespace $ Map.keysSet ts)
    <> pathKeyNss (Map.keysSet ds) <> pathKeyNss (alKeysSet dd)
    <> pathKeyNss (Map.keysSet co)
  where
    pathKeyNss = onlyJusts . Set.map pNs
    onlyJusts = Set.map fromJust . Set.delete Nothing
    pNs (ns :</ _) = Just ns
    pNs _ = Nothing

frcdNull :: FrcDigest -> Bool
frcdNull (FrcDigest tyUns datUns defs tas dd cops errs) =
  null tyUns && null datUns && null defs && null tas && null dd && null cops
  && null errs

-- | "Split" because kinda like :: Map k1 a -> Map k2 (Map k3 a)
splitMap :: (Ord a, Ord b) => [(a, (b, c))] -> Map a (Map b c)
splitMap = foldl mush mempty
  where
    mush m (a, bc) = Map.alter (mush' bc) a m
    mush' (b, c) = Just . Map.insert b c . maybe mempty id

digestDataUpdateMessages :: [DataUpdateMessage] -> DataDigest
digestDataUpdateMessages = alFromList . fmap procMsg
  where
    procMsg msg = case msg of
      MsgConstSet np args att -> (np, ConstChange att args)
      MsgSet np uuid t args i att ->
        (np, TimeChange (Map.singleton uuid (att, OpSet t args i)))
      MsgRemove np uuid att ->
        (np, TimeChange (Map.singleton uuid (att, OpRemove)))

produceDataUpdateMessages :: DataDigest -> [DataUpdateMessage]
produceDataUpdateMessages = mconcat . alValues . alFmapWithKey procDc
  where
    procDc :: Path -> DataChange -> [DataUpdateMessage]
    procDc p dc = case dc of
      ConstChange att wvs -> [MsgConstSet p wvs att]
      TimeChange m -> Map.foldlWithKey (\msgs tpid (att, op) -> (case op of
        OpSet t wvs i -> MsgSet p tpid t wvs i att
        OpRemove -> MsgRemove p tpid att) : msgs) [] m

digestContOpMessages :: [ContainerUpdateMessage] -> ContainerOps
digestContOpMessages = splitMap . fmap procMsg
  where
    procMsg msg = case msg of
      MsgPresentAfter p targ ref att -> (p, (targ, (att, SoPresentAfter ref)))
      MsgAbsent p targ att -> (p, (targ, (att, SoAbsent)))

produceContOpMessages :: ContainerOps -> [ContainerUpdateMessage]
produceContOpMessages = mconcat . Map.elems . Map.mapWithKey
    (\p -> Map.elems . Map.mapWithKey (procCo p))
  where
    procCo p targ (att, co) = case co of
      SoPresentAfter ref -> MsgPresentAfter p targ ref att
      SoAbsent -> MsgAbsent p targ att


qualifyDefMessage :: Seg -> DefMessage Seg -> DefMessage TypeName
qualifyDefMessage ns dm = case dm of
  MsgDefine s d -> MsgDefine (TypeName ns s) d
  MsgUndefine s -> MsgUndefine $ TypeName ns s

digestDefMessages :: Ord a => [DefMessage a] -> Map a DefOp
digestDefMessages = Map.fromList . fmap procMsg
  where
    procMsg msg = case msg of
      MsgDefine a def -> (a, OpDefine def)
      MsgUndefine a -> (a, OpUndefine)

produceDefMessages :: Map a DefOp -> [DefMessage a]
produceDefMessages = Map.elems . Map.mapWithKey
  (\a op -> case op of
     OpDefine def -> MsgDefine a def
     OpUndefine -> MsgUndefine a)

digestSubMessages :: [SubMessage] -> (Map TypeName SubOp, Map Path SubOp)
digestSubMessages msgs =
  let (tss, dss) = partitionEithers $ fmap procMsg msgs in
    (Map.fromList tss, Map.fromList dss)
  where
    procMsg msg = case msg of
      MsgSubscribe p -> Right $ (p, OpSubscribe)
      MsgTypeSubscribe tn -> Left $ (tn, OpSubscribe)
      MsgUnsubscribe p -> Right $ (p, OpUnsubscribe)
      MsgTypeUnsubscribe tn -> Left $ (tn, OpUnsubscribe)

produceSubMessages :: Map TypeName SubOp -> Map Path SubOp -> [SubMessage]
produceSubMessages tySubs datSubs = tySubMsgs ++ datSubMsgs
  where
    tySubMsgs = Map.elems $ Map.mapWithKey (\tn op -> case op of
      OpSubscribe -> MsgTypeSubscribe tn
      OpUnsubscribe -> MsgTypeUnsubscribe tn) tySubs
    datSubMsgs = Map.elems $ Map.mapWithKey (\p op -> case op of
      OpSubscribe -> MsgSubscribe p
      OpUnsubscribe -> MsgUnsubscribe p) datSubs


digestTypeMessages :: [TypeMessage] -> Map Path (TypeName, Liberty)
digestTypeMessages = Map.fromList . fmap procMsg
  where
    procMsg (MsgAssignType p tn lib) = (p, (tn, lib))

produceTypeMessages :: Map Path (TypeName, Liberty) -> [TypeMessage]
produceTypeMessages = Map.elems . Map.mapWithKey
  (\p (tn, l) -> MsgAssignType p tn l)

digestErrMessages :: Ord a => [MsgError a] -> Map (ErrorIndex a) [Text]
digestErrMessages = foldl (Map.unionWith (<>)) mempty . fmap procMsg
  where
    procMsg (MsgError ei t) = Map.singleton ei [t]

produceErrMessages :: Map (ErrorIndex a) [Text] -> [MsgError a]
produceErrMessages =
  mconcat . Map.elems . Map.mapWithKey (\ei errs -> MsgError ei <$> errs)

digestToRelayBundle :: ToRelayBundle -> TrDigest
digestToRelayBundle trb = case trb of
    Trpb b -> Trpd $ digestToRelayProviderBundle b
    Trpr b -> Trprd $ digestToRelayProviderRelinquish b
    Trcb b -> Trcd $ digestToRelayClientBundle b
  where
    digestToRelayProviderBundle :: ToRelayProviderBundle -> TrpDigest
    digestToRelayProviderBundle (ToRelayProviderBundle ns errs defs dat cont) =
      TrpDigest ns
        (digestDefMessages defs)
        (digestDataUpdateMessages dat)
        (digestContOpMessages cont)
        (digestErrMessages errs)

    digestToRelayProviderRelinquish :: ToRelayProviderRelinquish -> TrprDigest
    digestToRelayProviderRelinquish (ToRelayProviderRelinquish ns) =
      TrprDigest ns

    digestToRelayClientBundle :: ToRelayClientBundle -> TrcDigest
    digestToRelayClientBundle (ToRelayClientBundle subs dat cont) =
      let
        (tySubs, datSubs) = digestSubMessages subs
        dd = digestDataUpdateMessages dat
        co = digestContOpMessages cont
      in
        TrcDigest tySubs datSubs dd co

produceToRelayBundle :: TrDigest -> ToRelayBundle
produceToRelayBundle trd = case trd of
    Trpd d -> Trpb $ produceToRelayProviderBundle d
    Trprd d -> Trpr $ produceToRelayProviderRelinquish d
    Trcd d -> Trcb $ produceToRelayClientBundle d
  where
    produceToRelayProviderBundle (TrpDigest ns defs dat cops errs) =
      ToRelayProviderBundle
        ns (produceErrMessages errs) (produceDefMessages defs)
        (produceDataUpdateMessages dat) (produceContOpMessages cops)

    produceToRelayProviderRelinquish (TrprDigest ns) =
      ToRelayProviderRelinquish ns

    produceToRelayClientBundle (TrcDigest tySubs datSubs dd co) =
      let
        subs = produceSubMessages tySubs datSubs
        dat = produceDataUpdateMessages dd
        cont = produceContOpMessages co
      in
        ToRelayClientBundle subs dat cont

digestFromRelayBundle :: FromRelayBundle -> FrDigest
digestFromRelayBundle frb = case frb of
    Frpb b -> Frpd $ digestFromRelayProviderBundle b
    Frpeb b -> Frped $ digestFromRelayProviderErrorBundle b
    Frcb b -> Frcd $ digestFromRelayClientBundle b
  where
    digestFromRelayProviderBundle (FromRelayProviderBundle ns dums coms) =
        FrpDigest ns (digestDataUpdateMessages dums) (digestContOpMessages coms)

    digestFromRelayProviderErrorBundle (FromRelayProviderErrorBundle errs) =
        FrpErrorDigest $ digestErrMessages errs

    digestFromRelayClientBundle (FromRelayClientBundle tSubs dSubs errs defs tas dums coms) =
        FrcDigest
          (Set.fromList tSubs)
          (Set.fromList dSubs)
          (digestDefMessages defs)
          (digestTypeMessages tas)
          (digestDataUpdateMessages dums)
          (digestContOpMessages coms)
          (digestErrMessages errs)

produceFromRelayBundle :: FrDigest -> FromRelayBundle
produceFromRelayBundle frd = case frd of
    Frpd d -> Frpb $ produceFromRelayProviderBundle d
    Frped d -> Frpeb $ produceFromRelayProviderErrorBundle d
    Frcd d -> Frcb $ produceFromRelayClientBundle d
  where
    produceFromRelayProviderBundle :: FrpDigest -> FromRelayProviderBundle
    produceFromRelayProviderBundle (FrpDigest ns dd co) =
      FromRelayProviderBundle ns (produceDataUpdateMessages dd)
      (produceContOpMessages co)

    produceFromRelayProviderErrorBundle
      :: FrpErrorDigest -> FromRelayProviderErrorBundle
    produceFromRelayProviderErrorBundle (FrpErrorDigest errs) =
      FromRelayProviderErrorBundle $ produceErrMessages errs

    produceFromRelayClientBundle :: FrcDigest -> FromRelayClientBundle
    produceFromRelayClientBundle (FrcDigest tyUns datUns defs tas dd co errs) =
      FromRelayClientBundle
        (Set.toList tyUns) (Set.toList datUns) (produceErrMessages errs)
        (produceDefMessages defs) (produceTypeMessages tas)
        (produceDataUpdateMessages dd) (produceContOpMessages co)

-- The following are slightly different (and more internal to the relay), they
-- are not neccessarily intended for a single recipient

data InboundClientDigest = InboundClientDigest
  { icdGets :: Set Path
  , icdTypeGets :: Set TypeName
  , icdContainerOps :: ContainerOps
  , icdData :: DataDigest
  } deriving (Show, Eq)

-- -- | This is basically a TrpDigest with the namespace expanded out
-- data InboundProviderDigest = InboundProviderDigest
--   { ipdContainerOps :: ContainerOps
--   , ipdDefinitions :: Map TypeName DefOp
--   , ipdData :: DataDigest
--   } deriving (Show, Eq)

-- qualifyTrpd :: TrpDigest -> InboundProviderDigest
-- qualifyTrpd (TrpDigest ns reords defs dd _) = InboundProviderDigest
--     (Map.mapKeys qualifyPath reords)
--     (Map.mapKeys (TypeName ns) defs)
--     (fromJust $ alMapKeys qualifyPath dd)
--   where
--     qualifyPath p = ns :</ p

data InboundDigest
  = Icd InboundClientDigest
  | Ipd TrpDigest
  | Iprd TrprDigest
  deriving (Show, Eq)

data OutboundClientDigest = OutboundClientDigest
  { ocdContainerOps :: ContainerOps
  , ocdDefinitions :: Map TypeName DefOp
  , ocdTypeAssignments :: Map Path (TypeName, Liberty)
  , ocdData :: DataDigest
  , ocdErrors :: Map (ErrorIndex TypeName) [Text]
  } deriving (Show, Eq)

outboundClientDigest :: OutboundClientDigest
outboundClientDigest = OutboundClientDigest mempty mempty mempty alEmpty mempty

type OutboundClientInitialisationDigest = OutboundClientDigest

data OutboundProviderDigest = OutboundProviderDigest
  { opdContainerOps :: ContainerOps
  , opdData :: DataDigest
  } deriving (Show, Eq)

data OutboundDigest
  = Ocid OutboundClientInitialisationDigest
  | Ocd OutboundClientDigest
  | Opd OutboundProviderDigest
  | Ope FrpErrorDigest
  deriving (Show, Eq)
