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
  ( AssocList, alEmpty, alFromList, alFmapWithKey, alValues, alKeysSet
  , alMapKeys)
import Clapi.Types.Base (Attributee, Time, Interpolation)
import Clapi.Types.Definitions (Definition, Liberty)
import Clapi.Types.Messages
import Clapi.Types.Path (Seg, Path, TypeName(..), pattern (:</))
import Clapi.Types.Wire (WireValue)
import Clapi.Util (foldlNestedMaps)

data SubOp = OpSubscribe | OpUnsubscribe deriving (Show, Eq)
data DefOp = OpDefine {odDef :: Definition} | OpUndefine deriving (Show, Eq)

data TimeSeriesDataOp =
  OpSet Time [WireValue] Interpolation | OpRemove deriving (Show, Eq)
data DataChange
  = InitChange (Maybe Attributee) -- Basically for empty array?
  | DeleteChange (Maybe Attributee)
  | ConstChange (Maybe Attributee) [WireValue]
  | TimeChange (Map Word32 (Maybe Attributee, TimeSeriesDataOp))
  deriving (Show, Eq)
type DataDigest = AssocList Path DataChange

type Reorderings = Map Path (Map Seg (Maybe Attributee, Maybe Seg))

data TrpDigest = TrpDigest
  { trpdNamespace :: Seg
  , trpdReorderings :: Reorderings
  , trpdDefinitions :: Map Seg DefOp
  , trpdData :: DataDigest
  , trpdErrors :: Map (ErrorIndex Seg) [Text]
  } deriving (Show, Eq)

trpDigest :: Seg -> TrpDigest
trpDigest ns = TrpDigest ns mempty mempty alEmpty mempty

data FrpDigest = FrpDigest
  { frpdNamespace :: Seg
  , frpdReorderings :: Reorderings
  , frpdData :: DataDigest
  } deriving (Show, Eq)

data FrpErrorDigest = FrpErrorDigest
  { frpedNamespace :: Seg
  , frpedErrors :: Map (ErrorIndex Seg) [Text]
  } deriving (Show, Eq)

data TrcDigest = TrcDigest
  { trcdTypeSubs :: Map TypeName SubOp
  , trcdDataSubs :: Map Path SubOp
  , trcdReorderings :: Reorderings
  , trcdData :: DataDigest
  } deriving (Show, Eq)

data FrcDigest = FrcDigest
  { frcdReorderings :: Reorderings
  , frcdDefinitions :: Map TypeName DefOp
  , frcdTypeAssignments :: Map Path (TypeName, Liberty)
  , frcdData :: DataDigest
  , frcdErrors :: Map (ErrorIndex TypeName) [Text]
  } deriving (Show, Eq)

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
trcdNamespaces (TrcDigest ts ds cas dd) =
    (Set.map tnNamespace $ Map.keysSet ts)
    <> pathKeyNss (Map.keysSet ds) <> pathKeyNss (Map.keysSet cas)
    <> pathKeyNss (alKeysSet dd)
  where
    pathKeyNss = onlyJusts . Set.map pNs
    onlyJusts = Set.map fromJust . Set.delete Nothing
    pNs (ns :</ _) = Just ns
    pNs _ = Nothing

frcdNull :: FrcDigest -> Bool
frcdNull (FrcDigest cas defs tas dd errs) =
  null cas && null defs && null tas && null dd && null errs

-- | "Split" because kinda like :: Map k1 a -> Map k2 (Map k3 a)
splitMaps :: (Ord a, Ord b) => [(a, (b, c))] -> Map a (Map b c)
splitMaps = foldl mush mempty
  where
    mush m (a, bc) = Map.alter (mush' bc) a m
    mush' (b, c) = Just . Map.insert b c . maybe mempty id

digestDataUpdateMessages
  :: [DataUpdateMessage] -> (Reorderings, DataDigest)
digestDataUpdateMessages msgs =
  let (reords, dds) = partitionEithers $ fmap procMsg msgs in
    (splitMaps reords, alFromList dds)
  where
    procMsg msg = case msg of
      MsgInit np att -> Right $ (np, InitChange att)
      MsgDelete np att -> Right $ (np, DeleteChange att)
      MsgConstSet np args att -> Right $ (np, ConstChange att args)
      MsgSet np uuid t args i att -> Right $
        (np, TimeChange (Map.singleton uuid (att, OpSet t args i)))
      MsgRemove np uuid att -> Right $
        (np, TimeChange (Map.singleton uuid (att, OpRemove)))
      MsgReorder np targ ref att -> Left $ (np, (targ, (att, ref)))

produceDataUpdateMessages
  :: Reorderings -> DataDigest -> [DataUpdateMessage]
produceDataUpdateMessages reords dd = reordMsgs ++ ddMsgs
  where
    reordMsgs = foldlNestedMaps
      (\msgs p targ (att, ref) -> MsgReorder p targ ref att : msgs) [] reords
    ddMsgs = mconcat $ alValues $ alFmapWithKey procDc dd
    procDc :: Path -> DataChange -> [DataUpdateMessage]
    procDc p dc = case dc of
      InitChange att -> [MsgInit p att]
      DeleteChange att -> [MsgDelete p att]
      ConstChange att wvs -> [MsgConstSet p wvs att]
      TimeChange m -> Map.foldlWithKey (\msgs tpid (att, op) -> (case op of
        OpSet t wvs i -> MsgSet p tpid t wvs i att
        OpRemove -> MsgRemove p tpid att) : msgs) [] m


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
    digestToRelayProviderBundle (ToRelayProviderBundle ns errs defs dat) =
      let (cas, dd) = digestDataUpdateMessages dat in
        TrpDigest ns cas (digestDefMessages defs) dd (digestErrMessages errs)

    digestToRelayProviderRelinquish :: ToRelayProviderRelinquish -> TrprDigest
    digestToRelayProviderRelinquish (ToRelayProviderRelinquish ns) =
      TrprDigest ns

    digestToRelayClientBundle :: ToRelayClientBundle -> TrcDigest
    digestToRelayClientBundle (ToRelayClientBundle subs dat) =
      let
        (tySubs, datSubs) = digestSubMessages subs
        (cas, dd) = digestDataUpdateMessages dat
      in
        TrcDigest tySubs datSubs cas dd


produceFromRelayBundle :: FrDigest -> FromRelayBundle
produceFromRelayBundle frd = case frd of
    Frpd d -> Frpb $ produceFromRelayProviderBundle d
    Frped d -> Frpeb $ produceFromRelayProviderErrorBundle d
    Frcd d -> Frcb $ produceFromRelayClientBundle d
  where
    produceFromRelayProviderBundle :: FrpDigest -> FromRelayProviderBundle
    produceFromRelayProviderBundle (FrpDigest ns cas dd) =
      FromRelayProviderBundle ns $ produceDataUpdateMessages cas dd

    produceFromRelayProviderErrorBundle
      :: FrpErrorDigest -> FromRelayProviderErrorBundle
    produceFromRelayProviderErrorBundle (FrpErrorDigest ns errs) =
      FromRelayProviderErrorBundle ns $ produceErrMessages errs

    produceFromRelayClientBundle :: FrcDigest -> FromRelayClientBundle
    produceFromRelayClientBundle (FrcDigest cas defs tas dd errs) =
      FromRelayClientBundle (produceErrMessages errs) (produceDefMessages defs)
      (produceTypeMessages tas) (produceDataUpdateMessages cas dd)

-- The following are slightly different (and more internal to the relay), they
-- are not neccessarily intended for a single recipient

data InboundClientDigest = InboundClientDigest
  { icdGets :: Set Path
  , icdTypeGets :: Set TypeName
  , icdReorderings :: Reorderings
  , icdData :: DataDigest
  } deriving (Show, Eq)

-- -- | This is basically a TrpDigest with the namespace expanded out
-- data InboundProviderDigest = InboundProviderDigest
--   { ipdReorderings :: Reorderings
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
  { ocdReorderings :: Reorderings
  , ocdDefinitions :: Map TypeName Definition
  , ocdTypeAssignments :: Map Path (TypeName, Liberty)
  , ocdData :: DataDigest
  , ocdErrors :: Map (ErrorIndex TypeName) [Text]
  } deriving (Show, Eq)

outboundClientDigest :: OutboundClientDigest
outboundClientDigest = OutboundClientDigest mempty mempty mempty alEmpty mempty

type OutboundClientInitialisationDigest = OutboundClientDigest

data OutboundProviderDigest = OutboundProviderDigest
  { opdReorderings :: Reorderings
  , opddData :: DataDigest
  } deriving (Show, Eq)

data OutboundDigest
  = Ocid OutboundClientInitialisationDigest
  | Ocd OutboundClientDigest
  | Opd OutboundProviderDigest
  | Ope FrpErrorDigest
  deriving (Show, Eq)
