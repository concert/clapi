module Clapi.Types.Digests where

import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Data.Word (Word32)

import Clapi.Types.AssocList
  (AssocList, alNull, alEmpty, alFromList, alFmapWithKey, alValues, alKeysSet)
import Clapi.Types.Base (Attributee, Time, Interpolation)
import Clapi.Types.Definitions (Definition, Liberty, PostDefinition)
import Clapi.Types.Messages
import Clapi.Types.Path
  ( Seg, Path, TypeName(..), tTnNamespace, pattern (:</), pattern (:/)
  , Namespace(..))
import Clapi.Types.SequenceOps (SequenceOp(..), isSoAbsent)
import Clapi.Types.Wire (WireValue)

data SubOp = OpSubscribe | OpUnsubscribe deriving (Show, Eq)
data DefOp def = OpDefine {odDef :: def} | OpUndefine deriving (Show, Eq)

isUndef :: DefOp a -> Bool
isUndef OpUndefine = True
isUndef _ = False

data TimeSeriesDataOp =
  OpSet Time [WireValue] Interpolation | OpRemove deriving (Show, Eq)

isRemove :: TimeSeriesDataOp -> Bool
isRemove OpRemove = True
isRemove _ = False

data DataChange
  = ConstChange (Maybe Attributee) [WireValue]
  | TimeChange (Map Word32 (Maybe Attributee, TimeSeriesDataOp))
  deriving (Show, Eq)
type DataDigest = AssocList Path DataChange

type ContainerOps args
  = Map Path (Map Seg (Maybe Attributee, SequenceOp Seg args))

data PostOp
  = OpPost {opPath :: Path, opArgs :: Map Seg WireValue} deriving (Show, Eq)

data TrpDigest = TrpDigest
  { trpdNamespace :: Namespace
  , trpdPostDefs :: Map (Tagged PostDefinition Seg) (DefOp PostDefinition)
  , trpdDefinitions :: Map (Tagged Definition Seg) (DefOp Definition)
  , trpdData :: DataDigest
  , trpdContainerOps :: ContainerOps ()
  , trpdErrors :: Map (ErrorIndex Seg) [Text]
  } deriving (Show, Eq)

trpDigest :: Namespace -> TrpDigest
trpDigest ns = TrpDigest ns mempty mempty alEmpty mempty mempty

trpdRemovedPaths :: TrpDigest -> [Path]
trpdRemovedPaths trpd =
    ((unNamespace $ trpdNamespace trpd) :</) <$>
    Map.foldlWithKey f [] (trpdContainerOps trpd)
  where
    f acc p segMap = acc ++
      (fmap (p :/) $ Map.keys $ Map.filter isSoAbsent $ fmap snd segMap)

trpdNull :: TrpDigest -> Bool
trpdNull (TrpDigest _ns postDefs defs dd cops errs) =
  null postDefs && null defs && alNull dd && null cops && null errs

data FrpDigest = FrpDigest
  { frpdNamespace :: Namespace
  , frpdData :: DataDigest
  , frpdContainerOps :: ContainerOps [WireValue]
  } deriving (Show, Eq)

frpDigest :: Namespace -> FrpDigest
frpDigest ns = FrpDigest ns alEmpty mempty

data FrpErrorDigest = FrpErrorDigest
  { frpedErrors :: Map (ErrorIndex TypeName) [Text]
  } deriving (Show, Eq)

data TrcDigest = TrcDigest
  { trcdPostTypeSubs :: Map (Tagged PostDefinition TypeName) SubOp
  , trcdTypeSubs :: Map (Tagged Definition TypeName) SubOp
  , trcdDataSubs :: Map Path SubOp
  , trcdData :: DataDigest
  , trcdContainerOps :: ContainerOps [WireValue]
  } deriving (Show, Eq)

trcdEmpty :: TrcDigest
trcdEmpty = TrcDigest mempty mempty mempty alEmpty mempty

data FrcDigest = FrcDigest
  { frcdPostTypeUnsubs :: Set (Tagged PostDefinition TypeName)
  , frcdTypeUnsubs :: Set (Tagged Definition TypeName)
  , frcdDataUnsubs :: Set Path
  , frcdPostDefs :: Map (Tagged PostDefinition TypeName) (DefOp PostDefinition)
  , frcdDefinitions :: Map (Tagged Definition TypeName) (DefOp Definition)
  , frcdTypeAssignments :: Map Path (Tagged Definition TypeName, Liberty)
  , frcdData :: DataDigest
  , frcdContainerOps :: ContainerOps ()
  , frcdErrors :: Map (ErrorIndex TypeName) [Text]
  } deriving (Show, Eq)

frcdEmpty :: FrcDigest
frcdEmpty = FrcDigest mempty mempty mempty mempty mempty mempty alEmpty mempty
  mempty

newtype TrprDigest
  = TrprDigest {trprdNamespace :: Namespace}
  deriving (Show, Eq)

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

trcdNamespaces :: TrcDigest -> Set Namespace
trcdNamespaces (TrcDigest pts ts ds dd co) =
    (Set.map tTnNamespace $ Map.keysSet pts)
    <> (Set.map tTnNamespace $ Map.keysSet ts)
    <> pathKeyNss (Map.keysSet ds)
    <> pathKeyNss (alKeysSet dd) <> pathKeyNss (Map.keysSet co)
  where
    pathKeyNss = onlyJusts . Set.map pNs
    onlyJusts = Set.map fromJust . Set.delete Nothing
    pNs (ns :</ _) = Just $ Namespace ns
    pNs _ = Nothing

frcdNull :: FrcDigest -> Bool
frcdNull (FrcDigest pTyUns tyUns datUns postDefs defs tas dd cops errs) =
  null pTyUns && null tyUns && null datUns && null postDefs && null defs
  && null tas && null dd && null cops && null errs

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

digestContOpMessages :: [ContainerUpdateMessage args] -> ContainerOps argsg
digestContOpMessages = splitMap . fmap procMsg
  where
    procMsg msg = case msg of
      MsgMoveAfter p targ ref att -> (p, (targ, (att, SoMoveAfter ref)))
      MsgAbsent p targ att -> (p, (targ, (att, SoAbsent)))

produceContOpMessages :: ContainerOps args -> [ContainerUpdateMessage args]
produceContOpMessages = mconcat . Map.elems . Map.mapWithKey
    (\p -> Map.elems . Map.mapWithKey (procCo p))
  where
    procCo p targ (att, co) = case co of
      SoMoveAfter ref -> MsgMoveAfter p targ ref att
      SoAbsent -> MsgAbsent p targ att


qualifyDefMessage :: Namespace -> DefMessage Seg def -> DefMessage TypeName def
qualifyDefMessage ns dm = case dm of
  MsgDefine s d -> MsgDefine (TypeName ns s) d
  MsgUndefine s -> MsgUndefine $ TypeName ns s

digestDefMessages
  :: Ord a => [DefMessage (Tagged def a) def] -> Map (Tagged def a) (DefOp def)
digestDefMessages = Map.fromList . fmap procMsg
  where
    procMsg msg = case msg of
      MsgDefine a def -> (a, OpDefine def)
      MsgUndefine a -> (a, OpUndefine)

produceDefMessages
  :: Map (Tagged def a) (DefOp def) -> [DefMessage (Tagged def a) def]
produceDefMessages = Map.elems . Map.mapWithKey
  (\a op -> case op of
     OpDefine def -> MsgDefine a def
     OpUndefine -> MsgUndefine a)

digestSubMessages
  :: [SubMessage]
  -> ( Map (Tagged PostDefinition TypeName) SubOp
     , Map (Tagged Definition TypeName) SubOp
     , Map Path SubOp)
digestSubMessages msgs = foldl' procMsg mempty msgs
  where
    procMsg (post, ty, dat) msg = case msg of
      MsgSubscribe p -> (post, ty, Map.insert p OpSubscribe dat)
      MsgPostTypeSubscribe tn -> (Map.insert tn OpSubscribe post, ty, dat)
      MsgTypeSubscribe tn -> (post, Map.insert tn OpSubscribe ty, dat)
      MsgUnsubscribe p -> (post, ty, Map.insert p OpUnsubscribe dat)
      MsgPostTypeUnsubscribe tn -> (Map.insert tn OpUnsubscribe post, ty, dat)
      MsgTypeUnsubscribe tn -> (post, Map.insert tn OpUnsubscribe ty, dat)

produceSubMessages
  :: Map (Tagged PostDefinition TypeName) SubOp
  -> Map (Tagged Definition TypeName) SubOp
  -> Map Path SubOp -> [SubMessage]
produceSubMessages pTySubs tySubs datSubs =
    pTySubMsgs ++ tySubMsgs ++ datSubMsgs
  where
    pTySubMsgs = Map.elems $ Map.mapWithKey (\tn op -> case op of
      OpSubscribe -> MsgPostTypeSubscribe tn
      OpUnsubscribe -> MsgPostTypeUnsubscribe tn) pTySubs
    tySubMsgs = Map.elems $ Map.mapWithKey (\tn op -> case op of
      OpSubscribe -> MsgTypeSubscribe tn
      OpUnsubscribe -> MsgTypeUnsubscribe tn) tySubs
    datSubMsgs = Map.elems $ Map.mapWithKey (\p op -> case op of
      OpSubscribe -> MsgSubscribe p
      OpUnsubscribe -> MsgUnsubscribe p) datSubs


digestTypeMessages
  :: [TypeMessage] -> Map Path (Tagged Definition TypeName, Liberty)
digestTypeMessages = Map.fromList . fmap procMsg
  where
    procMsg (MsgAssignType p tn lib) = (p, (tn, lib))

produceTypeMessages
  :: Map Path (Tagged Definition TypeName, Liberty) -> [TypeMessage]
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
    digestToRelayProviderBundle
        (ToRelayProviderBundle ns errs postDefs defs dat cont) =
      TrpDigest ns
        (digestDefMessages postDefs)
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
        (postTySubs, tySubs, datSubs) = digestSubMessages subs
        dd = digestDataUpdateMessages dat
        co = digestContOpMessages cont
      in
        TrcDigest postTySubs tySubs datSubs dd co

produceToRelayBundle :: TrDigest -> ToRelayBundle
produceToRelayBundle trd = case trd of
    Trpd d -> Trpb $ produceToRelayProviderBundle d
    Trprd d -> Trpr $ produceToRelayProviderRelinquish d
    Trcd d -> Trcb $ produceToRelayClientBundle d
  where
    produceToRelayProviderBundle (TrpDigest ns postDefs defs dat cops errs) =
      ToRelayProviderBundle
        ns (produceErrMessages errs)
        (produceDefMessages postDefs) (produceDefMessages defs)
        (produceDataUpdateMessages dat) (produceContOpMessages cops)

    produceToRelayProviderRelinquish (TrprDigest ns) =
      ToRelayProviderRelinquish ns

    produceToRelayClientBundle (TrcDigest pTySubs tySubs datSubs dd co) =
      let
        subs = produceSubMessages pTySubs tySubs datSubs
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

    digestFromRelayClientBundle
        (FromRelayClientBundle ptSubs tSubs dSubs errs postDefs defs tas dums coms) =
      FrcDigest
        (Set.fromList ptSubs)
        (Set.fromList tSubs)
        (Set.fromList dSubs)
        (digestDefMessages postDefs)
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
      FromRelayProviderBundle ns
      (produceDataUpdateMessages dd) (produceContOpMessages co)

    produceFromRelayProviderErrorBundle
      :: FrpErrorDigest -> FromRelayProviderErrorBundle
    produceFromRelayProviderErrorBundle (FrpErrorDigest errs) =
      FromRelayProviderErrorBundle $ produceErrMessages errs

    produceFromRelayClientBundle :: FrcDigest -> FromRelayClientBundle
    produceFromRelayClientBundle
        (FrcDigest postTyUns tyUns datUns postDefs defs tas dd co errs) =
      FromRelayClientBundle
        (Set.toList postTyUns) (Set.toList tyUns) (Set.toList datUns)
        (produceErrMessages errs)
        (produceDefMessages postDefs) (produceDefMessages defs)
        (produceTypeMessages tas)
        (produceDataUpdateMessages dd) (produceContOpMessages co)

-- The following are slightly different (and more internal to the relay), they
-- are not neccessarily intended for a single recipient

data InboundClientDigest = InboundClientDigest
  { icdGets :: Set Path
  , icdPostTypeGets :: Set (Tagged PostDefinition TypeName)
  , icdTypeGets :: Set (Tagged Definition TypeName)
  , icdContainerOps :: ContainerOps [WireValue]
  , icdData :: DataDigest
  } deriving (Show, Eq)

inboundClientDigest :: InboundClientDigest
inboundClientDigest = InboundClientDigest mempty mempty mempty mempty alEmpty

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
  { ocdContainerOps :: ContainerOps ()
  , ocdPostDefs :: Map (Tagged PostDefinition TypeName) (DefOp PostDefinition)
  , ocdDefinitions :: Map (Tagged Definition TypeName) (DefOp Definition)
  , ocdTypeAssignments :: Map Path (Tagged Definition TypeName, Liberty)
  , ocdData :: DataDigest
  , ocdErrors :: Map (ErrorIndex TypeName) [Text]
  } deriving (Show, Eq)

outboundClientDigest :: OutboundClientDigest
outboundClientDigest = OutboundClientDigest mempty mempty mempty mempty alEmpty
    mempty

ocdNull :: OutboundClientDigest -> Bool
ocdNull (OutboundClientDigest cops postDefs defs tas dd errs) =
    null cops && null postDefs && null defs && null tas && alNull dd
    && null errs

type OutboundClientInitialisationDigest = OutboundClientDigest

data OutboundProviderDigest = OutboundProviderDigest
  { opdContainerOps :: ContainerOps [WireValue]
  , opdData :: DataDigest
  } deriving (Show, Eq)

opdNull :: OutboundProviderDigest -> Bool
opdNull (OutboundProviderDigest cops dd) = null cops && alNull dd

data OutboundDigest
  = Ocid OutboundClientInitialisationDigest
  | Ocd OutboundClientDigest
  | Opd OutboundProviderDigest
  | Ope FrpErrorDigest
  deriving (Show, Eq)
