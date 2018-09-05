{-# LANGUAGE
    LambdaCase
#-}

module Clapi.Types.Digests where

import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Data.Word (Word32)

import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos
import Data.Map.Mol (Mol(..))
import qualified Data.Map.Mol as Mol

import Clapi.Types.AssocList
  (AssocList, alNull, alEmpty, alFromList, alFmapWithKey, alValues)
import Clapi.Types.Base (Attributee, Time, Interpolation)
import Clapi.Types.Definitions (Definition, Editable, PostDefinition)
import Clapi.Types.Messages
import Clapi.Types.Path
  (Seg, Path, pattern (:/), Namespace(..), Placeholder(..))
import Clapi.Types.SequenceOps (SequenceOp(..), isSoAbsent)
import Clapi.Types.Wire (WireValue)

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

data CreateOp
  = OpCreate
  -- FIXME: Nested lists of WireValues is a legacy hangover because our tree
  -- data nodes still contain [WireValue] as a single "value":
  { ocArgs :: [[WireValue]]
  , ocAfter :: Maybe (Either Placeholder Seg)
  } deriving (Show, Eq)
type Creates = Map Path (Map Placeholder (Maybe Attributee, CreateOp))

type RootContOps = Map Namespace (SequenceOp Namespace)
-- FIXME: might this be better as Map (Path, Seg) (blah)? We spend a lot of time
-- coping with the nested map-ness:
type ContOps after = Map Path (Map Seg (Maybe Attributee, SequenceOp after))

data PostOp
  = OpPost {opPath :: Path, opArgs :: Map Seg WireValue} deriving (Show, Eq)

data TrpDigest = TrpDigest
  { trpdNamespace :: Namespace
  , trpdPostDefs :: Map (Tagged PostDefinition Seg) (DefOp PostDefinition)
  , trpdDefinitions :: Map (Tagged Definition Seg) (DefOp Definition)
  , trpdData :: DataDigest
  , trpdContOps :: ContOps Seg
  -- FIXME: should errors come in a different digest to data updates? At the
  -- moment we just check a TrpDigest isn't null when processing namespace
  -- claims...
  , trpdErrors :: Map DataErrorIndex [Text]
  } deriving (Show, Eq)

trpdEmpty :: Namespace -> TrpDigest
trpdEmpty ns = TrpDigest ns mempty mempty alEmpty mempty mempty

trpdRemovedPaths :: TrpDigest -> [Path]
trpdRemovedPaths trpd =
    Map.foldlWithKey f [] (trpdContOps trpd)
  where
    f acc p segMap = acc ++
      (fmap (p :/) $ Map.keys $ Map.filter isSoAbsent $ fmap snd segMap)

trpdNull :: TrpDigest -> Bool
trpdNull (TrpDigest _ns postDefs defs dd cops errs) =
  null postDefs && null defs && alNull dd && null cops && null errs

data FrpDigest = FrpDigest
  { frpdNamespace :: Namespace
  , frpdData :: DataDigest
  , frpdCreates :: Creates
  , frpdContOps :: ContOps (Either Placeholder Seg)
  } deriving (Show, Eq)

frpdEmpty :: Namespace -> FrpDigest
frpdEmpty ns = FrpDigest ns mempty mempty mempty

frpdNull :: FrpDigest -> Bool
frpdNull (FrpDigest _ dd creates cops) = null dd && null creates && null cops

newtype FrpErrorDigest = FrpErrorDigest
  { frpedErrors :: Map DataErrorIndex [Text]
  } deriving (Show, Eq)

frpedNull :: FrpErrorDigest -> Bool
frpedNull = null . frpedErrors

data SubOp = OpSubscribe | OpUnsubscribe deriving (Show, Eq)

isSub :: SubOp -> Bool
isSub OpSubscribe = True
isSub _ = False

data TrcSubDigest = TrcSubDigest
  { trcsdPostTypes :: Map (Namespace, Tagged PostDefinition Seg) SubOp
  , trcsdTypes :: Map (Namespace, Tagged Definition Seg) SubOp
  , trcsdData :: Map (Namespace, Path) SubOp
  } deriving (Show, Eq)

trcsdEmpty :: TrcSubDigest
trcsdEmpty = mempty

instance Monoid TrcSubDigest where
  mempty = TrcSubDigest mempty mempty mempty
  TrcSubDigest p1 t1 d1 `mappend` TrcSubDigest p2 t2 d2 = TrcSubDigest
      (p2 <> p1) (t2 <> t1) (d2 <> d1)

trcsdNamespaces :: TrcSubDigest -> Set Namespace
trcsdNamespaces (TrcSubDigest p t d) =
  let f = Set.mapMonotonic fst . Map.keysSet in mconcat [f p, f t, f d]

data ClientRegs
  = ClientRegs
  { crPostTypeRegs :: Mos Namespace (Tagged PostDefinition Seg)
  , crTypeRegs :: Mos Namespace (Tagged Definition Seg)
  , crDataRegs :: Mos Namespace Path
  } deriving (Show)

instance Monoid ClientRegs where
  mempty = ClientRegs mempty mempty mempty
  (ClientRegs pt1 t1 d1) `mappend` (ClientRegs pt2 t2 d2) =
    ClientRegs (pt1 <> pt2) (t1 <> t2) (d1 <> d2)

crNull :: ClientRegs -> Bool
crNull (ClientRegs p t d) = null p && null t && null d

crDifference :: ClientRegs -> ClientRegs -> ClientRegs
crDifference (ClientRegs p1 t1 d1) (ClientRegs p2 t2 d2) = ClientRegs
  (Mos.difference p1 p2)
  (Mos.difference t1 t2)
  (Mos.difference d1 d2)

crIntersection :: ClientRegs -> ClientRegs -> ClientRegs
crIntersection (ClientRegs p1 t1 d1) (ClientRegs p2 t2 d2) = ClientRegs
  (Mos.intersection p1 p2)
  (Mos.intersection t1 t2)
  (Mos.intersection d1 d2)

trcsdClientRegs :: TrcSubDigest -> (ClientRegs, ClientRegs)
trcsdClientRegs (TrcSubDigest p t d) =
  let
    f :: Ord x => Map (Namespace, x) SubOp -> (Mos Namespace x, Mos Namespace x)
    f = bimap mosify mosify . Map.partition isSub
    mosify :: Ord x => Map (Namespace, x) a -> Mos Namespace x
    mosify = Mos.fromList . Map.keys
    (pSub, pUnsub) = f p
    (tSub, tUnsub) = f t
    (dSub, dUnsub) = f d
  in
    (ClientRegs pSub tSub dSub, ClientRegs pUnsub tUnsub dUnsub)

data TrcUpdateDigest = TrcUpdateDigest
  { trcudNamespace :: Namespace
  , trcudData :: DataDigest
  , trcudCreates :: Creates
  , trcudContOps :: ContOps (Either Placeholder Seg)
  } deriving (Show, Eq)

trcudEmpty :: Namespace -> TrcUpdateDigest
trcudEmpty ns = TrcUpdateDigest ns mempty mempty mempty

newtype FrcRootDigest = FrcRootDigest
  { frcrdContOps :: RootContOps
  } deriving (Show, Eq)

frcrdNull :: FrcRootDigest -> Bool
frcrdNull = null . frcrdContOps

data FrcSubDigest = FrcSubDigest
  -- FIXME: really this is a Mol:
  { frcsdErrors :: Map SubErrorIndex [Text]
  , frcsdPostTypeUnsubs :: Mos Namespace (Tagged PostDefinition Seg)
  , frcsdTypeUnsubs :: Mos Namespace (Tagged Definition Seg)
  , frcsdDataUnsubs :: Mos Namespace Path
  } deriving (Show, Eq)

frcsdNull :: FrcSubDigest -> Bool
frcsdNull (FrcSubDigest errs ptUnsubs tUnsubs dUnsubs) =
  null errs && null ptUnsubs && null tUnsubs && null dUnsubs

instance Monoid FrcSubDigest where
  mempty = FrcSubDigest mempty mempty mempty mempty
  (FrcSubDigest e1 pt1 t1 d1) `mappend` (FrcSubDigest e2 pt2 t2 d2) =
    FrcSubDigest (e1 <> e2) (pt1 <> pt2) (t1 <> t2) (d1 <> d2)

frcsdEmpty :: FrcSubDigest
frcsdEmpty = mempty

frcsdFromClientRegs :: ClientRegs -> FrcSubDigest
frcsdFromClientRegs (ClientRegs p t d) = FrcSubDigest mempty p t d

data FrcUpdateDigest = FrcUpdateDigest
  { frcudNamespace :: Namespace
  , frcudPostDefs :: Map (Tagged PostDefinition Seg) (DefOp PostDefinition)
  , frcudDefinitions :: Map (Tagged Definition Seg) (DefOp Definition)
  , frcudTypeAssignments :: Map Path (Tagged Definition Seg, Editable)
  , frcudData :: DataDigest
  , frcudContOps :: ContOps Seg
  , frcudErrors :: Map DataErrorIndex [Text]
  } deriving (Show, Eq)

frcudEmpty :: Namespace -> FrcUpdateDigest
frcudEmpty ns = FrcUpdateDigest ns mempty mempty mempty alEmpty mempty mempty

frcudNull :: FrcUpdateDigest -> Bool
frcudNull (FrcUpdateDigest _ postDefs defs tas dd cops errs) =
  null postDefs && null defs && null tas && null dd && null cops && null errs

newtype TrprDigest
  = TrprDigest {trprdNamespace :: Namespace}
  deriving (Show, Eq)

data TrDigest
  = Trpd TrpDigest
  | Trprd TrprDigest
  | Trcsd TrcSubDigest
  | Trcud TrcUpdateDigest
  deriving (Show, Eq)

data FrDigest
  = Frpd FrpDigest
  | Frped FrpErrorDigest
  | Frcrd FrcRootDigest
  | Frcsd FrcSubDigest
  | Frcud FrcUpdateDigest
  deriving (Show, Eq)

frNull :: FrDigest -> Bool
frNull = \case
  Frpd d -> frpdNull d
  Frped d -> frpedNull d
  Frcrd d -> frcrdNull d
  Frcsd d -> frcsdNull d
  Frcud d -> frcudNull d

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

unwrapTccum
  :: ToClientContainerUpdateMessage
  -> (Seg, (Maybe Attributee, SequenceOp Seg))
unwrapTccum = \case
  TccumPresentAfter targ ref att -> (targ, (att, SoAfter ref))
  TccumAbsent targ att -> (targ, (att, SoAbsent))

toTccum
  :: (Seg, (Maybe Attributee, SequenceOp Seg))
  -> ToClientContainerUpdateMessage
toTccum (targ, (att, so)) = case so of
  SoAfter ref -> TccumPresentAfter targ ref att
  SoAbsent -> TccumAbsent targ att

digestRootContOpMsgs :: [ToClientContainerUpdateMessage] -> RootContOps
digestRootContOpMsgs = Map.fromList
  . fmap (bimap Namespace (fmap Namespace . snd) . unwrapTccum)

produceRootContOpMsgs :: RootContOps -> [ToClientContainerUpdateMessage]
produceRootContOpMsgs =
    fmap (toTccum . bimap unNamespace ((Nothing,) . fmap unNamespace))
  . Map.toList

digestTccums :: [(Path, ToClientContainerUpdateMessage)] -> ContOps Seg
digestTccums = splitMap . fmap (fmap unwrapTccum)

produceTccums :: ContOps Seg -> [(Path, ToClientContainerUpdateMessage)]
produceTccums = mconcat . Map.elems . Map.mapWithKey
    (\p -> Map.elems . Map.mapWithKey (\t x -> (p, toTccum (t, x))))

digestTpcums
  :: [(Path, ToProviderContainerUpdateMessage)]
  -> (Creates, ContOps (Either Placeholder Seg))
digestTpcums msgs =
  let
    cs :: [(Path, (Placeholder, (Maybe Attributee, CreateOp)))]
    xs :: [(Path, (Seg, (Maybe Attributee, SequenceOp (Either Placeholder Seg))))]
    (cs, xs) = partitionEithers $ fmap (f . fmap procTpcum) msgs in
    (mash cs, mash xs)
  where
    f :: (a, Either b c) -> Either (a, b) (a, c)
    f (a, e) = either (Left . (a,)) (Right . (a,)) e
    mash :: (Ord k1, Ord k2) => [(k1, (k2, v))] -> Map k1 (Map k2 v)
    mash = fmap Map.fromList . unMol . Mol.fromList
    procTpcum
      :: ToProviderContainerUpdateMessage
      -> Either
           (Placeholder, (Maybe Attributee, CreateOp))
           (Seg, (Maybe Attributee, SequenceOp (Either Placeholder Seg)))
    procTpcum t = case t of
      TpcumCreateAfter args ph ref att -> Left (ph, (att, OpCreate args ref))
      TpcumMoveAfter targ ref att -> Right (targ, (att, SoAfter ref))
      TpcumAbsent targ att -> Right (targ, (att, SoAbsent))


produceTpcums
  :: Creates -> ContOps (Either Placeholder Seg)
  -> [(Path, ToProviderContainerUpdateMessage)]
produceTpcums creates cops = createMsgs <> copMsgs
  where
    unmash f = Mol.toList . Mol . fmap (Map.elems . Map.mapWithKey f)
    procCreate ph (att, OpCreate args ref) = TpcumCreateAfter args ph ref att
    createMsgs = unmash procCreate creates
    procCop targ (att, op) = case op of
      SoAfter ref -> TpcumMoveAfter targ ref att
      SoAbsent -> TpcumAbsent targ att
    copMsgs = unmash procCop cops

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

digestSubMessages :: [(Namespace, SubMessage)] -> TrcSubDigest
digestSubMessages msgs = foldMap procMsg msgs
  where
     s ns x = Map.singleton (ns, x)
     procMsg (ns, msg) = case msg of
       MsgSubscribe p -> mempty {trcsdData = s ns p OpSubscribe}
       MsgUnsubscribe p -> mempty {trcsdData = s ns p OpUnsubscribe}
       MsgPostTypeSubscribe tn -> mempty {trcsdPostTypes = s ns tn OpSubscribe}
       MsgPostTypeUnsubscribe tn ->
         mempty {trcsdPostTypes = s ns tn OpUnsubscribe}
       MsgTypeSubscribe tn -> mempty {trcsdTypes = s ns tn OpSubscribe}
       MsgTypeUnsubscribe tn -> mempty {trcsdTypes = s ns tn OpUnsubscribe}

produceSubMessages :: TrcSubDigest -> [(Namespace, SubMessage)]
produceSubMessages (TrcSubDigest p t d) =
       f MsgPostTypeSubscribe MsgPostTypeUnsubscribe p
    ++ f MsgTypeSubscribe MsgTypeUnsubscribe t
    ++ f MsgSubscribe MsgUnsubscribe d
  where
    f :: (x -> SubMessage) -> (x -> SubMessage) -> Map (Namespace, x) SubOp
      -> [(Namespace, SubMessage)]
    f onSub onUnsub =
        fmap (\((ns, x), so) ->
                (ns, if isSub so then onSub x else onUnsub x))
      . Map.toList


digestTypeMessages
  :: [TypeMessage] -> Map Path (Tagged Definition Seg, Editable)
digestTypeMessages = Map.fromList . fmap procMsg
  where
    procMsg (MsgAssignType p s ed) = (p, (s, ed))

produceTypeMessages
  :: Map Path (Tagged Definition Seg, Editable) -> [TypeMessage]
produceTypeMessages = Map.elems . Map.mapWithKey
  (\p (s, l) -> MsgAssignType p s l)

digestDataErrMsgs :: [DataErrorMessage] -> Map DataErrorIndex [Text]
digestDataErrMsgs = foldl (Map.unionWith (<>)) mempty . fmap procMsg
  where
    procMsg (MsgDataError ei t) = Map.singleton ei [t]

produceDataErrMsgs :: Map DataErrorIndex [Text] -> [DataErrorMessage]
produceDataErrMsgs =
  mconcat . Map.elems . Map.mapWithKey (\ei errs -> MsgDataError ei <$> errs)

digestSubErrMsgs :: [SubErrorMessage] -> Map SubErrorIndex [Text]
digestSubErrMsgs = foldl (Map.unionWith (<>)) mempty . fmap procMsg
  where
    procMsg (MsgSubError ei t) = Map.singleton ei [t]

produceSubErrMsgs :: Map SubErrorIndex [Text] -> [SubErrorMessage]
produceSubErrMsgs = mconcat . Map.elems
  . Map.mapWithKey (\ei errs -> MsgSubError ei <$> errs)

digestToRelayBundle :: ToRelayBundle -> TrDigest
digestToRelayBundle trb = case trb of
    Trpb b -> Trpd $ digestToRelayProviderBundle b
    Trpr b -> Trprd $ digestToRelayProviderRelinquish b
    Trcsb b -> Trcsd $ digestToRelayClientSubBundle b
    Trcub b -> Trcud $ digestToRelayClientUpdateBundle b
  where
    digestToRelayProviderBundle :: ToRelayProviderBundle -> TrpDigest
    digestToRelayProviderBundle
        (ToRelayProviderBundle ns errs postDefs defs dat cont) =
      TrpDigest ns
        (digestDefMessages postDefs)
        (digestDefMessages defs)
        (digestDataUpdateMessages dat)
        (digestTccums cont)
        (digestDataErrMsgs errs)

    digestToRelayProviderRelinquish :: ToRelayProviderRelinquish -> TrprDigest
    digestToRelayProviderRelinquish (ToRelayProviderRelinquish ns) =
      TrprDigest ns

    digestToRelayClientSubBundle :: ToRelayClientSubBundle -> TrcSubDigest
    digestToRelayClientSubBundle (ToRelayClientSubBundle subs) =
      digestSubMessages subs

    digestToRelayClientUpdateBundle
      :: ToRelayClientUpdateBundle -> TrcUpdateDigest
    digestToRelayClientUpdateBundle
        (ToRelayClientUpdateBundle ns dat cont) =
      let
        dd = digestDataUpdateMessages dat
        (creates, co) = digestTpcums cont
      in
        TrcUpdateDigest ns dd creates co

produceToRelayBundle :: TrDigest -> ToRelayBundle
produceToRelayBundle trd = case trd of
    Trpd d -> Trpb $ produceToRelayProviderBundle d
    Trprd d -> Trpr $ produceToRelayProviderRelinquish d
    Trcsd d -> Trcsb $ produceToRelayClientSubBundle d
    Trcud d -> Trcub $ produceToRelayClientUpdateBundle d
  where
    produceToRelayProviderBundle (TrpDigest ns postDefs defs dat cops errs) =
      ToRelayProviderBundle
        ns (produceDataErrMsgs errs)
        (produceDefMessages postDefs) (produceDefMessages defs)
        (produceDataUpdateMessages dat) (produceTccums cops)

    produceToRelayProviderRelinquish (TrprDigest ns) =
      ToRelayProviderRelinquish ns

    produceToRelayClientSubBundle trcsd =
      ToRelayClientSubBundle $ produceSubMessages trcsd

    produceToRelayClientUpdateBundle (TrcUpdateDigest ns dd creates co) =
      let
        dat = produceDataUpdateMessages dd
        cont = produceTpcums creates co
      in
        ToRelayClientUpdateBundle ns dat cont

digestFromRelayBundle :: FromRelayBundle -> FrDigest
digestFromRelayBundle frb = case frb of
    Frpb b -> Frpd $ digestFromRelayProviderBundle b
    Frpeb b -> Frped $ digestFromRelayProviderErrorBundle b
    Frcrb b -> Frcrd $ digestFromRelayClientRootBundle b
    Frcsb b -> Frcsd $ digestFromRelayClientSubBundle b
    Frcub b -> Frcud $ digestFromRelayClientUpdateBundle b
  where
    digestFromRelayProviderBundle (FromRelayProviderBundle ns dums coms) =
      let
        (creates, cops) = digestTpcums coms
      in
        FrpDigest ns (digestDataUpdateMessages dums) creates cops

    digestFromRelayProviderErrorBundle (FromRelayProviderErrorBundle errs) =
        FrpErrorDigest $ digestDataErrMsgs errs

    digestFromRelayClientRootBundle =
      FrcRootDigest . digestRootContOpMsgs . frcrbContMsgs

    digestFromRelayClientSubBundle
        (FromRelayClientSubBundle subErrs ptSubs tSubs dSubs) =
      FrcSubDigest
        (digestSubErrMsgs subErrs)
        (Mos.fromList ptSubs)
        (Mos.fromList tSubs)
        (Mos.fromList dSubs)

    digestFromRelayClientUpdateBundle
        (FromRelayClientUpdateBundle ns errs postDefs defs tas dums coms) =
      FrcUpdateDigest ns
        (digestDefMessages postDefs)
        (digestDefMessages defs)
        (digestTypeMessages tas)
        (digestDataUpdateMessages dums)
        (digestTccums coms)
        (digestDataErrMsgs errs)

produceFromRelayBundle :: FrDigest -> FromRelayBundle
produceFromRelayBundle frd = case frd of
    Frpd d -> Frpb $ produceFromRelayProviderBundle d
    Frped d -> Frpeb $ produceFromRelayProviderErrorBundle d
    Frcrd d -> Frcrb $ produceFromRelayClientRootBundle d
    Frcsd d -> Frcsb $ produceFromRelayClientSubBundle d
    Frcud d -> Frcub $ produceFromRelayClientUpdateBundle d
  where
    produceFromRelayProviderBundle :: FrpDigest -> FromRelayProviderBundle
    produceFromRelayProviderBundle (FrpDigest ns dd creates co) =
      FromRelayProviderBundle ns
      (produceDataUpdateMessages dd) (produceTpcums creates co)

    produceFromRelayProviderErrorBundle
      :: FrpErrorDigest -> FromRelayProviderErrorBundle
    produceFromRelayProviderErrorBundle (FrpErrorDigest errs) =
      FromRelayProviderErrorBundle $ produceDataErrMsgs errs

    produceFromRelayClientRootBundle =
      FromRelayClientRootBundle . produceRootContOpMsgs . frcrdContOps

    produceFromRelayClientSubBundle :: FrcSubDigest -> FromRelayClientSubBundle
    produceFromRelayClientSubBundle
        (FrcSubDigest subErrs postTyUns tyUns datUns) =
      FromRelayClientSubBundle
        (produceSubErrMsgs subErrs)
        (Mos.toList postTyUns)
        (Mos.toList tyUns)
        (Mos.toList datUns)

    produceFromRelayClientUpdateBundle
      :: FrcUpdateDigest -> FromRelayClientUpdateBundle
    produceFromRelayClientUpdateBundle
        (FrcUpdateDigest ns postDefs defs tas dd co errs) =
      FromRelayClientUpdateBundle ns
        (produceDataErrMsgs errs)
        (produceDefMessages postDefs) (produceDefMessages defs)
        (produceTypeMessages tas)
        (produceDataUpdateMessages dd) (produceTccums co)

-- The following are slightly different (and more internal to the relay), they
-- are not neccessarily intended for a single recipient

type OutboundClientUpdateDigest = FrcUpdateDigest
type OutboundClientInitialisationDigest = OutboundClientUpdateDigest
type OutboundClientSubErrsDigest = Map SubErrorIndex [Text]
type OutboundProviderDigest = FrpDigest

ocsedNull :: OutboundClientSubErrsDigest -> Bool
ocsedNull = null

data OutboundDigest
  = Ocrid FrcRootDigest  -- "Outbound client root initialisation digest"
  | Ocid OutboundClientInitialisationDigest
  | Ocsed OutboundClientSubErrsDigest
  | Ocrd FrcRootDigest
  | Ocud OutboundClientUpdateDigest
  | Opd OutboundProviderDigest
  | Ope FrpErrorDigest
  deriving (Show, Eq)
