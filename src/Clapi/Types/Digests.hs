{-# LANGUAGE
    LambdaCase
#-}

module Clapi.Types.Digests where

import Data.Either (partitionEithers)
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Data.Word (Word32)

import qualified Data.Map.Mol as Mol

import Clapi.Types.AssocList
  (AssocList, alNull, alEmpty, alFromList, alFmapWithKey, alValues)
import Clapi.Types.Base (Attributee, Time, Interpolation)
import Clapi.Types.Definitions (Definition, Liberty, PostDefinition)
import Clapi.Types.Messages
import Clapi.Types.Path
  ( Seg, Path, TypeName(..), pattern (:</), pattern (:/), Namespace(..)
  , Placeholder(..))
import qualified Clapi.Types.Path as Path
import Clapi.Types.SequenceOps (SequenceOp(..), isSoAbsent)
import Clapi.Types.Wire (WireValue)

data SubOp = OpSubscribe | OpUnsubscribe deriving (Show, Eq)

isSub :: SubOp -> Bool
isSub OpSubscribe = True
isSub OpUnsubscribe = False

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
  { ocArgs :: [WireValue]
  , ocAfter :: Maybe (Either Placeholder Seg)
  } deriving (Show, Eq)
type Creates = Map Path (Map Placeholder (Maybe Attributee, CreateOp))

type RootContOps = Map Seg (SequenceOp Seg)
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
  , trpdErrors :: Map DataErrorIndex [Text]
  } deriving (Show, Eq)

trpDigest :: Namespace -> TrpDigest
trpDigest ns = TrpDigest ns mempty mempty alEmpty mempty mempty

trpdRemovedPaths :: TrpDigest -> [Path]
trpdRemovedPaths trpd =
    ((unNamespace $ trpdNamespace trpd) :</) <$>
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

frpDigest :: Namespace -> FrpDigest
frpDigest ns = FrpDigest ns mempty mempty mempty

frpdNull :: FrpDigest -> Bool
frpdNull (FrpDigest _ dd creates cops) = null dd && null creates && null cops

newtype FrpErrorDigest = FrpErrorDigest
  { frpedErrors :: Map DataErrorIndex [Text]
  } deriving (Show, Eq)

data TrcSubDigest = TrcSubDigest
  { trcsdPostTypeSubs :: Map (Tagged PostDefinition TypeName) SubOp
  , trcsdTypeSubs :: Map (Tagged Definition TypeName) SubOp
  , trcsdDataSubs :: Map Path SubOp
  } deriving (Show, Eq)

trcsdNamespaces :: TrcSubDigest -> Set Namespace
trcsdNamespaces (TrcSubDigest ptSubs tSubs dSubs) =
       -- Assumes TypeName ordered by Namespace first ;-)
       Set.mapMonotonic (tnNamespace . unTagged) (Map.keysSet ptSubs)
    <> Set.mapMonotonic (tnNamespace . unTagged) (Map.keysSet tSubs)
       -- Assumes Path ordered in segment order
    <> (Set.fromAscList
        $ fmap (Namespace . fst)
        $ mapMaybe Path.splitHead
        $ Set.toAscList
        $ Map.keysSet dSubs)

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

data FrcSubDigest = FrcSubDigest
  { frcsdErrors :: Map SubErrorIndex [Text]
  , frcsdPostTypeUnusbs :: Set (Tagged PostDefinition TypeName)
  , frcsdTypeUnusbs :: Set (Tagged Definition TypeName)
  , frcsdDataUnusbs :: Set Path
  } deriving (Show, Eq)

frcsdNull :: FrcSubDigest -> Bool
frcsdNull (FrcSubDigest errs ptUnsubs tUnsubs dUnsubs) =
  null errs && null ptUnsubs && null tUnsubs && null dUnsubs

frcsdEmpty :: FrcSubDigest
frcsdEmpty = FrcSubDigest mempty mempty mempty mempty

data FrcUpdateDigest = FrcUpdateDigest
  { frcudNamespace :: Namespace
  , frcudPostDefs :: Map (Tagged PostDefinition Seg) (DefOp PostDefinition)
  , frcudDefinitions :: Map (Tagged Definition Seg) (DefOp Definition)
  , frcudTypeAssignments :: Map Path (Tagged Definition TypeName, Liberty)
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
digestRootContOpMsgs = fmap snd . Map.fromList . fmap unwrapTccum

produceRootContOpMsgs :: RootContOps -> [ToClientContainerUpdateMessage]
produceRootContOpMsgs = fmap toTccum . Map.toList . fmap (Nothing,)

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
    mash = fmap Map.fromList . Mol.fromList
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
    unmash f = Mol.toList . fmap (Map.elems . Map.mapWithKey f)
    procCreate ph (att, OpCreate args ref) = TpcumCreateAfter args ph ref att
    createMsgs = unmash procCreate creates
    procCop targ (att, op) = case op of
      SoAfter ref -> TpcumMoveAfter targ ref att
      SoAbsent -> TpcumAbsent targ att
    copMsgs = unmash procCop cops

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
produceSubErrMsgs =
  mconcat . Map.elems . Map.mapWithKey (\ei errs -> MsgSubError ei <$> errs)

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
      let
        (postTySubs, tySubs, datSubs) = digestSubMessages subs
      in
        TrcSubDigest postTySubs tySubs datSubs

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

    produceToRelayClientSubBundle (TrcSubDigest pTySubs tySubs datSubs) =
      ToRelayClientSubBundle $ produceSubMessages pTySubs tySubs datSubs

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
        (Set.fromList ptSubs)
        (Set.fromList tSubs)
        (Set.fromList dSubs)

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
        (Set.toList postTyUns)
        (Set.toList tyUns)
        (Set.toList datUns)

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

-- data InboundClientDigest = InboundClientDigest
--   { icdGets :: Set Path
--   , icdPostTypeGets :: Set (Tagged PostDefinition TypeName)
--   , icdTypeGets :: Set (Tagged Definition TypeName)
--   , icdContOps :: ContOps [WireValue]
--   , icdData :: DataDigest
--   } deriving (Show, Eq)

-- inboundClientDigest :: InboundClientDigest
-- inboundClientDigest = InboundClientDigest mempty mempty mempty mempty
--   alEmpty

type OutboundClientUpdateDigest = FrcUpdateDigest
type OutboundClientInitialisationDigest = OutboundClientUpdateDigest
type OutboundClientSubErrsDigest = Map SubErrorIndex [Text]
type OutboundProviderDigest = FrpDigest

-- data OutboundProviderDigest = OutboundProviderDigest
--   { opdContainerOps :: ContainerOps [WireValue]
--   , opdData :: DataDigest
--   } deriving (Show, Eq)

-- opdNull :: OutboundProviderDigest -> Bool
-- opdNull (OutboundProviderDigest cops dd) = null cops && alNull dd

data OutboundDigest
  = Ocid OutboundClientInitialisationDigest
  | Ocsed OutboundClientSubErrsDigest
  | Ocud OutboundClientUpdateDigest
  | Ocrd FrcRootDigest
  | Opd OutboundProviderDigest
  | Ope FrpErrorDigest
  deriving (Show, Eq)
