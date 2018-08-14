{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE
    FlexibleContexts
  , LambdaCase
  , OverloadedStrings
  , PartialTypeSignatures
#-}

module Clapi.Relay where

import Control.Monad (unless, forever)
import Control.Monad.State (StateT(..), evalStateT, get, modify)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first, second, bimap)
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Strict.Merge (merge, preserveMissing, mapMissing, zipWithMaybeMatched)
import qualified Data.Set as Set
import Data.Maybe (fromJust, mapMaybe)
import Data.Monoid
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)

import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos

import Clapi.Types.Base (Attributee)
import qualified Clapi.Types.Dkmap as Dkmap
import Clapi.Types.AssocList
  ( AssocList, alSingleton, unAssocList , alFoldlWithKey)
import Clapi.Types.Messages
  ( DataErrorIndex(..), SubErrorIndex(..), MkSubErrIdx(..))
import Clapi.Types.Digests
  ( TrpDigest(..), TrprDigest(..)
  , FrpErrorDigest(..), FrcUpdateDigest(..), frcudEmpty, FrcRootDigest(..)
  , FrpDigest(..)
  , DataChange(..)
  , TrcUpdateDigest(..)
  , TimeSeriesDataOp(..), DefOp(..)
  , OutboundDigest(..), OutboundClientUpdateDigest
  , OutboundClientSubErrsDigest, OutboundClientInitialisationDigest
  , OutboundProviderDigest
  , DataDigest, ContOps, ocsedNull)
import Clapi.Types.Path (Seg, Path, parentPath, Namespace(..))
import Clapi.Types.Definitions (Editable(..), Definition, PostDefinition)
import Clapi.Types.Wire (WireValue)
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Tree (RoseTreeNode(..), TimeSeries, treeLookupNode)
import Clapi.Valuespace
  ( Valuespace(..), baseValuespace, vsLookupDef
  , processToRelayProviderDigest, processTrcUpdateDigest, valuespaceGet
  , getEditable, ProtoFrpDigest(..), VsLookupDef(..))
import Clapi.Protocol (Protocol, sendRev)
import Clapi.Util (nestMapsByKey, partitionDifference)

-- FIXME: Don't like this dependency, even though at some point NST and Relay
-- need to share this type...
import Clapi.NamespaceTracker (
  PostNstInboundDigest(..), ClientGetDigest, ClientRegs(..), liftedWaitThen)

oppifyTimeSeries :: TimeSeries [WireValue] -> DataChange
oppifyTimeSeries ts = TimeChange $
  Dkmap.flatten (\t (att, (i, wvs)) -> (att, OpSet t wvs i)) ts

-- FIXME: return type might make more sense swapped...
oppifySequence :: Ord k => AssocList k v -> Map k (v, SequenceOp k)
oppifySequence al =
  let (alKs, alVs) = unzip $ unAssocList al in
    Map.fromList $ zipWith3
      (\k afterK v -> (k, (v, SoAfter afterK)))
      alKs (Nothing : (Just <$> alKs)) alVs


-- | A datatype for collecting together all the parts to make up a full
--   _namespaced_ FrcUpdateDigest, but without the actual namespace so that we
--   can make a monoid instance. Namespaces must be tracked separately!
--   This may or may not be a good idea *shrug*
data ProtoFrcUpdateDigest = ProtoFrcUpdateDigest
  { pfrcudPostDefs :: Map (Tagged PostDefinition Seg) (DefOp PostDefinition)
  , pfrcudDefinitions :: Map (Tagged Definition Seg) (DefOp Definition)
  , pfrcudTypeAssignments :: Map Path (Tagged Definition Seg, Editable)
  , pfrcudData :: DataDigest
  , pfrcudContOps :: ContOps Seg
  , pfrcudErrors :: Map DataErrorIndex [Text]
  }

instance Monoid ProtoFrcUpdateDigest where
  mempty = ProtoFrcUpdateDigest mempty mempty mempty mempty mempty mempty
  (ProtoFrcUpdateDigest pd1 defs1 ta1 d1 co1 err1)
      `mappend` (ProtoFrcUpdateDigest pd2 defs2 ta2 d2 co2 err2) =
    ProtoFrcUpdateDigest
      (pd1 <> pd2) (defs1 <> defs2) (ta1 <> ta2) (d1 <> d2) (co1 <> co2)
      (Map.unionWith (<>) err1 err2)

toFrcud :: Namespace -> ProtoFrcUpdateDigest -> FrcUpdateDigest
toFrcud ns (ProtoFrcUpdateDigest defs pdefs tas dat cops errs) =
  FrcUpdateDigest ns defs pdefs tas dat cops errs

vsmLookupVs
  :: Namespace -> Map Namespace Valuespace
  -> Either (SubErrorIndex, String) Valuespace
vsmLookupVs ns vsm = maybe
  (Left $ (NamespaceSubError ns, "Namespace not found")) return $
  Map.lookup ns vsm

rLookupDef
  :: (VsLookupDef def, MkSubErrIdx (Tagged def Seg))
  => Map Namespace Valuespace -> Namespace -> Tagged def Seg
  -> Either (SubErrorIndex, String) def
rLookupDef vsm ns s =
    vsmLookupVs ns vsm >>= first (mkSubErrIdx ns s,) . vsLookupDef s

rGet
  :: Map Namespace Valuespace -> Namespace -> Path
  -> Either
       (SubErrorIndex, String)
       (Definition, Tagged Definition Seg, Editable, RoseTreeNode [WireValue])
rGet vsm ns p =
    vsmLookupVs ns vsm >>= first (mkSubErrIdx ns p,) . valuespaceGet p

genInitDigests
  :: ClientGetDigest -> Map Namespace Valuespace
  -> (OutboundClientSubErrsDigest, [OutboundClientInitialisationDigest])
genInitDigests (ClientRegs ptGets tGets dGets) vsm =
  let
    g :: (a -> b -> Either c d) -> (a, b) -> Either c ((a, b), d)
    g f x@(a, b) = second (x,) $ f a b

    h
      :: (Ord b, MkSubErrIdx b)
      => (Namespace -> b -> Either (SubErrorIndex, String) d)
      -> Mos Namespace b
      -> (Map SubErrorIndex String, Map (Namespace, b) d)
    h f m = bimap Map.fromList Map.fromList
      $ partitionEithers $ g f <$> Mos.toList m

    ptSubErrs, tSubErrs, dSubErrs :: Map SubErrorIndex String
    postDefs :: Map (Namespace, Tagged PostDefinition Seg) PostDefinition
    defs :: Map (Namespace, Tagged Definition Seg) Definition
    treeData :: Map (Namespace, Path) _
    (ptSubErrs, postDefs) = h (rLookupDef vsm) ptGets
    (tSubErrs, defs) = h (rLookupDef vsm) tGets
    (dSubErrs, treeData) = h (rGet vsm) dGets

    -- FIXME: can we only ever have a single error per SubErrIdx now(?)
    -- therefore could ditch the `pure`?
    ocsed = fmap (pure . Text.pack) $ ptSubErrs <> tSubErrs <> dSubErrs

    ocids = Map.elems $ Map.mapWithKey toFrcud $ Map.unionsWith (<>)
      [ i (toDefPfrcud setPostDefs) postDefs
      , i (toDefPfrcud setDefs) defs
      , i toDatPfrcud treeData
      ]
  in
    (ocsed, ocids)
  where
    i
      :: (Ord k1, Ord k2, Monoid b)
      => (k2 -> a -> b) -> Map (k1, k2) a -> Map k1 b
    i f = fmap (Map.foldMapWithKey f) . snd . nestMapsByKey Just

    toDatPfrcud path (def, tn, ed, treeNode) =
      let
        pfrcud = mempty
          { pfrcudDefinitions = Map.singleton tn (OpDefine def)
          , pfrcudTypeAssignments = Map.singleton path (tn, ed)
          }
      in
        case treeNode of
          RtnEmpty -> undefined -- FIXME: might want to get rid of RtnEmpty
          RtnChildren kids -> pfrcud
            { pfrcudContOps = Map.singleton path (oppifySequence kids) }
          RtnConstData att vals -> pfrcud
            { pfrcudData = alSingleton path (ConstChange att vals) }
          RtnDataSeries ts -> pfrcud
            { pfrcudData = alSingleton path (oppifyTimeSeries ts) }

    toDefPfrcud setter tn def = setter $ Map.singleton tn $ OpDefine def
    setPostDefs x = mempty { pfrcudPostDefs = x }
    setDefs x = mempty { pfrcudDefinitions = x }


contDiff
  :: AssocList Seg (Maybe Attributee) -> AssocList Seg (Maybe Attributee)
  -> Map Seg (Maybe Attributee, SequenceOp Seg)
contDiff a b = merge
    asAbsent preserveMissing dropMatched (aftersFor a) (aftersFor b)
  where
    asAfter (acc, prev) k ma = ((k, (ma, SoAfter prev)) : acc, Just k)
    aftersFor = Map.fromList . fst . alFoldlWithKey asAfter ([], Nothing)
    dropMatched = zipWithMaybeMatched $ const $ const $ const Nothing
    asAbsent = mapMissing $ \_ (ma, _) -> (ma, SoAbsent)

mayContDiff
  :: Maybe (RoseTreeNode a) -> AssocList Seg (Maybe Attributee)
  -> Maybe (Map Seg (Maybe Attributee, SequenceOp Seg))
mayContDiff ma kb = case ma of
    Just (RtnChildren ka) -> if ka == kb
        then Nothing
        else Just $ contDiff ka kb
    _ -> Nothing

handleTrcud
  :: Valuespace -> TrcUpdateDigest
  -> (OutboundClientUpdateDigest, OutboundProviderDigest)
handleTrcud vs trcud@(TrcUpdateDigest {trcudNamespace = ns}) =
  let
    (errMap, ProtoFrpDigest dat cr cont) = processTrcUpdateDigest vs trcud
    ocud = (frcudEmpty ns) {frcudErrors = fmap (Text.pack . show) <$> errMap}
  in
    (ocud, FrpDigest ns dat cr cont)

data RelayState
  = RelayState
  { rsProvCache :: Map Namespace Valuespace
  } deriving (Show)

instance Monoid RelayState where
  mempty = RelayState mempty
  (RelayState pc1) `mappend` (RelayState pc2) = RelayState $ pc1 <> pc2

relay
  :: Monad m => RelayState
  -> Protocol (i, PostNstInboundDigest) Void (i, OutboundDigest) Void m ()
relay = evalStateT relay_

relay_
  :: Monad m
  => StateT
        RelayState
        (Protocol (i, PostNstInboundDigest) Void (i, OutboundDigest) Void m)
        ()
relay_ = forever $
    liftedWaitThen fwd (const $ error "Message from the void right")
  where
    sendRev' i d = lift $ sendRev (i, d)
    fwd (i, dig) = case dig of
      PnidRootGet -> do
        vsm <- rsProvCache <$> get
        sendRev' i $ Ocrid $ FrcRootDigest $
          Map.fromSet (const $ SoAfter Nothing) $ Map.keysSet vsm
      PnidClientGet cgd -> do
        (ocsed, ocids) <- genInitDigests cgd . rsProvCache <$> get
        unless (ocsedNull ocsed) $ sendRev' i $ Ocsed ocsed
        mapM_ (sendRev' i . Ocid) ocids
      PnidTrcud trcud -> let ns = trcudNamespace trcud in
        (Map.lookup ns . rsProvCache <$> get) >>= maybe
        ( sendRev' i $ Ocud $ (frcudEmpty ns)
            -- FIXME: Not a global error. NamespaceError?
            { frcudErrors =
              Map.singleton GlobalError ["fixthis: Bad namespace"]}

        )
        (\vs -> let (ocud, opd) = handleTrcud vs trcud in do
            sendRev' i $ Opd opd
            sendRev' i $ Ocud ocud
        )
      PnidTrpd trpd -> let ns = trpdNamespace trpd in do
        vs <- Map.findWithDefault
            (baseValuespace (Tagged $ unNamespace ns) Editable) ns
            . rsProvCache
            <$> get
        either
          (sendRev' i . Ope . FrpErrorDigest) (handleOwnerSuccess i trpd vs) $
          processToRelayProviderDigest trpd vs
      PnidTrprd (TrprDigest ns) -> do
        modify $ RelayState . Map.delete ns . rsProvCache
        sendRev' i $ Ocrd $ FrcRootDigest $ Map.singleton ns SoAbsent
    handleOwnerSuccess
        i (TrpDigest ns postDefs defs dd contOps errs)
        vs (updatedTyAssns, vs') =
      let
        dd' = vsMinimiseDataDigest dd vs
        contOps' = vsMinimiseContOps contOps vs
        mungedTas = Map.mapWithKey
          (\p tn -> (tn, either error id $ getEditable p vs')) updatedTyAssns
        getContOps p = case fromJust $ treeLookupNode p $ vsTree vs' of
          RtnChildren kb -> (p,) <$> mayContDiff (treeLookupNode p $ vsTree vs) kb
          _ -> Nothing
        extraCops = Map.fromAscList $ mapMaybe (\p -> parentPath p >>= getContOps) $
          Set.toAscList $ Map.keysSet updatedTyAssns
        contOps'' = extraCops <> contOps'
        frcrd = FrcRootDigest $ Map.singleton ns $ SoAfter Nothing
      in do
        vsm <- rsProvCache <$> get
        sendRev' i $
          Ocud $ FrcUpdateDigest ns
            -- FIXME: these functions should probably operate on just a
            -- single vs:
            (vsMinimiseDefinitions postDefs ns vsm)
            -- FIXME: we need to provide defs for type assignments too.
            (vsMinimiseDefinitions defs ns vsm)
            mungedTas dd' contOps'' errs
        unless (null $ frcrdContOps frcrd) $
            sendRev' i $ Ocrd frcrd
        modify $ RelayState . Map.insert ns vs' . rsProvCache

-- FIXME: Worst case implementation
-- FIXME: should probably just operate on one Valuespace
vsMinimiseDefinitions
  :: Map (Tagged def Seg) (DefOp def) -> Namespace -> Map Namespace Valuespace
  -> Map (Tagged def Seg) (DefOp def)
vsMinimiseDefinitions defs _ _ = defs

-- FIXME: Worst case implementation
vsMinimiseDataDigest :: DataDigest -> Valuespace -> DataDigest
vsMinimiseDataDigest dd _ = dd

-- FIXME: Worst case implementation
vsMinimiseContOps :: ContOps k -> Valuespace -> ContOps k
vsMinimiseContOps contOps _ = contOps

generateRootUpdates
  :: Map Namespace Valuespace -> Map Namespace Valuespace -> FrcRootDigest
generateRootUpdates vsm vsm' =
  let
    (addedRootNames, removedRootNames) = partitionDifference
      (Map.keysSet vsm') (Map.keysSet vsm)
    addedRcos = Map.fromSet (const $ SoAfter Nothing) addedRootNames
    removedRcos = Map.fromSet (const SoAbsent) removedRootNames
  in
    FrcRootDigest $ addedRcos <> removedRcos
