{-# LANGUAGE
    FlexibleContexts
  , LambdaCase
#-}

module Clapi.Relay where

import Control.Monad (unless)
import Data.Bifunctor (first, bimap)
import Data.Either (isLeft, fromLeft, fromRight)
import Data.Foldable (fold, foldMap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Strict.Merge (merge, preserveMissing, mapMissing, zipWithMaybeMatched)
import qualified Data.Set as Set
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Monoid
import Data.Set (Set)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Data.Word (Word32)

import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos

import Clapi.Types.Base (Attributee)
import qualified Clapi.Types.Dkmap as Dkmap
import Clapi.Types.AssocList
  ( AssocList, alEmpty, alSingleton, alInsert, alFilterKey, unAssocList
  , alFmapWithKey, alMapKeys, alFoldlWithKey, alPartitionWithKey)
import Clapi.Types.Messages (DataErrorIndex(..), SubErrorIndex(..))
import Clapi.Types.Digests
  ( TrpDigest(..), TrprDigest(..)
  , FrpErrorDigest(..), FrcUpdateDigest(..), frcudEmpty, FrcRootDigest(..)
  , DataChange(..)
  , TrcUpdateDigest(..)
  , TimeSeriesDataOp(..), DefOp(..)
  , OutboundDigest(..), OutboundClientUpdateDigest
  , OutboundClientSubErrsDigest, OutboundClientInitialisationDigest
  , FrpDigest(..), frpdNull
  , OutboundProviderDigest(..)
  , DataDigest, ContOps, Creates, ocsedNull)
import Clapi.Types.Path
  ( Seg, Path, TypeName(..), qualify, unqualify, pattern (:</), pattern Root
  , parentPath, Namespace(..))
import qualified Clapi.Types.Path as Path
import Clapi.Types.Definitions (Liberty, Definition, PostDefinition)
import Clapi.Types.Wire (WireValue)
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Tree (RoseTreeNode(..), TimeSeries, treeLookupNode, treeChildNames)
import Clapi.Valuespace
  ( Valuespace(..), vsRelinquish, vsLookupPostDef, vsLookupDef
  , processToRelayProviderDigest, processTrcUpdateDigest, valuespaceGet
  , getLiberty, ValidationErr)
import Clapi.Protocol (Protocol, waitThenFwdOnly, sendRev)
import Clapi.Util (nestMapsByKey, mapPartitionEither, partitionDifference)

-- FIXME: Don't like this dependency, even though at some point NST and Relay
-- need to share this type...
import Clapi.NamespaceTracker (PostNstInboundDigest(..), ClientGetDigest(..))

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

splitNs :: Path -> Maybe (Namespace, Path)
splitNs = fmap (first Namespace) . Path.splitHead


-- | A datatype for collecting together all the parts to make up a full
--   _namespaced_ FrcUpdateDigest, but without the actual namespace so that we
--   can make a monoid instance. Namespaces must be tracked separately!
--   This may or may not be a good idea *shrug*
data ProtoFrcUpdateDigest = ProtoFrcUpdateDigest
  { pfrcudPostDefs :: Map (Tagged PostDefinition Seg) (DefOp PostDefinition)
  , pfrcudDefinitions :: Map (Tagged Definition Seg) (DefOp Definition)
  , pfrcudTypeAssignments :: Map Path (Tagged Definition TypeName, Liberty)
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

genInitDigests
  :: ClientGetDigest -> Valuespace
  -> (OutboundClientSubErrsDigest, [OutboundClientInitialisationDigest])
genInitDigests (ClientGetDigest ptGets tGets dGets) vs =
  let
    (ptSubErrs, postDefs) = mapPartitionEither
      $ Map.fromSet (flip vsLookupPostDef vs) ptGets
    (tSubErrs, defs) = mapPartitionEither
      $ Map.fromSet (flip vsLookupDef vs) tGets
    (dSubErrs, treeData) = mapPartitionEither
      $ Map.fromSet (flip valuespaceGet vs) dGets

    -- FIXME: can we only ever have a single error per SubErrIdx now(?)
    -- therefore could ditch the `pure`?
    ocsed = fmap (pure . Text.pack)
        $ Map.mapKeys PostTypeSubError ptSubErrs
       <> Map.mapKeys TypeSubError tSubErrs
       <> Map.mapKeys PathSubError dSubErrs

    ocids = Map.elems $ Map.mapWithKey toFrcud $ Map.unionsWith (<>)
      [ translateDefData setPostDefs postDefs
      , translateDefData setDefs defs
      , translateTreeData treeData
      ]
  in
    (ocsed, ocids)
  where
    translateTreeData =
        fmap fold
      . snd
      . nestMapsByKey splitNs
      . Map.mapMaybeWithKey (\path x -> (mkPfrcud x) . snd <$> splitNs path)
    mkPfrcud (def, ttn, lib, treeNode) path =
      -- We could assert here that the namespace of the (Tagged TypeName) == ns
      let
        pfrcud = mempty
          { pfrcudDefinitions = Map.singleton (tnName <$> ttn) (OpDefine def)
          , pfrcudTypeAssignments = Map.singleton path (ttn, lib)
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

    translateDefData setter =
        fmap fold
      . snd
      . nestMapsByKey (Just . unqualify)
      . Map.mapWithKey (mkDefPfrcud setter)
    mkDefPfrcud setter ttn def = setter
      (Map.singleton (tnName <$> ttn) (OpDefine def))
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

someFunc
  :: Valuespace -> TrcUpdateDigest
  -> (OutboundClientUpdateDigest, OutboundProviderDigest)
someFunc vs trcud@(TrcUpdateDigest ns dat creates cops) =
  let
    (errMap, opd) = processTrcUpdateDigest vs trcud
    ocud = (frcudEmpty ns) {frcudErrors = fmap (Text.pack . show) <$> errMap}
  in
    (ocud, opd)

relay
  :: Monad m => Valuespace
  -> Protocol (i, PostNstInboundDigest) Void (i, OutboundDigest) Void m ()
relay vs = waitThenFwdOnly fwd
  where
    fwd (i, dig) = case dig of
        PnidRootGet -> sendRev
            ( i
            , Ocrid $ FrcRootDigest $ Map.fromSet (const $ SoAfter Nothing) $
              Set.fromList $ treeChildNames $ vsTree vs)
        PnidCgd cgd ->
          let (ocsed, ocids) = genInitDigests cgd vs in do
            unless (ocsedNull ocsed) $ sendRev (i, Ocsed ocsed)
            mapM_ (sendRev . (i,) . Ocid) ocids
            relay vs
        PnidTrcud trcud ->
          let (ocud, opd) = someFunc vs trcud in do
            sendRev (i, Opd $ opd)
            sendRev (i, Ocud $ ocud)
            relay vs
        PnidTrpd trpd -> either
          terminalErrors
          (handleOwnerSuccess trpd) $ processToRelayProviderDigest trpd vs
        PnidTrprd (TrprDigest ns) ->
          let vs' = vsRelinquish ns vs in do
            sendRev (i, Ocrd $ generateRootUpdates vs vs')
            relay vs'
      where
        handleOwnerSuccess
            (TrpDigest ns postDefs defs dd contOps errs) (qUpdatedTyAssns, vs') =
          let
            updatedTyAssns = Map.mapKeys (maybe (error "Fix vs mess") snd . Path.splitHead) qUpdatedTyAssns
            dd' = vsMinimiseDataDigest dd vs
            contOps' = vsMinimiseContOps contOps vs
            mungedTas = Map.mapWithKey
              (\p tn -> (tn, either error id $ getLiberty p vs')) updatedTyAssns
            getContOps p = case fromJust $ treeLookupNode p $ vsTree vs' of
              RtnChildren kb -> (p,) <$> mayContDiff (treeLookupNode p $ vsTree vs) kb
              _ -> Nothing
            extraCops = Map.fromAscList $ mapMaybe (\p -> parentPath p >>= getContOps) $
              Set.toAscList $ Map.keysSet updatedTyAssns
            contOps'' = extraCops <> contOps'
            frcrd = generateRootUpdates vs vs'
          in do
            sendRev (i,
              Ocud $ FrcUpdateDigest ns
                (vsMinimiseDefinitions postDefs ns vs)
                -- FIXME: we need to provide defs for type assignments too.
                (vsMinimiseDefinitions defs ns vs)
                mungedTas dd' contOps'' errs)
            unless (null $ frcrdContOps frcrd) $
                sendRev (i, Ocrd frcrd)
            relay vs'
        terminalErrors errMap = do
          sendRev (i, Ope $ FrpErrorDigest errMap)
          relay vs

-- FIXME: Worst case implementation
vsMinimiseDefinitions
  :: Map (Tagged def Seg) (DefOp def) -> Namespace -> Valuespace
  -> Map (Tagged def Seg) (DefOp def)
vsMinimiseDefinitions defs _ _ = defs

-- FIXME: Worst case implementation
vsMinimiseDataDigest :: DataDigest -> Valuespace -> DataDigest
vsMinimiseDataDigest dd _ = dd

-- FIXME: Worst case implementation
vsMinimiseContOps :: ContOps k -> Valuespace -> ContOps k
vsMinimiseContOps contOps _ = contOps

generateRootUpdates :: Valuespace -> Valuespace -> FrcRootDigest
generateRootUpdates vs vs' =
  let
    (addedRootNames, removedRootNames) = partitionDifference
      (Set.fromList $ treeChildNames $ vsTree vs')
      (Set.fromList $ treeChildNames $ vsTree vs)
    addedRcos = Map.fromSet (const $ SoAfter Nothing) addedRootNames
    removedRcos = Map.fromSet (const SoAbsent) removedRootNames
  in
    FrcRootDigest $ addedRcos <> removedRcos
