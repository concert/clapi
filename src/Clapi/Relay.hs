{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Clapi.Relay where

import Control.Monad (unless)
import Data.Either (isLeft, fromLeft, fromRight)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Strict.Merge (merge, preserveMissing, mapMissing, zipWithMaybeMatched)
import qualified Data.Set as Set
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Void (Void)

import Clapi.Types.Base (Attributee)
import qualified Clapi.Types.Dkmap as Dkmap
import Clapi.Types.AssocList
  (AssocList, alEmpty, alInsert, alFilterKey, unAssocList, alMapKeys, alFoldlWithKey)
import Clapi.Types.Messages (ErrorIndex(..), namespaceErrIdx)
import Clapi.Types.Digests
  ( TrpDigest(..), TrprDigest(..)
  , FrpErrorDigest(..), DataChange(..)
  , TimeSeriesDataOp(..), DefOp(..)
  , OutboundDigest(..), InboundDigest(..)
  , OutboundClientDigest(..), OutboundClientInitialisationDigest, ocdNull, opdNull
  , InboundClientDigest(..), OutboundProviderDigest(..)
  , DataDigest, ContainerOps)
import Clapi.Types.Path (Seg, Path, TypeName(..), pattern (:</), pattern Root, parentPath)
import Clapi.Types.Definitions (Liberty, Definition)
import Clapi.Types.Wire (WireValue)
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Tree (RoseTreeNode(..), TimeSeries, treeLookupNode)
import Clapi.Valuespace
  ( Valuespace(..), vsRelinquish, vsLookupDef
  , processToRelayProviderDigest, processToRelayClientDigest, valuespaceGet
  , getLiberty, rootTypeName)
import Clapi.Protocol (Protocol, waitThenFwdOnly, sendRev)

mapPartitionJust :: Map k (Maybe a) -> (Map k a, Set k)
mapPartitionJust m = let (js, ns) = Map.partition isJust m in
  (fromJust <$> js, Map.keysSet ns)

mapPartitionEither :: Map k (Either a b) -> (Map k a, Map k b)
mapPartitionEither m = let (ls, rs) = Map.partition isLeft m in
  (fromLeft undefined <$> ls, fromRight undefined <$> rs)

oppifyTimeSeries :: TimeSeries [WireValue] -> DataChange
oppifyTimeSeries ts = TimeChange $
  Dkmap.flatten (\t (att, (i, wvs)) -> (att, OpSet t wvs i)) ts

genInitDigest
  :: Set Path -> Set TypeName -> Valuespace
  -> OutboundClientInitialisationDigest
genInitDigest ps tns vs =
  let
    rtns = Map.fromSet (flip valuespaceGet vs) ps
    (tnErrs, defs) = mapPartitionEither $ Map.fromSet (flip vsLookupDef vs) tns
    initialOcd = OutboundClientDigest mempty (OpDefine <$> defs) mempty alEmpty
      (pure . Text.pack <$> Map.mapKeys TypeError tnErrs)
  in
    Map.foldlWithKey go initialOcd rtns
  where
    go
      :: OutboundClientInitialisationDigest -> Path
      -> Either String (Definition, TypeName, Liberty, RoseTreeNode [WireValue])
      -> OutboundClientInitialisationDigest
    go d p (Left errStr) = d {
        ocdErrors = Map.unionWith (<>) (ocdErrors d)
          (Map.singleton (PathError p) [Text.pack errStr])
      }
    go d p (Right (def, tn, lib, rtn)) =
      let
        d' = d {
          ocdDefinitions = Map.insert tn (OpDefine def) (ocdDefinitions d),
          ocdTypeAssignments = Map.insert p (tn, lib) (ocdTypeAssignments d)}
      in case rtn of
        RtnEmpty -> error "Valid tree should not contain empty nodes, but did"
        RtnChildren kidsAl ->
          let (kidSegs, kidAtts) = unzip $ unAssocList kidsAl in
          d' {
            ocdContainerOps = Map.insert p
              (Map.fromList $ zipWith3
                (\s a att -> (s, (att, SoPresentAfter a)))
                kidSegs (Nothing : (Just <$> kidSegs)) kidAtts)
            (ocdContainerOps d')
          }
        RtnConstData att vals -> d'{ocdData =
          alInsert p (ConstChange att vals) $ ocdData d}
        RtnDataSeries ts ->
          d'{ocdData = alInsert p (oppifyTimeSeries ts) $ ocdData d}

contDiff
  :: AssocList Seg (Maybe Attributee) -> AssocList Seg (Maybe Attributee)
  -> Map Seg (Maybe Attributee, SequenceOp Seg)
contDiff a b = merge
    asAbsent preserveMissing dropMatched (aftersFor a) (aftersFor b)
  where
    asAfter (acc, prev) k ma = ((k, (ma, SoPresentAfter prev)) : acc, Just k)
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

relay
  :: Monad m => Valuespace
  -> Protocol (i, InboundDigest) Void (i, OutboundDigest) Void m ()
relay vs = waitThenFwdOnly fwd
  where
    fwd (i, dig) = case dig of
        Ipd d -> either
          terminalErrors
          (handleOwnerSuccess d) $ processToRelayProviderDigest d vs
        Icd d -> handleClientDigest d
          $ processToRelayClientDigest (icdContainerOps d) (icdData d) vs
        Iprd (TrprDigest ns) -> do
          let vs' = vsRelinquish ns vs
          sendRev (i, Ocd $ OutboundClientDigest
            -- FIXME: Attributing revocation to nobody!
            (Map.singleton Root $ Map.singleton ns (Nothing, SoAbsent))
            (Map.insert rootTypeName
              (OpDefine $ fromJust $ vsLookupDef rootTypeName vs') $
              (fmap (const OpUndefine) $ Map.mapKeys (TypeName ns) $
                 Map.findWithDefault mempty ns $ vsTyDefs vs))
            mempty alEmpty mempty)
          relay vs'
      where
        handleOwnerSuccess
            (TrpDigest ns defs valDefs dd contOps errs) (updatedTyAssns, vs') =
          let
            shouldPubRoot =
              Map.member ns defs &&
              Map.notMember ns (vsTyDefs vs)
            rootDef = fromJust $ vsLookupDef rootTypeName vs'
            qDd = maybe (error "Bad sneakers") id $ alMapKeys (ns :</) dd
            qDd' = vsMinimiseDataDigest qDd vs
            errs' = Map.mapKeys (namespaceErrIdx ns) errs
            qDefs = Map.mapKeys (TypeName ns) defs
            qDefs' = vsMinimiseDefinitions qDefs vs
            qDefs'' = if shouldPubRoot
              then Map.insert rootTypeName (OpDefine rootDef) qDefs'
              else qDefs'
            qContOps = Map.mapKeys (ns :</) contOps
            qContOps' = vsMinimiseContOps qContOps vs
            mungedTas = Map.mapWithKey
              (\p tn -> (tn, either error id $ getLiberty p vs')) updatedTyAssns
            getContOps p = case fromJust $ treeLookupNode p $ vsTree vs' of
              RtnChildren kb -> (p,) <$> mayContDiff (treeLookupNode p $ vsTree vs) kb
              _ -> Nothing
            extraCops = Map.fromAscList $ mapMaybe (\p -> parentPath p >>= getContOps) $
              Set.toAscList $ Map.keysSet updatedTyAssns
            qContOps'' = extraCops <> qContOps'
          in do
            sendRev (i,
              Ocd $ OutboundClientDigest
                qContOps''
                -- FIXME: we need to provide defs for type assignments too.
                qDefs''
                mungedTas qDd' errs')
            relay vs'
        handleClientDigest
            (InboundClientDigest gets typeGets contOps dd) errMap =
          let
            -- TODO: Be more specific in what we reject (filtering by TpId
            -- rather than entire path)
            eidxPath eidx = case eidx of
                PathError p -> Just p
                TimePointError p _ -> Just p
                _ -> Nothing
            errPaths = Set.fromList $ mapMaybe eidxPath $ Map.keys errMap
            dd' = alFilterKey (\k -> not $ Set.member k errPaths) dd
            contOps' = Map.filterWithKey
              (\k _ -> not $ Set.member k errPaths) contOps
            dd'' = vsMinimiseDataDigest dd' vs
            contOps'' = vsMinimiseContOps contOps' vs
            -- FIXME: above uses errors semantically and shouldn't (thus throws
            -- away valid time point changes)
            cid = genInitDigest gets typeGets vs
            cid' = cid{ocdErrors =
              Map.unionWith (<>) (ocdErrors cid) (fmap (Text.pack . show) <$> errMap)}
            opd = OutboundProviderDigest contOps'' dd''
          in do
            unless (ocdNull cid') $ sendRev (i, Ocid cid')
            unless (opdNull opd) $ sendRev (i, Opd opd)
            relay vs
        terminalErrors errMap = do
          sendRev (i, Ope $ FrpErrorDigest errMap)
          relay vs

-- FIXME: Worst case implementation
vsMinimiseDefinitions :: Map TypeName (DefOp d) -> Valuespace -> Map TypeName (DefOp d)
vsMinimiseDefinitions defs _ = defs

-- FIXME: Worst case implementation
vsMinimiseDataDigest :: DataDigest -> Valuespace -> DataDigest
vsMinimiseDataDigest dd _ = dd

-- FIXME: Worst case implementation
vsMinimiseContOps :: ContainerOps -> Valuespace -> ContainerOps
vsMinimiseContOps contOps _ = contOps
