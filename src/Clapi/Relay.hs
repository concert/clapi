{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Clapi.Relay where

import Data.Either (isLeft, fromLeft, fromRight)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)

import qualified Clapi.Types.Dkmap as Dkmap
import Clapi.Types.AssocList
  (alEmpty, alInsert, alFilterKey, alSetDefault, unAssocList)
import Clapi.Types.Messages (ErrorIndex(..), namespaceErrIdx)
import Clapi.Types.Digests
  ( ContainerOps, DataDigest, TrpDigest(..), TrprDigest(..)
  , FrpErrorDigest(..), DataChange(..)
  , TimeSeriesDataOp(..), DefOp(..)
  , OutboundDigest(..), InboundDigest(..)
  , OutboundClientDigest(..), OutboundClientInitialisationDigest(..)
  , InboundClientDigest(..), OutboundProviderDigest(..))
import Clapi.Types.Path (Path, TypeName(..), pattern (:</), pattern (:/))
import Clapi.Types.Definitions (Liberty, Definition)
import Clapi.Types.Wire (WireValue)
import Clapi.Types.SequenceOps (SequenceOp(SoPresentAfter))
import Clapi.Tree (RoseTreeNode(..), TimeSeries)
import Clapi.Valuespace
  ( Valuespace(..), vsRelinquish, vsLookupDef
  , processToRelayProviderDigest, processToRelayClientDigest, valuespaceGet
  , getLiberty)
import Clapi.Protocol (Protocol, waitThenFwdOnly, sendRev)
import Clapi.Util (flattenNestedMaps)

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
    initialOcd = OutboundClientDigest
      mempty (OpDefine <$> defs) mempty alEmpty (pure . Text.pack <$> Map.mapKeys TypeError tnErrs)
  in
    Map.foldlWithKey go initialOcd rtns
  where
    go :: OutboundClientInitialisationDigest -> Path -> Either String (Definition, TypeName, Liberty, RoseTreeNode [WireValue]) -> OutboundClientInitialisationDigest
    go d p (Left errStr) = d{ocdErrors =
      Map.unionWith (<>) (ocdErrors d) (Map.singleton (PathError p) [Text.pack errStr])}
    go d p (Right (def, tn, lib, rtn)) =
      let
        d' = d{
          ocdDefinitions = Map.insert tn (OpDefine def) (ocdDefinitions d),
          ocdTypeAssignments = Map.insert p (tn, lib) (ocdTypeAssignments d)}
      in case rtn of
        RtnEmpty -> error "Valid tree should not contain empty nodes, but did"
        RtnChildren kidsAl -> let (kidSegs, kidAtts) = unzip $ unAssocList kidsAl in
          d'{ocdContainerOps = Map.insert p
            (Map.fromList $ zipWith3 (\s a att -> (s, (att, SoPresentAfter a))) kidSegs
               (Nothing : (Just <$> kidSegs)) kidAtts)
            (ocdContainerOps d')
          }
        RtnConstData att vals -> d'{ocdData =
          alInsert p (ConstChange att vals) $ ocdData d}
        RtnDataSeries ts ->
          d'{ocdData = alInsert p (oppifyTimeSeries ts) $ ocdData d}

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
        Iprd (TrprDigest ns) -> relay $ vsRelinquish ns vs
      where
        handleOwnerSuccess
            (TrpDigest ns defs dd contOps errs) vs'@(Valuespace _ _ tas) =
          let
            defs' = vsMinimiseDefinitions defs vs
            dd' = vsMinimiseDataDigest dd vs
            contOps' = vsMinimiseReords contOps vs
            errs' = Map.mapKeys (namespaceErrIdx ns) errs
          in do
            sendRev (i,
              Ocd $ OutboundClientDigest contOps'
                -- FIXME: we need to provide defs for type assignments too.
                (Map.mapKeys (TypeName ns) defs')
                (mungedTas vs) dd' errs')
            relay vs'
        mungedTas vs = Map.mapWithKey
          (\p tn -> (tn, either error id $ getLiberty p vs))
          $ fst (vsTyAssns vs)
        handleClientDigest (InboundClientDigest gets typeGets reords dd) errMap =
          let
            dd' = alFilterKey (\k -> not $ Map.member k errMap) dd
            reords' = Map.filterWithKey (\k _ -> not $ Map.member k errMap) reords
            dd'' = vsMinimiseDataDigest dd' vs
            reords'' = vsMinimiseReords reords' vs
            -- FIXME: above uses errors semantically and shouldn't (thus throws
            -- away valid time point changes)
            cid = genInitDigest gets typeGets vs
            cid' = cid{ocdErrors =
              Map.unionWith (<>) (ocdErrors cid) (Map.mapKeys PathError errMap)}
          in do
            sendRev (i, Ocid $ cid')
            sendRev (i, Opd $ OutboundProviderDigest reords'' dd'')
            relay vs
        terminalErrors errMap = do
          sendRev (i, Ope $ FrpErrorDigest errMap)
          relay vs

-- FIXME: these are worst case implementations right now!
vsMinimiseDefinitions defs _ = defs
vsMinimiseDataDigest dd _ = dd
vsMinimiseReords reords _ = reords
