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
import Clapi.Types.Messages (ErrorIndex(..), namespaceErrIdx)
import Clapi.Types.Digests
  ( ChildAssignments, DataDigest, TrpDigest(..), TrprDigest(..)
  , FrpErrorDigest(..), DataChange(..)
  , TimeSeriesDataOp(..))
import Clapi.Types.Path (Path, TypeName(..))
import Clapi.Types.Definitions (Liberty, Definition)
import Clapi.Types.Wire (WireValue)
import Clapi.Tree (RoseTreeNode(..), TimeSeries)
import Clapi.Valuespace
  ( Valuespace(..), vsRelinquish, vsLookupDef
  , processToRelayProviderDigest, processToRelayClientDigest, valuespaceGet
  , getLiberty)
import Clapi.Protocol (Protocol, waitThenFwdOnly, sendRev)

data InboundClientDigest = InboundClientDigest
  { icdGets :: Set Path
  , icdTypeGets :: Set TypeName
  , icdChildAssignments :: ChildAssignments
  , icdData :: DataDigest
  } deriving Show

data InboundDigest
  = Icd InboundClientDigest
  | Ipd TrpDigest
  | Iprd TrprDigest

data OutboundClientDigest = OutboundClientDigest
  { ocdChildAssignments :: ChildAssignments
  , ocdDefinitions :: Map TypeName Definition
  , ocdTypeAssignments :: Map Path (TypeName, Liberty)
  , ocdData :: DataDigest
  , ocdErrors :: Map (ErrorIndex TypeName) [Text]
  } deriving Show

type OutboundClientInitialisationDigest = OutboundClientDigest

data OutboundProviderDigest = OutboundProviderDigest
  { opdChildAssignments :: ChildAssignments
  , opddData :: DataDigest
  } deriving Show

data OutboundDigest
  = Ocid OutboundClientInitialisationDigest
  | Ocd OutboundClientDigest
  | Opd OutboundProviderDigest
  | Ope FrpErrorDigest
  deriving Show

mapPartitionJust :: Map k (Maybe a) -> (Map k a, Set k)
mapPartitionJust m = let (js, ns) = Map.partition isJust m in
  (fromJust <$> js, Map.keysSet ns)

mapPartitionEither :: Map k (Either a b) -> (Map k a, Map k b)
mapPartitionEither m = let (ls, rs) = Map.partition isLeft m in
  (fromLeft undefined <$> ls, fromRight undefined <$> rs)

flattenNestedMaps
  :: (Ord k0, Ord k1, Ord k2)
  => (k0 -> k1 -> k2) -> Map k0 (Map k1 v) -> Map k2 v
flattenNestedMaps f = Map.foldlWithKey inner mempty
  where
    inner acc k0 m = Map.union acc $ Map.mapKeys (\k1 -> f k0 k1) m

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
      mempty defs mempty mempty (pure . Text.pack <$> Map.mapKeys TypeNameError tnErrs)
  in
    Map.foldlWithKey go initialOcd rtns
  where
    go :: OutboundClientInitialisationDigest -> Path -> Either String (Definition, TypeName, Liberty, RoseTreeNode [WireValue]) -> OutboundClientInitialisationDigest
    go d p (Left errStr) = d{ocdErrors =
      Map.unionWith (<>) (ocdErrors d) (Map.singleton (PathError p) [Text.pack errStr])}
    go d p (Right (def, tn, lib, rtn)) =
      let
        d' = d{
          ocdDefinitions = Map.insert tn def (ocdDefinitions d),
          ocdTypeAssignments = Map.insert p (tn, lib) (ocdTypeAssignments d)}
      in case rtn of
        RtnEmpty -> error "Valid tree should not contain empty nodes, but did"
        RtnChildren att kids -> d'{ocdChildAssignments =
          Map.insert p (att, kids) $ ocdChildAssignments d}
        RtnConstData att vals -> d'{ocdData =
          Map.insert p (ConstChange att vals) $ ocdData d}
        RtnDataSeries ts ->
          d'{ocdData = Map.insert p (oppifyTimeSeries ts) $ ocdData d}

relay
  :: Monad m => Valuespace
  -> Protocol (i, InboundDigest) Void (i, OutboundDigest) Void m ()
relay vs = waitThenFwdOnly fwd
  where
    fwd (i, dig) = case dig of
        Ipd d -> either
          (terminalError $ trpdNamespace d)
          (handleOwnerSuccess d) $ processToRelayProviderDigest d vs
        Icd d -> handleClientDigest d
          $ processToRelayClientDigest (icdChildAssignments d) (icdData d) vs
        Iprd (TrprDigest ns) -> relay $ vsRelinquish ns vs
      where
        handleOwnerSuccess
            (TrpDigest ns cas _ dd errs) vs'@(Valuespace _ defs tas) =
          let
            dd' = vsMinimiseDataDigest dd vs
            cas' = vsMinimiseCas cas vs
            errs' = Map.mapKeys (namespaceErrIdx ns) errs
          in do
            sendRev (i,
              Ocd $ OutboundClientDigest cas'
                (flattenNestedMaps TypeName defs) (mungedTas vs) dd' errs')
            relay vs'
        mungedTas vs = Map.mapWithKey
          (\p tn -> (tn, either error id $ getLiberty p vs))
          $ fst (vsTyAssns vs)
        handleClientDigest (InboundClientDigest gets typeGets cas dd) errMap =
          let
            dd' = Map.filterWithKey (\k _ -> not $ Map.member k errMap) dd
            cas' = Map.filterWithKey (\k _ -> not $ Map.member k errMap) cas
            dd'' = vsMinimiseDataDigest dd' vs
            cas'' = vsMinimiseCas cas' vs
            -- FIXME: above uses errors semantically and shouldn't (thus throws
            -- away valid time point changes)
            cid = genInitDigest gets typeGets vs
            cid' = cid{ocdErrors =
              Map.unionWith (<>) (ocdErrors cid) (Map.mapKeys PathError errMap)}
          in do
            sendRev (i, Ocid $ cid')
            sendRev (i, Opd $ OutboundProviderDigest cas'' dd'')
            relay vs
        terminalError ns str = do
          sendRev
            ( i, Ope $ FrpErrorDigest ns
            $ Map.singleton GlobalError [Text.pack str])
          relay vs

-- FIXME: these are worst case implementations right now!
vsMinimiseDataDigest dd _ = dd
vsMinimiseCas cas _ = cas
