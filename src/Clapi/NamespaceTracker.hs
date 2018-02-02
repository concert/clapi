{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Clapi.NamespaceTracker where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad (forever, unless, void)
import Control.Monad.State (StateT(..), evalStateT, get, put, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Strict.Merge (merge, zipWithMatched, mapMissing)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text


import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos
import Clapi.Types.AssocList
  (AssocList, alEmpty,  alFilterKey, alInsert, alFoldlWithKey, alKeysSet)
import Clapi.Types.Messages (ErrorIndex(..))
import Clapi.Types.Digests
  ( TrDigest(..), TrcDigest(..), TrpDigest(..), TrprDigest(..)
  , FrDigest(..), FrcDigest(..), FrpDigest(..), FrpErrorDigest(..)
  , DefOp(..), SubOp(..), frcdNull, trcdNamespaces
  , InboundDigest(..), InboundClientDigest(..), OutboundDigest(..)
  , OutboundClientDigest(..), OutboundClientInitialisationDigest
  , OutboundProviderDigest(..))
import Clapi.Types () -- Either String a MonadFail instance
import Clapi.Types.Path (Seg, Path, TypeName(..))
import qualified Clapi.Types.Path as Path
import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Protocol (Protocol, Directed(..), wait, sendFwd, sendRev)

data Ownership = Owner | Client deriving (Eq, Show)

newtype Originator i = Originator i

type NstProtocol m i = Protocol
    (ClientEvent i TrDigest)
    ((Originator i, InboundDigest))
    (ServerEvent i FrDigest)
    ((Originator i, OutboundDigest))
    m

data NstState i
  = NstState
  { nstOwners :: Map Seg i
  , nstTypeRegistrations :: Mos i TypeName
  , nstDataRegistrations :: Mos i Path
  } deriving Show


nstProtocol
  :: (Monad m, Ord i)
  => (Map Seg i -> m ())
  -> NstProtocol m i ()
nstProtocol publish =
  evalStateT (nstProtocol_ publish) $ NstState mempty mempty mempty

nstProtocol_
  :: (Monad m, Ord i)
  => (Map Seg i -> m ())
  -> StateT (NstState i) (NstProtocol m i) ()
nstProtocol_ publish = forever $ liftedWaitThen fwd rev
  where
    sendFwd' i d = lift $ sendFwd (Originator i, d)
    fwd (ClientConnect _) = return ()
    fwd (ClientData i trd) =
      case trd of
        Trpd d -> eitherState
          (throwOutProvider i (Set.singleton $ trpdNamespace d))
          (const $ sendFwd' i (Ipd d))
          (claimNamespace i (trpdNamespace d))
        Trprd d -> eitherState
          (throwOutProvider i (Set.singleton $ trprdNamespace d))
          (const $ sendFwd' i (Iprd d))
          (relinquishNamespace i (trprdNamespace d))
        Trcd d -> eitherState
          (uncurry $ throwOutProvider i)
          (const $ toInboundClientDigest i d >>= sendFwd' i . Icd)
          (guardNsClientDigest i d)
    fwd (ClientDisconnect i) = handleDisconnect i
    rev (Originator i, od) = case od of
      Ocid d -> registerSubs i d
        >> lift (sendRev $ ServerData i $ Frcd $ subResponse d)
      Ocd d -> broadcastClientDigest d
      Opd d -> dispatchProviderDigest d
      Ope d -> (lift $ sendRev $ ServerData i $ Frped d)
        >> (lift $ sendRev $ ServerDisconnect i) >> handleDisconnect i

throwOutProvider
  :: (Ord i, Monad m)
  => i -> Set Seg -> String -> StateT (NstState i) (NstProtocol m i) ()
throwOutProvider i nss msg = do
  mapM_ (\ns -> lift $ sendRev $ ServerData i $ Frped $ FrpErrorDigest ns $
    Map.singleton GlobalError [Text.pack msg]) nss
  lift $ sendRev $ ServerDisconnect i
  handleDisconnect i

modify' :: MonadFail m => (s -> m s) -> StateT s m ()
modify' f = get >>= lift . f >>= put

eitherState
  :: Monad m
  => (l -> StateT s m b) -> (a -> StateT s m b)
  -> StateT s (Either l) a -> StateT s m b
eitherState onFail onSuccess m = StateT $ \s -> either
    (\l -> runStateT (onFail l) s)
    (\(a, s') -> runStateT (onSuccess a) s')
    (runStateT m s)

claimNamespace :: (Eq i, MonadFail m) => i -> Seg -> StateT (NstState i) m ()
claimNamespace i ns = modify' go
  where
    go nst =
      let
        (existing, owners') = Map.insertLookupWithKey
          (\_ _ _ -> i) ns i (nstOwners nst)
      in
        case existing of
          Nothing -> return nst{nstOwners = owners'}
          Just i' -> if (i' == i)
            then return nst
            else fail "Already owned by someone else guv"

relinquishNamespace
  :: (Eq i, MonadFail m) => i -> Seg -> StateT (NstState i) m ()
relinquishNamespace i ns = modify' go
  where
    go nst =
      let
        (existing, owners') = Map.updateLookupWithKey
          (\_ _ -> Nothing) ns (nstOwners nst)
      in
        case existing of
          Nothing -> fail "You're not the owner"
          Just i' -> if (i' == i)
            then return nst{nstOwners = owners'}
            else fail "You're not the owner"

guardNsClientDigest
  :: Eq i => i -> TrcDigest -> StateT (NstState i) (Either (Set Seg, String)) ()
guardNsClientDigest i d =
  let
    nss = trcdNamespaces d
    getBadNss = Map.keysSet . Map.filter (== i) . flip Map.restrictKeys nss
  in do
    nsts <- get
    let ownedAndMentioned = getBadNss $ nstOwners nsts
    lift $ unless (null $ ownedAndMentioned) $
      Left (ownedAndMentioned, "Acted as client on own namespace")

toInboundClientDigest
  :: (Ord i, Monad m)
  => i -> TrcDigest -> StateT (NstState i) m InboundClientDigest
toInboundClientDigest i trcd =
  let
    isSub OpSubscribe = True
    isSub OpUnsubscribe = False
    (dSubs, dUnsubs) = mapPair Map.keysSet $ Map.partition isSub $ trcdDataSubs trcd
    (tSubs, tUnsubs) = mapPair Map.keysSet $ Map.partition isSub $ trcdTypeSubs trcd
  in do
    nsts <- get
    put nsts{
      nstDataRegistrations = Mos.difference
        (nstDataRegistrations nsts) (Map.singleton i dUnsubs),
      nstTypeRegistrations = Mos.difference
        (nstTypeRegistrations nsts) (Map.singleton i tUnsubs)}
    return $ InboundClientDigest
      (Set.difference dSubs $
        Map.findWithDefault mempty i $ nstDataRegistrations nsts)
      (Set.difference tSubs $
        Map.findWithDefault mempty i $ nstTypeRegistrations nsts)
      (trcdReorderings trcd)
      (trcdData trcd)
  where
    mapPair f (a, b) = (f a, f b)

handleDisconnect
  :: (Ord i, Monad m) => i -> StateT (NstState i) (NstProtocol m i) ()
handleDisconnect i = do
    nsts <- get
    let (removed, remaining) = Map.partition (== i) $ nstOwners nsts
    put $ nsts{nstOwners = remaining}
    mapM_ send $ Map.keys removed
  where
    send ns = lift $ sendFwd (Originator i, Iprd $ TrprDigest ns)

registerSubs
  :: (Ord i, Monad m)
  => i -> OutboundClientInitialisationDigest -> StateT (NstState i) m ()
registerSubs i (OutboundClientDigest cas defs _tas dd _errs) = modify go
  where
    newDRegsForI = Set.union (Map.keysSet cas) (alKeysSet dd)
    go (NstState owners tRegs dRegs) = NstState owners
      (Map.insertWith (<>) i (Map.keysSet defs) tRegs)
      (Map.insertWith (<>) i newDRegsForI dRegs)

subResponse :: OutboundClientInitialisationDigest -> FrcDigest
subResponse (OutboundClientDigest cas defs tas dd errs) =
  FrcDigest cas (OpDefine <$> defs) tas dd errs

broadcastClientDigest
  :: (Ord i, Monad m)
  => OutboundClientDigest -> StateT (NstState i) (NstProtocol m i) ()
broadcastClientDigest d = do
    nsts <- get
    let fromRelayClientDigests =
          Map.filter (not . frcdNull)
          $ uncurry (produceFromRelayClientDigest d)
          <$> pairMaps (,) (nstDataRegistrations nsts) (nstTypeRegistrations nsts)
    void $ sequence $ Map.mapWithKey sendRevWithI fromRelayClientDigests
  where
    sendRevWithI i frcd = lift $ sendRev $ ServerData i $ Frcd frcd

dispatchProviderDigest
  :: Monad m
  => OutboundProviderDigest -> StateT (NstState i) (NstProtocol m i) ()
dispatchProviderDigest d =
  let
    send i = lift . sendRev . ServerData i . Frpd
  in do
    nsts <- get
    let dispatch ns =
          maybe (error "no owner for namespace") send
          $ Map.lookup ns $ nstOwners nsts
    void $ sequence $ Map.mapWithKey dispatch $ frpdsByNamespace d

frpdsByNamespace :: OutboundProviderDigest -> Map Seg FrpDigest
frpdsByNamespace (OutboundProviderDigest reords dd) =
  let
    (casRoots, casByNs) = nestMapsByKey Path.splitHead reords
    (_, ddByNs) = nestAlByKey Path.splitHead dd
    -- FIXME: whacking the global stuff to everybody isn't quite right - we need
    -- to know who originated the opd?
    f ns reords' dd' = FrpDigest ns (reords' <> casRoots) dd'
  in
    pairMapsWithKey mempty alEmpty f casByNs ddByNs

produceFromRelayClientDigest
  :: OutboundClientDigest -> Set Path -> Set TypeName -> FrcDigest
produceFromRelayClientDigest
  (OutboundClientDigest cas defs tas dd errs) ps tns = FrcDigest
    (Map.restrictKeys cas ps)
    (OpDefine <$> Map.restrictKeys defs tns)
    (Map.restrictKeys tas ps)
    (alFilterKey (`Set.member` ps) dd)
    (Map.filterWithKey relevantErr errs)
  where
    relevantErr ei _ = case ei of
      GlobalError -> True
      PathError p -> p `Set.member` ps
      TimePointError p _ -> p `Set.member` ps
      TypeNameError tn -> tn `Set.member` tns

liftedWaitThen ::
  (Monad m, MonadTrans t, Monad (t (Protocol a a' b' b m))) =>
  (a -> t (Protocol a a' b' b m) ()) ->
  (b -> t (Protocol a a' b' b m) ()) ->
  t (Protocol a a' b' b m) ()
liftedWaitThen onFwd onRev = do
  d <- lift wait
  case d of
    Fwd a -> onFwd a
    Rev b -> onRev b

pairMapsWithKey
  :: Ord k
  => a -> b -> (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
pairMapsWithKey defaultA defaultB f = merge
  (mapMissing $ \k a -> f k a defaultB)
  (mapMissing $ \k b -> f k defaultA b)
  (zipWithMatched f)

pairMaps
  :: (Ord k, Monoid a, Monoid b)
  => (a -> b -> c) -> Map k a -> Map k b -> Map k c
pairMaps f = pairMapsWithKey mempty mempty $ const f

nestMapsByKey
  :: (Ord k, Ord k0, Ord k1)
  => (k -> Maybe (k0, k1)) -> Map k a -> (Map k a, Map k0 (Map k1 a))
nestMapsByKey f = Map.foldlWithKey g mempty
  where
    g (unsplit, nested) k val = case f k of
      Just (k0, k1) ->
        ( unsplit
        , Map.alter (Just . Map.insert k1 val . maybe mempty id) k0 nested)
      Nothing -> (Map.insert k val unsplit, nested)

nestAlByKey
  :: (Ord k, Ord k0, Ord k1)
  => (k -> Maybe (k0, k1)) -> AssocList k a -> (AssocList k a, Map k0 (AssocList k1 a))
nestAlByKey f = alFoldlWithKey g (alEmpty, mempty)
  where
    g (unsplit, nested) k val = case f k of
      Just (k0, k1) ->
        ( unsplit
        , Map.alter (Just . alInsert k1 val . maybe alEmpty id) k0 nested)
      Nothing -> (alInsert k val unsplit, nested)
