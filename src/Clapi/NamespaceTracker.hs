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
import Data.Tagged (Tagged)
import qualified Data.Text as Text


import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos
import Clapi.Types.AssocList
  (AssocList, alEmpty,  alFilterKey, alInsert, alFoldlWithKey, alKeysSet)
import Clapi.Types.Messages (ErrorIndex(..))
import Clapi.Types.Digests
  ( TrDigest(..), TrcDigest(..), TrpDigest(..), TrprDigest(..)
  , FrDigest(..), FrcDigest(..), frcdEmpty, FrpDigest(..), FrpErrorDigest(..)
  , DefOp(..), SubOp(..), frcdNull, trcdNamespaces
  , InboundDigest(..), InboundClientDigest(..), OutboundDigest(..)
  , OutboundClientDigest(..), OutboundClientInitialisationDigest
  , OutboundProviderDigest(..))
-- Also `Either String a` MonadFail instance:
import Clapi.Types (Definition, PostDefinition)
import Clapi.Types.Path
  (Path, TypeName(..), pattern (:/), pattern Root, Namespace(..))
import qualified Clapi.Types.Path as Path
import Clapi.Types.SequenceOps (isSoAbsent)
import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Protocol (Protocol, Directed(..), wait, sendFwd, sendRev)
import Clapi.Util (flattenNestedMaps)

data Ownership = Owner | Client deriving (Eq, Show)

newtype Originator i = Originator i deriving (Eq, Show)

type NstProtocol m i = Protocol
    (ClientEvent i TrDigest)
    ((Originator i, InboundDigest))
    (Either (Map Namespace i) (ServerEvent i FrDigest))
    ((Originator i, OutboundDigest))
    m

data NstState i
  = NstState
  { nstOwners :: Map Namespace i
  , nstPostTypeRegistrations :: Mos i (Tagged PostDefinition TypeName)
  , nstTypeRegistrations :: Mos i (Tagged Definition TypeName)
  , nstDataRegistrations :: Mos i Path
  } deriving Show


nstProtocol :: (Monad m, Ord i) => NstProtocol m i ()
nstProtocol = evalStateT nstProtocol_ $ NstState mempty mempty mempty mempty

nstProtocol_ :: (Monad m, Ord i) => StateT (NstState i) (NstProtocol m i) ()
nstProtocol_ = forever $ liftedWaitThen fwd rev
  where
    sendFwd' i d = lift $ sendFwd (Originator i, d)
    fwd (ClientConnect _ _) = return ()
    fwd (ClientData i trd) =
      case trd of
        Trpd d -> claimNamespace i d
          (throwOutProvider i (Set.singleton $ trpdNamespace d))
          (sendFwd' i (Ipd d))
        Trprd d -> relinquishNamespace i (trprdNamespace d)
          (throwOutProvider i (Set.singleton $ trprdNamespace d))
          (sendFwd' i (Iprd d))
        Trcd d -> eitherState
          (uncurry $ throwOutProvider i)
          (const $ do
              (icd, frcd) <- toInboundClientDigest i d
              unless (frcdNull frcd) $
                lift $ sendRev $ Right $ ServerData i $ Frcd frcd
              sendFwd' i $ Icd icd
          )
          (guardNsClientDigest i d)
    fwd (ClientDisconnect i) = handleDisconnect i
    rev (Originator i, od) = case od of
      Ocid d -> registerSubs i d
        >> lift (sendRev $ Right $ ServerData i $ Frcd $ subResponse d)
      Ocd d -> unsubDeleted d >>= lift . broadcastClientDigest d
      Opd d -> dispatchProviderDigest d
      Ope d -> (lift $ sendRev $ Right $ ServerData i $ Frped d)
        >> (lift $ sendRev $ Right $ ServerDisconnect i) >> handleDisconnect i

nonClaim :: TrpDigest -> Bool
nonClaim trpd = trpdData trpd == alEmpty && null (trpdContainerOps trpd)

updateOwners
  :: Monad m => Map Namespace i ->  StateT (NstState i) (NstProtocol m i) ()
updateOwners owners = do
  modify $ \nsts -> nsts {nstOwners = owners}
  lift $ sendRev $ Left owners

throwOutProvider
  :: (Ord i, Monad m)
  => i -> Set Namespace -> String -> StateT (NstState i) (NstProtocol m i) ()
throwOutProvider i nss msg = do
  lift $ sendRev $ Right $ ServerData i $ Frped $ FrpErrorDigest $
    Set.foldl (\acc ns ->
      Map.insert (PathError $ Root :/ unNamespace ns) [Text.pack msg] acc)
    mempty nss
  lift $ sendRev $ Right $ ServerDisconnect i
  handleDisconnect i

eitherState
  :: Monad m
  => (l -> StateT s m b) -> (a -> StateT s m b)
  -> StateT s (Either l) a -> StateT s m b
eitherState onFail onSuccess m = StateT $ \s -> either
    (\l -> runStateT (onFail l) s)
    (\(a, s') -> runStateT (onSuccess a) s')
    (runStateT m s)

claimNamespace
  :: (Eq i, Monad m)
  => i -> TrpDigest
  -> (String -> StateT (NstState i) (NstProtocol m i) ())
  -> StateT (NstState i) (NstProtocol m i) ()
  -> StateT (NstState i) (NstProtocol m i) ()
claimNamespace i d failureAction successAction = get >>= either
    failureAction
    (\owners' ->
      maybe (return ()) updateOwners owners' >>
      successAction)
    . go . nstOwners
  where
    go owners =
      let
        (existing, owners') = Map.insertLookupWithKey
          (\_ _ _ -> i) (trpdNamespace d) i owners
      in
        case existing of
          Nothing -> if nonClaim d
            then fail "Empty claim"
            else return $ Just owners'
          Just i' -> if (i' == i)
            then return Nothing
            else fail "Already owned by someone else guv"

relinquishNamespace
  :: (Eq i, Monad m)
  => i -> Namespace
  -> (String -> StateT (NstState i) (NstProtocol m i) ())
  -> StateT (NstState i) (NstProtocol m i) ()
  -> StateT (NstState i) (NstProtocol m i) ()
relinquishNamespace i ns failureAction successAction = get >>= either
    failureAction
    (\owners' -> updateOwners owners' >> successAction)
    . go . nstOwners
  where
    go owners =
      let
        (existing, owners') = Map.updateLookupWithKey
          (\_ _ -> Nothing) ns owners
      in
        case existing of
          Nothing -> fail "You're not the owner"
          Just i' -> if (i' == i)
            then return owners'
            else fail "You're not the owner"

guardNsClientDigest
  :: Eq i
  => i -> TrcDigest -> StateT (NstState i) (Either (Set Namespace, String)) ()
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
  => i -> TrcDigest -> StateT (NstState i) m (InboundClientDigest, FrcDigest)
toInboundClientDigest i trcd =
  let
    isSub OpSubscribe = True
    isSub OpUnsubscribe = False
    (dSubs, dUnsubs) = mapPair Map.keysSet $ Map.partition isSub $ trcdDataSubs trcd
    (ptSubs, ptUnsubs) = mapPair Map.keysSet $ Map.partition isSub $ trcdPostTypeSubs trcd
    (tSubs, tUnsubs) = mapPair Map.keysSet $ Map.partition isSub $ trcdTypeSubs trcd
  in do
    nsts <- get
    put nsts{
      nstDataRegistrations = Mos.difference
        (nstDataRegistrations nsts) (Map.singleton i dUnsubs),
      nstPostTypeRegistrations = Mos.difference
        (nstPostTypeRegistrations nsts) (Map.singleton i ptUnsubs),
      nstTypeRegistrations = Mos.difference
        (nstTypeRegistrations nsts) (Map.singleton i tUnsubs)}
    let icd = InboundClientDigest
          (Set.difference dSubs $
            Map.findWithDefault mempty i $ nstDataRegistrations nsts)
          (Set.difference ptSubs $
            Map.findWithDefault mempty i $ nstPostTypeRegistrations nsts)
          (Set.difference tSubs $
            Map.findWithDefault mempty i $ nstTypeRegistrations nsts)
          (trcdContainerOps trcd)
          (trcdData trcd)
    let frcd = frcdEmpty
          { frcdPostTypeUnsubs = Set.intersection ptUnsubs $
              Map.findWithDefault mempty i $ nstPostTypeRegistrations nsts
          , frcdTypeUnsubs = Set.intersection tUnsubs $
              Map.findWithDefault mempty i $ nstTypeRegistrations nsts
          , frcdDataUnsubs = Set.intersection dUnsubs $
            Map.findWithDefault mempty i $ nstDataRegistrations nsts}
    return (icd, frcd)
  where
    mapPair f (a, b) = (f a, f b)

handleDisconnect
  :: (Ord i, Monad m) => i -> StateT (NstState i) (NstProtocol m i) ()
handleDisconnect i = do
    nsts <- get
    let (removed, remaining) = Map.partition (== i) $ nstOwners nsts
    unless (null removed) $ updateOwners remaining
    mapM_ send $ Map.keys removed
  where
    send ns = lift $ sendFwd (Originator i, Iprd $ TrprDigest ns)

registerSubs
  :: (Ord i, Monad m)
  => i -> OutboundClientInitialisationDigest -> StateT (NstState i) m ()
registerSubs i (OutboundClientDigest cas postDefs defs _tas dd _errs) =
    modify go
  where
    newDRegsForI = Set.union (Map.keysSet cas) (alKeysSet dd)
    go (NstState owners ptRegs tRegs dRegs) = NstState owners
      (Map.insertWith (<>) i (Map.keysSet postDefs) ptRegs)
      (Map.insertWith (<>) i (Map.keysSet defs) tRegs)
      (Map.insertWith (<>) i newDRegsForI dRegs)

subResponse :: OutboundClientInitialisationDigest -> FrcDigest
subResponse (OutboundClientDigest cOps postDefs defs tas dd errs) =
    FrcDigest postTypesInError typesInError pathsInError postDefs defs tas dd
    cOps errs
  where
    (pathsInError, postTypesInError, typesInError) =
        foldl collectError mempty $ Map.keys errs
    collectError (pie, ptie, tie) ei = case ei of
        PathError p -> (Set.insert p pie, ptie, tie)
        PostTypeError t -> (pie, Set.insert t ptie, tie)
        TypeError t -> (pie, ptie, Set.insert t tie)
        _ -> (pie, ptie, tie)

unsubDeleted
  :: (Monad m, Ord i) => OutboundClientDigest
  -> StateT (NstState i) m
       ( Mos i (Tagged PostDefinition TypeName)
       , Mos i (Tagged Definition TypeName)
       , Mos i (Tagged PostDefinition TypeName)
       , Mos i (Tagged Definition TypeName)
       , Mos i Path, Mos i Path)
unsubDeleted d = do
    nsts <- get
    let (ptUnsubs, ptRemainingSubs) = Mos.partition
          (`Set.member` allUndefPTys) $ nstPostTypeRegistrations nsts
    let (tUnsubs, tRemainingSubs) = Mos.partition
          (`Set.member` allUndefTys) $ nstTypeRegistrations nsts
    let (dUnsubs, dRemainingSubs) = Mos.partition
          (`Path.isChildOfAny` allDeletePaths) $ nstDataRegistrations nsts
    put $ nsts
      { nstDataRegistrations = dRemainingSubs
      , nstPostTypeRegistrations = ptRemainingSubs
      , nstTypeRegistrations = tRemainingSubs
      }
    return
      ( ptUnsubs, tUnsubs
      , ptRemainingSubs, tRemainingSubs
      , dUnsubs, dRemainingSubs
      )
  where
    allUndefPTys = Map.keysSet $ Map.filter isUndefTy $ ocdPostDefs d
    allUndefTys = Map.keysSet $ Map.filter isUndefTy $ ocdDefinitions d
    isUndefTy defOp = case defOp of
      OpUndefine -> True
      _ -> False
    allDeletePaths = Map.keys $ flattenNestedMaps (:/) $
      Map.filter isSoAbsent . fmap snd <$> ocdContainerOps d

broadcastClientDigest
  :: (Ord i, Monad m)
  => OutboundClientDigest
  -> ( Mos i (Tagged PostDefinition TypeName)
     , Mos i (Tagged Definition TypeName)
     , Mos i (Tagged PostDefinition TypeName)
     , Mos i (Tagged Definition TypeName)
     , Mos i Path, Mos i Path)
  -> NstProtocol m i ()
broadcastClientDigest d
  (ptUnsubs, tUnsubs, ptRemSubs, tRemSubs, dUnsubs, dRemSubs) = do
    let fromRelayClientDigests = Map.filter (not . frcdNull) $ zipMaps6
          (produceFromRelayClientDigest d) dUnsubs ptUnsubs tUnsubs dRemSubs
          ptRemSubs tRemSubs
    void $ sequence $ Map.mapWithKey sendRevWithI fromRelayClientDigests
  where
    sendRevWithI i frcd = sendRev $ Right $ ServerData i $ Frcd frcd

dispatchProviderDigest
  :: Monad m
  => OutboundProviderDigest -> StateT (NstState i) (NstProtocol m i) ()
dispatchProviderDigest d =
  let
    send i = lift . sendRev . Right . ServerData i . Frpd
  in do
    nsts <- get
    let dispatch ns =
          maybe (error "no owner for namespace") send
          $ Map.lookup ns $ nstOwners nsts
    void $ sequence $ Map.mapWithKey dispatch $ frpdsByNamespace d

frpdsByNamespace :: OutboundProviderDigest -> Map Namespace FrpDigest
frpdsByNamespace (OutboundProviderDigest contOps dd) =
  let
    (rootCOps, casByNs) = nestMapsByKey Path.splitHead contOps
    (_, ddByNs) = nestAlByKey Path.splitHead dd
    -- FIXME: whacking the global stuff to everybody isn't quite right - we need
    -- to know who originated the opd?
    f ns contOps' dd' = FrpDigest ns dd' (contOps' <> rootCOps)
  in
    zipMapsWithKey mempty alEmpty f
      (Map.mapKeys Namespace casByNs)
      (Map.mapKeys Namespace ddByNs)

produceFromRelayClientDigest
  :: OutboundClientDigest -> Set Path -> Set (Tagged PostDefinition TypeName)
  -> Set (Tagged Definition TypeName)
  -> Set Path -> Set (Tagged PostDefinition TypeName)
  -> Set (Tagged Definition TypeName) -> FrcDigest
produceFromRelayClientDigest
  (OutboundClientDigest cOps postDefs defs tas dd errs) pUsubs ptUnsubs tUsubs
  ps ptns tns = FrcDigest
    ptUnsubs tUsubs pUsubs
    (Map.restrictKeys postDefs ptns)
    (Map.restrictKeys defs tns)
    (Map.restrictKeys tas ps)
    (alFilterKey (`Set.member` ps) dd)
    (Map.restrictKeys cOps ps)
    (Map.filterWithKey relevantErr errs)
  where
    relevantErr ei _ = case ei of
      GlobalError -> True
      PathError p -> p `Set.member` ps
      TimePointError p _ -> p `Set.member` ps
      PostTypeError tn -> tn `Set.member` ptns
      TypeError tn -> tn `Set.member` tns

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

zipMapsWithKey
  :: Ord k
  => a -> b -> (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
zipMapsWithKey defaultA defaultB f = merge
  (mapMissing $ \k a -> f k a defaultB)
  (mapMissing $ \k b -> f k defaultA b)
  (zipWithMatched f)

zipMaps
  :: (Ord k, Monoid a, Monoid b)
  => (a -> b -> c) -> Map k a -> Map k b -> Map k c
zipMaps f = zipMapsWithKey mempty mempty $ const f

zipMaps4
  :: (Ord k, Monoid a, Monoid b, Monoid c, Monoid d)
  => (a -> b -> c -> d -> e) -> Map k a -> Map k b -> Map k c -> Map k d
  -> Map k e
zipMaps4 f ma mb mc md = zipMaps (\(a, b) (c, d) -> f a b c d)
  (zipMaps (,) ma mb) (zipMaps (,) mc md)

zipMaps6
  :: (Ord k, Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f)
  => (a -> b -> c -> d -> e -> f -> g) -> Map k a -> Map k b -> Map k c
  -> Map k d -> Map k e -> Map k f -> Map k g
zipMaps6 fun ma mb mc md me mf =
  zipMaps (\(a, (b, c)) (d, (e, f)) -> fun a b c d e f)
  (zipMaps (,) ma (zipMaps (,) mb mc))
  (zipMaps (,) md (zipMaps (,) me mf))

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
  => (k -> Maybe (k0, k1)) -> AssocList k a
  -> (AssocList k a, Map k0 (AssocList k1 a))
nestAlByKey f = alFoldlWithKey g (alEmpty, mempty)
  where
    g (unsplit, nested) k val = case f k of
      Just (k0, k1) ->
        ( unsplit
        , Map.alter (Just . alInsert k1 val . maybe alEmpty id) k0 nested)
      Nothing -> (alInsert k val unsplit, nested)
