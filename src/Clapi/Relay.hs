{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE
    FlexibleContexts
  , LambdaCase
  , OverloadedStrings
  , PartialTypeSignatures
  , TemplateHaskell
#-}

module Clapi.Relay where

import Control.Lens (makeLenses, view, over, set, at, non, assign, use)
import Control.Monad (unless, when, forever, void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT(..), evalStateT, get, modify)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first, second, bimap)
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Strict.Merge (merge, preserveMissing, mapMissing, zipWithMaybeMatched)
import qualified Data.Set as Set
import Data.Maybe (fromJust, mapMaybe, fromMaybe)
import Data.Semigroup
import Data.Set (Set)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void, absurd)

import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos

import Clapi.Types.Base (Attributee)
import qualified Clapi.Types.Dkmap as Dkmap
import Clapi.Types.AssocList
  ( AssocList, alSingleton, unAssocList , alFoldlWithKey, alFilterKey)
import Clapi.Types.Messages
  ( DataErrorIndex(..), SubErrorIndex(..), MkSubErrIdx(..))
import Clapi.Types.Digests
  ( TrpDigest(..), TrprDigest(..), TrcUpdateDigest(..), TrcSubDigest(..)
  , trcsdNamespaces
  , FrpErrorDigest(..), FrcSubDigest(..), FrcUpdateDigest(..)
  , FrcRootDigest(..), FrpDigest(..)
  , frcsdEmpty, frcudEmpty, frcudNull
  , DataChange(..)
  , TimeSeriesDataOp(..), DefOp(..)
  , OutboundClientUpdateDigest
  , OutboundProviderDigest
  , DataDigest, ContOps
  , FrDigest(..), frNull, TrDigest(..), isUndef
  , ClientRegs(..), crNull, crDifference, crIntersection
  , trcsdClientRegs, frcsdFromClientRegs)
import Clapi.Types.Path (Seg, Path, parentPath, Namespace(..), pattern (:/))
import Clapi.Types.Definitions (Editable(..), Definition, PostDefinition)
import Clapi.Types.Wire (WireValue)
import Clapi.Types.SequenceOps (SequenceOp(..), isSoAbsent)
import Clapi.Tree (RoseTreeNode(..), TimeSeries, treeLookupNode)
import Clapi.Valuespace
  ( Valuespace(..), baseValuespace, vsLookupDef
  , processToRelayProviderDigest, processTrcUpdateDigest, valuespaceGet
  , getEditable, ProtoFrpDigest(..), VsLookupDef(..))
import Clapi.Protocol (Protocol, sendRev)
import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Util (nestMapsByKey, partitionDifference, flattenNestedMaps)

import Clapi.NamespaceTracker (liftedWaitThen)

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

instance Semigroup ProtoFrcUpdateDigest where
  ProtoFrcUpdateDigest pd1 defs1 ta1 d1 co1 err1
      <> ProtoFrcUpdateDigest pd2 defs2 ta2 d2 co2 err2 =
    ProtoFrcUpdateDigest
      (pd1 <> pd2) (defs1 <> defs2) (ta1 <> ta2) (d1 <> d2) (co1 <> co2)
      (Map.unionWith (<>) err1 err2)
instance Monoid ProtoFrcUpdateDigest where
  mempty = ProtoFrcUpdateDigest mempty mempty mempty mempty mempty mempty
  mappend = (<>)

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
  :: ClientRegs -> Map Namespace Valuespace
  -> (Map SubErrorIndex [Text], [FrcUpdateDigest])
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
    errMap = fmap (pure . Text.pack) $ ptSubErrs <> tSubErrs <> dSubErrs

    frcuds = Map.elems $ Map.mapWithKey toFrcud $ Map.unionsWith (<>)
      [ i (toDefPfrcud setPostDefs) postDefs
      , i (toDefPfrcud setDefs) defs
      , i toDatPfrcud treeData
      ]
  in
    (errMap, frcuds)
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

handleTrcudOrig
  :: Valuespace -> TrcUpdateDigest
  -> (OutboundClientUpdateDigest, OutboundProviderDigest)
handleTrcudOrig vs trcud@(TrcUpdateDigest {trcudNamespace = ns}) =
  let
    (errMap, ProtoFrpDigest dat cr cont) = processTrcUpdateDigest vs trcud
    ocud = (frcudEmpty ns) {frcudErrors = fmap (Text.pack . show) <$> errMap}
  in
    (ocud, FrpDigest ns dat cr cont)

data RelayState i
  = RelayState
  { _rsVsMap :: Map Namespace Valuespace
  , _rsOwners :: Map Namespace i
  , _rsRegs :: Map i ClientRegs
  , _rsAllClients :: Set i
  } deriving (Show)

makeLenses ''RelayState

instance Ord i => Monoid (RelayState i) where
  mempty = RelayState mempty mempty mempty mempty
  (RelayState vsm1 o1 r1 c1) `mappend` (RelayState vsm2 o2 r2 c2) =
    RelayState (vsm1 <> vsm2) (o1 <> o2) (r1 <> r2) (c1 <> c2)

type RelayProtocol i m = Protocol
    (ClientEvent i TrDigest) Void
    (Either (Map Namespace i) (ServerEvent i FrDigest)) Void
    m

relay
  :: (Ord i, Monad m) => RelayState i -> RelayProtocol i m ()
relay = evalStateT relay_

relay_
  :: (Ord i, Monad m)
  => StateT (RelayState i) (RelayProtocol i m) ()
relay_ = forever $ liftedWaitThen fwd absurd
  where
    fwd (ClientConnect _ i) = handleConnect i
    fwd (ClientData i trd) = case trd of
      Trpd d -> handleTrpd i d
      Trprd d -> handleTrprd i d
      Trcsd d -> handleTrcsd i d
      Trcud d -> handleTrcud i d
    fwd (ClientDisconnect i) = handleDisconnect i

handleConnect
  :: (Ord i, Monad m) => i -> StateT (RelayState i) (RelayProtocol i m) ()
handleConnect i = do
  modify $ over rsAllClients $ Set.insert i
  vsm <- view rsVsMap <$> get
  sendData' i $ Frcrd $ FrcRootDigest $ Map.fromSet
    (const $ SoAfter Nothing) $ Map.keysSet vsm

handleTrpd
  :: (Ord i, Monad m)
  => i -> TrpDigest -> StateT (RelayState i) (RelayProtocol i m) ()
handleTrpd i d = do
    (existingOwner, newOwners) <-
      Map.insertLookupWithKey (\_ v _ -> v) ns i . view rsOwners <$> get
    result <- runExceptT $ case existingOwner of
      Nothing -> do
        frcud <- tryVsUpdate
        lift $ updateOwners newOwners
        -- FIXME: would be nice if we didn't have to track ourselves that we'd
        -- added a new Namespace to the Valuepace Map...
        lift $ sendData' i $ Frcrd $ FrcRootDigest $ Map.singleton ns $
          SoAfter Nothing
        return frcud
      (Just i') -> if (i' == i)
        then tryVsUpdate -- no owner map change
        else throwError $ Map.singleton (NamespaceError ns)
          ["Already owned by another provider"]
    either
      (\errMap -> do
        sendData' i $ Frped $ FrpErrorDigest errMap
        -- FIXME: want to use throwOutProvider, but it does making of
        -- NamespaceErrors at the moment...
        lift $ sendRev $ Right $ ServerDisconnect i
        handleDisconnect i
      )
      (\frcud -> do
         generateClientDigests frcud >>= lift . multicast . fmap Frcud
         unsubscribeFromDeleted frcud >>= lift . multicast . fmap Frcsd
      )
      result
  where
    ns = trpdNamespace d
    bvs = baseValuespace (Tagged $ unNamespace ns) Editable
    tryVsUpdate :: (Ord i, Monad m) => ExceptT _ (StateT (RelayState i) m) _
    tryVsUpdate =
      let
        -- NB: Type signature required to avoid monomorphism of f when trying to
        -- use his lens in two different contexts:
        l :: Functor f => (Valuespace -> f Valuespace) -> RelayState i
          -> f (RelayState i)
        l = rsVsMap . at ns . non bvs
      in do
        vs <- view l <$> get
        let result = processToRelayProviderDigest d vs
        case result of
          Left errMap -> throwError errMap
          Right (updatedTyAssns, vs') -> do
            modify (set l vs')
            return $ produceFrcud vs vs' updatedTyAssns
    produceFrcud vs vs' updatedTyAssns =
      let
        augmentedTyAssns = Map.mapWithKey
           (\p tn -> (tn, either error id $ getEditable p vs')) updatedTyAssns
        getContOps p = case fromJust $ treeLookupNode p $ vsTree vs' of
          RtnChildren kb ->
            (p,) <$> mayContDiff (treeLookupNode p $ vsTree vs) kb
          _ -> Nothing
        extraCops = Map.fromAscList $
          mapMaybe (\p -> parentPath p >>= getContOps) $ Set.toAscList $
          Map.keysSet updatedTyAssns
        contOps = extraCops <> vsMinimiseContOps (trpdContOps d) vs
      in
        FrcUpdateDigest ns
          (vsMinimiseDefinitions (trpdPostDefs d) vs)
          (vsMinimiseDefinitions (trpdDefinitions d) vs)
          augmentedTyAssns
          (vsMinimiseDataDigest (trpdData d) vs)
          contOps
          (trpdErrors d)

handleTrprd
  :: (Eq i, Ord i, Monad m)
  => i -> TrprDigest -> StateT (RelayState i) (RelayProtocol i m) ()
handleTrprd i d = runExceptT dropNamespace >>= either
    (throwOutProvider i (Set.singleton ns))
    return
  where
    ns = trprdNamespace d
    dropNamespace = do
      (existing, owners') <- Map.updateLookupWithKey (\_ _ -> Nothing)  ns
        . view rsOwners <$> get
      if (existing == Just i)
        then lift $ do
          updateOwners owners'
          unsubscribeFromNs (Set.singleton ns) >>= lift . multicast . fmap Frcsd
          broadcast $ Frcrd $ FrcRootDigest $ Map.singleton ns SoAbsent
        else throwError "You're not the owner"

handleTrcsd
  :: (Eq i, Ord i, Monad m)
  => i -> TrcSubDigest -> StateT (RelayState i) (RelayProtocol i m) ()
handleTrcsd i d = runExceptT go >>= either
    (uncurry $ throwOutProvider i)
    return
  where
    (crSubs, crUnsubs) = trcsdClientRegs d
    go = do
      ownedAndSubd <- Map.keysSet
        . Map.filter (== i)
        . flip Map.restrictKeys (trcsdNamespaces d)
        . view rsOwners <$> get
      unless (null ownedAndSubd) $
        throwError (ownedAndSubd, "Acted as client on own namespace")
      current <- fromMaybe mempty <$> use (rsRegs . at i)
      let regs' = mappend crSubs $ current `crDifference` crUnsubs
      assign (rsRegs . at i) $ if (crNull regs') then Nothing else Just regs'
      lift $ sendData' i $ Frcsd $
        frcsdFromClientRegs $ crIntersection current crUnsubs
      (errMap, frcuds) <- genInitDigests (crSubs `crDifference` current)
        <$> use rsVsMap
      lift $ sendData' i $ Frcsd $ frcsdEmpty { frcsdErrors = errMap }
      mapM_ (lift . sendData' i . Frcud) frcuds

handleTrcud
  :: (Eq i, Ord i, Monad m)
  => i -> TrcUpdateDigest -> StateT (RelayState i) (RelayProtocol i m) ()
handleTrcud i d = runExceptT go >>= either
    (throwOutProvider i $ Set.singleton ns)
    undefined
  where
    ns = trcudNamespace d
    go = do
      -- FIXME: this is where vs and owner being tied together might be
      -- useful...
      owner <- use (rsOwners . at ns)
      when (owner == Just i) $ throwError "Acted as client on own namespace"
      use (rsVsMap . at ns) >>= \case
        Nothing -> lift $ sendData' i $ Frcud $ (frcudEmpty ns) {frcudErrors =
          Map.singleton (NamespaceError ns) ["Namespace does not exist"]}
        Just vs ->
          let
            (errMap, ProtoFrpDigest dat cr cont) = processTrcUpdateDigest vs d
          in do
            lift $ sendData' i $ Frcud $
              (frcudEmpty ns) {frcudErrors = fmap (Text.pack . show) <$> errMap}
            case owner of
              Nothing -> error "Namespace doesn't have owner, apparently"
              Just oi -> lift $ sendData' oi $ Frpd $ FrpDigest ns dat cr cont

handleDisconnect
  :: (Ord i, Monad m) => i -> StateT (RelayState i) (RelayProtocol i m) ()
handleDisconnect i = do
  modify $ over rsAllClients $ Set.delete i
  (removed, remaining) <- Map.partition (== i) . view rsOwners
    <$> get
  modify $ over rsVsMap $ flip Map.withoutKeys (Map.keysSet removed)
  unsubscribeFromNs (Map.keysSet removed) >>=
    lift . multicast . fmap Frcsd
  unless (null removed) $ updateOwners remaining

sendData :: Monad m => i -> FrDigest -> RelayProtocol i m ()
sendData i d = unless (frNull d) $ sendRev $ Right $ ServerData i d

sendData' ::
  Monad m => i -> FrDigest -> StateT (RelayState i) (RelayProtocol i m) ()
sendData' i = lift . sendRev . Right . ServerData i

multicast :: Monad m => Map i FrDigest -> RelayProtocol i m ()
multicast = void . sequence . Map.mapWithKey sendData

broadcast :: Monad m => FrDigest -> StateT (RelayState i) (RelayProtocol i m) ()
broadcast d = get
  >>= lift . multicast . Map.fromSet (const d) . view rsAllClients

unsubscribe
  :: Monad m
  => (ClientRegs -> (ClientRegs, ClientRegs))
  -> StateT (RelayState i) m (Map i FrcSubDigest)
unsubscribe partitionRegs = do
    partdRegs <- fmap partitionRegs . view rsRegs <$> get
    modify $ set rsRegs $ snd <$> partdRegs
    pure $ mkFrcsd . fst <$> partdRegs
  where
    mkFrcsd (ClientRegs goneP goneT goneD) =
      FrcSubDigest mempty goneP goneT goneD

unsubscribeFromNs
  :: Monad m => Set Namespace -> StateT (RelayState i) m (Map i FrcSubDigest)
unsubscribeFromNs nss = unsubscribe partitionRegs
  where
    partitionRegs :: ClientRegs -> (ClientRegs, ClientRegs)
    partitionRegs (ClientRegs p t d) =
      let
        part = Mos.partitionKey (`Set.member` nss)
        (goneP, keepP) = part p
        (goneT, keepT) = part t
        (goneD, keepD) = part d
      in
        (ClientRegs goneP goneT goneD, ClientRegs keepP keepT keepD)

unsubscribeFromDeleted
  :: Monad m => FrcUpdateDigest -> StateT (RelayState i) m (Map i FrcSubDigest)
unsubscribeFromDeleted frcud = unsubscribe partitionRegs
  where
    ns = frcudNamespace frcud
    undefs = Map.keysSet . Map.filter isUndef
    allUndefPTys = undefs $ frcudPostDefs frcud
    allUndefTys = undefs $ frcudDefinitions frcud
    allDeletePaths = Map.keysSet $ flattenNestedMaps (:/) $
      Map.filter isSoAbsent . fmap snd <$> frcudContOps frcud

    partitionRegs (ClientRegs p t d) =
      let
        f s mos = bimap (Mos.singletonSet ns) (\s' -> Mos.replaceSet ns s' mos)
          $ Set.partition (`Set.member` s) $ Mos.lookup ns mos
        (goneP, keepP) = f allUndefPTys p
        (goneT, keepT) = f allUndefTys t
        (goneD, keepD) = f allDeletePaths d
      in
        (ClientRegs goneP goneT goneD, ClientRegs keepP keepT keepD)

generateClientDigests
  :: Monad m
  => FrcUpdateDigest -> StateT (RelayState i) m (Map i FrcUpdateDigest)
generateClientDigests (FrcUpdateDigest ns ptds tds tas dat cops errs) =
    Map.filter (not . frcudNull) . fmap filterFrcud . view rsRegs <$> get
  where
    filterFrcud :: ClientRegs -> FrcUpdateDigest
    filterFrcud (ClientRegs pt t d) =
      let
        nsPs = Mos.lookup ns pt
        nsTs = Mos.lookup ns t
        nsDs = Mos.lookup ns d
      in
        FrcUpdateDigest ns
          (Map.restrictKeys ptds nsPs)
          (Map.restrictKeys tds nsTs)
          (Map.restrictKeys tas nsDs)
          (alFilterKey (`Set.member` nsDs) dat)
          (Map.restrictKeys cops nsDs)
          (Map.filterWithKey (\dei _ -> case dei of
            GlobalError -> True
            NamespaceError errNs ->
                 (pt `Mos.hasKey` errNs)
              || (t `Mos.hasKey` errNs)
              || (d `Mos.hasKey` errNs)
            PathError p -> p `Set.member` nsDs
            TimePointError p _ -> p `Set.member` nsDs)
            errs)


throwOutProvider
  :: (Ord i, Monad m) => i -> Set Namespace -> String
  -> StateT (RelayState i) (RelayProtocol i m) ()
throwOutProvider i nss msg = do
  -- FIXME: I'm quite sure that we should be taking namespaces...
  lift $ sendRev $ Right $ ServerData i $ Frped $ FrpErrorDigest $
    Map.fromSet (const [Text.pack msg]) $ Set.map NamespaceError nss
  lift $ sendRev $ Right $ ServerDisconnect i
  handleDisconnect i

-- | A helper function to make sure we broadcast sneaky sideways updates
--   whenever we change the owners map.
updateOwners
  :: Monad m
  => Map Namespace i -> StateT (RelayState i) (RelayProtocol i m) ()
updateOwners owners = do
  modify $ set rsOwners owners
  lift $ sendRev $ Left owners

-- FIXME: Worst case implementation
vsMinimiseDefinitions
  :: Map (Tagged def Seg) (DefOp def)
  -> Valuespace
  -> Map (Tagged def Seg) (DefOp def)
vsMinimiseDefinitions defs _ = defs

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
