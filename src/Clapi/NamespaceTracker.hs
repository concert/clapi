module Clapi.NamespaceTracker where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad (forever, when, unless, void)
import Control.Monad.State (StateT(..), evalStateT, get, put, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Strict.Merge (merge, zipWithMatched, mapMissing)
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tagged (Tagged, untag)
import qualified Data.Text as Text


import Clapi.Types.AssocList
  (AssocList, alEmpty,  alFilterKey, alInsert, alFoldlWithKey, alKeysSet)
import Clapi.Types.Messages (DataErrorIndex(..))
import Clapi.Types.Digests
  ( TrDigest(..), TrcSubDigest(..), trcsdNamespaces, TrcUpdateDigest(..)
  , TrpDigest(..), TrprDigest(..)
  , FrDigest(..), FrcSubDigest(..), FrcUpdateDigest(..)
  , FrpDigest(..), FrpErrorDigest(..)
  , DefOp(..), isUndef, isSub, frcudNull, frcsdNull, frcsdEmpty
  , OutboundDigest(..)
  , OutboundClientInitialisationDigest, OutboundClientUpdateDigest)
-- Also `Either String a` MonadFail instance:
import Clapi.Types (Definition, PostDefinition)
import Clapi.Types.Path
  ( Path, TypeName(..), pattern (:/), pattern (:</), pattern Root
  , Namespace(..), Seg, unqualify, qualify)
import qualified Clapi.Types.Path as Path
import Clapi.Types.SequenceOps (isSoAbsent)
import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Protocol (Protocol, Directed(..), wait, sendFwd, sendRev)
import Clapi.Util (flattenNestedMaps)

data Ownership = Owner | Client deriving (Eq, Show)

newtype Originator i = Originator i deriving (Eq, Show)

data ClientGetDigest = ClientGetDigest
  { cdgPostTypeGets :: Set (Tagged PostDefinition TypeName)
  , cdgTypeGets :: Set (Tagged Definition TypeName)
  , cgdDataGets :: Set Path
  } deriving (Show, Eq)

data PostNstInboundDigest
  = PnidCgd ClientGetDigest
  | PnidTrcud TrcUpdateDigest
  | PnidTrpd TrpDigest
  | PnidTrprd TrprDigest
  deriving (Show, Eq)

type NstProtocol m i = Protocol
    (ClientEvent i TrDigest)
    ((Originator i, PostNstInboundDigest))
    (Either (Map Namespace i) (ServerEvent i FrDigest))
    ((Originator i, OutboundDigest))
    m

data ClientRegs
  = ClientRegs
  { crPostTypeRegs :: Set (Tagged PostDefinition TypeName)
  , crTypeRegs :: Set (Tagged Definition TypeName)
  , crDataRegs :: Set Path
  } deriving (Show)

instance Monoid ClientRegs where
  mempty = ClientRegs mempty mempty mempty
  (ClientRegs pt1 t1 d1) `mappend` (ClientRegs pt2 t2 d2) =
    ClientRegs (pt1 <> pt2) (t1 <> t2) (d1 <> d2)

data NstState i
  = NstState
  { nstOwners :: Map Namespace i
  , nstRegs :: Map i ClientRegs
  } deriving Show


nstProtocol :: (Monad m, Ord i) => NstProtocol m i ()
nstProtocol = evalStateT nstProtocol_ $ NstState mempty mempty

nstProtocol_ :: (Monad m, Ord i) => StateT (NstState i) (NstProtocol m i) ()
nstProtocol_ = forever $ liftedWaitThen fwd rev
  where
    sendFwd' i d = lift $ sendFwd (Originator i, d)
    fwd (ClientConnect _ _) = return ()
    fwd (ClientData i trd) =
      case trd of
        Trpd d -> claimNamespace i d
          (throwOutProvider i $ Set.singleton $ trpdNamespace d)
          (sendFwd' i $ PnidTrpd d)
        Trprd d -> relinquishNamespace i (trprdNamespace d)
          (throwOutProvider i $ Set.singleton $ trprdNamespace d)
          (sendFwd' i $ PnidTrprd d)
        Trcsd d -> guardNsClientSubDigest i d
          (throwOutProvider i)
          (do
              (cgd, frcsd) <- handleSubDigest i d
              unless (frcsdNull frcsd) $
                lift $ sendRev $ Right $ ServerData i $ Frcsd frcsd
              sendFwd' i $ PnidCgd cgd
          )
        Trcud d -> guardNsClientUpdateDigest i d
          (throwOutProvider i $ Set.singleton $ trcudNamespace d)
          (sendFwd' i $ PnidTrcud d)
    fwd (ClientDisconnect i) = handleDisconnect i
    rev (Originator i, od) =
      let send i' = lift . sendRev . Right . ServerData i' in case od of
        Ocid d -> registerSubs i d >> send i (Frcud d)
        Ocsed errMap -> send i $ Frcsd $ frcsdEmpty { frcsdErrors = errMap}
        -- FIXME: eventually we may collapse the client/time tracking roles of
        -- the RelayAPI, perhaps to here, and then this, which is really a
        -- broadcast, could happen here. Right now, we don't actually have info
        -- about all the connected clients:
        Ocrd d -> send i $ Frcrd d
        Ocud d -> do
          generateClientDigests d >>= lift . broadcastRev . fmap Frcud
          unsubDeleted d >>= lift . broadcastRev . fmap Frcsd
        Opd d -> do
          i' <- maybe (error "no owner for namespace") id .
            Map.lookup (frpdNamespace d) . nstOwners <$> get
          send i' $ Frpd d
        Ope d -> do
          send i $ Frped d
          lift $ sendRev $ Right $ ServerDisconnect i
          handleDisconnect i

nonClaim :: TrpDigest -> Bool
nonClaim trpd = trpdData trpd == alEmpty && null (trpdContOps trpd)

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
    Map.fromSet (const [Text.pack msg]) $
    Set.map (PathError . (Root :/) . unNamespace) nss
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
claimNamespace i d failureAction successAction = either
    failureAction
    (\owners' ->
      maybe (return ()) updateOwners owners' >>
      successAction)
    . go . nstOwners =<< get
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
relinquishNamespace i ns failureAction successAction = either
    failureAction
    (\owners' -> updateOwners owners' >> successAction)
    . go . nstOwners =<< get
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

guardNsClientSubDigest
  :: (Eq i, Monad m)
  => i -> TrcSubDigest
  -> (Set Namespace -> String -> StateT (NstState i) (NstProtocol m i) ())
  -> StateT (NstState i) (NstProtocol m i) ()
  -> StateT (NstState i) (NstProtocol m i) ()
guardNsClientSubDigest i d failureAction successAction = either
    (uncurry failureAction)
    (const successAction)
    . go . nstOwners =<< get
  where
    go owners =
      let
        ownedAndSubd = Map.keysSet $ Map.filter (== i) $ Map.restrictKeys owners
          $ trcsdNamespaces d
      in
        unless (null ownedAndSubd) $
          Left (ownedAndSubd, "Acted as client on own namespace")

guardNsClientUpdateDigest
  :: (Eq i, Monad m)
  => i -> TrcUpdateDigest
  -> (String -> StateT (NstState i) (NstProtocol m i) ())
  -> StateT (NstState i) (NstProtocol m i) ()
  -> StateT (NstState i) (NstProtocol m i) ()
guardNsClientUpdateDigest i d failureAction successAction = either
    failureAction
    (const successAction)
    . go . nstOwners =<< get
  where
    go owners = maybe
      (return ())
      (\i' -> when (i' == i) $ fail "Acted as client on own namespace")
      $ Map.lookup (trcudNamespace d) owners

handleSubDigest
  :: (Monad m, Ord i)
  => i -> TrcSubDigest -> StateT (NstState i) m (ClientGetDigest, FrcSubDigest)
handleSubDigest i trcsd =
  let
    (ptSubs, ptUnsubs) = partSubs $ trcsdPostTypeSubs trcsd
    (tSubs, tUnsubs) = partSubs $ trcsdTypeSubs trcsd
    (dSubs, dUnsubs) = partSubs $ trcsdDataSubs trcsd
  in do
    nsts <- get
    let current = Map.findWithDefault mempty i $ nstRegs nsts
    let cdg = ClientGetDigest
          (Set.difference ptSubs $ crPostTypeRegs current)
          (Set.difference tSubs $ crTypeRegs current)
          (Set.difference dSubs $ crDataRegs current)
    let frcsd = FrcSubDigest mempty
          (Set.intersection ptUnsubs $ crPostTypeRegs current)
          (Set.intersection tUnsubs $ crTypeRegs current)
          (Set.intersection dUnsubs $ crDataRegs current)
    return (cdg, frcsd)
  where
    mapPair f (a1, a2) = (f a1, f a2)
    partSubs = mapPair Map.keysSet . Map.partition isSub

handleDisconnect
  :: (Ord i, Monad m) => i -> StateT (NstState i) (NstProtocol m i) ()
handleDisconnect i = do
    nsts <- get
    let (removed, remaining) = Map.partition (== i) $ nstOwners nsts
    unless (null removed) $ updateOwners remaining
    mapM_ send $ Map.keys removed
  where
    send ns = lift $ sendFwd (Originator i, PnidTrprd $ TrprDigest ns)

registerSubs
  :: (Ord i, Monad m)
  => i -> OutboundClientInitialisationDigest -> StateT (NstState i) m ()
registerSubs i (FrcUpdateDigest ns postDefs defs _tas dd cops _errs) =
    modify go
  where
    newClientRegs = ClientRegs
      (Set.map (qualify ns) $ Map.keysSet postDefs)
      (Set.map (qualify ns) $ Map.keysSet defs)
      (Set.map (unNamespace ns :</) $
       Set.union (Map.keysSet cops) (alKeysSet dd))
    go nsts = nsts {
      nstRegs = Map.insertWith (<>) i newClientRegs $ nstRegs nsts}

unsubDeleted
  :: (Monad m, Ord i)
  => OutboundClientUpdateDigest -> StateT (NstState i) m (Map i FrcSubDigest)
unsubDeleted d = do
    nsts <- get
    let results = fmap updateClientReg $ nstRegs nsts
    put nsts{ nstRegs = fmap fst results }
    return $ fmap snd results
  where
    ns = frcudNamespace d
    allUndefPTys = qualifiedUndefs $ frcudPostDefs d
    allUndefTys = qualifiedUndefs $ frcudDefinitions d
    qualifiedUndefs :: Map (Tagged a Seg) (DefOp a) -> Set (Tagged a TypeName)
    qualifiedUndefs = Set.map (qualify ns) . Map.keysSet . Map.filter isUndef
    allDeletePaths = Set.map (unNamespace ns :</) $ Map.keysSet $
      flattenNestedMaps (:/) $
      Map.filter isSoAbsent . fmap snd <$> frcudContOps d
    updateClientReg :: ClientRegs -> (ClientRegs, FrcSubDigest)
    updateClientReg (ClientRegs ptSubs tSubs dSubs) =
      let
        (ptUnsubs, ptSubs') = Set.partition (`Set.member` allUndefPTys) ptSubs
        (tUnsubs, tSubs') = Set.partition (`Set.member` allUndefTys) tSubs
        (dUnsubs, dSubs') = Set.partition (`Set.member` allDeletePaths) dSubs
      in
        ( ClientRegs ptSubs' tSubs' dSubs'
        , FrcSubDigest mempty ptUnsubs tUnsubs dUnsubs
        )

broadcastRev :: Monad m => Map i FrDigest -> NstProtocol m i ()
broadcastRev = void . sequence . Map.mapWithKey sendRevWithI
  where
    sendRevWithI i digest = sendRev $ Right $ ServerData i digest

generateClientDigests
  :: Monad m
  => OutboundClientUpdateDigest -> StateT (NstState i) m (Map i FrcUpdateDigest)
generateClientDigests (FrcUpdateDigest ns ptds tds tas d cops errs) =
    Map.filter (not . frcudNull) . fmap filterFrcud . nstRegs <$> get
  where
    ttnOfInterest :: Tagged a TypeName -> Bool
    ttnOfInterest = (== ns) . tnNamespace . untag
    unqualTySubs :: Set (Tagged a TypeName) -> Set (Tagged a Seg)
    unqualTySubs = foldl' (\acc ttn ->
      if ttnOfInterest ttn
        then Set.insert (snd $ unqualify ttn) acc
        else acc
      ) mempty
    filterFrcud :: ClientRegs -> FrcUpdateDigest
    filterFrcud (ClientRegs ptSubs tSubs dSubs) =
      let
        unqualdDatSubs = Set.fromAscList $ mapMaybe (fmap snd . Path.splitHead)
          $ filter (`Path.isChildOf` (Root :/ unNamespace ns))
          $ Set.toAscList dSubs
      in
      FrcUpdateDigest ns
      (Map.restrictKeys ptds $ unqualTySubs ptSubs)
      (Map.restrictKeys tds $ unqualTySubs tSubs)
      (Map.restrictKeys tas unqualdDatSubs)
      (alFilterKey (`Set.member` unqualdDatSubs) d)
      (Map.restrictKeys cops unqualdDatSubs)
      (Map.filterWithKey  (\dem _ -> case dem of
        GlobalError -> True
        PathError p -> p `Set.member` unqualdDatSubs
        TimePointError p _ -> p `Set.member` unqualdDatSubs)
        errs)

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
