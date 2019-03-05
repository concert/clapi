{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , OverloadedStrings
  , RankNTypes
  , TemplateHaskell
  , TypeFamilies
  , TypeFamilyDependencies
#-}

module Clapi.Relay where

import Control.Lens ((&), assign, at, makeLenses, modifying, set, use, view)
import qualified Control.Lens as Lens
import Control.Monad (forever, void)
import Control.Monad.Except (MonadError(..), runExceptT)
import Control.Monad.State
  ( MonadState(..), StateT, evalStateT, evalState, runStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter(..), execWriterT)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Last(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void, absurd)

import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos
import Data.Map.Mol (Mol)
import qualified Data.Map.Mol as Mol

import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Protocol (Protocol, liftedWaitThen, sendRev)
import Clapi.Tree (RoseTreeNode(..), TimeSeries)
import Clapi.Types.AssocList (AssocList(..))
import qualified Clapi.Types.AssocList as AL
import Clapi.Types.Definitions
  (PostDefinition, SomeDefinition, Editability(..))
import Clapi.Types.Digests
  ( ClientRegs(..)
  , crDeleteLookupNs, crPostTypeRegs, crTypeRegs, crDataRegs
  , DataErrorIndex(..), DataChange(..), TimeSeriesDataOp(..), DefOp(..)
  , OriginatorRole(..), DigestAction(..)
  , TrDigest(..), SomeTrDigest(..)
  , trcsdNamespaces, trcsdClientRegs
  , FrDigest(..), SomeFrDigest(..)
  , FrpDigest, FrpErrorDigest, FrcRootDigest, FrcSubDigest, FrcUpdateDigest
  , frDigestNull, frcudEmpty
  )
import qualified Clapi.Types.Dkmap as Dkmap
import qualified Clapi.Types.Error as Error
import Clapi.Types.Name (DefName, Namespace, PostDefName, castName)
import Clapi.Types.Path (Path)
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Types.Wire (SomeWireValue)
import Clapi.Valuespace
  ( Valuespace, ProviderError, baseValuespace
  , lookupPostDef, lookupDef, pathNode, pathTyInfo
  , processTrpd, processTrcud)


newtype SemigroupMap k v = SemigroupMap {unSemigroupMap :: Map k v}

instance (Ord k, Semigroup v) => Semigroup (SemigroupMap k v) where
  SemigroupMap m1 <> SemigroupMap m2  = SemigroupMap $ Map.unionWith (<>) m1 m2

instance (Ord k, Semigroup v) => Monoid (SemigroupMap k v) where
  mempty = SemigroupMap mempty

type instance Lens.Index (SemigroupMap k a) = k
type instance Lens.IxValue (SemigroupMap k a) = a

instance Ord k => Lens.Ixed (SemigroupMap k a)

instance Ord k => Lens.At (SemigroupMap k a) where
  at k f m = fmap SemigroupMap $ Map.alterF f k $ unSemigroupMap m


data RelayState i
  = RelayState
  { _rsVsMap :: Map Namespace (i, Valuespace)
  , _rsRegs :: Map i ClientRegs
  }

makeLenses ''RelayState

-- FIXME: RelayState semigroup and monoid instances should be for testing only,
-- so move there?
instance Ord i => Semigroup (RelayState i) where
  RelayState vss1 regs1 <> RelayState vss2 regs2
    = RelayState (vss1 <> vss2) (regs1 <> regs2)

instance Ord i => Monoid (RelayState i) where
  mempty = RelayState mempty mempty

-- FIXME: eventually we want to remove the strange transmission back up the
-- pipeline of namespace owner changes!
type RelayProtocol i m = Protocol
  (ClientEvent i SomeTrDigest) Void
  (Either (Map Namespace i) (ServerEvent i SomeFrDigest)) Void
  m

data MessageBuffer
  = MessageBuffer
  { _mbFrpds :: SemigroupMap Namespace FrpDigest
  , _mbFrped :: FrpErrorDigest
  , _mbFrcsd :: FrcSubDigest
  , _mbFrcuds :: SemigroupMap Namespace FrcUpdateDigest
  }

makeLenses ''MessageBuffer

instance Semigroup MessageBuffer where
  MessageBuffer frpds1 frped1 frcsd1 frcuds1
    <> MessageBuffer frpds2 frped2 frcsd2 frcuds2 =
      MessageBuffer
        (frpds2 <> frpds1)
        (frped1 <> frped2)
        (frcsd1 <> frcsd2)
        (frcuds2 <> frcuds1)

instance Monoid MessageBuffer where
  mempty = MessageBuffer mempty mempty mempty mempty

-- FIXME: MessageBuffer was supposed to be the only message caching state thing,
-- but we ended up needing this too and it got a bit messy:
data Buffer i
  = Buffer
  { _bMsgs :: SemigroupMap i MessageBuffer
  , _bFrcrds :: Map i FrcRootDigest
  , _bOwners :: Last (Map Namespace i)
  , _bDisconnected :: Set i
  }

makeLenses ''Buffer

type Sends i m = MonadWriter (Buffer i) m
type Queries i = MonadState (RelayState i)

relay
  :: (Ord i, Monad m)
  => RelayState i -> RelayProtocol i m ()
relay = evalStateT (forever $ liftedWaitThen dispatch absurd)
  where
    dispatch
      :: (Ord i, Monad m)
      => ClientEvent i SomeTrDigest
      -> StateT (RelayState i) (RelayProtocol i m) ()
    dispatch event = do
      msgs <- execWriterT $ case event of
        ClientConnect _ i -> handleConnect i
        ClientData i (SomeTrDigest d) -> case d of
          Trpd {} -> handleTrpd i d
          Trprd {} -> handleTrprd i d
          Trcsd {} -> handleTrcsd i d
          Trcud {} -> handleTrcud i d
        ClientDisconnect i -> handleDisconnect i
      lift $ doSend msgs


handleConnect :: (Queries i m, Sends i m, Ord i) => i -> m ()
handleConnect i = do
  modifying rsRegs $ Map.insert i mempty
  vsMap <- use rsVsMap
  collectFrd i $ Frcrd $ const (SoAfter Nothing) <$> vsMap


handleDisconnect :: (Queries i m, Sends i m, Ord i) => i -> m ()
handleDisconnect i = do
  modifying rsRegs $ Map.delete i
  nss <- Map.keysSet . Map.filter ((== i) . fst) <$> use rsVsMap
  mapM_ relinquish nss


handleTrpd
  :: (Ord i, Sends i m, Queries i m) => i -> TrDigest 'Provider 'Update -> m ()
handleTrpd i trpd = Error.modifying (rsVsMap . at ns) $ \case
    Nothing -> go bvs $
      broadcast $ Frcrd $ Map.singleton ns $ SoAfter Nothing
    Just x@(ownerI, vs) ->
      if i == ownerI
        then go vs $ return ()
        else stop >> return (Just x)
  where
    ns = trpdNs trpd
    bvs = baseValuespace (castName ns) Editable
    go vs onSuccess = do
      (res, vs') <- runStateT (processTrpd trpd) vs
      case res of
        Left errs -> throwOutProvider' i errs
        Right frcud -> do
          regMap <- use rsRegs
          multicast $ filterFrcud frcud <$> regMap
          onSuccess
      return $ Just (i, vs')

    stop = throwOutProvider i ns "Already owned by another provider"

    filterFrcud frcud regs =
      let
        dns = Mos.lookup ns $ view crTypeRegs regs
        pdns = Mos.lookup ns $ view crPostTypeRegs regs
        paths = Mos.lookup ns $ view crDataRegs regs
        keepErrIdx = \case
          GlobalError -> True
          NamespaceError errNs -> ns == errNs
          PathError p -> p `Set.member` paths
          TimePointError p _ -> p `Set.member` paths

      in
        frcud
          { frcudPostDefs = Map.restrictKeys (frcudPostDefs frcud) pdns
          , frcudDefs = Map.restrictKeys (frcudDefs frcud) dns
          , frcudTyAssigns = Map.restrictKeys (frcudTyAssigns frcud) paths
          , frcudData = AL.restrictKeys (frcudData frcud) paths
          , frcudContOps = Map.restrictKeys (frcudContOps frcud) paths
          , frcudErrors = Mol.filterWithKey (\dei _ -> keepErrIdx dei)
              $ frcudErrors frcud
          }

handleTrprd
  :: (Queries i m, Sends i m, Ord i)
  => i -> TrDigest 'Provider 'Relinquish -> m ()
handleTrprd i d = use (rsVsMap . at ns) >>= doRelinquish . fmap fst
  where
    ns = trprdNs d
    doRelinquish (Just ownerI) | i == ownerI = relinquish ns
    doRelinquish _ = do
      collectFrd i $ Frped $
        Mol.singleton (NamespaceError ns) "You're not the owner"
      handleDisconnect i

defaulting :: Functor f => a -> (a -> f a) -> Maybe a -> f (Maybe a)
defaulting def f = fmap Just . f . fromMaybe def

handleTrcsd
  :: (Queries i m, Sends i m, Ord i)
  => i -> TrDigest 'Consumer 'Subscribe -> m ()
handleTrcsd i d = do
    ownedAndSubd <- Map.keysSet
        . Map.filter ((== i) . fst)
        . flip Map.restrictKeys (trcsdNamespaces d)
      <$> use rsVsMap
    if null ownedAndSubd
      then do
        manageSub subscribeWithAuto crDataRegs requestedSubs
        manageSub (subscribe i) crTypeRegs requestedSubs
        manageSub (subscribe i) crPostTypeRegs requestedSubs

        manageSub (unsubscribe i) crDataRegs requestedUnsubs
        manageSub (unsubscribe i) crTypeRegs requestedUnsubs
        manageSub (unsubscribe i) crPostTypeRegs requestedUnsubs
      else mapM_
        (\ns -> throwOutProvider i ns "Acted as client on own namespace")
        ownedAndSubd
  where
    (requestedSubs, requestedUnsubs) = trcsdClientRegs d
    manageSub f lens subs = mapM_ (uncurry f) $ Mos.toList $ view lens subs
    subscribeWithAuto ns p = do
      subscribe i ns p >>= \case
        Nothing -> return ()
        Just (_node, (dn, _ed)) -> void $ subscribe i ns dn

handleTrcud
  :: (Ord i, Sends i m, Queries i m) => i -> TrDigest 'Consumer 'Update -> m ()
handleTrcud i trcud = use (rsVsMap . at ns) >>= \case
    Nothing -> sendBackErrors $
        Mol.singleton (NamespaceError ns) "Namespace does not exist"
    Just (ownerI, vs) ->
      if i == ownerI
        then throwOutProvider i ns "Acted as client on own namespace"
        else do
          (errs, frpd) <- processTrcud trcud vs
          sendBackErrors errs
          collectFrd ownerI frpd
  where
    ns = trcudNs trcud
    sendBackErrors errs = collectFrd i $ (frcudEmpty ns) { frcudErrors = errs }

updateLookup
  :: (Ord k, MonadState s m)
  => Lens.Lens' s (Map k a) -> (Maybe a -> Maybe a) -> k -> m (Maybe a)
updateLookup lens f k = do
  m <- use lens
  -- NB. could replace with `at k (\a -> (a, f a)) m` and make polymorphic, but
  -- it makes the type harder to grok:
  let (a, m') = Map.alterF (\a -> (a, f a)) k m
  assign lens m'
  return a

relinquish :: (Ord i, Queries i m, Sends i m) => Namespace -> m ()
relinquish ns = do
  ma <- updateLookup rsVsMap (const Nothing) ns
  case ma of
    Nothing -> return ()
    Just (i, vs) -> do
      -- FIXME: This doesn't check that the namespace is owned by the
      -- reqlinquisher
      broadcast $ Frcrd $ Map.singleton ns SoAbsent
  unsubscribeNs ns

unsubscribeNs :: (Ord i, Queries i m, Sends i m) => Namespace -> m ()
unsubscribeNs ns = do
    partitionedRegs <- fmap (crDeleteLookupNs ns) <$> use rsRegs
    assign rsRegs $ snd <$> partitionedRegs
    multicast $ toFrcsd . fst <$> partitionedRegs
  where
    toFrcsd (ClientRegs p t d) = Frcsd mempty p t d

throwOutProvider'
  :: (Ord i, Sends i m, Queries i m) => i -> Mol DataErrorIndex Text -> m ()
throwOutProvider' i mol = do
  collectFrd i $ Frped mol
  disconnect i
  handleDisconnect i

throwOutProvider
  :: (Ord i, Sends i m, Queries i m) => i -> Namespace -> Text -> m ()
throwOutProvider i ns reason =
  throwOutProvider' i $ Mol.singleton (NamespaceError ns) reason

subscribe
  :: (Ord a, Ord i, Subscribable a, Sends i m, Queries i m)
  => i -> Namespace -> a -> m (Maybe (SubData a))
subscribe i ns a =
  use (rsVsMap . at ns) >>= \case
    Nothing -> sendUnsubscribe i ns a >> return Nothing
    Just (_ownerI, vs) -> case evalState (runExceptT $ vsGet a) vs of
      Left _ -> sendUnsubscribe i ns a >> return Nothing
      Right sd -> do
        Error.modifying (rsRegs . at i . defaulting mempty . crLens)
          $ \mos -> if Mos.contains ns a mos
              then return mos
              else do
                collectFrd i $ mkFrcud ns a sd
                return $ Mos.insert ns a mos
        -- FIXME: we might want only to return you the data if this is a new
        -- subscription:
        return $ Just sd

sendUnsubscribe
  :: (Ord i, Subscribable a, Sends i m) => i -> Namespace -> a -> m ()
sendUnsubscribe i ns a = collectFrd i $ mkFrcsd ns a

unsubscribe
  :: (Ord i, Ord a, Subscribable a, Queries i m, Sends i m)
  => i -> Namespace -> a -> m ()
unsubscribe i ns a =
  Error.modifying (rsRegs . at i . defaulting mempty . crLens) $ \mos ->
    if Mos.contains ns a mos
      then do
        sendUnsubscribe i ns a
        return $ Mos.delete ns a mos
      else return mos

class Subscribable a where
  type SubData a = r | r -> a
  crLens :: Lens.Lens' ClientRegs (Mos Namespace a)
  mkFrcsd :: Namespace -> a -> FrcSubDigest
  vsGet
    :: (MonadState Valuespace m, MonadError ProviderError m)
    => a -> m (SubData a)
  mkFrcud :: Namespace -> a -> SubData a -> FrcUpdateDigest

instance Subscribable PostDefName where
  type SubData PostDefName = PostDefinition
  crLens = crPostTypeRegs
  mkFrcsd ns dn = Frcsd mempty (Mos.singleton ns dn) mempty mempty
  vsGet = lookupPostDef
  mkFrcud ns dn pd = (frcudEmpty ns)
    { frcudPostDefs = Map.singleton dn $ OpDefine pd }

instance Subscribable DefName where
  type SubData DefName = SomeDefinition
  crLens = crTypeRegs
  mkFrcsd ns dn = Frcsd mempty mempty (Mos.singleton ns dn) mempty
  vsGet = lookupDef
  mkFrcud ns dn d = (frcudEmpty ns)
    { frcudDefs = Map.singleton dn $ OpDefine d }

instance Subscribable Path where
  type SubData Path = (RoseTreeNode [SomeWireValue], (DefName, Editability))
  crLens = crDataRegs
  mkFrcsd ns p = Frcsd mempty mempty mempty $ Mos.singleton ns p
  vsGet p = (,) <$> pathNode p <*> pathTyInfo p
  mkFrcud ns p (node, tyInfo) =
      (explainNode node){ frcudTyAssigns = Map.singleton p tyInfo }
    where
      explainNode :: RoseTreeNode [SomeWireValue] -> FrcUpdateDigest
      explainNode = \case
        RtnEmpty -> undefined  -- FIXME: want to get rid of RtnEmpty
        RtnChildren kids -> (frcudEmpty ns)
          { frcudContOps = Map.singleton p $ oppifySequence kids }
        RtnConstData att vals -> (frcudEmpty ns)
          { frcudData = AL.singleton p (ConstChange att vals) }
        RtnDataSeries ts -> (frcudEmpty ns)
          { frcudData = AL.singleton p $ oppifyTimeSeries ts}

      oppifySequence :: Ord k => AssocList k v -> Map k (v, SequenceOp k)
      oppifySequence al =
        let (alKs, alVs) = unzip $ unAssocList al in
          Map.fromList $ zipWith3
            (\k afterK v -> (k, (v, SoAfter afterK)))
            alKs (Nothing : (Just <$> alKs)) alVs

-- Used by Cape to publish initial time series:
oppifyTimeSeries :: TimeSeries [SomeWireValue] -> DataChange
oppifyTimeSeries ts = TimeChange $
    Dkmap.flatten (\t (att, (i, wvs)) -> (att, OpSet t wvs i)) ts


{- The idea with the MonadWriter here is that the logic handling bits of the
Relay code can emit messages as and when they please without worrying about how
to keep them condensed into the smallest number of digests for the clients.

When we are done processing the digest that came in, we can look at the list of
digests for each client and condense it down to the minimal set.
-}

instance Ord i => Semigroup (Buffer i) where
  Buffer m1 frcrds1 o1 d1 <> Buffer m2 frcrds2 o2 d2 =
    Buffer (m1 <> m2) (frcrds1 <> frcrds2) (o1 <> o2) (d1 <> d2)

instance Ord i => Monoid (Buffer i) where
  mempty = Buffer mempty mempty mempty mempty

collectFrd :: (Ord i, Sends i m) => i -> FrDigest r a -> m ()
collectFrd i = tell . bufferDigest i

multicast :: (Ord i, Sends i m) => Map i (FrDigest r a) -> m ()
multicast = mapM_ (uncurry collectFrd) . Map.toList

broadcast :: (Ord i, Queries i m, Sends i m) => FrDigest r a -> m ()
broadcast d = use rsRegs >>= multicast . fmap (const d)

disconnect :: (Ord i, Sends i m) => i -> m ()
disconnect = tell . Buffer mempty mempty mempty . Set.singleton

doSend :: Monad m => Buffer i -> RelayProtocol i m ()
doSend = mapM_ sendRev . toServerEvents

bufferDigest :: forall i r a. Ord i => i -> FrDigest r a -> Buffer i
bufferDigest i d = mempty @(Buffer i) & case d of
    Frpd {} -> set (bMsgs . at i . defaulting mempty . mbFrpds)
      $ SemigroupMap $ Map.singleton (frpdNs d) d
    Frped {} -> set (bMsgs . at i . defaulting mempty . mbFrped) d
    Frcrd {} -> set (bFrcrds . at i) $ Just d
    Frcsd {} -> set (bMsgs . at i . defaulting mempty . mbFrcsd) d
    Frcud {} -> set (bMsgs . at i . defaulting mempty . mbFrcuds)
      $ SemigroupMap $ Map.singleton (frcudNs d) d

toServerEvents
  :: Buffer i -> [Either (Map Namespace i) (ServerEvent i SomeFrDigest)]
toServerEvents
  (Buffer (SemigroupMap msgBuffers) frcrds (Last owners) disconnected) =
    (Map.foldMapWithKey
      (\i mb -> Right . ServerData i <$> toDigests mb)
      msgBuffers)
    ++ (
      Right . (\(i, frcrd) -> ServerData i $ SomeFrDigest frcrd)
      <$> Map.toList frcrds)
    ++ (Right . ServerDisconnect <$> Set.toList disconnected)
    ++ case owners of
         -- No updates:
         Nothing -> []
         -- Changes:
         Just ownerMap -> [Left ownerMap]

toDigests :: MessageBuffer -> [SomeFrDigest]
toDigests (MessageBuffer frpds frped frcsd frcuds) =
  (SomeFrDigest
     <$> filter (not . frDigestNull) (Map.elems $ unSemigroupMap frpds))
  ++ (if frDigestNull frped then [] else [SomeFrDigest frped])
  ++ (if frDigestNull frcsd then [] else [SomeFrDigest frcsd])
  ++
  (SomeFrDigest
     <$> filter (not . frDigestNull) (Map.elems $ unSemigroupMap frcuds))
