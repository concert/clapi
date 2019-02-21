{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , OverloadedStrings
  , RankNTypes
  , TemplateHaskell
  , TypeFamilies
  , TypeFamilyDependencies
#-}

module Clapi.Relay2 where

import Control.Lens
  ( (&), _1, _2, _3, _4, _5, assign, at, makeLenses, modifying, over, set
  , use, view)
import qualified Control.Lens as Lens
import Control.Monad (forever, unless, void)
import Control.Monad.Except (MonadError(..), runExceptT)
import Control.Monad.State (MonadState(..), StateT, evalStateT, evalState)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter(..), WriterT, execWriterT)
import Data.Bifunctor (second)
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
import Data.Map.Mol (Mol, unMol)
import qualified Data.Map.Mol as Mol

import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Protocol (Protocol, liftedWaitThen, sendRev)
import Clapi.Tree (RoseTreeNode(..), TimeSeries)
import Clapi.Types.AssocList (AssocList)
import qualified Clapi.Types.AssocList as AL
import Clapi.Types.Definitions
  (PostDefinition, SomeDefinition, PostDefName, DefName, Editability)
import Clapi.Types.Digests
  ( ClientRegs(..)
  , crNull, crDeleteLookupNs, crDifference, crIntersection
  , crPostTypeRegs, crTypeRegs, crDataRegs
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
import Clapi.Types.Path (Namespace, Path)
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Types.Wire (SomeWireValue)
import Clapi.Valuespace2
  (Valuespace, ProviderError, lookupPostDef, lookupDef, pathNode, pathTyInfo)


newtype SemigroupMap k v = SemigroupMap {unSemigroupMap :: Map k v}

instance (Ord k, Semigroup v) => Semigroup (SemigroupMap k v) where
  SemigroupMap m1 <> SemigroupMap m2  = SemigroupMap $ Map.unionWith (<>) m1 m2

instance (Ord k, Semigroup v) => Monoid (SemigroupMap k v) where
  mempty = SemigroupMap mempty


data RelayState i
  = RelayState
  { _rsVsMap :: Map Namespace (i, Valuespace)
  , _rsRegs :: Map i ClientRegs
  }

makeLenses ''RelayState

-- FIXME: eventually we want to remove the strange transmission back up the
-- pipeline of namespace owner changes!
type RelayProtocol i m = Protocol
  (ClientEvent i SomeTrDigest) Void
  (Either (Map Namespace i) (ServerEvent i SomeFrDigest)) Void
  m

data MessageBuffer
  = MessageBuffer
  { _mbFrpds :: Map Namespace FrpDigest
  , _mbFrped :: FrpErrorDigest
  , _mbFrcrd :: FrcRootDigest
  , _mbFrcsd :: FrcSubDigest
  , _mbFrcuds :: Map Namespace FrcUpdateDigest
  }

makeLenses ''MessageBuffer

instance Semigroup MessageBuffer where
  MessageBuffer frpds1 frped1 frcrd1 frcsd1 frcuds1
    <> MessageBuffer frpds2 frped2 frcrd2 frcsd2 frcuds2 =
      MessageBuffer
        (frpds2 <> frpds1)
        (frped1 <> frped2)
        (frcrd1 <> frcrd2)
        (frcsd1 <> frcsd2)
        (frcuds2 <> frcuds1)

instance Monoid MessageBuffer where
  mempty = MessageBuffer mempty mempty mempty mempty mempty

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


handleConnect :: (Queries i m, Ord i) => i -> m ()
handleConnect i = do
  modifying rsRegs $ Map.insert i mempty

handleDisconnect :: (Queries i m, Sends i m, Ord i) => i -> m ()
handleDisconnect i = do
  modifying rsRegs $ Map.delete i
  nss <- Map.keysSet . Map.filter ((== i) . fst) <$> use rsVsMap
  mapM_ relinquish nss


handleTrpd :: i -> TrDigest 'Provider 'Update -> m ()
handleTrpd = undefined

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

handleTrcud :: i -> TrDigest 'Consumer 'Update -> m ()
handleTrcud = undefined

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
    multicast $ mkFrcsd . fst <$> partitionedRegs
  where
    mkFrcsd (ClientRegs p t d) = Frcsd mempty p t d

throwOutProvider
  :: (Ord i, Sends i m, Queries i m) => i -> Namespace -> Text -> m ()
throwOutProvider i ns reason = do
  collectFrd i $ Frped $ Mol.singleton (NamespaceError ns) reason
  disconnect i
  handleDisconnect i

subscribe
  :: (Ord a, Ord i, Subscribable a, Sends i m, Queries i m)
  => i -> Namespace -> a -> m (Maybe (SubData a))
subscribe i ns a =
  use (rsVsMap . at ns) >>= \case
    Nothing -> unsubscribe i ns a >> return Nothing
    Just (_ownerI, vs) -> case evalState (runExceptT $ vsGet a) vs of
      Left _ -> unsubscribe i ns a >> return Nothing
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

unsubscribe
  :: (Ord i, Ord a, Subscribable a, Queries i m, Sends i m)
  => i -> Namespace -> a -> m ()
unsubscribe i ns a =
  Error.modifying (rsRegs . at i . defaulting mempty . crLens) $ \mos ->
    if Mos.contains ns a mos
      then do
        collectFrd i $ mkFrcsd ns a
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
      (explainNode ns p node){ frcudTyAssigns = Map.singleton p tyInfo }
    where
      explainNode
        :: Namespace -> Path -> RoseTreeNode [SomeWireValue] -> FrcUpdateDigest
      explainNode ns p = \case
        RtnEmpty -> undefined  -- FIXME: want to get rid of RtnEmpty
        RtnChildren kids -> (frcudEmpty ns)
          { frcudContOps = Map.singleton p $ oppifySequence kids }
        RtnConstData att vals -> (frcudEmpty ns)
          { frcudData = AL.alSingleton p (ConstChange att vals) }
        RtnDataSeries ts -> (frcudEmpty ns)
          { frcudData = AL.alSingleton p $ oppifyTimeSeries ts}

      oppifyTimeSeries :: TimeSeries [SomeWireValue] -> DataChange
      oppifyTimeSeries ts = TimeChange $
        Dkmap.flatten (\t (att, (i, wvs)) -> (att, OpSet t wvs i)) ts

      oppifySequence :: Ord k => AssocList k v -> Map k (v, SequenceOp k)
      oppifySequence al =
        let (alKs, alVs) = unzip $ AL.unAssocList al in
          Map.fromList $ zipWith3
            (\k afterK v -> (k, (v, SoAfter afterK)))
            alKs (Nothing : (Just <$> alKs)) alVs


{- The idea with the MonadWriter here is that the logic handling bits of the
Relay code can emit messages as and when they please without worrying about how
to keep them condensed into the smallest number of digests for the clients.

When we are done processing the digest that came in, we can look at the list of
digests for each client and condense it down to the minimal set.
-}

data Buffer i
  = Buffer
  { _bMessages :: SemigroupMap i MessageBuffer
  , _bOwners :: Last (Map Namespace i)
  , _bDisconnected :: Set i
  }

bufferMsg :: Ord i => Map i MessageBuffer -> Buffer i
bufferMsg m = Buffer (SemigroupMap m) mempty mempty

instance Ord i => Semigroup (Buffer i) where
  Buffer m1 o1 d1 <> Buffer m2 o2 d2 = Buffer (m1 <> m2) (o1 <> o2) (d1 <> d2)

instance Ord i => Monoid (Buffer i) where
  mempty = Buffer mempty mempty mempty

collectFrd :: (Ord i, Sends i m) => i -> FrDigest r a -> m ()
collectFrd i = tell . bufferMsg . Map.singleton i . bufferDigest

multicast :: (Ord i, Sends i m) => Map i (FrDigest r a) -> m ()
multicast = tell . bufferMsg . fmap bufferDigest

broadcast :: (Ord i, Queries i m, Sends i m) => FrDigest r a -> m ()
broadcast d = use rsRegs >>= multicast . fmap (const d)

disconnect :: (Ord i, Sends i m) => i -> m ()
disconnect = tell . Buffer mempty mempty . Set.singleton

doSend :: Monad m => Buffer i -> RelayProtocol i m ()
doSend = mapM_ sendRev . toServerEvents
  -- (\i mb -> mapM_ (sendRev . second (ServerData i)) $ toServerEvents mb)

bufferDigest :: FrDigest r a -> MessageBuffer
bufferDigest d = mempty @MessageBuffer & case d of
    Frpd {} -> set mbFrpds $ Map.singleton (frpdNs d) d
    Frped {} -> set mbFrped d
    Frcrd {} -> set mbFrcrd d
    Frcsd {} -> set mbFrcsd d
    Frcud {} -> set mbFrcuds $ Map.singleton (frcudNs d) d

toServerEvents
  :: Buffer i -> [Either (Map Namespace i) (ServerEvent i SomeFrDigest)]
toServerEvents (Buffer (SemigroupMap msgBuffers) (Last owners) disconnected) =
    (Map.foldMapWithKey
      (\i mb -> Right . ServerData i <$> toDigests mb)
      msgBuffers)
    ++ (Right . ServerDisconnect <$> Set.toList disconnected)
    ++ case owners of
         -- No updates:
         Nothing -> []
         -- Changes:
         Just ownerMap -> [Left ownerMap]

toDigests :: MessageBuffer -> [SomeFrDigest]
toDigests (MessageBuffer frpds frped frcrd frcsd frcuds) =
  (SomeFrDigest <$> Map.elems frpds)
  ++ if frDigestNull frped then [] else [SomeFrDigest frped]
  ++ if frDigestNull frcrd then [] else [SomeFrDigest frcrd]
  ++ if frDigestNull frcsd then [] else [SomeFrDigest frcsd]
  ++ (SomeFrDigest <$> Map.elems frcuds)
