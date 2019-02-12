{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , FlexibleContexts
  , GADTs
  , LambdaCase
  , OverloadedStrings
  , RankNTypes
  , TemplateHaskell
#-}

module Clapi.Relay2 where

import Control.Lens
  ((&), _1, _2, _3, _4, _5, assign, at, makeLenses, modifying, set, over, use)
import qualified Control.Lens as Lens
import Control.Monad (forever, unless, void)
import Control.Monad.State (MonadState(..), StateT, evalStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter(..), WriterT, execWriterT)
import Data.Bifunctor (second)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Last(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void, absurd)

import Data.Map.Mos (Mos)
import qualified Data.Map.Mos as Mos
import Data.Map.Mol (Mol, unMol)
import qualified Data.Map.Mol as Mol

import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Protocol (Protocol, liftedWaitThen, sendRev)
import Clapi.Types.Digests
  ( ClientRegs(..), crNull, crDeleteLookupNs
  , DataErrorIndex(..)
  , OriginatorRole(..), DigestAction(..)
  , TrDigest(..), SomeTrDigest(..)
  , trcsdNamespaces
  , FrDigest(..), SomeFrDigest(..)
  , FrpDigest, FrpErrorDigest, FrcRootDigest, FrcSubDigest, FrcUpdateDigest
  , frDigestNull
  )
import Clapi.Types.Path (Namespace)
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Valuespace2 (Valuespace)


newtype SemigroupMap k v = SemigroupMap {unSemigroupMap :: Map k v}

instance (Ord k, Semigroup v) => Semigroup (SemigroupMap k v) where
  SemigroupMap m1 <> SemigroupMap m2  = SemigroupMap $ Map.unionWith (<>) m1 m2

instance (Ord k, Semigroup v) => Monoid (SemigroupMap k v) where
  mempty = SemigroupMap mempty


data RelayState i
  = RelayState
  { _rsVsMap :: Map Namespace (i, Valuespace)
  , _rsRegs :: Map i ClientRegs
  , _rsClients :: Set i
  }

makeLenses ''RelayState

-- FIXME: eventually we want to remove the strange transmission back up the
-- pipeline of namespace owner changes!
type RelayProtocol i m = Protocol
  (ClientEvent i SomeTrDigest) Void
  (Either (Map Namespace i) (ServerEvent i SomeFrDigest)) Void
  m

data MessageBuffer i
  = MessageBuffer
  { _mbFrpds :: Map Namespace FrpDigest
  , _mbFrped :: FrpErrorDigest
  , _mbFrcrd :: FrcRootDigest
  , _mbFrcsd :: FrcSubDigest
  , _mbFrcuds :: Map Namespace FrcUpdateDigest
  , _mbOwners :: Last (Map Namespace i)
  }

makeLenses ''MessageBuffer

instance Semigroup (MessageBuffer i) where
  MessageBuffer frpds1 frped1 frcrd1 frcsd1 frcuds1 o1
    <> MessageBuffer frpds2 frped2 frcrd2 frcsd2 frcuds2 o2 =
      MessageBuffer
        (frpds2 <> frpds1)
        (frped1 <> frped2)
        (frcrd1 <> frcrd2)
        (frcsd1 <> frcsd2)
        (frcuds2 <> frcuds1)
        (o1 <> o2)

instance Monoid (MessageBuffer i) where
  mempty = MessageBuffer mempty mempty mempty mempty mempty mempty

type Sends i m =
  (Monoid (MessageBuffer i), MonadWriter (SemigroupMap i (MessageBuffer i)) m)
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
  modifying rsClients $ Set.insert i

handleDisconnect :: (Queries i m, Sends i m, Ord i) => i -> m ()
handleDisconnect i = do
  modifying rsClients $ Set.delete i
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
      queueSend i $ Frped $
        Mol.singleton (NamespaceError ns) "You're not the owner"
      handleDisconnect i

handleTrcsd
  :: (Queries i m, Ord i)
  => i -> TrDigest 'Consumer 'Subscribe -> m ()
handleTrcsd i d = do
  ownedAndSubd <- Map.keysSet
      . Map.filter ((== i) . fst)
      . flip Map.restrictKeys (trcsdNamespaces d)
    <$> use rsVsMap
  undefined

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

relinquish :: (Queries i m, Sends i m) => Namespace -> m ()
relinquish ns = do
  ma <- updateLookup rsVsMap (const Nothing) ns
  case ma of
    Nothing -> return ()
    Just (i, vs) -> do
      broadcast $ Frcrd $ Map.singleton ns SoAbsent
  unsubscribeNs ns

unsubscribeNs :: (Queries i m, Sends i m) => Namespace -> m ()
unsubscribeNs ns = do
    partitionedRegs <- fmap (crDeleteLookupNs ns) <$> use rsRegs
    assign rsRegs $ snd <$> partitionedRegs
    multicast $ mkFrcsd . fst <$> partitionedRegs
  where
    mkFrcsd (ClientRegs p t d) = Frcsd mempty p t d


{- The idea with the MonadWriter here is that the logic handling bits of the
Relay code can emit messages as and when they please without worrying about how
to keep them condensed into the smallest number of digests for the clients.

When we are done processing the digest that came in, we can look at the list of
digests for each client and condense it down to the minimal set.
-}

queueSend :: Sends i m => i -> FrDigest r a -> m ()
queueSend i = tell . SemigroupMap . Map.singleton i . bufferDigest

multicast :: Sends i m => Map i (FrDigest r a) -> m ()
multicast = tell . SemigroupMap . fmap bufferDigest

broadcast :: (Queries i m, Sends i m) => FrDigest r a -> m ()
broadcast d = use rsClients >>= multicast . Map.fromSet (const d)

doSend :: Monad m => SemigroupMap i (MessageBuffer i) -> RelayProtocol i m ()
doSend = void . Map.traverseWithKey
  (\i c -> mapM_ (sendRev . second (ServerData i)) $ toDigests c)
  . unSemigroupMap

bufferDigest
  :: forall r a i. FrDigest r a -> MessageBuffer i
bufferDigest d = mempty @(MessageBuffer i) & case d of
    Frpd {} -> set mbFrpds $ Map.singleton (frpdNs d) d
    Frped {} -> set mbFrped d
    Frcrd {} -> set mbFrcrd d
    Frcsd {} -> set mbFrcsd d
    Frcud {} -> set mbFrcuds $ Map.singleton (frcudNs d) d

toDigests :: MessageBuffer i -> [Either (Map Namespace i) SomeFrDigest]
toDigests (MessageBuffer frpds frped frcrd frcsd frcuds (Last owners)) =
  (Right <$>
     (SomeFrDigest <$> Map.elems frpds)
  ++ if frDigestNull frped then [] else [SomeFrDigest frped]
  ++ if frDigestNull frcrd then [] else [SomeFrDigest frcrd]
  ++ if frDigestNull frcsd then [] else [SomeFrDigest frcsd]
  ++ (SomeFrDigest <$> Map.elems frcuds))
  ++ case owners of
       -- No updates:
       Nothing -> []
       -- Changes:
       Just ownerMap -> [Left ownerMap]
