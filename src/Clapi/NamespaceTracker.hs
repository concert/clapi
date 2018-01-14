{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, Rank2Types #-}
{-# LANGUAGE PatternSynonyms #-}

module Clapi.NamespaceTracker where

import Control.Monad (forever, when)
import Control.Monad.State (StateT(..), evalStateT, get, modify)
import Control.Monad.Trans (MonadTrans, lift)
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (catMaybes, fromJust)
import Data.Either (lefts)

import Control.Lens (view, set, Lens', _1, _2)

import qualified Data.Map.Mos as Mos
import Clapi.Types
  ( DataUpdateMessage(..), TreeUpdateMessage(..)
  , OwnerUpdateMessage, ToRelayBundle(..), FromRelayBundle(..)
  , OwnerRequestBundle(..), UpdateBundle(..), RequestBundle(..)
  , MsgError(..), Msg(..), SubMessage(..))
import Clapi.Types.Path (Seg, Path, pattern Root, pattern (:</), pattern (:/))
import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Protocol (Protocol, Directed(..), wait, sendFwd, sendRev)

data Ownership = Owner | Client deriving (Eq, Show)
type Owners i = Map.Map Seg i

type Registered i = Mos.Mos i Path
-- FIXME:
type User = B.ByteString

data Request
  = ClientRequest [Path] [DataUpdateMessage]
  | OwnerRequest [OwnerUpdateMessage] deriving (Eq, Show)

data Response
  = ClientResponse [OwnerUpdateMessage] [MsgError] [DataUpdateMessage]
  | BadOwnerResponse [MsgError]
  | GoodOwnerResponse [OwnerUpdateMessage] deriving (Eq, Show)

maybeNamespace :: (Seg -> a) -> Path -> Maybe a
maybeNamespace f (n :</ Root) = Just $ f n
maybeNamespace _ _ = Nothing

namespaceOf :: Path -> Maybe Seg
namespaceOf (n :</ _) = Just n
namespaceOf _ = Nothing

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just a) f = f a

updateOwnerships ::
    forall m i. (Monad m, Ord i, Show i) => i -> [TreeUpdateMessage] -> StateT (Owners i) m ()
updateOwnerships i = mapM_ $ handle
  where
    handle :: TreeUpdateMessage -> StateT (Owners i) m ()
    handle (MsgAssignType path _) =
        whenJust (maybeNamespace id path) (\seg -> modify $ Map.insert seg i)
    handle (MsgDelete path) =
        whenJust (maybeNamespace id path) (\seg -> modify $ Map.delete seg)

registerSubscription ::
    (Monad m, Ord i) => i -> OwnerUpdateMessage -> StateT (Registered i) m ()
registerSubscription i m = case m of
    (Left (MsgAssignType path _)) -> modify (Mos.insert i path)
    _ -> return mempty

handleDeletedNamespace ::
    (Monad m, Ord i) => OwnerUpdateMessage -> StateT (Registered i) m ()
handleDeletedNamespace (Left (MsgDelete path)) =
    whenJust (maybeNamespace id path) (\_ -> modify $ Mos.remove path)
handleDeletedNamespace _ = return ()


stateL :: (Monad m) => Lens' t s -> StateT s m r -> StateT t m r
stateL l f = StateT $ \t -> let s = view l t in do
    -- FIXME: might be nice to combine the view and set into a single update
    (a, s') <- runStateT f s
    return (a, set l s' t)

stateL' :: (Monad m) => Lens' t s -> (a -> StateT s m r) -> a -> StateT t m r
stateL' l f a = stateL l (f a)


stateFst :: (Monad m) => StateT s1 m r -> StateT (s1, s2) m r
stateFst st = StateT $ \(s1, s2) -> do
    (r, s1') <- runStateT st s1
    return (r, (s1', s2))

stateSnd :: (Monad m) => StateT s2 m r -> StateT (s1, s2) m r
stateSnd st = StateT $ \(s1, s2) -> do
    (r, s2') <- runStateT st s2
    return (r, (s1, s2'))

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

type DownstreamCtx i = (i, Maybe [MsgError])

type NsProtocol m i = Protocol
    (ClientEvent i ToRelayBundle)
    ((DownstreamCtx i, Request))
    (ServerEvent i FromRelayBundle)
    ((DownstreamCtx i, Response))
    m


namespaceTrackerProtocol ::
    (Monad m, Ord i, Show i) =>
    (Owners i -> m ()) ->
    Owners i -> Registered i ->
    NsProtocol m i ()
namespaceTrackerProtocol p o r = evalStateT (_namespaceTrackerProtocol p) (o, r)


allPaths :: ToRelayBundle -> [Path]
allPaths b = case b of
    (TRBOwner (UpdateBundle errs oums)) -> ps errs ++ ps oums
    (TRBClient (RequestBundle subs dums)) -> ps subs ++ ps dums
  where
    ps :: (Msg m) => [m] -> [Path]
    ps = map uMsgPath

_namespaceTrackerProtocol ::
    (Monad m, Ord i, Show i) =>
    (Owners i -> m ()) ->
    StateT
        (Owners i, Registered i)
        (NsProtocol m i)
        ()
_namespaceTrackerProtocol publish = forever $ do
    (o, _) <- get
    liftedWaitThen fwd rev
    (o', _) <- get
    when (o /= o') (lift . lift $ publish o')
  where
    sendErrorBundle i s ps = lift . sendRev $ ServerData i $ FRBOwner $
        OwnerRequestBundle (map (flip MsgError s) ps) []
    kick i = do
        lift . sendRev $ ServerDisconnect i
        fwd $ ClientDisconnect i
    hasOwnership :: (Monad m, Msg msg, Ord i, Show i) => i -> Ownership -> [msg] -> StateT (Owners i) m [Path]
    hasOwnership i eo ms = get >>= \s -> return $ filter (\p -> Just eo == getPathOwnership i s p) $ map uMsgPath ms
    fwd (ClientConnect _i) = return mempty
    fwd (ClientData i b@(TRBOwner (UpdateBundle errs oums))) = do
        badErrPaths <- stateFst $ hasOwnership i Client errs
        badOumPaths <- stateFst $ hasOwnership i Client oums
        if not $ null $ badErrPaths ++ badOumPaths
            then lift . sendRev $ ServerData i $ FRBOwner $ OwnerRequestBundle (
                map (flip MsgError "Path already has another owner") $ allPaths b) []
            else lift $ sendFwd ((i, Just errs), OwnerRequest oums)
    fwd (ClientData i (TRBClient (RequestBundle subs dums))) = do
        badSubPaths <- stateFst $ hasOwnership i Owner subs
        badDumPaths <- stateFst $ hasOwnership i Owner dums
        let allBadPaths = badSubPaths ++ badDumPaths
        if not $ null allBadPaths
            then do
                sendErrorBundle i "Request bundle contains paths you own" $ allBadPaths
                kick i
            else do
                getPaths <- stateSnd $ handleUnsubscriptions i subs
                lift $ sendFwd ((i, Nothing), ClientRequest getPaths dums)
    fwd (ClientDisconnect i) = do
        tums <- handleClientDisconnect i
        lift $ sendRev $ ServerDisconnect i
        lift $ sendFwd ((i, Just []), OwnerRequest $ map Left tums)
    rev ((i, _ownErrs), ClientResponse getMsgs valErrs updates) = do
        stateSnd $ mapM_ (registerSubscription i) getMsgs
        stateFst $ sendToOwners updates
        if (null valErrs) && (null getMsgs)
            then return mempty
            else lift $ sendRev $ ServerData i $ FRBClient $ UpdateBundle valErrs getMsgs
      where
        sendToOwners ::
            (Monad m, Ord i) =>
            [DataUpdateMessage] ->
            StateT (Owners i) (NsProtocol m i) ()
        sendToOwners updates' = do
            po <- get
            lift $ mapM_ sendRev $ sendables po
          where
            sendables po =
              (\(i', dums) -> ServerData i' $ FRBOwner $
                  OwnerRequestBundle [] $ dums)
              <$> Map.toList (sendableMap po)
            sendableMap po = foldl (appendUpdate po) mempty updates'
            appendUpdate po sm u = Map.insertWith
              (++)
              (maybe (error "No owner!") id $ namespaceOf (uMsgPath u) >>= \ns -> Map.lookup ns po)
              [u] sm
    rev ((i, _ownErrs), BadOwnerResponse errs) = do
        lift $ sendRev $ ServerData i $ FRBOwner $ OwnerRequestBundle errs []
    rev ((i, ownErrs), GoodOwnerResponse updates) = do
        stateFst $ updateOwnerships i $ lefts updates
        stateSnd $ sendToSubs (fromJust ownErrs) updates  -- ownErrors should never be Nothing here
        stateSnd $ mapM_ handleDeletedNamespace updates
      where
        sendToSubs ::
            (Monad m, Ord i) =>
            [MsgError] ->
            [OwnerUpdateMessage] ->
            StateT (Registered i) (NsProtocol m i) ()
        sendToSubs errs oms = do
            ps <- get
            lift $ mapM_ sendRev $ filter nonEmptySendable $ getSendables ps
          where
            getSendables ps = map (\(i', paths) -> ServerData i' $
                                    filteredByPath paths) $ Map.toList ps
            filteredByPath paths = FRBClient $ UpdateBundle (filter (inPaths paths) errs) (filter (inPaths paths) oms)
            inPaths :: Msg msg => Set.Set Path -> msg -> Bool
            inPaths paths = flip elem paths . uMsgPath
            nonEmptySendable (ServerData _ (FRBClient (UpdateBundle [] []))) = False
            nonEmptySendable _ = True

handleUnsubscriptions ::
    forall m i. (Monad m, Ord i, Show i) =>
    i ->
    [SubMessage] ->
    StateT (Registered i) m [Path]
handleUnsubscriptions i ms = catMaybes <$> mapM handle ms
  where
    handle :: Monad m => SubMessage -> StateT (Registered i) m (Maybe Path)
    handle (MsgSubscribe p) = return $ Just p
    handle (MsgUnsubscribe p) = modify (Mos.delete i p) >> return Nothing

getPathOwnership :: (Ord i) => i -> Owners i -> Path -> Maybe Ownership
getPathOwnership i owners p = namespaceOf p >>= getNsOwnership
  where
    getNsOwnership name = (\i' -> if i' == i then Owner else Client) <$> Map.lookup name owners

handleClientDisconnect ::
    (Monad m, Ord i) =>
    i ->
    StateT (Owners i, Registered i) m [TreeUpdateMessage]
handleClientDisconnect i =
  do
    -- Drop what the client cares about:
    stateL _2 (modify (Map.delete i))
    -- Tell downstream that the owner wants to delete all their data:
    stateL _1 (get >>=
        return . fmap namespaceDeleteMsg . Map.keys . Map.filter (== i))
  where
    namespaceDeleteMsg seg = MsgDelete $ Root :/ seg
