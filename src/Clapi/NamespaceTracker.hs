{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, Rank2Types #-}
module Clapi.NamespaceTracker where

import Control.Monad (filterM, forever, when)
import Control.Monad.State (StateT(..), evalStateT, get, gets, modify)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Free
import qualified Data.ByteString as B
import Data.List (partition)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Control.Lens (view, set, Lens', _1, _2)
import Pipes (lift)
import Pipes.Core (Proxy, request, respond)

import qualified Data.Map.Mos as Mos
import qualified Data.Map.Mol as Mol
import Clapi.Path (Name, Path)
import Clapi.Types (Message(..), msgMethod', ClapiMethod(..), ClapiValue(ClString))
import Clapi.Server (AddrWithUser, ClientAddr, ClientEvent(..), ServerEvent(..), awuAddr)
import Clapi.Protocol (Protocol, Directed(..), wait, sendFwd, sendRev)

data Ownership = Owner | Client | Unclaimed deriving (Eq, Show)
type Owners i = Map.Map Name i
data TrackedMessage i = TrackedMessage {tmOwnership :: Ownership, tmMessage :: Message, tmSubscriber :: Maybe i} deriving (Show)
type Registered i = Mos.Mos i Path

-- FIXME:
type User = B.ByteString

namespace :: Path -> Name
namespace [] = ""
namespace (n:ns) = n

isNamespace :: Path -> Bool
isNamespace (n:[]) = True
isNamespace _ = False

methodAllowed :: (Ownership, ClapiMethod) -> Bool
methodAllowed (Client, m) = m /= Error
methodAllowed (Unclaimed, Error) = False
methodAllowed (_, Subscribe) = False
methodAllowed (_, Unsubscribe) = False
methodAllowed _ = True

disallowedMsg :: Ownership -> Message -> Message
disallowedMsg o m = MsgError (msgPath' m) txt
  where
    roleTxt Client = "a client"
    roleTxt _ = "the owner"
    txt = T.concat [
        "Method ", T.pack $ show $ msgMethod' m, " forbidden when acting as ",
        roleTxt o]

toOm :: TrackedMessage i -> (Ownership, Message)
toOm tm = (tmOwnership tm, tmMessage tm)

updateOwnerships ::
    forall m i u. (Monad m, Ord i, Ord u, Show i) => AddrWithUser i u -> [TrackedMessage i] -> StateT (Owners (AddrWithUser i u)) m ()
updateOwnerships i = mapM_ $ handle . toOm
  where
    handle :: (Ownership, Message) -> StateT (Owners (AddrWithUser i u)) m ()
    handle (Unclaimed, MsgAssignType path _) =
        when (isNamespace path) (modify $ \x -> Map.insert (namespace path) i x)
    handle (ownership, MsgDelete path) =
        when
            (ownership /= Client && isNamespace path)
            (modify (Map.delete $ namespace path))
    handle _ = return ()

registerSubscriptions ::
    forall m i u. (Monad m, Ord i, Ord u) => AddrWithUser i u -> [TrackedMessage i] -> StateT (Registered (AddrWithUser i u)) m [TrackedMessage i]
registerSubscriptions i tms = filterM (processMsg . toOm) tms
  where
    processMsg :: (Ownership, Message) -> StateT (Registered (AddrWithUser i u)) m Bool
    processMsg (Client, MsgSubscribe path) =
        modify (Mos.insert i path) >> return True
    processMsg (Client, MsgUnsubscribe path) =
        modify (Mos.delete i path) >> return False
    processMsg _ = return True

fanOutBundle ::
    (Monad m, Ord i, Ord u, Show i, Show u) => AddrWithUser i u -> [TrackedMessage i] ->
    StateT (Registered (AddrWithUser i u)) (NsProtocol m i u) ()
fanOutBundle i tms = get >>=
    mapM_ (lift . sendRev . uncurry ServerData) .
        Map.toList . Map.filter (not . null) . Map.mapWithKey (deriveMsgs tms)
  where
    deriveMsgs tms i' ps = tmMessage <$> filter (includeMsg i' ps) tms
    includeMsg i' ps tm =
        -- NOT SURE THIS IS TRUE!
        -- Client messages should only go to their targeted client, others go to
        -- whoever's registered:
        maybe
            (if (tmOwnership tm) == Client then i' == i else msgPath' (tmMessage tm) `elem` ps)
            (\addr -> addr == (awuAddr i'))
            (tmSubscriber tm)

handleDeletedNamespace ::
    (Monad m, Ord i, Ord u) => TrackedMessage i -> StateT (Registered (AddrWithUser i u)) m ()
handleDeletedNamespace (TrackedMessage ownership (MsgDelete path) _) =
    when (isNamespace path) (modify (Mos.remove path))
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


type NsProtocol m i u = Protocol
    (ClientEvent (AddrWithUser i u) [Message] ())
    (ClientEvent (AddrWithUser i u) [TrackedMessage i] ())
    (ServerEvent (AddrWithUser i u) [Message])
    (ServerEvent (AddrWithUser i u) [TrackedMessage i])
    m


namespaceTrackerProtocol ::
    (Monad m, Ord i, Ord u, Show i, Show u) =>
    Owners (AddrWithUser i u) -> Registered (AddrWithUser i u) ->
    NsProtocol m i u ()
namespaceTrackerProtocol o r = evalStateT _namespaceTrackerProtocol (o, r)


_namespaceTrackerProtocol ::
    (Monad m, Ord i, Ord u, Show i, Show u) =>
    StateT
        (Owners (AddrWithUser i u), Registered (AddrWithUser i u))
        (NsProtocol m i u)
        ()
_namespaceTrackerProtocol = forever $ liftedWaitThen fwd rev
  where
    fwd (ClientConnect awu x) = lift . sendFwd $ ClientConnect awu ()
    fwd (ClientData awu ms) = do
        (toFwd, toReject) <- stateFst $ handleClientData awu ms
        when (not $ null toReject) $ lift . sendRev $ ServerData awu toReject
        when (not $ null toFwd) $ lift . sendFwd $ ClientData awu toFwd
    fwd (ClientDisconnect awu) = do
        tms <- handleClientDisconnect awu
        lift . sendFwd $ ClientData awu tms
        lift . sendFwd $ ClientDisconnect awu
    rev :: (Ord i, Ord u, Monad m, Show i, Show u) => ServerEvent (AddrWithUser i u) [TrackedMessage i] -> StateT
        (Owners (AddrWithUser i u), Registered (AddrWithUser i u))
        (NsProtocol m i u)
        ()
    rev (ServerData awu tms) = do
        -- FIXME: is the fact that this looks like it's totally arsing up the
        -- state to do with having to do the blasted contexts thing? Try
        -- rewriting without using flippin' lenses:
        stateFst $ updateOwnerships awu tms
        -- FIXME: use of registereSubscriptions whined and whined about needing
        -- FlexibleContexts, but I have not idea why...
        stateSnd (
            registerSubscriptions awu tms >>=
            fanOutBundle awu >>
            mapM_ handleDeletedNamespace tms)
    -- What do we do if we told the client to go away? Obviously pass that on
    -- and drop registrations, but what about ownership?
    rev (ServerDisconnect awu) =
        lift . sendRev $ (ServerDisconnect awu)

handleClientData ::
    (Monad m, Ord i, Ord u, Show i, Show u) =>
    AddrWithUser i u ->
    [Message] ->
    StateT (Owners (AddrWithUser i u)) m ([TrackedMessage i], [Message])
handleClientData awu ms =
  do
    owners <- get
    let (goodTrackedMessages, badTrackedMessages) = partition
            (\tm -> methodAllowed (tmOwnership tm, msgMethod' $ tmMessage tm))
            (map (tagOwnershipAndSubscriber awu owners) ms)
    return (goodTrackedMessages, map (\tm -> disallowedMsg (tmOwnership tm) (tmMessage tm)) badTrackedMessages)


tagOwnershipAndSubscriber :: (Ord i, Ord u) => AddrWithUser i u -> Owners (AddrWithUser i u) -> Message -> TrackedMessage i
tagOwnershipAndSubscriber i owners msg = TrackedMessage owner msg subber
  where
    subber' (MsgSubscribe _) = Just $ awuAddr i
    subber' _ = Nothing
    subber = subber' msg
    owner = getNsOwnership . namespace . msgPath' $ msg
    getNsOwnership name = case Map.lookup name owners of
      Nothing -> Unclaimed
      Just i' -> if i' == i then Owner else Client


handleClientDisconnect ::
    (Monad m, Ord i, Ord u) =>
    AddrWithUser i u ->
    StateT (Owners (AddrWithUser i u), Registered (AddrWithUser i u)) m [TrackedMessage i]
handleClientDisconnect i =
  do
    -- Drop what the client cares about:
    stateL _2 (modify (Map.delete i))
    -- Tell downstream that the owner wants to delete all their data:
    stateL _1 (get >>=
        return . fmap namespaceDeleteMsg . Map.keys . Map.filter (== i))
  where
    namespaceDeleteMsg name = TrackedMessage Owner (MsgDelete [name]) Nothing
