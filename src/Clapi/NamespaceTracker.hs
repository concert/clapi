{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, Rank2Types #-}
module Clapi.NamespaceTracker where

import Control.Monad (filterM, forever, when)
import Control.Monad.State (StateT(..), evalStateT, get, gets, modify, put)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Free
import qualified Data.ByteString as B
import Data.List (partition)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Data.Either (partitionEithers)

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
-- This is kinda fanoutable rather than routable
-- Also not sure about the either, should there be a strategy type instead?
data RoutableMessage i = RoutableMessage {rmRoute :: Either i Ownership, rmMsg :: Message} deriving (Eq, Show)
type Om = (Ownership, Message)
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

updateOwnerships ::
    forall m i u. (Monad m, Ord i, Ord u, Show i) => AddrWithUser i u -> [RoutableMessage i] -> StateT (Owners (AddrWithUser i u)) m ()
updateOwnerships i = mapM_ $ handle
  where
    handle :: RoutableMessage i -> StateT (Owners (AddrWithUser i u)) m ()
    handle (RoutableMessage (Right Owner) (MsgAssignType path _)) =
        when (isNamespace path) (modify $ \x -> Map.insert (namespace path) i x)
    handle (RoutableMessage (Right Owner) (MsgDelete path)) =
        when (isNamespace path) (modify (Map.delete $ namespace path))
    handle _ = return ()

-- FIXME: not filtering anything out so refactor accordingly
registerSubscriptions ::
    forall m i u. (Monad m, Ord i, Ord u) => AddrWithUser i u -> [RoutableMessage i] -> StateT (Registered (AddrWithUser i u)) m [RoutableMessage i]
registerSubscriptions i rms = filterM processMsg rms
  where
    processMsg :: RoutableMessage i -> StateT (Registered (AddrWithUser i u)) m Bool
    processMsg (RoutableMessage (Left _) (MsgAssignType path _)) =
        modify (Mos.insert i path) >> return True
    processMsg _ = return True

fanOutBundle ::
    (Monad m, Ord i, Ord u, Show i, Show u) => AddrWithUser i u -> [RoutableMessage i] ->
    StateT (Registered (AddrWithUser i u)) (NsProtocol m i u) ()
fanOutBundle i rms = get >>=
    mapM_ (lift . sendRev . uncurry ServerData) .
        Map.toList . Map.filter (not . null) . Map.mapWithKey (deriveMsgs rms)
  where
    deriveMsgs rms i' ps = rmMsg <$> filter (includeMsg i' ps) rms
    includeMsg i' ps rm =
        -- NOT SURE THIS IS TRUE! don't they go to the owner? isn't it owner ones that do this?
        -- Client messages should only go to their targeted client, others go to
        -- whoever's registered:
        either
            (\addr -> addr == (awuAddr i'))
            (\o -> if o == Client then i' == i else msgPath' (rmMsg rm) `elem` ps)
            (rmRoute rm)

handleDeletedNamespace ::
    (Monad m, Ord i, Ord u) => RoutableMessage i -> StateT (Registered (AddrWithUser i u)) m ()
handleDeletedNamespace (RoutableMessage ownership (MsgDelete path)) =
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
    (ClientEvent (AddrWithUser i u) [Om] ())
    (ServerEvent (AddrWithUser i u) [Message])
    (ServerEvent (AddrWithUser i u) [RoutableMessage i])
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
        (toFwd', unsubErrors) <- stateSnd $ handleUnsubscriptions awu toFwd
        when (not $ null $ toReject ++ unsubErrors) $
            lift . sendRev $ ServerData awu toReject
        when (not $ null toFwd) $ lift . sendFwd $ ClientData awu toFwd'
    fwd (ClientDisconnect awu) = do
        rms <- handleClientDisconnect awu
        lift . sendFwd $ ClientData awu rms
        lift . sendFwd $ ClientDisconnect awu
    rev :: (Ord i, Ord u, Monad m, Show i, Show u) => ServerEvent (AddrWithUser i u) [RoutableMessage i] -> StateT
        (Owners (AddrWithUser i u), Registered (AddrWithUser i u))
        (NsProtocol m i u)
        ()
    rev (ServerData awu rms) = do
        -- FIXME: is the fact that this looks like it's totally arsing up the
        -- state to do with having to do the blasted contexts thing? Try
        -- rewriting without using flippin' lenses:
        stateFst $ updateOwnerships awu rms
        -- FIXME: use of registereSubscriptions whined and whined about needing
        -- FlexibleContexts, but I have not idea why...
        stateSnd (
            registerSubscriptions awu rms >>=
            fanOutBundle awu >>
            mapM_ handleDeletedNamespace rms)
    -- What do we do if we told the client to go away? Obviously pass that on
    -- and drop registrations, but what about ownership?
    -- Do they lose everything they owned?
    rev (ServerDisconnect awu) =
        lift . sendRev $ (ServerDisconnect awu)

-- Theoretically the left of the returned pair from this is a different type
-- since it should never contain an unsubscribe
handleUnsubscriptions ::
    (Monad m, Ord i, Ord u, Show i, Show u) =>
    AddrWithUser i u ->
    [Om] ->
    StateT (Registered (AddrWithUser i u)) m ([Om], [Message])
handleUnsubscriptions awu ms = do
    let (otherMs, unsubs) = partitionEithers $ map (unsubPath) ms
    merrs <- mapM handleUnsubscription unsubs
    return (otherMs, catMaybes merrs)
  where
    unsubPath (_, MsgUnsubscribe p) = Right p
    unsubPath m = Left m
    handleUnsubscription ::
        (Monad m) =>
        Path ->
        StateT (Registered (AddrWithUser i u)) m (Maybe Message)
    handleUnsubscription p = do
        subs <- get
        let subs' = Mos.delete (awuAddr awu) p subs
        put subs'
        return $ if subs == subs' then Just $ invalidUnsub p else Nothing
    invalidUnsub p = MsgError p "unsubscribe when not subscribed"

handleClientData ::
    (Monad m, Ord i, Ord u, Show i, Show u) =>
    AddrWithUser i u ->
    [Message] ->
    StateT (Owners (AddrWithUser i u)) m ([Om], [Message])
handleClientData awu ms =
  do
    -- FIXME: handle unsubscribes here?
    -- FIXME: fanout of owner errors here?
    owners <- get
    let (goodTrackedMessages, badTrackedMessages) = partition
            (\(o, m) -> methodAllowed (o, msgMethod' m))
            (map (tagOwnership awu owners) ms)
    return (goodTrackedMessages, map (uncurry disallowedMsg) badTrackedMessages)


tagOwnership :: (Ord i, Ord u) => AddrWithUser i u -> Owners (AddrWithUser i u) -> Message -> Om
tagOwnership i owners msg = (owner msg, msg)
  where
    owner = getNsOwnership . namespace . msgPath'
    getNsOwnership name = case Map.lookup name owners of
      Nothing -> Unclaimed
      Just i' -> if i' == i then Owner else Client


handleClientDisconnect ::
    (Monad m, Ord i, Ord u) =>
    AddrWithUser i u ->
    StateT (Owners (AddrWithUser i u), Registered (AddrWithUser i u)) m [Om]
handleClientDisconnect i =
  do
    -- Drop what the client cares about:
    stateL _2 (modify (Map.delete i))
    -- Tell downstream that the owner wants to delete all their data:
    stateL _1 (get >>=
        return . fmap namespaceDeleteMsg . Map.keys . Map.filter (== i))
  where
    namespaceDeleteMsg name = (Owner, MsgDelete [name])
