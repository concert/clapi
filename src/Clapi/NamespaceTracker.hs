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
import Data.Either (lefts)

import Control.Lens (view, set, Lens', _1, _2)
import Pipes (lift)
import Pipes.Core (Proxy, request, respond)

import qualified Data.Map.Mos as Mos
import qualified Data.Map.Mol as Mol
import Clapi.Path (Name, Path(..))
import Clapi.Types (ClapiValue(ClString), DataUpdateMessage(..), TreeUpdateMessage(..), OwnerUpdateMessage(..), Bundle(..), UpdateBundle(..), RequestBundle(..), UMsgError(..), UMsg(..), SubMessage(..))
import Clapi.Server (ClientEvent(..), ServerEvent(..))
import Clapi.Protocol (Protocol, Directed(..), wait, sendFwd, sendRev)

data Ownership = Owner | Client deriving (Eq, Show)
type Owners i = Map.Map Name i

type Registered i = Mos.Mos i Path
-- FIXME:
type User = B.ByteString

-- Maybe the data types would be clearer/more precise as:
-- data Request
--   = ClientRequest [Path] [DataUpdateMessage]
--   | OwnerRequest [OwnerUpdateMessage]

-- data Response
--   = ClientResponse [OwnerUpdateMessage] [UMsgError] [DataUpdateMessage]
--   | BadOwnerResponse [UMsgError]
--   | GoodOwnerResponse [OwnerUpdateMessage]

data Request = Request
  { reqGet :: Maybe [Path]
  , reqUpdate :: [OwnerUpdateMessage]
  } deriving (Eq, Show)
data Response = Response
  { respGet :: [OwnerUpdateMessage]
  , respErrs :: [UMsgError]
  , respUpdate :: [OwnerUpdateMessage]
  } deriving (Eq, Show)

namespace :: Path -> Name
namespace (Path []) = ""
namespace (Path (n:ns)) = n

isNamespace :: Path -> Bool
isNamespace (Path (n:[])) = True
isNamespace _ = False

maybeNamespace :: (Name -> a) -> Path -> Maybe a
maybeNamespace f (Path (n:[])) = Just $ f n
maybeNamespace _ _ = Nothing

updateOwnerships ::
    forall m i. (Monad m, Ord i, Show i) => i -> [TreeUpdateMessage] -> StateT (Owners i) m ()
updateOwnerships i = mapM_ $ handle
  where
    handle :: TreeUpdateMessage -> StateT (Owners i) m ()
    handle (UMsgAssignType path _) =
        when (isNamespace path) (modify $ \x -> Map.insert (namespace path) i x)
    handle (UMsgDelete path) =
        when (isNamespace path) (modify (Map.delete $ namespace path))

registerSubscription ::
    (Monad m, Ord i) => i -> OwnerUpdateMessage -> StateT (Registered i) m ()
registerSubscription i m = case m of
    (Left (UMsgAssignType path _)) -> modify (Mos.insert i path)
    _ -> return mempty

fanOutBundle ::
    (Monad m, Ord i, Show i) =>
    i ->
    Maybe [UMsgError] ->
    [OwnerUpdateMessage] ->
    [UMsgError] ->
    [OwnerUpdateMessage] ->
    StateT (Owners i, Registered i) (NsProtocol m i) ()
fanOutBundle i mOwnerErrs gets valErrs updates = do
    (po, ps) <- get
    let allRecipients = Set.toList $ foldl Set.union (Set.singleton i) [owners po, Map.keysSet ps]
    let bundles = map (bundleFor po ps) allRecipients
    let sendables = map (uncurry ServerData) $ filter (nonEmptyBundle . snd) $ zip allRecipients bundles
    lift $ mapM_ sendRev sendables
  where
    bundleFor po ps i' = UpdateBundle
        (unicast valErrs i' ++ broadcastErrs ps i')
        (unicast gets i' ++ updatesFor po ps i')
    unicast ms i' = if i == i' then ms else []
    broadcastSubs :: (Ord i, UMsg msg) => Registered i -> [msg] -> i -> [msg]
    broadcastSubs ps ms i' = filter (isSubscriber ps i') ms
    isSubscriber :: (Ord i, UMsg msg) => Registered i -> i -> msg -> Bool
    isSubscriber ps i' m = uMsgPath m `elem` Map.findWithDefault mempty i' ps
    broadcastErrs ps = maybe (const []) (\oes -> broadcastSubs ps oes) mOwnerErrs
    updatesFor po ps i' = if null mOwnerErrs
        then filter (isOwner po i' . uMsgPath) updates
        else broadcastSubs ps updates i'
    isOwner po i' p = Map.lookup (namespace p) po == Just i'
    nonEmptyBundle (UpdateBundle errs oums) = (not $ null errs) || (not $ null oums)
    owners po = Set.fromList $ Map.elems po

handleDeletedNamespace ::
    (Monad m, Ord i) => OwnerUpdateMessage -> StateT (Registered i) m ()
handleDeletedNamespace (Left (UMsgDelete path)) =
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

type DownstreamCtx i = (i, Maybe [UMsgError])

type NsProtocol m i = Protocol
    (ClientEvent i Bundle ())
    ((DownstreamCtx i, Request))
    (ServerEvent i UpdateBundle)
    ((DownstreamCtx i, Response))
    m


namespaceTrackerProtocol ::
    (Monad m, Ord i, Show i) =>
    Owners i -> Registered i ->
    NsProtocol m i ()
namespaceTrackerProtocol o r = evalStateT _namespaceTrackerProtocol (o, r)


allPaths :: Bundle -> [Path]
allPaths b = case b of
    (Left (UpdateBundle errs oums)) -> ps errs ++ ps oums
    (Right (RequestBundle subs dums)) -> ps subs ++ ps dums
  where
    ps :: (UMsg m) => [m] -> [Path]
    ps = map uMsgPath

_namespaceTrackerProtocol ::
    (Monad m, Ord i, Show i) =>
    StateT
        (Owners i, Registered i)
        (NsProtocol m i)
        ()
_namespaceTrackerProtocol = forever $ liftedWaitThen fwd rev
  where
    sendErrorBundle i s ps = lift . sendRev $ ServerData i $
        UpdateBundle (map (flip UMsgError s) ps) []
    kick i = do
        lift . sendRev $ ServerDisconnect i
        fwd $ ClientDisconnect i
    hasOwnership :: (Monad m, UMsg msg, Ord i, Show i) => i -> Ownership -> [msg] -> StateT (Owners i) m [Path]
    hasOwnership i eo ms = get >>= \s -> return $ filter (\p -> Just eo == getPathOwnership i s p) $ map uMsgPath ms
    fwd :: (Monad m, Ord i, Show i) => ClientEvent i Bundle x -> StateT (Owners i, Registered i) (NsProtocol m i) ()
    fwd (ClientConnect i x) = return mempty
    fwd (ClientData i b@(Left (UpdateBundle errs oums))) = do
        badErrPaths <- stateFst $ hasOwnership i Client errs
        badOumPaths <- stateFst $ hasOwnership i Client oums
        if not $ null $ badErrPaths ++ badOumPaths
            then sendErrorBundle i "Client role message in update bundle" $ allPaths b
            else lift $ sendFwd ((i, Just errs), Request Nothing oums)
    fwd (ClientData i (Right (RequestBundle subs dums))) = do
        badSubPaths <- stateFst $ hasOwnership i Owner subs
        badDumPaths <- stateFst $ hasOwnership i Owner dums
        let allBadPaths = badSubPaths ++ badDumPaths
        if not $ null allBadPaths
            then do
                sendErrorBundle i "Client role taken for own path" $ allBadPaths
                kick i
            else do
                getPaths <- stateSnd $ handleUnsubscriptions i subs
                lift $ sendFwd ((i, Nothing), Request (Just getPaths) $ map Right dums)
    fwd (ClientDisconnect i) = do
        tums <- handleClientDisconnect i
        lift $ sendRev $ ServerDisconnect i
        lift $ sendFwd ((i, Just []), Request Nothing $ map Left tums)
    rev ((i, ownErrs), Response gets valErrs updates) = do
        -- FIXME: Should never have ownErrs and gets
        if null ownErrs
            then stateSnd $ mapM_ (registerSubscription i) gets
            else stateFst $ updateOwnerships i $ lefts updates
        fanOutBundle i ownErrs gets valErrs updates
        stateSnd $ mapM_ handleDeletedNamespace updates

handleUnsubscriptions ::
    forall m i. (Monad m, Ord i, Show i) =>
    i ->
    [SubMessage] ->
    StateT (Registered i) m [Path]
handleUnsubscriptions i ms = catMaybes <$> mapM handle ms
  where
    handle :: Monad m => SubMessage -> StateT (Registered i) m (Maybe Path)
    handle (UMsgSubscribe p) = return $ Just p
    handle (UMsgUnsubscribe p) = modify (Mos.delete i p) >> return Nothing

getPathOwnership :: (Ord i) => i -> Owners i -> Path -> Maybe Ownership
getPathOwnership i owners = getNsOwnership . namespace
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
    namespaceDeleteMsg name = UMsgDelete $ Path [name]
