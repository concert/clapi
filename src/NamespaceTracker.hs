{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module NamespaceTracker where

import Control.Monad (filterM)
import Control.Monad.State (StateT(..), evalStateT, get, modify)
import Data.List (partition)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Pipes (lift)
import Pipes.Core (Proxy, request, respond)

import qualified Data.Map.Mos as Mos
import qualified Data.Map.Mol as Mol
import Path (Name, Path)
import Types (ClapiMessage(..), ClapiMethod(..), ClapiValue(CString))
import Server (User)

data Ownership = Owner | Client | Unclaimed deriving (Eq, Show)
type Owners i = Map.Map Name i
type Registered i = Mos.Mos i Path

namespace :: Path -> Name
namespace [] = ""
namespace (n:ns) = n

isNamespace :: Path -> Bool
isNamespace (n:[]) = True
isNamespace _ = False

tag :: (Functor f) => (a -> b) -> f a -> f (b, a)
tag f = fmap (\a -> (f a, a))

methodAllowed :: (Ownership, ClapiMethod) -> Bool
methodAllowed (Client, m) = m /= Error
methodAllowed (Unclaimed, Error) = False
methodAllowed (_, Subscribe) = False
methodAllowed (_, Unsubscribe) = False
methodAllowed _ = True

disallowedMsg :: Ownership -> ClapiMessage -> ClapiMessage
disallowedMsg o m = m {msgMethod=Error, msgArgs=[CString $ txt]}
  where
    roleTxt Client = "a client"
    roleTxt _ = "the owner"
    txt = T.concat [
        "Method ", T.pack $ show $ msgMethod m, " forbidden when acting as ",
        roleTxt o]


updateOwnerships ::
    forall m i. (Monad m, Ord i) => i -> [(Ownership, ClapiMessage)] ->
    StateT (Owners i) m [(Ownership, ClapiMessage)]
updateOwnerships i = mapM handle
  where
    handle :: (Ownership, ClapiMessage) -> StateT (Owners i) m (Ownership, ClapiMessage)
    handle x@(Unclaimed, CMessage path AssignType _ _) | isNamespace path =
        modify (Map.insert (namespace path) i) >> return x
    handle x@(ownership, CMessage path Delete _ _)
        | ownership /= Client && isNamespace path =
            modify (Map.delete (namespace path)) >> return x
    handle x = return x

registerSubscriptions ::
    forall m i. (Monad m, Ord i) => i -> [(Ownership, ClapiMessage)] ->
    StateT (Registered i) m [(Ownership, ClapiMessage)]
registerSubscriptions i oms = filterM processMsg oms
  where
    processMsg :: (Ownership, ClapiMessage) -> StateT (Registered i) m Bool
    processMsg (Client, CMessage path Subscribe _ _) =
        modify (Mos.insert i path) >> return False
    processMsg (Client, CMessage path Unsubscribe _ _) =
        modify (Mos.delete i path) >> return False
    processMsg _ = return True

fanOutBundle ::
    (Monad m, Ord i) => i -> [(Ownership, ClapiMessage)] ->
    StateT (Registered i) m (Map.Map i [ClapiMessage])
fanOutBundle i oms = get >>=
    return . Map.filter (not . null) . Map.mapWithKey (deriveMsgs oms)
  where
    deriveMsgs oms i' ps = snd <$> filter (includeMsg i' ps) oms
    includeMsg i' ps (o, m) =
        if o == Client then i' == i else msgPath m `elem` ps

handleDeletedNamespace ::
    (Monad m, Ord i) => (Ownership, ClapiMessage) ->
    StateT (Registered i) m (Ownership, ClapiMessage)
handleDeletedNamespace om@(ownership, CMessage path Delete _ _) | isNamespace path =
    modify (Mos.remove path) >> return om
handleDeletedNamespace om = return om

handleDisconnectR ::
    (Monad m, Ord i) => i -> [a] -> StateT (Registered i) m [a]
handleDisconnectR i [] = modify (Map.delete i) >> return []
handleDisconnectR _ as  = return as

handleDisconnectO ::
    (Monad m, Eq i) => i -> [(Ownership, ClapiMessage)] ->
    StateT (Owners i) m [(Ownership, ClapiMessage)]
handleDisconnectO i [] =
    get >>= return . fmap namespaceDeleteMsg . Map.keys . Map.filter (== i)
  where
    namespaceDeleteMsg name = (Owner, CMessage [name] Delete [] [])
handleDisconnectO _ oms = return oms

tagOwnership ::
    (Monad m, Eq i) => i -> [ClapiMessage] ->
    StateT (Owners i) m [(Ownership, ClapiMessage)]
tagOwnership i ms = do
    owners <- get
    return $ tag (getNsOwnership owners . namespace . msgPath) ms
  where
    getNsOwnership owners name = case Map.lookup name owners of
      Nothing -> Unclaimed
      Just i' -> if i' == i then Owner else Client


type TrackerState i m = StateT (Owners i, Registered i) m

liftO :: (Monad m) => StateT (Owners i) m r -> TrackerState i m r
liftO f = StateT $ \(o, r) -> do
    (a, o') <- runStateT f o
    return (a, (o', r))

liftO' f a = liftO (f a)

liftR :: (Monad m) => StateT (Registered i) m r -> TrackerState i m r
liftR f = StateT $ \(o, r) -> do
    (a, r') <- runStateT f r
    return (a, (o, r'))

liftR' f a = liftR (f a)


type TrackerProxy i m =
    Proxy
        [(i, [ClapiMessage])] (i, User, [ClapiMessage])
        [(Ownership, ClapiMessage)] (User, [(Ownership, ClapiMessage)])
        m

-- Tracks both namespace ownership and path subscriptions
-- * Inbound we
--    * Tag individual messages with whether the originator owns the path or not.
--    * Perform basic validation as to whether methods are permitted given the
--      submitter's role
--    * Handle disconnections (empty bundles)
-- * Outbound we
--    * Intercept subscription and namespaces type assignments and deletions in
--      order to track the required state
--    * Filter out subscription messages
--    * "Fan out" messages according to who's registered to what paths
namespaceTracker ::
    (Monad m, Ord i) =>
    Owners i -> Registered i -> (i, User, [ClapiMessage]) -> TrackerProxy i m r
namespaceTracker o r x = evalStateT (_namespaceTracker x) (o, r)

_namespaceTracker ::
    (Monad m, Ord i) =>
    (i, User, [ClapiMessage]) ->
    TrackerState i (TrackerProxy i m) r
_namespaceTracker (i, u, ms) =
  do
    oms <- return ms >>=
        liftO' (tagOwnership i) >>=
        liftR' (handleDisconnectR i) >>=
        liftO' (handleDisconnectO i)
    let (fwdOms, badOms) = partition (methodAllowed . fmap msgMethod) oms
    oms' <- case fwdOms of
      [] -> return mempty
      _ -> (lift $ respond (u, fwdOms)) >>= liftO' (updateOwnerships i)
    bundleMap <- liftR (registerSubscriptions i oms') >>= liftR' (fanOutBundle i)
    liftR (mapM handleDeletedNamespace oms')
    let bundleMap' = case badOms of
          [] -> bundleMap
          _ -> Mol.prepend i (uncurry disallowedMsg <$> badOms) bundleMap
    (lift $ request $ Map.toList bundleMap') >>= _namespaceTracker
