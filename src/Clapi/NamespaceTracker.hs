{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, Rank2Types #-}
module Clapi.NamespaceTracker where

import Control.Monad (filterM)
import Control.Monad.State (StateT(..), evalStateT, get, modify)
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
import Clapi.Util (tagl)
import Clapi.Types (Message(..), msgMethod', ClapiMethod(..), ClapiValue(CString))
import Clapi.Server (User)

data Ownership = Owner | Client | Unclaimed deriving (Eq, Show)
type Owners i = Map.Map Name i
type Om = (Ownership, Message)
type Registered i = Mos.Mos i Path

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
    forall m i. (Monad m, Ord i) => i -> [Om] -> StateT (Owners i) m [Om]
updateOwnerships i = mapM handle
  where
    handle :: (Ownership, Message) -> StateT (Owners i) m (Ownership, Message)
    handle x@(Unclaimed, MsgAssignType path _) | isNamespace path =
        modify (Map.insert (namespace path) i) >> return x
    handle x@(ownership, MsgDelete path)
        | ownership /= Client && isNamespace path =
            modify (Map.delete (namespace path)) >> return x
    handle x = return x

registerSubscriptions ::
    forall m i. (Monad m, Ord i) => i -> [Om] -> StateT (Registered i) m [Om]
registerSubscriptions i oms = filterM processMsg oms
  where
    processMsg :: Om -> StateT (Registered i) m Bool
    processMsg (Client, MsgSubscribe path) =
        modify (Mos.insert i path) >> return False
    processMsg (Client, MsgUnsubscribe path) =
        modify (Mos.delete i path) >> return False
    processMsg _ = return True

fanOutBundle ::
    (Monad m, Ord i) => i -> [Om] ->
    StateT (Registered i) m (Map.Map i [Message])
fanOutBundle i oms = get >>=
    return . Map.filter (not . null) . Map.mapWithKey (deriveMsgs oms)
  where
    deriveMsgs oms i' ps = snd <$> filter (includeMsg i' ps) oms
    includeMsg i' ps (o, m) =
        if o == Client then i' == i else msgPath' m `elem` ps

handleDeletedNamespace ::
    (Monad m, Ord i) => Om -> StateT (Registered i) m Om
handleDeletedNamespace om@(ownership, MsgDelete path) | isNamespace path =
    modify (Mos.remove path) >> return om
handleDeletedNamespace om = return om

handleDisconnectR ::
    (Monad m, Ord i) => i -> [a] -> StateT (Registered i) m [a]
handleDisconnectR i [] = modify (Map.delete i) >> return []
handleDisconnectR _ as  = return as

handleDisconnectO ::
    (Monad m, Eq i) => i -> [Om] -> StateT (Owners i) m [Om]
handleDisconnectO i [] =
    get >>= return . fmap namespaceDeleteMsg . Map.keys . Map.filter (== i)
  where
    namespaceDeleteMsg name = (Owner, MsgDelete [name])
handleDisconnectO _ oms = return oms

tagOwnership ::
    (Monad m, Eq i) => i -> [Message] -> StateT (Owners i) m [Om]
tagOwnership i ms = do
    owners <- get
    return $ tagl (getNsOwnership owners . namespace . msgPath') <$> ms
  where
    getNsOwnership owners name = case Map.lookup name owners of
      Nothing -> Unclaimed
      Just i' -> if i' == i then Owner else Client

stateL :: (Monad m) => Lens' t s -> StateT s m r -> StateT t m r
stateL l f = StateT $ \t -> let s = view l t in do
    -- FIXME: might be nice to combine the view and set into a single update
    (a, s') <- runStateT f s
    return (a, set l s' t)

stateL' :: (Monad m) => Lens' t s -> (a -> StateT s m r) -> a -> StateT t m r
stateL' l f a = stateL l (f a)



type TrackerProxy i m =
    Proxy [(i, [Message])] (i, User, [Message]) [Om] (User, [Om]) m

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
    Owners i -> Registered i -> (i, User, [Message]) -> TrackerProxy i m r
namespaceTracker o r x = evalStateT (_namespaceTracker x) (o, r)

_namespaceTracker ::
    (Monad m, Ord i) =>
    (i, User, [Message]) ->
    StateT (Owners i, Registered i) (TrackerProxy i m) r
_namespaceTracker (i, u, ms) =
  do
    oms <- return ms >>=
        stateL' _1 (tagOwnership i) >>=
        stateL' _2 (handleDisconnectR i) >>=
        stateL' _1 (handleDisconnectO i)
    let (fwdOms, badOms) = partition (methodAllowed . fmap msgMethod') oms
    oms' <- case fwdOms of
      [] -> return mempty
      _ -> (lift $ respond (u, fwdOms)) >>= stateL' _1 (updateOwnerships i)
    bundleMap <- stateL _2 (registerSubscriptions i oms') >>=
        stateL' _2 (fanOutBundle i)
    stateL _2 (mapM handleDeletedNamespace oms')
    let bundleMap' = case badOms of
          [] -> bundleMap
          _ -> Mol.prepend i (uncurry disallowedMsg <$> badOms) bundleMap
    (lift $ request $ Map.toList bundleMap') >>= _namespaceTracker
