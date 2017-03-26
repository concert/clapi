{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Pipeline where

import Control.Monad (when)
import Control.Monad.State (StateT(..), evalStateT, get, modify)
import Data.List (partition)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Pipes (lift)
import Pipes.Core (Proxy, request, respond)

import qualified Data.Map.Mos as Mos
import Path (Name, Path)
import Types (ClapiMessage(..), ClapiMethod(..))
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

methodAllowed :: Ownership -> ClapiMethod -> Bool
methodAllowed Client m = m /= Error
methodAllowed Unclaimed Error = False
methodAllowed _ Subscribe = False
methodAllowed _ Unsubscribe = False
methodAllowed _ _ = True


handleOutboundMsgsO ::
    (Monad m, Ord i) => i -> [(Ownership, ClapiMessage)] ->
    StateT (Owners i) m [(Ownership, ClapiMessage)]
handleOutboundMsgsO i = mapM (handleOutboundMsgO i)

handleOutboundMsgO ::
    (Monad m) => i -> (Ownership, ClapiMessage) ->
    StateT (Owners i) m (Ownership, ClapiMessage)
handleOutboundMsgO i x@(Unclaimed, CMessage path AssignType _ _)
  | isNamespace path =
        modify (Map.insert (namespace path) i) >> return x
handleOutboundMsgO _ x@(Owner, CMessage path Delete _ _)
  | isNamespace path =
        modify (Map.delete (namespace path)) >> return x
handleOutboundMsgO _ x = return x

handleOutboundMsgsR ::
    forall m i. (Monad m, Ord i) => i -> [(Ownership, ClapiMessage)] ->
    StateT (Registered i) m (Map.Map i [ClapiMessage])
handleOutboundMsgsR _ [] = return mempty
handleOutboundMsgsR i ((o, m):oms) =
  do
    dropMsg <- handleSubs o m
    if dropMsg
        then handleOutboundMsgsR i oms
        else do
            msgMap <- bar m
            bundleMap <- handleOutboundMsgsR i oms
            return $ Map.unionWith (++) msgMap bundleMap
  where
    handleSubs :: Ownership -> ClapiMessage -> StateT (Registered i) m Bool
    handleSubs Client (CMessage path Subscribe _ _) =
        modify (Mos.insert i path) >> return True
    handleSubs Client (CMessage path Unsubscribe _ _) =
        modify (Mos.delete i path) >> return True
    handleSubs _ _ = return False

    bar :: ClapiMessage -> StateT (Registered i) m (Map.Map i [ClapiMessage])
    bar m = get >>= return . Map.mapWithKey (includeMsg m)
    includeMsg m i' paths =
        if (o == Client && i' == i) || (msgPath m `elem` paths) then [m] else []

handleInboundMsgsR ::
    (Monad m, Ord i) => i -> [a] -> StateT (Registered i) m [a]
handleInboundMsgsR i [] = modify (Map.delete i) >> return []
handleInboundMsgsR _ as  = return as

handleInboundMsgsO ::
    (Monad m, Eq i) => i -> [(Ownership, ClapiMessage)] ->
    StateT (Owners i) m [(Ownership, ClapiMessage)]
handleInboundMsgsO i [] =
    get >>= return . fmap namespaceDeleteMsg . Map.keys . Map.filter (== i)
  where
    namespaceDeleteMsg name = (Owner, CMessage [name] Delete [] [])
handleInboundMsgsO _ oms = return oms

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
    (Monad m, Ord i) => Owners i -> Registered i -> (i, User, [ClapiMessage]) -> TrackerProxy i m r
namespaceTracker o r x = evalStateT (_namespaceTracker x) (o, r)

_namespaceTracker ::
    (Monad m, Ord i) => (i, User, [ClapiMessage]) ->
    TrackerState i (TrackerProxy i m) r
_namespaceTracker (i, u, ms) =
  do
    oms <- return ms >>=
        liftO' (tagOwnership i) >>=
        liftR' (handleInboundMsgsR i) >>=
        liftO' (handleInboundMsgsO i)
    let (fwdOms, badOms) = partition (uncurry methodAllowed . fmap msgMethod) oms
    -- FIXME handle badOms
    bundleMap <- (lift $ respond (u, fwdOms)) >>=
        liftO' (handleOutboundMsgsO i) >>=
        liftR' (handleOutboundMsgsR i)
    (lift $ request $ Map.toList bundleMap) >>= _namespaceTracker
