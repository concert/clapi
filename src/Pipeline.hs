{-# LANGUAGE OverloadedStrings #-}
module Pipeline where

import Control.Applicative (liftA2)
import Data.Bifunctor (bimap)
import Data.List (partition)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network.Socket as NS

import Pipes.Core (Proxy, request, respond)

import Data.Maybe.Clapi (toMonoid, fromFoldable)
import qualified Data.Map.Mos as Mos
import Path (Name, Path)
import Types (
    CanFail, ClapiBundle, ClapiMessage, ClapiMethod(..), ClapiValue(..),
    msgPath, msgMethod, msgArgs)
import Server (User)

namespace :: Path -> Name
namespace [] = ""
namespace (n:ns) = n

isNamespace :: Path -> Bool
isNamespace (n:[]) = True
isNamespace _ = False

data Ownership = Owner | Client | Unclaimed deriving (Eq, Show)

tag :: (a -> b) -> [a] -> [(b, a)]
tag f as = zip (f <$> as) as

methodAllowed :: Ownership -> ClapiMethod -> Bool
methodAllowed Client m = m /= Error
methodAllowed _ Subscribe = False
methodAllowed _ Unsubscribe = False
methodAllowed _ _ = True


partitionMap :: (a -> Bool) -> (a -> b) -> (a -> c) -> [a] -> ([b], [c])
partitionMap p ft ff = foldr select ([], [])
  where
    select a (ts, fs) | p a = (ft a:ts, fs)
                      | otherwise = (ts, ff a:fs)

forwardOrBounce :: [(Ownership, ClapiMessage)] ->
    ([(Ownership, ClapiMessage)], [ClapiMessage])
forwardOrBounce =
    partitionMap (uncurry methodAllowed . fmap msgMethod) id (uncurry toErrMsg)
  where
    toErrMsg owner msg = msg {
        msgMethod=Error,
        msgArgs=errMsgArgs owner msg
    }
    role Client = "a client"
    role _ = "the owner"
    errMsgTxt ownership method = T.concat [
        "Method ", T.pack $ show method, " forbidden when acting as ",
        role ownership]
    errMsgArgs owner msg = [CString $ errMsgTxt owner (msgMethod msg)]


-- Tracks both namespace ownership and path subscriptions
namespaceTracker ::
    (Monad m, Ord i) =>
    (i, User, ClapiBundle) ->
    Proxy [(i, ClapiBundle)]  (i, User, ClapiBundle)
    ClapiBundle (User, [(Ownership, ClapiMessage)]) m r
namespaceTracker =
    loop
        (mempty :: Ord i => Map.Map Name i)
        (mempty :: Ord i => Mos.Mos i Path)
  where
    loop owners registered (addr, user, ms) = do
        let (forwardMs, bounceMs) = forwardOrBounce $ tag getMsgOwnership ms
        ms' <- respond (user, forwardMs)
        let registered' = updateRegistered ms'
        let toSend = fanOutBundle registered' ms'
        let toSend' = Map.alter (\maybeMs -> fromFoldable (bounceMs <> toMonoid maybeMs)) addr toSend
        (addr', user', bundle') <- request $ Map.toList toSend'
        loop (updateOwners ms') registered' (addr', user', bundle')
      where
        getNsOwnership name = case Map.lookup name owners of
            -- FIXME: Should we define a constant for the relay NS somewhere?
            -- FIXME: Should these instead be prepopulated in the map as being
            -- owned by NS.SockAddrCan 0 or something like that?
            Nothing -> if name == "" || name == "api" then Client else Unclaimed
            Just addr' -> if addr' == addr then Owner else Client
        getMsgOwnership = getNsOwnership . namespace . msgPath
        updateOwners ms' = updateMapByKeys addr owners
            (extractNewOwnerships  ms')
            (extractCeasedOwnerships ms')
        updateRegistered ms' = updateMosAtKey addr registered
            (extractNewSubscriptions ms')
            (extractCeasedSubscriptions ms')

-- FIXME: an actually useful name would be nice
check :: ClapiMethod -> ClapiMessage -> Bool
-- FIXME: would like to avoid taking length of Path
check method msg = msgMethod msg == method && (isNamespace $ msgPath msg)

extractNewOwnerships :: [ClapiMessage] -> [Name]
extractNewOwnerships = fmap (head . msgPath) . filter (check AssignType)

extractCeasedOwnerships :: [ClapiMessage] -> [Name]
extractCeasedOwnerships = fmap (head . msgPath) . filter (check Delete)

constMap :: (Ord k) => a -> [k] -> Map.Map k a
constMap a ks = Map.fromList $ (flip (,) a) <$> ks

updateMapByKeys :: (Ord k) => a -> Map.Map k a -> [k] -> [k] -> Map.Map k a
updateMapByKeys a m addedKeys removedKeys =
    Map.difference (Map.union (constMap a addedKeys) m) (constMap a removedKeys)

updateMosAtKey :: (Ord k, Ord a) => k -> Mos.Mos k a -> [a] -> [a] -> Mos.Mos k a
updateMosAtKey k m addedVals removedVals = m'
  where
    withAdded = foldr (Mos.insert k) m addedVals
    m' = foldr (Mos.delete k) withAdded removedVals

extractNewSubscriptions :: [ClapiMessage] -> [Path]
extractNewSubscriptions msgs = msgPath <$> filter ((== Subscribe) . msgMethod) msgs

extractCeasedSubscriptions :: [ClapiMessage] -> [Path]
extractCeasedSubscriptions msgs = msgPath <$> filter ((== Unsubscribe) . msgMethod) msgs

fanOutBundle :: (Ord i) => Mos.Mos i Path -> ClapiBundle ->
    Map.Map i ClapiBundle
fanOutBundle registered bundle = filterBundle bundle <$> registered

filterBundle :: ClapiBundle -> Set.Set Path -> ClapiBundle
filterBundle b ps = filter (\msg -> (msgPath msg) `elem` ps) b
