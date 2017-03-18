{-# LANGUAGE OverloadedStrings #-}
module Pipeline where

import Control.Applicative (liftA2)
import Data.Bifunctor (bimap)
import Data.List (partition)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network.Socket as NS

import Pipes.Core (Proxy, request, respond)

import qualified Data.Map.Mos as Mos
import Path (Name, Path)
import Types (
    CanFail, ClapiBundle, ClapiMessage, ClapiMethod(..), ClapiValue(..),
    msgPath, msgMethod, msgArgs)
import Server (User)

namespace :: Path -> CanFail Name
namespace [] = Left "Root path does not imply a namespace"
namespace (n:ns) = Right n

data Ownership = Owner | Client | Unclaimed deriving (Eq, Show)
type BundleWithOwnership = [(Ownership, ClapiMessage)]

assignOwnership :: (Name -> Ownership) -> ClapiBundle ->
    CanFail BundleWithOwnership
assignOwnership getOwnership ms = liftA2 zip ownerships (pure ms)
  where
    namespaces = traverse (namespace . msgPath) ms
    ownerships = fmap getOwnership <$> namespaces

methodAllowed :: Ownership -> ClapiMethod -> Bool
methodAllowed Client m = m /= Error
methodAllowed _ Subscribe = False
methodAllowed _ Unsubscribe = False
methodAllowed _ _ = True

bounceOrForward :: BundleWithOwnership -> (ClapiBundle, BundleWithOwnership)
bounceOrForward bwo = (errorBundle, goodBwo)
  where
    (badBwo, goodBwo) = partition (not . uncurry methodAllowed . fmap msgMethod) bwo
    errorBundle = fmap (uncurry toErrMsg) badBwo
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
    (Monad m) =>
    (NS.SockAddr, User, ClapiBundle) ->
    Proxy [(Ownership, ClapiMessage)]  (NS.SockAddr, User, ClapiBundle)
    [(Ownership, ClapiMessage)] (User, [(Ownership, ClapiMessage)]) m ()
namespaceTracker =
    loop
        (mempty :: Map.Map Name NS.SockAddr)
        (mempty :: Mos.Mos Path NS.SockAddr)
  where
    loop owners registered (addr, user, ms) = do
        let (bounceMs, forwardMs) = unright $ bounceOrForward <$> assignOwnership getOwnership ms
        ms' <- respond (user, forwardMs)
        (addr', user', bundle') <- request ms'
        loop (owners' ms') (registered' ms') (addr', user', bundle')
      where
        getOwnership name = case Map.lookup name owners of
            -- FIXME: Should we define a constant for the relay NS somewhere?
            Nothing -> if name == "api" then Client else Unclaimed
            Just addr' -> if addr' == addr then Owner else Client
        unright (Right a) = a  -- FIXME: arg!
        owners' ms' = updateMapByKeys addr owners
            (extractNewOwnerships $ snd <$> ms')
            (extractCeasedOwnerships $ snd <$> ms')
        registered' :: [(a, ClapiMessage)] -> Mos.Mos Path NS.SockAddr
        registered' ms' = updateMosByKeys addr registered
            (extractNewSubscriptions $ snd <$> ms')
            (extractCeasedSubscriptions $ snd <$> ms')

-- FIXME: an actually useful name would be nice
check :: ClapiMethod -> ClapiMessage -> Bool
-- FIXME: would like to avoid taking length of Path
check method msg = msgMethod msg == method && (length $ msgPath msg) == 1

extractNewOwnerships :: [ClapiMessage] -> [Name]
extractNewOwnerships = fmap (head . msgPath) . filter (check AssignType)

extractCeasedOwnerships :: [ClapiMessage] -> [Name]
extractCeasedOwnerships = fmap (head . msgPath) . filter (check Delete)

constMap :: (Ord k) => a -> [k] -> Map.Map k a
constMap a ks = Map.fromList $ (flip (,) a) <$> ks

updateMapByKeys :: (Ord k) => a -> Map.Map k a -> [k] -> [k] -> Map.Map k a
updateMapByKeys a m addedKeys removedKeys =
    Map.difference (Map.union (constMap a addedKeys) m) (constMap a removedKeys)

updateMosByKeys :: (Ord k, Ord a) => a -> Mos.Mos k a -> [k] -> [k] -> Mos.Mos k a
updateMosByKeys a m addedKeys removedKeys =
    let sa = Set.singleton a in
    Mos.difference (Mos.union (constMap sa addedKeys) m) (constMap sa removedKeys)

extractNewSubscriptions :: [ClapiMessage] -> [Path]
extractNewSubscriptions msgs = msgPath <$> filter ((== Subscribe) . msgMethod) msgs

extractCeasedSubscriptions :: [ClapiMessage] -> [Path]
extractCeasedSubscriptions msgs = msgPath <$> filter ((== Unsubscribe) . msgMethod) msgs
