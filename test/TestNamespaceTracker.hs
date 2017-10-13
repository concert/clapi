{-# LANGUAGE OverloadedStrings #-}
module TestNamespaceTracker where

import Test.HUnit (assertEqual, assertBool, assertFailure)
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (partition)
import Data.Maybe (fromJust)
import Control.Monad (forever, join)

import Pipes (runEffect, liftIO)
import Pipes.Core (Client, Server, request, respond, (>>~))
import qualified Pipes.Prelude as PP
import Pipes.Safe (runSafeT)

import Data.Map.Clapi (joinM)
import Clapi.Util ((+|))
import Clapi.Path (Path)
import Clapi.Types (Time(..), Interpolation(..), Message(..), msgPath', msgMethod', ClapiMethod(..), ClapiValue(..))
import Clapi.Server (User(..))
import Clapi.NamespaceTracker (namespaceTracker, Ownership(..), Owners, Registered)

import TestServer (echoMap)

tests = [
    testCase "owner-like msg to preowned path" testMessageToPreowned,
    testCase "subscribe to unclaimed" testSubscribeUnclaimed,
    testCase "subscribe as owner" testSubscribeAsOwner,
    testCase "unsubscribe unsubscribed" testUnsubscribeWhenNotSubscribed,
    testCase "subscribe as client" testSubscribeAsClient,
    testCase "second owner forbidden" testSecondOwnerForbidden,
    testCase "claim unclaim in bunde" testClaimUnclaimInBundle,
    testCase "client disconnect unsubscribes" testClientDisconnectUnsubs,
    testCase "client disconnects resubscribes" testClientDisconnectUnsubsResubs,
    testCase "owner disconnect disowns" testOwnerDisconnectDisowns
    ]

assertErrorMsg [msg] = assertEqual "blah" Error $ msgMethod' msg
assertMsgPath path [msg] = assertEqual "mleh" path $ msgPath' msg
assertOnlyKeysInMap expected m = assertEqual "keys in map" (Set.fromList expected) $ Map.keysSet m
-- assertNoEmptyLists = assertBool "empty list" . all (/= [])
assertMapKey k m = case Map.lookup k m of
    Nothing -> assertFailure (show k ++ " not found") >> undefined
    Just a -> return a
assertMapValue k a m =
    assertMapKey k m >>=
    assertEqual ("Map[" ++ show k ++ "]") a

msg :: Path -> ClapiMethod -> Message
msg path Error = MsgError path ""
msg path Set = MsgSet path (Time 0 0) [] IConstant Nothing Nothing
msg path Add = MsgAdd path (Time 0 0) [] IConstant Nothing Nothing
msg path Remove = MsgRemove path (Time 0 0) Nothing Nothing
msg path Clear = MsgClear path (Time 0 0) Nothing Nothing
msg path Subscribe = MsgSubscribe path
msg path Unsubscribe = MsgUnsubscribe path
msg path AssignType = MsgAssignType path []
msg path Delete = MsgDelete path
msg path Children = MsgChildren path []

assertSingleError i path response =
    let bundles = fromJust $ Map.lookup i response in do
    assertEqual "single bundle" 1 $ length bundles
    mapM_ (assertEqual "single msg" 1 . length) bundles
    mapM_ assertErrorMsg bundles
    mapM_ (assertMsgPath path) bundles


testMessageToPreowned =
    let owners = Map.singleton "" 0 in do
    response <- trackerHelper' owners mempty [(42, Alice, [msg [] Error])]
    assertEqual "single recipient" 1 $ Map.size response
    assertSingleError 42 [] response

testSubscribeUnclaimed = do
    response <- trackerHelper [(42, Alice, [msg ["hello"] Subscribe])]
    assertEqual "single recipient" 1 $ Map.size response
    assertSingleError 42 ["hello"] response

testSubscribeAsOwner = do
    response <- trackerHelper [
        (42, Alice, [msg ["hello"] AssignType]),
        (42, Alice, [msg ["hello"] Subscribe])]
    assertEqual "single recipient" 1 $ Map.size response
    assertSingleError 42 ["hello"] response

testUnsubscribeWhenNotSubscribed =
    let owners = Map.singleton "owned" 0 in do
    response <- trackerHelper' owners mempty [(42, Alice, [msg ["owned"] Unsubscribe])]
    assertEqual "no messages" 0 $ Map.size response

mapPartition :: (Ord k) => (a -> Bool) -> Map.Map k [a] ->
    (Map.Map k [a], Map.Map k [a])
mapPartition p m = let m' = partition p <$> m in (fst <$> m', snd <$> m')

testSubscribeAsClient = do
    -- Get informed of changes by owner
    -- Doesn't get bounced any Subscription messages
    -- Can Unsubscribe
    response <- trackerHelper [
        (42, Alice, [msg ["hello"] AssignType]),
        (43, Alice, [msg ["hello"] Subscribe]),
        (44, Alice, [msg ["hello"] Subscribe]),
        (45, Alice, [msg ["hello"] Subscribe, msg ["hello"] Unsubscribe]),
        (46, Alice, [msg ["hello"] Subscribe]),
        (46, Alice, [msg ["hello"] Unsubscribe]),
        (42, Alice, [msg ["hello"] Set])]
    assertEqual "response check" (Map.fromList [
        (43, [[msg ["hello"] Remove], [msg ["hello"] Set]]),
        (44, [[msg ["hello"] Remove], [msg ["hello"] Set]]),
        (46, [[msg ["hello"] Remove]])
        ])
        response

testSecondOwnerForbidden = do
    response <- trackerHelper [
        (42, Alice, [msg ["hello"] AssignType]),
        -- FIXME: Error is the only method a client is not allowed to
        -- send. However, our check for an error message doesn't check who sent
        -- it!
        (43, Alice, [msg ["hello"] Error])]
    assertEqual "single recipient" 1 $ Map.size response
    assertSingleError 43 ["hello"] response

testClaimUnclaimInBundle = do
    response <- trackerHelper [
        (42, Alice, [msg ["hello"] AssignType, msg ["hello"] Delete]),
        (43, Alice, [msg ["hello"] Subscribe])]
    assertSingleError 43 ["hello"] response

_disconnectUnsubsBase = [
    (42, Alice, [msg ["hello"] AssignType]),
    (43, Alice, [msg ["hello"] Subscribe]),
    (43, Alice, []),
    -- Should miss this message:
    (42, Alice, [msg ["hello"] Set])]

testClientDisconnectUnsubs = do
    response <- trackerHelper _disconnectUnsubsBase
    assertEqual "43: init msgs"
        (Map.singleton 43 [[msg ["hello"] Remove]])
        response

testClientDisconnectUnsubsResubs = do
    response <- trackerHelper $ _disconnectUnsubsBase ++ [
        (43, Alice, [msg ["hello"] Subscribe]),
        (42, Alice, [msg ["hello"] Add])]
    assertEqual "43: 2 * init msgs + add msg"
        (Map.singleton 43 [
            [msg ["hello"] Remove],
            [msg ["hello"] Remove],
            [msg ["hello"] Add]])
        response

testOwnerDisconnectDisowns = do
    -- and unregisters clients
    response <- trackerHelper [
        (42, Alice, [msg ["hello"] AssignType]),
        (43, Alice, [msg ["hello"] Subscribe]),
        (42, Alice, []),
        -- No owner => unclaimed => Subscribe disallowed:
        (44, Alice, [msg ["hello"] Subscribe]),
        -- No subscriber => no AssignType msg:
        (45, Alice, [msg ["hello"] AssignType])]
    assertOnlyKeysInMap [43, 44] response
    assertMapValue 43 [
        [msg ["hello"] Remove], [msg ["hello"] Delete]] response
    fourFour <- assertMapKey 44 response
    assertEqual "44: num msgs" 1 $ length fourFour
    assertErrorMsg $ head fourFour


listServer :: (Monad m) => [a] -> Server b a m [b]
listServer as = listServer' as mempty
  where
    listServer' [] bs = return bs
    listServer' (a:as) bs = respond a >>= \b -> listServer' as (bs +| b)


trackerHelper = trackerHelper' mempty mempty

trackerHelper' :: -- (Monad m, Ord i) =>
    (Ord i, Show i) =>
    Owners i -> Registered i -> [(i, User, [Message])] ->
    IO (Map.Map i [[Message]])
trackerHelper' owners registered as = collect <$> (runEffect $
    listServer as >>~ namespaceTracker owners registered >>~ echoMap dropDetails)
  where
    dropDetails (_, taggedMs) = join $ fmap foo taggedMs
    -- Adding in the extra Remove message simulates retreiving the current state
    -- from the deeper data layer
    foo (Client, m@MsgSubscribe {}) = [(Client, m), (Client, msg (msgPath' m) Remove)]
    foo taggedM = [taggedM]
    collect :: (Ord i) => [[(i, [Message])]] -> Map.Map i [[Message]]
    collect = joinM . fmap Map.fromList
