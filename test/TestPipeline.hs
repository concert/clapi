{-# LANGUAGE OverloadedStrings #-}
module TestPipeline where

import Test.HUnit (assertEqual, assertBool)
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
import Tree ((+|))
import Types (ClapiBundle, ClapiMessage(..), ClapiMethod(..), ClapiValue(..))
import Server (User(..))
import Pipeline

import TestServer (echoMap)

tests = [
    testCase "owner-like msg to root path" testMessageToRoot,
    testCase "subscribe to unclaimed" testSubscribeUnclaimed,
    testCase "subscribe as owner" testSubscribeAsOwner,
    testCase "unsubscribe unsubscribed" testUnsubscribeWhenNotSubscribed,
    testCase "subscribe as client" testSubscribeAsClient,
    testCase "second owner forbidden" testSecondOwnerForbidden,
    testCase "claim unclaim in bunde" testClaimUnclaimInBundle,
    testCase "client disconnect unsubscribes" testClientDisconnectUnsubs,
    testCase "client disconnect resubscribes" testClientDisconnectUnsubsResubs,
    testCase "owner disconnect disowns" testOwnerDisconnectDisowns
    ]

assertErrorMsg [msg] = assertEqual "blah" Error $ msgMethod msg
assertMsgPath path [msg] = assertEqual "mleh" path $ msgPath msg
assertOnlyKeysInMap expected m = assertEqual "keys in map" (Set.fromList expected) $ Map.keysSet m
assertNoEmptyLists = assertBool "empty list" . all (/= [])

msg path method = CMessage path method [] []

assertSingleError i path response =
    let bundles = fromJust $ Map.lookup i response in do
    assertEqual "single bundle" 1 $ length bundles
    mapM_ (assertEqual "single msg" 1 . length) bundles
    mapM_ assertErrorMsg bundles
    mapM_ (assertMsgPath path) bundles


testMessageToRoot = do
    response <- trackerHelper [(42, Alice, [msg [] Error])]
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

testUnsubscribeWhenNotSubscribed = do
    response <- trackerHelper [(42, Alice, [msg [] Unsubscribe])]
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
    mapM_ (putStrLn . show) $ Map.toList response
    assertEqual "response check" (Map.fromList [
        (43, [[msg ["hello"] Remove], [msg ["hello"] Set]]),
        (44, [[msg ["hello"] Remove], [msg ["hello"] Set]]),
        (46, [[msg ["hello"] Remove]])
        ])
        response
    assertOnlyKeysInMap [43, 44] response
    mapM_ assertNoEmptyLists response
    let (subMsgs, otherMsgs) = mapPartition ((== Subscribe) . msgMethod) $ join <$> response
    mapM_ (assertEqual "subscription msgs" []) subMsgs
    mapM_ (assertEqual "other msgs" [msg ["hello"] Remove, msg ["hello"] Set]) otherMsgs

testSecondOwnerForbidden = do
    response <- trackerHelper [
        (42, Alice, [msg ["hello"] AssignType]),
        -- FIXME: Error is the only method a client is not allowed to
        -- send. However, our check for an error message doesn't check who sent
        -- it!
        (43, Alice, [msg ["hello"] Error])]
    assertEqual "single recipient" 1 $ Map.size response
    assertSingleError 43 ["hello"] response

testClaimUnclaimInBundle = undefined

_disconnectUnsubsBase = [
    (42, Alice, [msg ["hello"] AssignType]),
    (43, Alice, [msg ["hello"] Subscribe]),
    (43, Alice, []),
    (42, Alice, [msg ["hello"] Set])]

testClientDisconnectUnsubs = do
    response <- trackerHelper _disconnectUnsubsBase
    assertEqual "no msgs" 0 $ Map.size response

testClientDisconnectUnsubsResubs = do
    response <- trackerHelper $ _disconnectUnsubsBase ++ [
        (43, Alice, [msg ["hello"] Subscribe]),
        (42, Alice, [msg ["hello"] Add])]
    mapM_ (assertEqual "add msg" [msg ["hello"] Add]) $ join <$> response

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
    -- assertOnlyKeysInMap []
    error $ show response


listServer :: (Monad m) => [a] -> Server b a m [b]
listServer as = listServer' as mempty
  where
    listServer' [] bs = return bs
    listServer' (a:as) bs = respond a >>= \b -> listServer' as (bs +| b)


trackerHelper :: -- (Monad m, Ord i) =>
    (Ord i, Show i) =>
    [(i, User, ClapiBundle)] -> IO (Map.Map i [ClapiBundle])
trackerHelper as = collect <$> (runEffect $
    listServer as >>~ namespaceTracker mempty mempty >>~ echoMap dropDetails)
  where
    dropDetails (_, taggedMs) = join $ fmap foo taggedMs
    -- Adding in the extra Remove message simulates retreiving the current state
    -- from the deeper data layer
    foo (Client, m@CMessage {msgMethod = Subscribe}) = [(Client, m), (Client, msg (msgPath m) Remove)]
    foo taggedM = [taggedM]
    collect :: (Ord i) => [[(i, ClapiBundle)]] -> Map.Map i [ClapiBundle]
    collect = joinM . fmap Map.fromList
