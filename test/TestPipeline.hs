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
    testCase "subscribe as client" testSubscribeAsClient
    ]

assertErrorMsg [msg] = assertEqual "blah" Error $ msgMethod msg
assertMsgPath path [msg] = assertEqual "mleh" path $ msgPath msg
assertOnlyKeysInMap expected m = assertEqual "keys in map" (Set.fromList expected) $ Map.keysSet m

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
    assertOnlyKeysInMap [43, 44] response
    let (subMsgs, otherMsgs) = mapPartition ((== Subscribe) . msgMethod) $ join <$> response
    mapM_ (assertEqual "subscription msgs" []) subMsgs
    mapM_ (assertEqual "other msgs" [msg ["hello"] Set]) otherMsgs


listServer :: (Monad m) => [a] -> Server b a m [b]
listServer as = listServer' as mempty
  where
    listServer' [] bs = return bs
    listServer' (a:as) bs = respond a >>= \b -> listServer' as (bs +| b)


trackerHelper :: (Monad m, Ord i) =>
    [(i, User, ClapiBundle)] -> m (Map.Map i [ClapiBundle])
trackerHelper as = collect <$> (runEffect $
    listServer as >>~ namespaceTracker >>~ echoMap dropDetails)
  where
    dropDetails (_, taggedMs) = snd <$> taggedMs
    collect :: (Ord i) => [[(i, ClapiBundle)]] -> Map.Map i [ClapiBundle]
    collect = joinM . fmap Map.fromList
