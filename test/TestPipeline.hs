{-# LANGUAGE OverloadedStrings #-}
module TestPipeline where

import Test.HUnit (assertEqual, assertBool)
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Monad (forever)

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
    testCase "subscribe to claimed " testSubscribeClaimed
    ]

assertErrorMsg [msg] = assertEqual "blah" Error $ msgMethod msg
assertMsgPath path [msg] = assertEqual "mleh" path $ msgPath msg

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

testSubscribeClaimed = do
    response <- trackerHelper [
        (42, Alice, [msg ["hello"] AssignType]),
        (42, Alice, [msg ["hello"] Subscribe])]
    assertEqual "single recipient" 1 $ Map.size response
    assertSingleError 42 ["hello"] response


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
