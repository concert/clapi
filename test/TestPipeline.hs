{-# LANGUAGE OverloadedStrings #-}
module TestPipeline where

import Test.HUnit (assertEqual, assertBool)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad (forever)

import Pipes (runEffect, liftIO)
import Pipes.Core (Client, Server, request, respond, (>>~))
import qualified Pipes.Prelude as PP
import Pipes.Safe (runSafeT)

import Tree ((+|))
import Types (ClapiBundle, ClapiMessage(..), ClapiMethod(..), ClapiValue(..))
import Server (User(..))
import Pipeline

import TestServer (echoMap)

tests = [
    testCase "subscribe to unclaimed forbidden" testSubscribeUnclaimed
    ]

assertErrorMsg [msg] = assertEqual "blah" Error $ msgMethod msg
assertMsgPath path [msg] = assertEqual "mleh" path $ msgPath msg

msg path method = CMessage path method [] []

testSubscribeUnclaimed = do
    response <- trackerHelper [(42, Alice, [msg path Subscribe])]
    assertEqual "single response set" 1 $ length response
    mapM_ (assertEqual "single msg" 1 . length) $ response
    (assertErrorMsg . snd) <$$> response
    (assertMsgPath path . snd) <$$> response
  where
    path = ["hello"]
    (<$$>) = mapM_ . mapM_


listServer :: (Monad m) => [a] -> Server b a m [b]
listServer as = listServer' as mempty
  where
    listServer' [] bs = return bs
    listServer' (a:as) bs = respond a >>= \b -> listServer' as (bs +| b)


trackerHelper :: (Monad m, Ord i) =>
    [(i, User, ClapiBundle)] -> m [[(i, ClapiBundle)]]
trackerHelper as = runEffect $
    listServer as >>~ namespaceTracker >>~ echoMap dropDetails
  where
    dropDetails (_, taggedMs) = snd <$> taggedMs
