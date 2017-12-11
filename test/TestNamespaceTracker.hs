{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes #-}
module TestNamespaceTracker where

import Test.HUnit (assertEqual, assertBool, assertFailure)
import Test.Framework.Providers.HUnit (testCase)

import qualified Control.Concurrent.Chan.Unagi as U
import Control.Monad (forever, join, void)
import Control.Monad.Trans (lift)
import qualified Data.Map as Map
import qualified Data.Map.Mol as Mol
import qualified Data.Set as Set
import Data.List (partition)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Void
import Text.Printf
import Control.Concurrent.MVar

import Data.Map.Clapi (joinM)
import Clapi.Util ((+|))
import Clapi.Path (Path, root)
import Clapi.PathQ
import Clapi.Types (
    Time(..), Interpolation(..), DataUpdateMessage(..), TreeUpdateMessage(..),
    UMsgError(..), OwnerUpdateMessage(..), UMsg(..), ClapiValue(..),
    UpdateBundle(..), RequestBundle(..), ToRelayBundle(..),
    FromRelayBundle(..), SubMessage(..), OwnerRequestBundle(..))
import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Server (neverDoAnything)
import Clapi.NamespaceTracker (namespaceTrackerProtocol, Ownership(..), Owners, Registered, Request(..), Response(..))
import qualified Clapi.Protocol as Protocol
import Clapi.Protocol (
  Protocol(..), Directed(..), fromDirected, wait, waitThen, sendFwd, sendRev,
  send, (<<->), runEffect, runProtocolIO)

tests = [
    testCase "owner-like msg to preowned path" testMessageToPreowned,
    testCase "subscribe as owner" testSubscribeAsOwner,
    testCase "unsubscribe unsubscribed" testUnsubscribeWhenNotSubscribed,
    testCase "subscribe as client" testSubscribeAsClient,
    testCase "second owner forbidden" testSecondOwnerForbidden,
    testCase "claim unclaim in bundle" testClaimUnclaimInBundle,
    testCase "client disconnect unsubscribes" testClientDisconnectUnsubs,
    testCase "client disconnects resubscribes" testClientDisconnectUnsubsResubs,
    testCase "owner disconnect disowns" testOwnerDisconnectDisowns,
    testCase "client set forwarded to owner" testClientSetForwarded,
    testCase "direct errors fanout" testValidationErrorReturned
    ]

assertErrorMsg :: [T.Text] -> UMsgError -> IO ()
assertErrorMsg substrs msg =
  do
    mapM_ (\s -> assertBool (stringErr s) (s `T.isInfixOf` string)) substrs
  where
    stringErr s =
        printf "Error %s message did not contain %s" (show string) (show s)
    getString (UMsgError _ str) = str
    string = getString msg

assertMsgPath :: UMsg msg => Path -> msg -> IO ()
assertMsgPath path msg = assertEqual "message path" path $ uMsgPath msg

assertOnlyKeysInMap :: (Ord k, Show k) => [k] -> Map.Map k a -> IO ()
assertOnlyKeysInMap expected m =
    assertEqual "keys in map" (Set.fromList expected) $ Map.keysSet m


assertMapKey :: (Ord k, Show k) => k -> Map.Map k a -> IO a
assertMapKey k m = case Map.lookup k m of
    Nothing -> assertFailure (show k ++ " not found") >> undefined
    Just a -> return a


assertMapValue ::
    (Ord k, Show k, Eq a, Show a) => k -> a -> Map.Map k a -> IO ()
assertMapValue k a m =
    assertMapKey k m >>=
    assertEqual ("Map[" ++ show k ++ "]") a

data DataUpdateMethod
  = Set
  | Add
  | Remove
  | Clear
  | Children

dum :: Path -> DataUpdateMethod -> DataUpdateMessage
dum path Set = UMsgSet path (Time 0 0) [] IConstant Nothing Nothing
dum path Add = UMsgAdd path (Time 0 0) [] IConstant Nothing Nothing
dum path Remove = UMsgRemove path (Time 0 0) Nothing Nothing
dum path Clear = UMsgClear path (Time 0 0) Nothing Nothing
dum path Children = UMsgSetChildren path [] Nothing

assertSingleError i path errStrings response =
    let bundles = (fromJust $ Map.lookup i response) :: [FromRelayBundle] in do
    assertEqual "single bundle" 1 $ length bundles
    let (errs, ul) = ulae $ head bundles
    assertEqual "No updates" 0 ul
    assertEqual "single error" 1 $ length errs
    mapM_ (assertErrorMsg errStrings) errs
    mapM_ (assertMsgPath path) errs
  where
    ulae (FRBOwner (OwnerRequestBundle errs dums)) = (errs, length dums)
    ulae (FRBClient (UpdateBundle errs oums)) = (errs, length oums)


waitN :: (Monad m) => Int -> Protocol a a' b' b m [Directed a b]
waitN n = inner n mempty
  where
    inner 0 ds = return ds
    inner n ds = wait >>= \d -> inner (n - 1) (d:ds)


-- Collects responses until the identified client disconnects
collectAllResponsesUntil ::
    (Monad m, Ord i) =>
    i -> Protocol Void a Void (ServerEvent i b) m (Mol.Mol i b)
collectAllResponsesUntil i = inner mempty
  where
    inner bs = waitThen undefined (rev bs)
    rev bs (ServerData i' b) = inner (Mol.append i' b bs)
    rev bs (ServerDisconnect i')
      | i' == i = return bs
      | otherwise = inner bs

fakeRelay ::
    (Monad m, Show c) =>
    Protocol ((c, Request)) Void ((c, Response)) Void m ()
fakeRelay = forever $ waitThen fwd undefined
  where
    fwd (ctx, ClientRequest gets updates) = sendRev (ctx, ClientResponse (Left . flip UMsgAssignType helloP <$> gets) [] updates)
    fwd (ctx, OwnerRequest updates) = sendRev (ctx, GoodOwnerResponse updates)

alice = "alice"
bob = "bob"

helloP = [pathq|/hello|]

noopPub :: Monad m => Owners i -> m ()
noopPub = void . return

testMessageToPreowned =
  let
    owners = Map.singleton "hello" "relay itself"
    protocol = forTest <<-> namespaceTrackerProtocol noopPub owners mempty <<-> fakeRelay
    forTest = do
      sendFwd $ ClientData alice $ TRBOwner $ UpdateBundle [] [Left $ UMsgAssignType helloP root]
      sendFwd $ ClientDisconnect alice
      resps <- collectAllResponsesUntil alice
      lift $ assertOnlyKeysInMap [alice] resps
      lift $ assertSingleError alice helloP ["Path", "another"] resps
  in
    runEffect protocol

testSubscribeAsOwner =
  let
    aliceOwns = Map.singleton "hello" "alice"
    protocol = forTest <<-> namespaceTrackerProtocol noopPub aliceOwns mempty <<-> fakeRelay
    forTest = do
      sendFwd $ ClientData alice $ TRBClient $ RequestBundle [UMsgSubscribe helloP] []
      resps <- collectAllResponsesUntil alice
      lift $ assertOnlyKeysInMap [alice] resps
      lift $ assertSingleError alice helloP ["Request", "own"] resps
  in
    runEffect protocol

testUnsubscribeWhenNotSubscribed =
  let
    ownedP = [pathq|/owned|]
    owners = Map.singleton "owned" bob
    protocol = forTest <<-> namespaceTrackerProtocol noopPub owners mempty <<-> fakeRelay
    forTest = do
      sendFwd $ ClientData alice $ TRBClient $ RequestBundle [UMsgUnsubscribe ownedP] []
      sendFwd $ ClientDisconnect alice
      resps <- collectAllResponsesUntil alice
      lift $ assertEqual "idempotent" mempty resps
  in
    runEffect protocol

testValidationErrorReturned =
  let
    protocol = assertions <<-> namespaceTrackerProtocol noopPub mempty mempty <<-> errorSender
    errorSender = sendRev $ ((alice, Nothing), BadOwnerResponse [err])
    err = UMsgError [pathq|/bad|] "wrong"
    assertions = do
        d <- wait
        case d of
            (Rev (ServerData i ms)) -> lift $ do
                assertEqual "recipient" i alice
                assertEqual "errs" (FRBOwner $ OwnerRequestBundle [err] []) ms
  in
    runEffect protocol

gogo ::
    (Eq i) =>
    [ClientEvent i a] ->
    i ->
    Protocol
        (ClientEvent i a) Void
        (ServerEvent i b) Void IO () ->
    IO [ServerEvent i b]
gogo as i p =
  do
    (toProtoIn, toProtoOut) <- U.newChan
    mapM_ (U.writeChan toProtoIn) as
    (fromProtoIn, fromProtoOut) <- U.newChan
    runProtocolIO
        (U.readChan toProtoOut) (error "bad times")
        (U.writeChan fromProtoIn) neverDoAnything
        (untilDisconnect i <<-> p)
    longHand i fromProtoOut


longHand :: (Eq i) => i -> U.OutChan (ServerEvent i a) -> IO [ServerEvent i a]
longHand i chan = reverse <$> inner []
  where
    inner es = U.readChan chan >>= onEvent es
    onEvent es e@(ServerData _ _) = inner (e:es)
    onEvent es e@(ServerDisconnect i')
        | i == i' = return (e:es)
        | otherwise = inner (e:es)

untilDisconnect ::
    (Eq i, Monad m) => i -> Protocol a a (ServerEvent i b) (ServerEvent i b) m ()
untilDisconnect i = waitThen fwd next
  where
    fwd m = sendFwd m >> untilDisconnect i
    next e@(ServerData _ _) = sendRev e >> untilDisconnect i
    next e@(ServerDisconnect i')
        | i == i' = sendRev e
        | otherwise = sendRev e >> untilDisconnect i

nstBounceProto = namespaceTrackerProtocol noopPub mempty mempty <<-> fakeRelay

testSubscribeAsClient =
    -- Get informed of changes by owner
    -- Doesn't get bounced any Subscription messages
    -- Can Unsubscribe
  let
    events = [
        ClientData alice $ TRBOwner $ UpdateBundle [] [Left $ UMsgAssignType helloP root],
        ClientData bob $ TRBClient $ RequestBundle [UMsgSubscribe helloP] [],
        ClientData alice $ TRBOwner $ UpdateBundle [] [Right $ dum helloP Add],
        ClientDisconnect alice
      ]
  in do
    resps <- gogo events alice nstBounceProto
    assertEqual "resps" [
        ServerData bob $ FRBClient $ UpdateBundle [] [Left $ UMsgAssignType helloP helloP],
        ServerData bob $ FRBClient $ UpdateBundle [] [Right $ dum helloP Add],
        ServerDisconnect alice
        ] resps

testSecondOwnerForbidden = do
    response <- trackerHelper [
        ClientData alice $ TRBOwner $ UpdateBundle [] [Left $ UMsgAssignType helloP root],
        ClientData bob $ TRBOwner $ UpdateBundle [UMsgError helloP ""] []]
    assertEqual "single recipient" 1 $ Map.size response
    assertSingleError bob helloP ["Path", "another"] response

testClaimUnclaimInBundle = do
    response <- trackerHelper [
        ClientData alice $ TRBOwner $ UpdateBundle [] [Left $ UMsgAssignType helloP root, Left $ UMsgDelete helloP],
        ClientData bob $ TRBOwner $ UpdateBundle [] [Left $ UMsgAssignType helloP root]]
    assertEqual "all good" mempty response

_disconnectUnsubsBase = [
    ClientData alice $ TRBOwner $ UpdateBundle [] [Left $ UMsgAssignType helloP root],
    ClientData bob $ TRBClient $ RequestBundle [UMsgSubscribe helloP] [],
    ClientDisconnect bob,
    -- Should miss this message:
    ClientData alice $ TRBOwner $ UpdateBundle [] [Right $ dum helloP Set]
    ]

testClientDisconnectUnsubs = do
    response <- trackerHelper _disconnectUnsubsBase
    assertEqual "bob: init msgs"
        (Map.singleton bob $ [FRBClient $ UpdateBundle [] [Left $ UMsgAssignType helloP helloP]])
        response

testClientDisconnectUnsubsResubs = do
    response <- trackerHelper $ _disconnectUnsubsBase ++ [
        ClientData bob $ TRBClient $ RequestBundle [UMsgSubscribe helloP] [],
        ClientData alice $ TRBOwner $ UpdateBundle [] [Right $ dum helloP Add]]
    assertEqual "bob: 2 * init msgs + add msg"
        (Map.singleton bob $ map FRBClient [
            UpdateBundle [] [Left $ UMsgAssignType helloP helloP],
            UpdateBundle [] [Left $ UMsgAssignType helloP helloP],
            UpdateBundle [] [Right $ dum helloP Add]])
        response

testOwnerDisconnectDisowns = do
    -- and unregisters clients
    response <- pubTrackerHelper expectedPubs [
        ClientData bob $ TRBOwner $ UpdateBundle [] [Left $ UMsgAssignType [pathq|/fudge|] root], -- Need something to disconnect at end
        ClientData alice $ TRBOwner $ UpdateBundle [] [Left $ UMsgAssignType helloP root],
        ClientData bob $ TRBClient $ RequestBundle [UMsgSubscribe helloP] [],
        ClientDisconnect alice,
        -- No subscriber => no AssignType msg:
        ClientData "dave" $ TRBOwner $ UpdateBundle [] [Left $ UMsgAssignType helloP root],
        -- New subscriber:
        ClientData "charlie" $ TRBClient $ RequestBundle [UMsgSubscribe helloP] []]
    assertOnlyKeysInMap [bob, "charlie"] response
    assertMapValue bob [
        FRBClient $ UpdateBundle [] [Left $ UMsgAssignType helloP helloP], FRBClient $ UpdateBundle [] [Left $ UMsgDelete helloP]] response
    assertMapValue "charlie" [FRBClient $ UpdateBundle [] [Left $ UMsgAssignType helloP helloP]] response
  where
    expectedPubs = map Map.fromList
      [ [("fudge", bob)]
      , [("fudge", bob), ("hello", alice)]
      , [("fudge", bob)]
      , [("fudge", bob), ("hello", "dave")]
      , [("hello", "dave")]
      ]

testClientSetForwarded = do
    response <- trackerHelper [
        ClientData alice $ TRBOwner $ UpdateBundle [] [Left $ UMsgAssignType helloP root],
        ClientData bob $ TRBClient $ RequestBundle [] [dum helloP Set]]
    assertOnlyKeysInMap [alice] response
    assertMapValue alice [FRBOwner $ OwnerRequestBundle [] [dum helloP Set]] response

pubTrackerHelper ::
    (Ord i, Show i) =>
    [Owners i] -> [ClientEvent i ToRelayBundle] ->
    IO (Map.Map i [FromRelayBundle])
pubTrackerHelper expectedPubs inMsgs = do
    pubList <- newMVar []
    rv <- trackerHelper' (listPub pubList) mempty mempty inMsgs
    actualPubs <- takeMVar pubList
    assertEqual "Publishes" expectedPubs (reverse actualPubs)
    return rv
  where
    listPub mv o = modifyMVar_ mv (\l -> return $ o : l)

trackerHelper = trackerHelper' noopPub mempty mempty

trackerHelper' ::
    forall i.  (Ord i, Show i) =>
    (Owners i -> IO ()) ->
    Owners i -> Registered i -> [ClientEvent i ToRelayBundle] ->
    IO (Map.Map i [FromRelayBundle])
trackerHelper' pub owners registered as =
    mapPack <$> gogo as' i (trackerProto <<-> fakeRelay)
  where
    trackerProto = namespaceTrackerProtocol pub owners registered
    i = i' $ head as
    i' :: ClientEvent i ToRelayBundle -> i
    i' (ClientData i _) = i
    i' (ClientConnect i) = i
    as' :: [ClientEvent i ToRelayBundle]
    as' = as ++ [ClientDisconnect i]
    mapPack :: [ServerEvent i FromRelayBundle] -> Map.Map i [FromRelayBundle]
    mapPack [] = Map.empty
    mapPack ((ServerDisconnect e):es) = mapPack es
    mapPack ((ServerData i a):es) = Map.insertWith (++) i [a] (mapPack es)
