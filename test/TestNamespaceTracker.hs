{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
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

-- import Pipes (runEffect, liftIO)
import Pipes.Core (Client, Server, request, respond, (>>~))
import qualified Pipes.Prelude as PP
import Pipes.Safe (runSafeT)

import Data.Map.Clapi (joinM)
import Clapi.Util ((+|))
import Clapi.Path (Path, root)
import Clapi.Types (
    Time(..), Interpolation(..), Message(..), msgPath', msgMethod',
    ClapiMethod(..), ClapiValue(..))
import Clapi.Server (newAwu, ClientEvent(..), ServerEvent(..), neverDoAnything, AddrWithUser, awuAddr)
import Clapi.NamespaceTracker (namespaceTrackerProtocol, Ownership(..), Owners, Registered, Om(..), RoutableMessage(..))
import qualified Clapi.Protocol as Protocol
import Clapi.Protocol (
  Protocol(..), Directed(..), fromDirected, wait, waitThen, sendFwd, sendRev,
  send, (<->), runEffect, runProtocolIO)

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

assertMsgMethod :: ClapiMethod -> Message -> IO ()
assertMsgMethod meth msg = assertEqual errMsg meth $ msgMethod' msg
  where
    errMsg = "Message is not of type " ++ show meth

assertErrorMsg :: [T.Text] -> Message -> IO ()
assertErrorMsg substrs msg =
  do
    assertMsgMethod Error msg
    mapM_ (\s -> assertBool (stringErr s) (s `T.isInfixOf` string)) substrs
  where
    stringErr s =
        printf "Error %s message did not contain %s" (show string) (show s)
    getString (MsgError _ str) = str
    string = getString msg

assertMsgPath :: Path -> Message -> IO ()
assertMsgPath path msg = assertEqual "message path" path $ msgPath' msg


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

assertSingleError i path errStrings response =
    let bundles = (fromJust $ Map.lookup i response) :: [[Message]] in do
    assertEqual "single bundle" 1 $ length bundles
    mapM_ (assertEqual "single msg" 1 . length) bundles
    mapM_ (mapM_ $ assertErrorMsg errStrings) bundles
    mapM_ (mapM_ $ assertMsgPath path) bundles


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
    (Monad m, Show i, Show u) =>
    Protocol (ClientEvent (AddrWithUser i u) [Om] x) Void (ServerEvent (AddrWithUser i u) [RoutableMessage i]) Void m ()
fakeRelay = forever $ waitThen fwd undefined
  where
    fwd (ClientConnect _ _) = return ()
    fwd (ClientData i oms) = sendRev $ ServerData i $ map (mkRoutable i) oms
    fwd (ClientDisconnect i) = sendRev $ ServerDisconnect i
    -- FIXME: the relay MUSTN'T come back with unclaimed, ATM types allow
    mkRoutable _ (Unclaimed, m) = RoutableMessage (Right Owner) m
    mkRoutable i (Client, MsgSubscribe p) = RoutableMessage (Left $ awuAddr i) (MsgAssignType p [])
    mkRoutable _ (o, m) = RoutableMessage (Right o) m

alice = newAwu 42 "alice"
alice' = newAwu 43 "alice"
bob = newAwu 121 "bob"
charlie = newAwu 7 "charlie"
dave = newAwu 14 "dave"
ethel = newAwu 96 "ethel"

testMessageToPreowned =
  let
    owners = Map.singleton "" (newAwu 0 "relay itself")
    protocol = forTest <-> namespaceTrackerProtocol owners mempty <-> fakeRelay
    forTest = do
      sendFwd $ ClientData alice [msg root Error]
      sendFwd $ ClientDisconnect alice
      resps <- collectAllResponsesUntil alice
      lift $ assertOnlyKeysInMap [alice] resps
      lift $ assertSingleError alice root ["forbidden", "client"] resps
  in
    runEffect protocol


testSubscribeUnclaimed =
  let
    protocol = forTest <-> namespaceTrackerProtocol mempty mempty <-> fakeRelay
    forTest = do
      sendFwd $ ClientData alice [msg ["hello"] Subscribe]
      sendFwd $ ClientDisconnect alice
      resps <- collectAllResponsesUntil alice
      lift $ assertOnlyKeysInMap [alice] resps
      lift $ assertSingleError alice ["hello"] ["forbidden", "owner"] resps
  in
    runEffect protocol

testSubscribeAsOwner =
  let
    protocol = forTest <-> namespaceTrackerProtocol mempty mempty <-> fakeRelay
    forTest = do
      sendFwd $ ClientData alice [msg ["hello"] AssignType]
      -- FIXME: single bundle too?
      sendFwd $ ClientData alice [msg ["hello"] Subscribe]
      sendFwd $ ClientDisconnect alice
      resps <- collectAllResponsesUntil alice
      lift $ assertOnlyKeysInMap [alice] resps
      lift $ assertSingleError alice ["hello"] ["forbidden", "owner"] resps
  in
    runEffect protocol

testUnsubscribeWhenNotSubscribed =
  let
    owners = Map.singleton "owned" bob
    protocol = forTest <-> namespaceTrackerProtocol owners mempty <-> fakeRelay
    forTest = do
      sendFwd $ ClientData alice [msg ["owned"] Unsubscribe]
      sendFwd $ ClientDisconnect alice
      resps <- collectAllResponsesUntil alice
      lift $ assertBool "empty" (null resps)
  in
    runEffect protocol

gogo ::
    (Eq i) =>
    [ClientEvent i a b] ->
    i ->
    Protocol
        (ClientEvent i a b) Void
        (ServerEvent i a) Void IO () ->
    IO [ServerEvent i a]
gogo as i p =
  do
    (toProtoIn, toProtoOut) <- U.newChan
    mapM_ (U.writeChan toProtoIn) as
    (fromProtoIn, fromProtoOut) <- U.newChan
    runProtocolIO
        (U.readChan toProtoOut) (error "bad times")
        (U.writeChan fromProtoIn) neverDoAnything
        (untilDisconnect i <-> p)
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

nstBounceProto = namespaceTrackerProtocol mempty mempty <-> fakeRelay

testSubscribeAsClient =
    -- Get informed of changes by owner
    -- Doesn't get bounced any Subscription messages
    -- Can Unsubscribe
  let
    events = [
        ClientData alice [msg ["hello"] AssignType],
        ClientData bob [msg ["hello"] Subscribe],
        ClientData alice [msg ["hello"] Add],
        ClientDisconnect alice
      ]
  in do
    resps <- gogo events alice nstBounceProto
    assertEqual "resps" [
        -- Because the fake server bounces everything you get subscribes
        -- instead of data here
        ServerData bob [msg ["hello"] AssignType],
        ServerData bob [msg ["hello"] Add],
        ServerData bob [msg ["hello"] Delete],
        ServerDisconnect alice
        ] resps

testSecondOwnerForbidden = do
    response <- trackerHelper [
        ClientData alice [msg ["hello"] AssignType],
        -- FIXME: Error is the only method a client is not allowed to
        -- send. However, our check for an error message doesn't check who sent
        -- it!
        ClientData alice' [msg ["hello"] Error]]
    assertEqual "single recipient" 1 $ Map.size response
    assertSingleError alice' ["hello"] ["forbidden", "client"] response

testClaimUnclaimInBundle = do
    response <- trackerHelper [
        ClientData alice [msg ["hello"] AssignType, msg ["hello"] Delete],
        ClientData alice' [msg ["hello"] Subscribe]]
    assertSingleError alice' ["hello"] ["forbidden", "owner"] response

_disconnectUnsubsBase = [
    ClientData alice [msg ["hello"] AssignType],
    ClientData alice' [msg ["hello"] Subscribe],
    ClientDisconnect alice',
    -- Should miss this message:
    ClientData alice [msg ["hello"] Set]
    ]

testClientDisconnectUnsubs = do
    response <- trackerHelper _disconnectUnsubsBase
    assertEqual "alice': init msgs"
        (Map.singleton alice' [[msg ["hello"] AssignType]])
        response

testClientDisconnectUnsubsResubs = do
    response <- trackerHelper $ _disconnectUnsubsBase ++ [
        ClientData alice' [msg ["hello"] Subscribe],
        ClientData alice [msg ["hello"] Add]]
    assertEqual "alice': 2 * init msgs + add msg"
        (Map.singleton alice' [
            [msg ["hello"] AssignType],
            [msg ["hello"] AssignType],
            [msg ["hello"] Add],
            [msg ["hello"] Delete]]) -- End of test disconnect
        response

testOwnerDisconnectDisowns = do
    -- and unregisters clients
    response <- trackerHelper [
        ClientData alice' [msg ["fudge"] AssignType], -- Need something to disconnect at end
        ClientData alice [msg ["hello"] AssignType],
        ClientData alice' [msg ["hello"] Subscribe],
        ClientDisconnect alice,
        -- No owner => unclaimed => Subscribe disallowed:
        ClientData alice'' [msg ["hello"] Subscribe],
        -- No subscriber => no AssignType msg:
        ClientData alice''' [msg ["hello"] AssignType]]
    assertOnlyKeysInMap [alice', alice''] response
    assertMapValue alice' [
        [msg ["hello"] AssignType], [msg ["hello"] Delete]] response
    assertSingleError alice'' ["hello"] ["forbidden"] response
  where
    alice'' = newAwu 44 "alice"
    alice''' = newAwu 45 "alice"

trackerHelper = trackerHelper' mempty mempty

trackerHelper' :: -- (Monad m, Ord i) =>
    forall i u.  (Ord i, Show i, Ord u, Show u) =>
    Owners (AddrWithUser i u) -> Registered (AddrWithUser i u) -> [ClientEvent (AddrWithUser i u) [Message] ()] ->
    IO (Map.Map (AddrWithUser i u) [[Message]])
trackerHelper' owners registered as =
    -- listServer as >>~ namespaceTracker owners registered >>~ echoMap dropDetails)
    mapPack <$> gogo as' i (trackerProto <-> fakeRelay)
  where
    trackerProto = namespaceTrackerProtocol owners registered
    i = i' $ head as
    i' :: ClientEvent (AddrWithUser i u) [Message] () -> AddrWithUser i u
    i' (ClientData i _) = i
    i' (ClientConnect i _) = i
    as' :: [ClientEvent (AddrWithUser i u) [Message] ()]
    as' = as ++ [ClientDisconnect i]
    mapPack :: [ServerEvent (AddrWithUser i u) [Message]] -> Map.Map (AddrWithUser i u) [[Message]]
    mapPack [] = Map.empty
    mapPack ((ServerDisconnect e):es) = mapPack es
    mapPack ((ServerData i a):es) = Map.insertWith (++) i [a] (mapPack es)
