{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
module NamespaceTrackerSpec where

import Test.Hspec

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
import Clapi.PathQ
import Clapi.Types (
    Time(..), Interpolation(..), DataUpdateMessage(..), TreeUpdateMessage(..),
    MsgError(..), OwnerUpdateMessage(..), Msg(..), WireValue(..),
    UpdateBundle(..), RequestBundle(..), ToRelayBundle(..),
    FromRelayBundle(..), SubMessage(..), OwnerRequestBundle(..))
import Clapi.Types.Path (Path, Seg, pattern Root)
import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Server (neverDoAnything)
import Clapi.NamespaceTracker (namespaceTrackerProtocol, Ownership(..), Owners, Registered, Request(..), Response(..))
import qualified Clapi.Protocol as Protocol
import Clapi.Protocol (
  Protocol(..), Directed(..), fromDirected, wait, waitThen, sendFwd, sendRev,
  send, (<<->), runEffect, runProtocolIO)

helloS :: Seg
helloS = [segq|hello|]

spec :: Spec
spec = do
    it "Rejects ownership claim on pre-owned path" $
      let
        owners = Map.singleton helloS "relay itself"
        protocol = forTest <<-> namespaceTrackerProtocol noopPub owners mempty <<-> fakeRelay
        forTest = do
            sendFwd $ ClientData alice $ TRBOwner $ UpdateBundle [] [Left $ MsgAssignType helloP Root]
            sendFwd $ ClientDisconnect alice
            resps <- collectAllResponsesUntil alice
            lift $ assertOnlyKeysInMap [alice] resps
            lift $ assertSingleError alice helloP ["Path", "another"] resps
      in
        runEffect protocol
    it "Rejects owner subscription" $
      let
        aliceOwns = Map.singleton helloS "alice"
        protocol = forTest <<-> namespaceTrackerProtocol noopPub aliceOwns mempty <<-> fakeRelay
        forTest = do
          sendFwd $ ClientData alice $ TRBClient $ RequestBundle [MsgSubscribe helloP] []
          resps <- collectAllResponsesUntil alice
          lift $ assertOnlyKeysInMap [alice] resps
          lift $ assertSingleError alice helloP ["Request", "own"] resps
      in
        runEffect protocol
    it "Is idempotent when unsubscribing" $
      let
        ownedP = [pathq|/owned|]
        owners = Map.singleton [segq|owned|] bob
        protocol = forTest <<-> namespaceTrackerProtocol noopPub owners mempty <<-> fakeRelay
        forTest = do
          sendFwd $ ClientData alice $ TRBClient $ RequestBundle [MsgUnsubscribe ownedP] []
          sendFwd $ ClientDisconnect alice
          resps <- collectAllResponsesUntil alice
          lift $ resps `shouldBe` mempty
      in
        runEffect protocol
    it "Has working client subscriptions" $
        -- Get informed of changes by owner
        -- Doesn't get bounced any Subscription messages
        -- Can Unsubscribe
      let
        events = [
            ClientData alice $ TRBOwner $ UpdateBundle [] [Left $ MsgAssignType helloP Root],
            ClientData bob $ TRBClient $ RequestBundle [MsgSubscribe helloP] [],
            ClientData alice $ TRBOwner $ UpdateBundle [] [Right $ dum helloP Add],
            ClientDisconnect alice
          ]
      in do
        resps <- gogo events alice nstBounceProto
        resps `shouldBe` [
            ServerData bob $ FRBClient $ UpdateBundle [] [Left $ MsgAssignType helloP helloP],
            ServerData bob $ FRBClient $ UpdateBundle [] [Right $ dum helloP Add],
            ServerDisconnect alice
            ]
    it "Forbids second owner" $ do
        response <- trackerHelper [
            ClientData alice $ TRBOwner $ UpdateBundle [] [Left $ MsgAssignType helloP Root],
            ClientData bob $ TRBOwner $ UpdateBundle [MsgError helloP ""] []]
        Map.size response `shouldBe` 1
        assertSingleError bob helloP ["Path", "another"] response
    it "Supports claiming and unclaiming in same bundle" $ do
        response <- trackerHelper [
            ClientData alice $ TRBOwner $ UpdateBundle [] [Left $ MsgAssignType helloP Root, Left $ MsgDelete helloP],
            ClientData bob $ TRBOwner $ UpdateBundle [] [Left $ MsgAssignType helloP Root]]
        response `shouldBe` mempty
    it "Unsubscribes client on disconnect" $ do
        response <- trackerHelper _disconnectUnsubsBase
        response `shouldBe` (Map.singleton bob $ [
            FRBClient $ UpdateBundle [] [Left $ MsgAssignType helloP helloP]])
    it "Supports client disconnecting then resubscribing" $ do
        response <- trackerHelper $ _disconnectUnsubsBase ++ [
            ClientData bob $ TRBClient $ RequestBundle [MsgSubscribe helloP] [],
            ClientData alice $ TRBOwner $ UpdateBundle [] [Right $ dum helloP Add]]
        response `shouldBe` (Map.singleton bob $ map FRBClient [
            UpdateBundle [] [Left $ MsgAssignType helloP helloP],
            UpdateBundle [] [Left $ MsgAssignType helloP helloP],
            UpdateBundle [] [Right $ dum helloP Add]])
    it "Disowns on owner disconnect" $ do
        -- and unregisters clients
        response <- pubTrackerHelper expectedPubs [
            ClientData bob $ TRBOwner $ UpdateBundle [] [Left $ MsgAssignType [pathq|/fudge|] Root], -- Need something to disconnect at end
            ClientData alice $ TRBOwner $ UpdateBundle [] [Left $ MsgAssignType helloP Root],
            ClientData bob $ TRBClient $ RequestBundle [MsgSubscribe helloP] [],
            ClientDisconnect alice,
            -- No subscriber => no AssignType msg:
            ClientData "dave" $ TRBOwner $ UpdateBundle [] [Left $ MsgAssignType helloP Root],
            -- New subscriber:
            ClientData "charlie" $ TRBClient $ RequestBundle [MsgSubscribe helloP] []]
        assertOnlyKeysInMap [bob, "charlie"] response
        assertMapValue bob [
            FRBClient $ UpdateBundle [] [Left $ MsgAssignType helloP helloP], FRBClient $ UpdateBundle [] [Left $ MsgDelete helloP]] response
        assertMapValue "charlie" [FRBClient $ UpdateBundle [] [Left $ MsgAssignType helloP helloP]] response
    it "Forwards client sets" $ do
        response <- trackerHelper [
            ClientData alice $ TRBOwner $ UpdateBundle [] [Left $ MsgAssignType helloP Root],
            ClientData bob $ TRBClient $ RequestBundle [] [dum helloP Set]]
        assertOnlyKeysInMap [alice] response
        assertMapValue alice [FRBOwner $ OwnerRequestBundle [] [dum helloP Set]] response
    it "Returns validation errors" $
      let
        protocol = assertions <<-> namespaceTrackerProtocol noopPub mempty mempty <<-> errorSender
        errorSender = sendRev $ ((alice, Nothing), BadOwnerResponse [err])
        err = MsgError [pathq|/bad|] "wrong"
        assertions = do
            d <- wait
            case d of
                (Rev (ServerData i ms)) -> lift $ do
                    i `shouldBe` alice
                    ms `shouldBe` (FRBOwner $ OwnerRequestBundle [err] [])
      in
        runEffect protocol
  where
    assertOnlyKeysInMap expected m = Map.keysSet m `shouldBe` Set.fromList expected
    assertSingleError i path errStrings response =
        let bundles = (fromJust $ Map.lookup i response) :: [FromRelayBundle] in do
        length bundles `shouldBe` 1
        let (errs, ul) = ulae $ head bundles
        ul `shouldBe` 0
        length errs `shouldBe` 1
        mapM_ (assertErrorMsg errStrings) errs
        mapM_ (assertMsgPath path) errs
      where
        ulae (FRBOwner (OwnerRequestBundle errs dums)) = (errs, length dums)
    assertErrorMsg substrs msg =
        mapM_ (\s -> getString msg `shouldSatisfy` T.isInfixOf s) substrs
      where
        getString (MsgError _ str) = str
    assertMsgPath path msg = uMsgPath msg `shouldBe` path
    expectedPubs = map Map.fromList
      [ [(fudgeS, bob)]
      , [(fudgeS, bob), (helloS, alice)]
      , [(fudgeS, bob)]
      , [(fudgeS, bob), (helloS, "dave")]
      , [(helloS, "dave")]
      ]
    assertMapValue k a m = Map.lookup k m `shouldBe` Just a
    fudgeS = [segq|fudge|]

data DataUpdateMethod
  = Set
  | Add
  | Remove
  | Clear
  | Children

dum :: Path -> DataUpdateMethod -> DataUpdateMessage
dum path Set = MsgSet path (Time 0 0) [] IConstant Nothing Nothing
dum path Add = MsgAdd path (Time 0 0) [] IConstant Nothing Nothing
dum path Remove = MsgRemove path (Time 0 0) Nothing Nothing
dum path Clear = MsgClear path (Time 0 0) Nothing Nothing
dum path Children = MsgSetChildren path [] Nothing

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
    fwd (ctx, ClientRequest gets updates) = sendRev (ctx, ClientResponse (Left . flip MsgAssignType helloP <$> gets) [] updates)
    fwd (ctx, OwnerRequest updates) = sendRev (ctx, GoodOwnerResponse updates)

alice = "alice"
bob = "bob"

helloP = [pathq|/hello|]

noopPub :: Monad m => Owners i -> m ()
noopPub = void . return

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

_disconnectUnsubsBase = [
    ClientData alice $ TRBOwner $ UpdateBundle [] [Left $ MsgAssignType helloP Root],
    ClientData bob $ TRBClient $ RequestBundle [MsgSubscribe helloP] [],
    ClientDisconnect bob,
    -- Should miss this message:
    ClientData alice $ TRBOwner $ UpdateBundle [] [Right $ dum helloP Set]
    ]

pubTrackerHelper ::
    (Ord i, Show i) =>
    [Owners i] -> [ClientEvent i ToRelayBundle] ->
    IO (Map.Map i [FromRelayBundle])
pubTrackerHelper expectedPubs inMsgs = do
    pubList <- newMVar []
    rv <- trackerHelper' (listPub pubList) mempty mempty inMsgs
    actualPubs <- takeMVar pubList
    reverse actualPubs `shouldBe` expectedPubs
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
