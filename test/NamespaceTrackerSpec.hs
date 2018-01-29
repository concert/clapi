{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
module NamespaceTrackerSpec where

import Test.Hspec

import qualified Control.Concurrent.Chan.Unagi as U
import Control.Monad (forever, join, void)
import Control.Monad.Trans (lift)
import Data.Map (Map)
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
import Clapi.TH
import Clapi.Types
    ( Time(..), Interpolation(..), WireValue(..)
    , FromRelayBundle(..), ToRelayBundle(..)
    , DataUpdateMessage(..)
    , FrDigest(..), FrpDigest(..), FrpErrorDigest(..)
    , TrDigest(..), TrpDigest(..), TrprDigest(..)
    , ErrorIndex(..)
    , DataChange(..)
    , digestToRelayBundle, produceFromRelayBundle)
import Clapi.Types.Path (Path, Seg, pattern Root)
import Clapi.Types.UniqList (ulEmpty, ulSingle)
import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Server (neverDoAnything)
import Clapi.NamespaceTracker
  (nstProtocol, NstState(..), Ownership(..), Originator(..))
import qualified Clapi.Protocol as Protocol
import Clapi.Protocol (
  Protocol(..), Directed(..), fromDirected, wait, waitThen, sendFwd, sendRev,
  send, (<<->), runEffect, runProtocolIO, mapProtocol)
import Clapi.Relay
  ( OutboundDigest(..), InboundDigest(..), InboundClientDigest(..)
  , OutboundProviderDigest(..))

type Owners i = Map Seg i

helloS :: Seg
helloS = [segq|hello|]

spec :: Spec
spec = do
    it "Rejects ownership claim on pre-owned path" $
      let
        owners = Map.singleton helloS "relay itself"
        protocol = forTest <<-> nstProtocol noopPub <<-> fakeRelay
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
        protocol = forTest <<-> nstProtocol noopPub <<-> fakeRelay
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
        protocol = forTest <<-> nstProtocol noopPub <<-> fakeRelay
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
    it "Forwards client mutations to provider" $
      let
        shouldBe' a = lift . shouldBe a
        protocol = clientSide <<-> nstProtocol noopPub <<-> fauxRelay
        clientSide = do
          sendFwd $ ClientData alice $ Trpd $ TrpDigest [segq|alician|] mempty mempty mempty mempty
          sendFwd $ ClientData bob $ Trpd $ TrpDigest [segq|bobian|] mempty mempty mempty mempty
          waitThen undefined $ \d -> d `shouldBe'` (ServerData alice $ Frpd $ FrpDigest
            [segq|alician|]
            (Map.singleton [pathq|/a|] (Nothing, ulSingle [segq|aseg|]))
            (Map.singleton [pathq|/aa|] $ ConstChange Nothing []))
          waitThen undefined $ \d -> d `shouldBe'` (ServerData bob $ Frpd $ FrpDigest
            [segq|bobian|]
            (Map.singleton [pathq|/b|] (Nothing, ulSingle [segq|bseg|]))
            (Map.singleton [pathq|/bb|] $ ConstChange Nothing []))
        kiddy = Map.fromList
          [ ([pathq|/alician/a|], (Nothing, ulSingle [segq|aseg|]))
          , ([pathq|/bobian/b|], (Nothing, ulSingle [segq|bseg|]))
          ]
        daty = Map.fromList
          [ ([pathq|/alician/aa|], ConstChange Nothing [])
          , ([pathq|/bobian/bb|], ConstChange Nothing [])
          ]
        fauxRelay = do
          waitThen (const $ return ()) undefined
          waitThen (const $ return ()) undefined
          sendRev (
            Originator undefined, Opd $ OutboundProviderDigest kiddy daty)
      in runEffect protocol
    it "Returns validation errors" $
      let
        protocol = assertions <<-> nstProtocol noopPub <<-> errorSender
        errorSender = sendRev (Originator alice, Ope errDig)
        errDig = FrpErrorDigest [segq|myApi|]
          $ Map.singleton GlobalError ["some error message"]
        assertions = do
            d <- wait
            case d of
                (Rev (ServerData i ms)) -> lift $ do
                    i `shouldBe` alice
                    ms `shouldBe` (Frped errDig)
      in
        runEffect protocol
  where
    assertOnlyKeysInMap expected m = Map.keysSet m `shouldBe` Set.fromList expected
    assertSingleError i path errStrings response = undefined
        -- let bundles = (fromJust $ Map.lookup i response) :: [FromRelayBundle] in do
        -- length bundles `shouldBe` 1
        -- let (errs, ul) = ulae $ head bundles
        -- ul `shouldBe` 0
        -- length errs `shouldBe` 1
        -- mapM_ (assertErrorMsg errStrings) errs
        -- mapM_ (assertMsgPath path) errs
      where
        -- ulae (FRBOwner (OwnerRequestBundle errs dums)) = (errs, length dums)
    assertErrorMsg substrs msg = undefined
      --   mapM_ (\s -> getString msg `shouldSatisfy` T.isInfixOf s) substrs
      -- where
      --   getString (MsgError _ str) = str
    assertMsgPath path msg = undefined -- uMsgPath msg `shouldBe` path
    expectedPubs = map Map.fromList
      [ [(fudgeS, bob)]
      , [(fudgeS, bob), (helloS, alice)]
      , [(fudgeS, bob)]
      , [(fudgeS, bob), (helloS, "dave")]
      , [(helloS, "dave")]
      ]
    assertMapValue k a m = Map.lookup k m `shouldBe` Just a
    fudgeS = [segq|fudge|]

_disconnectUnsubsBase = undefined -- [
    -- ClientData alice $ TRBOwner $ UpdateBundle [] [Left $ MsgAssignType helloP Root],
    -- ClientData bob $ TRBClient $ RequestBundle [MsgSubscribe helloP] [],
    -- ClientDisconnect bob,
    -- -- Should miss this message:
    -- ClientData alice $ TRBOwner $ UpdateBundle [] [Right $ dum helloP Set]
    -- ]

data DataUpdateMethod
  = ConstSet
  | Set
  | Remove
  | Children

dum :: Path -> DataUpdateMethod -> DataUpdateMessage
dum path ConstSet = MsgConstSet path [] Nothing
dum path Set = MsgSet path 14 (Time 0 0) [] IConstant Nothing
dum path Remove = MsgRemove path 14 Nothing
dum path Children = MsgSetChildren path ulEmpty Nothing

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
    Protocol ((c, InboundDigest)) Void ((c, OutboundDigest)) Void m ()
fakeRelay = forever $ waitThen fwd undefined
  where
    fwd (ctx, inDig) = case inDig of
      Icd (InboundClientDigest _ _ _ _) -> undefined
      Ipd (TrpDigest {}) -> undefined
      Iprd (TrprDigest ns) -> undefined
    -- fwd (ctx, ClientRequest gets updates) = sendRev (ctx, ClientResponse (Left . flip MsgAssignType helloP <$> gets) [] updates)
    -- fwd (ctx, OwnerRequest updates) = sendRev (ctx, GoodOwnerResponse updates)

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

nstBounceProto = nstProtocol noopPub <<-> fakeRelay

pubTrackerHelper ::
    (Ord i, Show i) =>
    [Owners i] -> [ClientEvent i ToRelayBundle] ->
    IO (Map.Map i [FromRelayBundle])
pubTrackerHelper expectedPubs inMsgs = do
    pubList <- newMVar []
    rv <- trackerHelper' (listPub pubList) inMsgs
    actualPubs <- takeMVar pubList
    reverse actualPubs `shouldBe` expectedPubs
    return rv
  where
    listPub mv o = modifyMVar_ mv (\l -> return $ o : l)

trackerHelper = trackerHelper' noopPub

trackerHelper' ::
    forall i.  (Ord i, Show i) =>
    (Owners i -> IO ()) ->
    [ClientEvent i ToRelayBundle] ->
    IO (Map.Map i [FromRelayBundle])
trackerHelper' pub as =
    mapPack <$> gogo as' i (digester <<-> trackerProto <<-> fakeRelay)
  where
    digester = mapProtocol
      (fmap digestToRelayBundle) (fmap produceFromRelayBundle)
    trackerProto = nstProtocol pub
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
