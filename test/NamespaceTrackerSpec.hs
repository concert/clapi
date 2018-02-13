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
    ( Time(..), Interpolation(..), InterpolationLimit(..), WireValue(..)
    , FromRelayBundle(..), ToRelayBundle(..)
    , DataUpdateMessage(..)
    , FrDigest(..), FrpDigest(..), FrpErrorDigest(..)
    , TrDigest(..), TrpDigest(..), trpDigest, TrprDigest(..), trcdEmpty, TrcDigest(..)
    , frcdEmpty, FrcDigest(..)
    , ErrorIndex(..)
    , DataChange(..)
    , digestToRelayBundle, produceFromRelayBundle)
import Clapi.Types.Definitions (tupleDef)
import Clapi.Types.Digests
  ( OutboundDigest(..), InboundDigest(..)
  , InboundClientDigest(..), OutboundClientDigest(..)
  , OutboundProviderDigest(..), SubOp(..), DefOp(..))
import Clapi.Types.Path (Path, Seg, pattern Root, TypeName(..))
import Clapi.Types.UniqList (ulEmpty, ulSingle)
import Clapi.Types.AssocList (alSingleton, alEmpty, alFromList)
import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Server (neverDoAnything)
import Clapi.NamespaceTracker
  (nstProtocol, NstState(..), Ownership(..), Originator(..))
import qualified Clapi.Protocol as Protocol
import Clapi.Protocol (
  Protocol(..), Directed(..), fromDirected, wait, waitThen, sendFwd, sendRev,
  send, (<<->), runEffect, runProtocolIO, mapProtocol, waitThenFwdOnly, waitThenRevOnly)

type Owners i = Map Seg i

alice, bob :: T.Text
alice = "alice"
bob = "bob"

helloS :: Seg
helloS = [segq|hello|]

helloP = [pathq|/hello|]

ocdEmpty :: OutboundClientDigest
ocdEmpty = OutboundClientDigest
  { ocdContainerOps = mempty
  , ocdDefinitions = mempty
  , ocdTypeAssignments = mempty
  , ocdData = alEmpty
  , ocdErrors = mempty}

-- Collects responses until the identified client disconnects
collectAllResponsesUntil ::
    (Monad m, Ord i) =>
    i -> Protocol Void a Void (ServerEvent i b) m (Mol.Mol i b)
collectAllResponsesUntil i = inner mempty
  where
    inner bs = Protocol.waitThenRevOnly $ rev bs
    rev bs (ServerData i' b) = inner (Mol.append i' b bs)
    rev bs (ServerDisconnect i')
      | i' == i = return bs
      | otherwise = inner bs

spec :: Spec
spec = do
    it "Rejects ownership claim on pre-owned path" $
      let
        forTest = do
            claimHello alice
            expectRev $ Left $ Map.fromList
              [ (helloS, alice)
              ] -- FIXME: should API be owned?
            claimHello bob
            expectErrors bob $ Map.singleton
                (PathError helloP) ["Already owned by someone else guv"]
            expectRev $ Right $ ServerDisconnect bob
      in runEffect $ forTest <<-> nstProtocol <<-> blackHoleRelay
    it "Rejects empty claim" $
      let
        forTest = do
            sendFwd $ ClientData alice $ Trpd $ trpDigest helloS
            expectErrors alice $ Map.singleton
                (PathError helloP) ["Empty Claim"]
            expectRev $ Right $ ServerDisconnect alice
      in runEffect $ forTest <<-> nstProtocol <<-> blackHoleRelay
    it "Rejects owner subscriptions" $
      let
        subDs =
          [ trcdEmpty
              {trcdDataSubs = Map.singleton [pathq|/hello|] OpSubscribe}
          , trcdEmpty
              {trcdTypeSubs = Map.singleton (TypeName helloS helloS) OpSubscribe}
          ]
        forTest subD = do
            claimHello alice
            expectRev $ Left $ Map.fromList
              [ (helloS, alice)
              ] -- FIXME: should API be owned?
            sendFwd $ ClientData alice $ Trcd subD
            expectErrors alice $ Map.singleton
                (PathError helloP) ["Acted as client on own namespace"]
            expectRev $ Right $ ServerDisconnect alice
      in mapM_ (\subD -> runEffect $ forTest subD <<-> nstProtocol <<-> blackHoleRelay) subDs
    it "Has working client subscriptions" $
      let
        forTest = do
            subHello alice
            expectRev $ Right $ ServerData alice $ Frcd $ frcdEmpty
              { frcdData = alSingleton helloP $ textChange "f"
              , frcdDefinitions = Map.singleton helloTn helloDef0
              }
            expectRev $ Right $ ServerData alice $ Frcd $ frcdEmpty
              { frcdData = alSingleton helloP $ textChange "t" }
            sendFwd $ ClientData alice $ Trcd $ trcdEmpty
              {trcdDataSubs = Map.singleton helloP OpUnsubscribe}
            expectRev $ Right $ ServerData alice $ Frcd $ frcdEmpty
              { frcdDataUnsubs = Set.singleton helloP
              }
            expectRev $ Right $ ServerData alice $ Frcd $ frcdEmpty
              { frcdDefinitions = Map.singleton helloTn helloDef1
              }
        helloDef0 = OpDefine $ tupleDef "Yoho" alEmpty ILUninterpolated
        helloDef1 = OpDefine $ tupleDef "Hoyo" alEmpty ILUninterpolated
        fauxRelay = do
            i <- waitThenFwdOnly $ \(i, d) -> do
                lift (d `shouldBe` Icd (InboundClientDigest
                  { icdGets = Set.singleton helloP
                  , icdTypeGets = mempty
                  , icdContainerOps = mempty
                  , icdData = alEmpty
                  }))
                return i
            sendRev (i, Ocid $ OutboundClientDigest
              { ocdContainerOps = mempty
              , ocdDefinitions = Map.singleton helloTn helloDef0
              , ocdTypeAssignments = mempty
              , ocdData = alSingleton helloP $ textChange "f"
              , ocdErrors = mempty})
            sendRev (i, Ocd $ OutboundClientDigest
              { ocdContainerOps = mempty
              , ocdDefinitions = mempty
              , ocdTypeAssignments = mempty
              , ocdData = alFromList
                [ (helloP, textChange "t")
                , ([pathq|/nowhere|], textChange "banana")
                ]
              , ocdErrors = mempty})
            sendRev (i, Ocd $ OutboundClientDigest
              { ocdContainerOps = mempty
              , ocdDefinitions = Map.singleton helloTn helloDef1
              , ocdTypeAssignments = mempty
              , ocdData = alSingleton helloP $ textChange "w"
              , ocdErrors = mempty})
            blackHoleRelay
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
    it "Unsubscribes on client disconnect" $
      let
        forTest = do
            subHello alice
            sendFwd $ ClientDisconnect alice
            subHello bob
            expectRev $ Right $ ServerData bob $ Frcd $ frcdEmpty
              { frcdData = alSingleton helloP $ textChange "f" }
        fauxRelay = do
            waitThenFwdOnly $ const return ()
            waitThenFwdOnly $ \(i, d) -> sendRev (i, Ocid $ ocdEmpty
              {ocdData = alSingleton helloP $ textChange "f"})
            blackHoleRelay
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
  where
    blackHoleRelay = waitThenFwdOnly $ const blackHoleRelay
    expectRev e = waitThenRevOnly $ \e' -> lift $ e' `shouldBe` e
    expectErrors addr =
        expectRev . Right . ServerData addr . Frped . FrpErrorDigest
    claimHello addr = sendFwd $ ClientData addr $ Trpd $ trpDigest helloS
    subHello addr = sendFwd $ ClientData addr $ Trcd $ trcdEmpty
      {trcdDataSubs = Map.singleton helloP OpSubscribe}
    textChange s = ConstChange Nothing [WireValue (s :: T.Text)]
    helloTn = TypeName helloS helloS

--     it "Is idempotent when unsubscribing" $
--       let
--         ownedP = [pathq|/owned|]
--         owners = Map.singleton [segq|owned|] bob
--         protocol = forTest <<-> nstProtocol noopPub <<-> fakeRelay
--         forTest = do
--           sendFwd $ ClientData alice $ TRBClient $ RequestBundle [MsgUnsubscribe ownedP] []
--           sendFwd $ ClientDisconnect alice
--           resps <- collectAllResponsesUntil alice
--           lift $ resps `shouldBe` mempty
--       in
--         runEffect protocol
--     it "Unsubscribes client on disconnect" $ do
--         response <- trackerHelper _disconnectUnsubsBase
--         response `shouldBe` (Map.singleton bob $ [
--             FRBClient $ UpdateBundle [] [Left $ MsgAssignType helloP helloP]])
--     it "Supports client disconnecting then resubscribing" $ do
--         response <- trackerHelper $ _disconnectUnsubsBase ++ [
--             ClientData bob $ TRBClient $ RequestBundle [MsgSubscribe helloP] [],
--             ClientData alice $ TRBOwner $ UpdateBundle [] [Right $ dum helloP Add]]
--         response `shouldBe` (Map.singleton bob $ map FRBClient [
--             UpdateBundle [] [Left $ MsgAssignType helloP helloP],
--             UpdateBundle [] [Left $ MsgAssignType helloP helloP],
--             UpdateBundle [] [Right $ dum helloP Add]])
--     it "Disowns on owner disconnect" $ do
--         -- and unregisters clients
--         response <- pubTrackerHelper expectedPubs [
--             ClientData bob $ TRBOwner $ UpdateBundle [] [Left $ MsgAssignType [pathq|/fudge|] Root], -- Need something to disconnect at end
--             ClientData alice $ TRBOwner $ UpdateBundle [] [Left $ MsgAssignType helloP Root],
--             ClientData bob $ TRBClient $ RequestBundle [MsgSubscribe helloP] [],
--             ClientDisconnect alice,
--             -- No subscriber => no AssignType msg:
--             ClientData "dave" $ TRBOwner $ UpdateBundle [] [Left $ MsgAssignType helloP Root],
--             -- New subscriber:
--             ClientData "charlie" $ TRBClient $ RequestBundle [MsgSubscribe helloP] []]
--         assertOnlyKeysInMap [bob, "charlie"] response
--         assertMapValue bob [
--             FRBClient $ UpdateBundle [] [Left $ MsgAssignType helloP helloP], FRBClient $ UpdateBundle [] [Left $ MsgDelete helloP]] response
--         assertMapValue "charlie" [FRBClient $ UpdateBundle [] [Left $ MsgAssignType helloP helloP]] response
--     it "Forwards client mutations to provider" $
--       let
--         shouldBe' a = lift . shouldBe a
--         protocol = clientSide <<-> nstProtocol noopPub <<-> fauxRelay
--         clientSide = do
--           sendFwd $ ClientData alice $ Trpd $ TrpDigest [segq|alician|] mempty mempty mempty mempty
--           sendFwd $ ClientData bob $ Trpd $ TrpDigest [segq|bobian|] mempty mempty mempty mempty
--           waitThen undefined $ \d -> d `shouldBe'` (ServerData alice $ Frpd $ FrpDigest
--             [segq|alician|]
--             (Map.singleton [pathq|/a|] (Nothing, ulSingle [segq|aseg|]))
--             (Map.singleton [pathq|/aa|] $ ConstChange Nothing []))
--           waitThen undefined $ \d -> d `shouldBe'` (ServerData bob $ Frpd $ FrpDigest
--             [segq|bobian|]
--             (Map.singleton [pathq|/b|] (Nothing, ulSingle [segq|bseg|]))
--             (Map.singleton [pathq|/bb|] $ ConstChange Nothing []))
--         kiddy = Map.fromList
--           [ ([pathq|/alician/a|], (Nothing, ulSingle [segq|aseg|]))
--           , ([pathq|/bobian/b|], (Nothing, ulSingle [segq|bseg|]))
--           ]
--         daty = Map.fromList
--           [ ([pathq|/alician/aa|], ConstChange Nothing [])
--           , ([pathq|/bobian/bb|], ConstChange Nothing [])
--           ]
--         fauxRelay = do
--           waitThen (const $ return ()) undefined
--           waitThen (const $ return ()) undefined
--           sendRev (
--             Originator undefined, Opd $ OutboundProviderDigest kiddy daty)
--       in runEffect protocol
--     it "Returns validation errors" $
--       let
--         protocol = assertions <<-> nstProtocol noopPub <<-> errorSender
--         errorSender = sendRev (Originator alice, Ope errDig)
--         errDig = FrpErrorDigest [segq|myApi|]
--           $ Map.singleton GlobalError ["some error message"]
--         assertions = do
--             d <- wait
--             case d of
--                 (Rev (ServerData i ms)) -> lift $ do
--                     i `shouldBe` alice
--                     ms `shouldBe` (Frped errDig)
--       in
--         runEffect protocol
