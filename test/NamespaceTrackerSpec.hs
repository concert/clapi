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
  , InboundClientDigest(..), OutboundClientDigest(..), outboundClientDigest
  , OutboundProviderDigest(..), SubOp(..), DefOp(..))
import Clapi.Types.Path (Path, Seg, pattern Root, TypeName(..))
import Clapi.Types.UniqList (ulEmpty, ulSingle)
import Clapi.Types.AssocList (alSingleton, alEmpty, alFromList)
import Clapi.Types.SequenceOps (SequenceOp(..))
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
                (PathError helloP) ["Empty claim"]
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
            waitThenFwdOnly $ const return ()
            relayNoMore
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
            relayNoMore
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
    it "Disowns on owner disconnect" $
      -- And unsubs clients
      let
        byeP = [pathq|/bye|]
        forTest = do
            sendFwd $ ClientData bob $ Trcd $ trcdEmpty
              {trcdDataSubs = Map.fromList
                [ (helloP, OpSubscribe)
                , (byeP, OpSubscribe)
                ]}
            expectRev $ Right $ ServerData bob $ Frcd $ frcdEmpty
              { frcdData = alFromList
                [ (helloP, textChange "f")
                , (byeP, textChange "t")
              ]}
            claimHello alice
            expectRev $ Left $ Map.fromList
              [ (helloS, alice)
              ] -- FIXME: should API be owned?
            sendFwd $ ClientDisconnect alice
            expectRev $ Left $ Map.fromList
              [] -- FIXME: should API be owned?
            expectRev $ Right $ ServerData bob $ Frcd $ frcdEmpty
              { frcdDataUnsubs = Set.singleton helloP }
            expectRev $ Right $ ServerData bob $ Frcd $ frcdEmpty
              { frcdData = alSingleton byeP $ textChange "f" }
        fauxRelay = do
            waitThenFwdOnly $ \(i, _) -> do
                sendRev (i, Ocid $ ocdEmpty
                  { ocdData = alFromList
                    [ (helloP, textChange "f")
                    , (byeP, textChange "t")
                  ]
                })
            waitThenFwdOnly $ const return ()
            waitThenFwdOnly $ \(i, d) -> do
              lift $ d `shouldBe` Iprd (TrprDigest helloS)
              sendRev (i, Ocd $ ocdEmpty
                {ocdContainerOps = Map.singleton Root $
                  Map.singleton helloS (Nothing, SoAbsent)})
              sendRev (i, Ocd $ ocdEmpty
                {ocdData = alFromList
                  [ (helloP, textChange "t")
                  , (byeP, textChange "f")
                  ]
                })
            relayNoMore
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
    it "Is idempotent when unsubscribing" $
      let
        forTest = do
            sendFwd $ ClientData alice $ Trcd $ trcdEmpty
              { trcdDataSubs = Map.singleton helloP OpUnsubscribe }
            claimHello bob
            expectRev $ Left $ Map.fromList
              [ (helloS, bob)
              ] -- FIXME: should API be owned?
      in runEffect $ forTest <<-> nstProtocol <<-> blackHoleRelay
    it "Forwards client mutations to provider" $
      let
        forTest = do
            claimHello alice
            expectRev $ Left $ Map.fromList
              [ (helloS, alice)
              ] -- FIXME: should API be owned?
            expectRev $ Right $ ServerData alice $ Frpd $ FrpDigest
              { frpdNamespace = helloS
              , frpdData = alSingleton Root $ textChange "x"
              , frpdContainerOps = mempty
              }
        fauxRelay = do
            waitThenFwdOnly $ \(i, d) -> sendRev (i, Opd $ OutboundProviderDigest
              { opdContainerOps = mempty
              , opdData = alSingleton helloP $ textChange "x"
              })
            relayNoMore
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
    it "Returns client validation errors" $
      let
        forTest = do
            subHello alice
            expectRev $ Right $ ServerData alice $ Frcd $ frcdEmpty
              { frcdErrors = Map.singleton (PathError helloP) ["pants"]
              , frcdDataUnsubs = Set.singleton helloP
              }
            subHello bob
            expectRev $ Right $ ServerData bob $ Frcd $ frcdEmpty
              { frcdData = alSingleton helloP $ textChange "i" }
            expectRev $ Right $ ServerData bob $ Frcd $ frcdEmpty
              { frcdData = alSingleton helloP $ textChange "a" }
        fauxRelay = do
            waitThenFwdOnly $ \(i, _) ->
                sendRev (i, Ocid $ outboundClientDigest
                  { ocdErrors = Map.singleton (PathError helloP) ["pants"] })
            waitThenFwdOnly $ \(i, _) -> do
                sendRev (i, Ocid $ outboundClientDigest
                  { ocdData = alSingleton helloP $ textChange "i" })
                sendRev (i, Ocd $ outboundClientDigest
                  { ocdData = alSingleton helloP $ textChange "a" })
            relayNoMore
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
    it "Returns provider validation errors" $
      let
        errD = FrpErrorDigest $ Map.singleton (PathError helloP) ["lol"]
        forTest = do
            claimHello alice
            expectRev $ Left $ Map.fromList
              [ (helloS, alice)
              ] -- FIXME: should API be owned?
            expectRev $ Right $ ServerData alice $ Frped errD
            expectRev $ Right $ ServerDisconnect alice
            expectRev $ Left $ Map.fromList
              [] -- FIXME: should API be owned?
        fauxRelay = do
            waitThenFwdOnly $ \(i, _) -> sendRev (i, Ope errD)
            waitThenFwdOnly $ \m -> lift $ m `shouldBe` (Originator alice, Iprd $ TrprDigest helloS)
            relayNoMore
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
  where
    blackHoleRelay = waitThenFwdOnly $ const blackHoleRelay
    relayNoMore = waitThenFwdOnly $ const $ lift $ fail "Relay got extra"
    expectRev e = waitThenRevOnly $ \e' -> lift $ e' `shouldBe` e
    expectErrors addr =
        expectRev . Right . ServerData addr . Frped . FrpErrorDigest
    claimHello addr = sendFwd $ ClientData addr $ Trpd $ (trpDigest helloS)
      {trpdData = alSingleton Root $ textChange "yo"}
    subHello addr = sendFwd $ ClientData addr $ Trcd $ trcdEmpty
      {trcdDataSubs = Map.singleton helloP OpSubscribe}
    textChange s = ConstChange Nothing [WireValue (s :: T.Text)]
    helloTn = TypeName helloS helloS
