{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    OverloadedStrings
  , StandaloneDeriving
#-}

module NamespaceTrackerSpec where

import Test.Hspec

import Control.Monad.Trans (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Mol as Mol
import Data.Tagged (Tagged(..))
import qualified Data.Text as T
import Data.Void

import qualified Data.Map.Mos as Mos

import Clapi.TH
import Clapi.Types (InterpolationLimit(..), WireValue(..))
import Clapi.Types.Definitions (tupleDef)
import Clapi.Types.Digests
import Clapi.Types.Messages (DataErrorIndex(..), SubErrorIndex(..))
import Clapi.Types.Path (Path, Seg, pattern Root, Namespace(..), pattern (:/))
import Clapi.Types.AssocList (alSingleton, alEmpty, alFromList)
import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.NamespaceTracker
  ( nstProtocol, Originator(..), PostNstInboundDigest(..), ClientRegs(..))
import qualified Clapi.Protocol as Protocol
import Clapi.Protocol
  ( Protocol, sendFwd, sendRev, (<<->), runEffect, waitThenFwdOnly
  , waitThenRevOnly)

deriving instance Eq ClientRegs
deriving instance Eq PostNstInboundDigest

type Owners i = Map Seg i

alice, bob :: T.Text
alice = "alice"
bob = "bob"

helloS :: Seg
helloS = [segq|hello|]

helloNs :: Namespace
helloNs = Namespace helloS

helloP :: Path
helloP = [pathq|/hello|]

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
              [ (Namespace helloS, alice)
              ] -- FIXME: should API be owned?
            claimHello bob
            expectErrors bob $ Map.singleton
                (PathError helloP) ["Already owned by someone else guv"]
            expectRev $ Right $ ServerDisconnect bob
      in runEffect $ forTest <<-> nstProtocol <<-> blackHoleRelay
    it "Rejects empty claim" $
      let
        forTest = do
            sendFwd $ ClientData alice $ Trpd $ trpdEmpty helloNs
            expectErrors alice $ Map.singleton
                (PathError helloP) ["Empty claim"]
            expectRev $ Right $ ServerDisconnect alice
      in runEffect $ forTest <<-> nstProtocol <<-> blackHoleRelay
    it "Rejects owner subscriptions" $
      let
        subDs =
          [ trcsdEmpty
              {trcsdDataSubs = Mos.singleton helloNs [pathq|/hello|]}
          , trcsdEmpty
              {trcsdTypeSubs = Mos.singleton helloNs (Tagged helloS) }
          ]
        forTest subD = do
            claimHello alice
            expectRev $ Left $ Map.fromList
              [ (Namespace helloS, alice)
              ] -- FIXME: should API be owned?
            sendFwd $ ClientData alice $ Trcsd subD
            expectErrors alice $ Map.singleton
                (PathError helloP) ["Acted as client on own namespace"]
            expectRev $ Right $ ServerDisconnect alice
      in mapM_ (\subD -> runEffect $ forTest subD <<-> nstProtocol <<-> blackHoleRelay) subDs
    it "Has working client subscriptions" $
      let
        forTest = do
            subHello alice
            expectRev $ Right $ ServerData alice $ Frcud $
              (frcudEmpty helloNs)
              { frcudData = alSingleton helloP $ textChange "f"
              , frcudDefinitions = Map.singleton (Tagged helloS) helloDef0
              }
            expectRev $ Right $ ServerData alice $ Frcud $
              (frcudEmpty helloNs)
              { frcudData = alSingleton helloP $ textChange "t" }
            sendFwd $ ClientData alice $ Trcsd $ trcsdEmpty
              { trcsdDataUnsubs = Mos.singleton helloNs helloP }
            expectRev $ Right $ ServerData alice $ Frcsd $ frcsdEmpty
              { frcsdDataUnsubs = Mos.singleton helloNs helloP }
            sendFwd $ ClientData alice $ Trcud $ trcudEmpty helloNs
            expectRev $ Right $ ServerData alice $ Frcud $
              (frcudEmpty helloNs)
              { frcudDefinitions = Map.singleton (Tagged helloS) helloDef1
              }
        helloDef0 = OpDefine $ tupleDef "Yoho" alEmpty ILUninterpolated
        helloDef1 = OpDefine $ tupleDef "Hoyo" alEmpty ILUninterpolated
        fauxRelay = do
            i <- waitThenFwdOnly $ \(i, d) -> do
                lift (d `shouldBe` PnidClientGet
                      (mempty {crDataRegs = Mos.singleton helloNs helloP}))
                return i
            sendRev (i, Ocid $
              (frcudEmpty helloNs)
              { frcudDefinitions = Map.singleton (Tagged helloS) helloDef0
              , frcudData = alSingleton helloP $ textChange "f"
              })
            sendRev (i, Ocud $
              (frcudEmpty helloNs)
              { frcudData = alFromList
                [ (helloP, textChange "t")
                , ([pathq|/nowhere|], textChange "banana")
                ]
              })
            waitThenFwdOnly $ \_ -> return ()
            sendRev (i, Ocud $
              (frcudEmpty helloNs)
              { frcudDefinitions = Map.singleton (Tagged helloS) helloDef1
              , frcudData = alSingleton helloP $ textChange "w"
              })
            _ <- waitThenFwdOnly $ const return ()
            relayNoMore
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
    it "Unsubscribes on client disconnect" $
      let
        forTest = do
            subHello alice
            sendFwd $ ClientDisconnect alice
            subHello bob
            expectRev $ Right $ ServerData bob $ Frcud $
              (frcudEmpty helloNs)
              { frcudData = alSingleton Root $ textChange "f" }
        fauxRelay = do
            _ <- waitThenFwdOnly $ const return ()
            waitThenFwdOnly $ \(i, _d) -> sendRev (i, Ocid $
              (frcudEmpty helloNs)
              { frcudData = alSingleton Root $ textChange "f" })
            relayNoMore
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
    it "Disowns on owner disconnect" $
      -- And unsubs clients
      let
        byeS = [segq|bye|]
        byeNs = Namespace byeS
        forTest = do
            sendFwd $ ClientData bob $ Trcsd $ trcsdEmpty
              {trcsdDataSubs = Mos.fromList
                [ (helloNs, Root)
                , (byeNs, Root)
                ]}
            expectRev $ Right $ ServerData bob $ Frcud $
              (frcudEmpty helloNs)
              { frcudData = alSingleton Root $ textChange "f" }
            expectRev $ Right $ ServerData bob $ Frcud $
              (frcudEmpty byeNs)
              { frcudData = alSingleton Root $ textChange "t" }
            claimHello alice
            expectRev $ Left $ Map.fromList
              [ (Namespace helloS, alice)
              ] -- FIXME: should API be owned?
            sendFwd $ ClientDisconnect alice
            expectRev $ Right $ ServerData bob $ Frcsd $ frcsdEmpty
              { frcsdDataUnsubs = Mos.singleton helloNs Root }
            expectRev $ Left $ Map.fromList
              [] -- FIXME: should API be owned?
            expectRev $ Right $ ServerData bob $ Frcud $
              (frcudEmpty byeNs)
              { frcudData = alSingleton Root $ textChange "f" }
        fauxRelay = do
            waitThenFwdOnly $ \(i, _) -> do
                sendRev (i, Ocid $
                  (frcudEmpty helloNs)
                  { frcudData = alSingleton Root $ textChange "f" })
                sendRev (i, Ocid $
                  (frcudEmpty byeNs)
                  { frcudData = alSingleton Root $ textChange "t" })
            _ <- waitThenFwdOnly $ const return ()
            waitThenFwdOnly $ \(i, d) -> do
              lift $ d `shouldBe` PnidTrprd (TrprDigest helloNs)
              -- To verify the client is unsubscribed:
              sendRev (i, Ocud $
                (frcudEmpty helloNs)
                { frcudData = alSingleton Root $ textChange "t" })
              sendRev (i, Ocud $
                (frcudEmpty byeNs)
                { frcudData = alSingleton Root $ textChange "f" })
            relayNoMore
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
    it "Is idempotent when unsubscribing" $
      let
        forTest = do
            sendFwd $ ClientData alice $ Trcsd $ trcsdEmpty
              { trcsdDataUnsubs = Mos.singleton helloNs helloP}
            claimHello bob
            expectRev $ Left $ Map.fromList
              [ (Namespace helloS, bob)
              ] -- FIXME: should API be owned?
      in runEffect $ forTest <<-> nstProtocol <<-> blackHoleRelay
    it "Forwards client mutations to provider" $
      let
        forTest = do
            claimHello alice
            expectRev $ Left $ Map.fromList
              [ (Namespace helloS, alice)
              ] -- FIXME: should API be owned?
            expectRev $ Right $ ServerData alice $
              Frpd (frpdEmpty helloNs)
              { frpdData = alSingleton Root $ textChange "x"
              }
        fauxRelay = do
            waitThenFwdOnly $ \(i, _d) -> sendRev (i, Opd $
              (frpdEmpty helloNs)
              { frpdData = alSingleton Root $ textChange "x"
              })
            relayNoMore
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
    it "Failed subscriptions do not result in continuing to receive data" $
      let
        forTest = do
            subHello alice
            expectRev $ Right $ ServerData alice $ Frcsd $
              frcsdEmpty
              { frcsdErrors = Map.singleton (PathSubError helloNs helloP) ["pants"] }
            subHello bob
            expectRev $ Right $ ServerData bob $ Frcud $
              (frcudEmpty helloNs)
              { frcudData = alSingleton Root $ textChange "i" }
            expectRev $ Right $ ServerData bob $ Frcud $
              (frcudEmpty helloNs)
              { frcudData = alSingleton Root $ textChange "a" }
        fauxRelay = do
            waitThenFwdOnly $ \(i, _) ->
                sendRev (i,
                  Ocsed $ Map.singleton (PathSubError helloNs helloP) ["pants"])
            waitThenFwdOnly $ \(i, _) -> do
                sendRev (i, Ocid $
                  (frcudEmpty helloNs)
                  { frcudData = alSingleton Root $ textChange "i" })
                sendRev (i, Ocud $
                  (frcudEmpty helloNs)
                  { frcudData = alSingleton Root $ textChange "a" })
            relayNoMore
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
    it "Returns provider validation errors" $
      let
        errD = FrpErrorDigest $ Map.singleton (PathError Root) ["lol"]
        forTest = do
            claimHello alice
            expectRev $ Left $ Map.fromList
              [ (Namespace helloS, alice)
              ] -- FIXME: should API be owned?
            expectRev $ Right $ ServerData alice $ Frped errD
            expectRev $ Right $ ServerDisconnect alice
            expectRev $ Left $ Map.fromList
              [] -- FIXME: should API be owned?
        fauxRelay = do
            waitThenFwdOnly $ \(i, _) -> sendRev (i, Ope errD)
            waitThenFwdOnly $ \m -> lift $
              m `shouldBe` (Originator alice, PnidTrprd $ TrprDigest $
               Namespace helloS)
            relayNoMore
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
  where
    blackHoleRelay = waitThenFwdOnly $ const blackHoleRelay
    relayNoMore = waitThenFwdOnly $ const $ lift $ fail "Relay got extra"
    expectRev e = waitThenRevOnly $ \e' -> lift $ e' `shouldBe` e
    expectErrors addr =
        expectRev . Right . ServerData addr . Frped . FrpErrorDigest
    claimHello addr = sendFwd $ ClientData addr $ Trpd $
      (trpdEmpty helloNs)
      {trpdData = alSingleton Root $ textChange "yo"}
    subHello addr = sendFwd $ ClientData addr $ Trcsd $ trcsdEmpty
      {trcsdDataSubs = Mos.singleton helloNs helloP}
    textChange s = ConstChange Nothing [WireValue (s :: T.Text)]
