module NamespaceTrackerSpec where

import Test.Hspec

import Control.Monad.Trans (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Mol as Mol
import qualified Data.Set as Set
import Data.Tagged (Tagged(..))
import qualified Data.Text as T
import Data.Void

import Clapi.TH
import Clapi.Types (InterpolationLimit(..), WireValue(..))
import Clapi.Types.Definitions (tupleDef)
import Clapi.Types.Digests
import Clapi.Types.Messages (DataErrorIndex(..))
import Clapi.Types.Path
  (Path, Seg, pattern Root, TypeName(..), tTypeName, Namespace(..))
import Clapi.Types.AssocList (alSingleton, alEmpty, alFromList)
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.NamespaceTracker
  ( nstProtocol, Originator(..), PostNstInboundDigest(..), ClientGetDigest(..)
  , cgdEmpty)
import qualified Clapi.Protocol as Protocol
import Clapi.Protocol
  ( Protocol, sendFwd, sendRev, (<<->), runEffect, waitThenFwdOnly
  , waitThenRevOnly)

type Owners i = Map Seg i

alice, bob :: T.Text
alice = "alice"
bob = "bob"

helloS :: Seg
helloS = [segq|hello|]

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
            sendFwd $ ClientData alice $ Trpd $ trpdEmpty $ Namespace helloS
            expectErrors alice $ Map.singleton
                (PathError helloP) ["Empty claim"]
            expectRev $ Right $ ServerDisconnect alice
      in runEffect $ forTest <<-> nstProtocol <<-> blackHoleRelay
    it "Rejects owner subscriptions" $
      let
        subDs =
          [ trcsdEmpty
              {trcsdDataSubs = Map.singleton [pathq|/hello|] OpSubscribe}
          , trcsdEmpty
              {trcsdTypeSubs = Map.singleton
                (tTypeName (Namespace helloS) helloS) OpSubscribe}
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
              (frcudEmpty $ Namespace helloS)
              { frcudData = alSingleton Root $ textChange "f"
              , frcudDefinitions = Map.singleton (Tagged helloS) helloDef0
              }
            expectRev $ Right $ ServerData alice $ Frcud $
              (frcudEmpty $ Namespace helloS)
              { frcudData = alSingleton Root $ textChange "t" }
            sendFwd $ ClientData alice $ Trcsd $ trcsdEmpty
              { trcsdDataSubs = Map.singleton helloP OpUnsubscribe }
            expectRev $ Right $ ServerData alice $ Frcsd $ frcsdEmpty
              { frcsdDataUnsubs = Set.singleton helloP }
            expectRev $ Right $ ServerData alice $ Frcud $
              (frcudEmpty $ Namespace helloS)
              { frcudDefinitions = Map.singleton (Tagged helloS) helloDef1
              }
        helloDef0 = OpDefine $ tupleDef "Yoho" alEmpty ILUninterpolated
        helloDef1 = OpDefine $ tupleDef "Hoyo" alEmpty ILUninterpolated
        fauxRelay = do
            i <- waitThenFwdOnly $ \(i, d) -> do
                lift (d `shouldBe` PnidCgd (cgdEmpty
                  { cgdDataGets = Set.singleton helloP
                  }))
                return i
            sendRev (i, Ocid $
              (frcudEmpty $ Namespace helloS)
              { frcudDefinitions = Map.singleton (Tagged helloS) helloDef0
              , frcudData = alSingleton Root $ textChange "f"
              })
            sendRev (i, Ocud $
              (frcudEmpty $ Namespace helloS)
              { frcudData = alFromList
                [ (Root, textChange "t")
                , ([pathq|/nowhere|], textChange "banana")
                ]
              })
            sendRev (i, Ocud $
              (frcudEmpty $ Namespace helloS)
              { frcudDefinitions = Map.singleton (Tagged helloS) helloDef1
              , frcudData = alSingleton Root $ textChange "w"
              })
            waitThenFwdOnly $ const return ()
            relayNoMore
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
    it "Unsubscribes on client disconnect" $
      let
        forTest = do
            subHello alice
            sendFwd $ ClientDisconnect alice
            subHello bob
            expectRev $ Right $ ServerData bob $ Frcud $
              (frcudEmpty $ Namespace helloS)
              { frcudData = alSingleton Root $ textChange "f" }
        fauxRelay = do
            waitThenFwdOnly $ const return ()
            waitThenFwdOnly $ \(i, d) -> sendRev (i, Ocid $
              (frcudEmpty $ Namespace helloS)
              { frcudData = alSingleton Root $ textChange "f" })
            relayNoMore
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
    it "Disowns on owner disconnect" $
      -- And unsubs clients
      let
        byeP = [pathq|/bye|]
        forTest = do
            sendFwd $ ClientData bob $ Trcsd $ trcsdEmpty
              {trcsdDataSubs = Map.fromList
                [ (helloP, OpSubscribe)
                , (byeP, OpSubscribe)
                ]}
            expectRev $ Right $ ServerData bob $ Frcud $
              (frcudEmpty $ Namespace helloS)
              { frcudData = alSingleton Root $ textChange "f" }
            expectRev $ Right $ ServerData bob $ Frcud $
              (frcudEmpty $ Namespace [segq|bye|])
              { frcudData = alSingleton Root $ textChange "t" }
            claimHello alice
            expectRev $ Left $ Map.fromList
              [ (Namespace helloS, alice)
              ] -- FIXME: should API be owned?
            sendFwd $ ClientDisconnect alice
            expectRev $ Left $ Map.fromList
              [] -- FIXME: should API be owned?
            expectRev $ Right $ ServerData bob $ Frcsd $ frcsdEmpty
              { frcsdDataUnsubs = Set.singleton helloP }
            expectRev $ Right $ ServerData bob $ Frcud $
              (frcudEmpty $ Namespace [segq|bye|])
              { frcudData = alSingleton Root $ textChange "f" }
        fauxRelay = do
            waitThenFwdOnly $ \(i, _) -> do
                sendRev (i, Ocid $
                  (frcudEmpty $ Namespace helloS)
                  { frcudData = alSingleton Root $ textChange "f" })
                sendRev (i, Ocid $
                  (frcudEmpty $ Namespace [segq|bye|])
                  { frcudData = alSingleton Root $ textChange "t" })
            waitThenFwdOnly $ const return ()
            waitThenFwdOnly $ \(i, d) -> do
              lift $ d `shouldBe` PnidTrprd (TrprDigest $ Namespace helloS)
              sendRev (i, Ocrd $ FrcRootDigest $ Map.singleton helloS SoAbsent)
              -- To verify the client is unsubscribed:
              sendRev (i, Ocud $
                (frcudEmpty $ Namespace helloS)
                { frcudData = alSingleton Root $ textChange "t" })
              sendRev (i, Ocud $
                (frcudEmpty $ Namespace [segq|bye|])
                { frcudData = alSingleton Root $ textChange "f" })
            relayNoMore
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
    it "Is idempotent when unsubscribing" $
      let
        forTest = do
            sendFwd $ ClientData alice $ Trcsd $ trcsdEmpty
              { trcsdDataSubs = Map.singleton helloP OpUnsubscribe }
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
              Frpd (frpdEmpty $ Namespace helloS)
              { frpdData = alSingleton Root $ textChange "x"
              }
        fauxRelay = do
            waitThenFwdOnly $ \(i, d) -> sendRev (i, Opd $
              (frpdEmpty $ Namespace helloS)
              { frpdData = alSingleton Root $ textChange "x"
              })
            relayNoMore
      in runEffect $ forTest <<-> nstProtocol <<-> fauxRelay
    it "Returns client validation errors" $
      let
        forTest = do
            subHello alice
            expectRev $ Right $ ServerData alice $ Frcud $
              (frcudEmpty $ Namespace helloS)
              { frcudErrors = Map.singleton (PathError Root) ["pants"] }
            expectRev $ Right $ ServerData alice $ Frcsd $ frcsdEmpty
              { frcsdDataUnsubs = Set.singleton helloP
              }
            subHello bob
            expectRev $ Right $ ServerData bob $ Frcud $
              (frcudEmpty $ Namespace helloS)
              { frcudData = alSingleton Root $ textChange "i" }
            expectRev $ Right $ ServerData bob $ Frcud $
              (frcudEmpty $ Namespace helloS)
              { frcudData = alSingleton Root $ textChange "a" }
        fauxRelay = do
            waitThenFwdOnly $ \(i, _) ->
                sendRev (i, Ocid $
                  (frcudEmpty $ Namespace helloS)
                  { frcudErrors = Map.singleton (PathError Root) ["pants"] })
            waitThenFwdOnly $ \(i, _) -> do
                sendRev (i, Ocid $
                  (frcudEmpty $ Namespace helloS)
                  { frcudData = alSingleton Root $ textChange "i" })
                sendRev (i, Ocud $
                  (frcudEmpty $ Namespace helloS)
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
      (trpdEmpty $ Namespace helloS)
      {trpdData = alSingleton Root $ textChange "yo"}
    subHello addr = sendFwd $ ClientData addr $ Trcsd $ trcsdEmpty
      {trcsdDataSubs = Map.singleton helloP OpSubscribe}
    textChange s = ConstChange Nothing [WireValue (s :: T.Text)]
    helloTn = TypeName (Namespace helloS) helloS
