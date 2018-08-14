{-# LANGUAGE
    OverloadedStrings
#-}

module RelaySpec where

import Test.Hspec

import Control.Monad.Trans (lift)
import qualified Data.Map as Map
import Data.Word
import Data.Tagged (Tagged(..))

import qualified Data.Map.Mos as Mos

import Clapi.TH
import Clapi.Protocol (waitThenRevOnly, sendFwd, runEffect, (<<->))
import Clapi.Relay (relay)
import Clapi.Tree (RoseTree(RtConstData, RtContainer))
import Clapi.Types.AssocList (alEmpty, alSingleton)
import Clapi.Types.Base (InterpolationLimit(..))
import Clapi.Types.Definitions (arrayDef, tupleDef, Editable(..))
import Clapi.Types.Digests
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Types.Messages (SubErrorIndex(..))
import Clapi.Types.Path (pattern Root, pattern (:/), Namespace(..))
import Clapi.Types.Tree (TreeType(..), unbounded)
import Clapi.Types.Wire (WireValue(..))
import Clapi.Valuespace (baseValuespace, Valuespace(..))
import Clapi.NamespaceTracker
  ( PostNstInboundDigest(..), ClientRegs(..))

import ValuespaceSpec (unsafeValidateVs)

spec :: Spec
spec = describe "the relay protocol" $ do
    it "handles claims" $
      let
        fooDef = tupleDef "Some Word32"
          (alSingleton [segq|value|] (TtWord32 unbounded)) ILUninterpolated
        dd = alSingleton Root $ ConstChange bob [WireValue (42 :: Word32)]
        inDefs = Map.singleton (Tagged foo) $ OpDefine fooDef
        inDig = PnidTrpd $ (trpdEmpty fooN)
          { trpdDefinitions = inDefs
          , trpdData = dd
          }
        expectedUpDig = Ocud $ (frcudEmpty fooN)
          { frcudDefinitions = inDefs
          , frcudData = dd
          }
        test = do
          sendFwd ((), inDig)
          waitThenRevOnly $ lift . (`shouldBe` expectedUpDig) . snd
      in runEffect $ test <<-> relay mempty
    it "should handle revoke" $
      let
        vsWithStuff = unsafeValidateVs $ (baseValuespace (Tagged foo) ReadOnly)
          { vsTree = RtConstData bob []
          , vsTyDefs = Map.singleton (Tagged foo)
            (tupleDef "Thing" alEmpty ILUninterpolated)
          }
        expectedOutDig1 = Ocrd $ FrcRootDigest $ Map.singleton fooN SoAbsent
        expectedOutDig2 = Ocsed $
          Map.singleton (NamespaceSubError fooN) ["Namespace not found"]
        test = do
          sendFwd ((), PnidTrprd $ TrprDigest fooN)
          sendFwd ((), PnidClientGet $ mempty {crDataRegs = Mos.singleton fooN Root})
          waitThenRevOnly $ lift . (`shouldBe` expectedOutDig1) . snd
          waitThenRevOnly $ lift . (`shouldBe` expectedOutDig2) . snd
      in runEffect $ test <<-> relay (Map.singleton fooN vsWithStuff)
    it "should reject subscriptions to non-existant paths" $
      -- FIXME: is currently testing namespaces! Add a case for an existing
      -- namespace but missing path
      let
        p = [pathq|/madeup|]
        expectedOutDig = Ocsed $
          Map.singleton (NamespaceSubError fooN) ["Namespace not found"]
        test = do
          sendFwd ((), PnidClientGet
            (mempty {crDataRegs = Mos.singleton fooN p}))
          waitThenRevOnly $ lift . (`shouldBe` expectedOutDig) . snd
      in runEffect $ test <<-> relay mempty
    it "should have container ops for implicitly created children" $
      let
        kid = [segq|kid|]
        vsWithStuff = unsafeValidateVs $ (baseValuespace (Tagged foo) ReadOnly)
          { vsTree = RtContainer alEmpty
          , vsTyDefs = Map.fromList
              [ ( Tagged foo
                , arrayDef "arr" Nothing (Tagged kid) ReadOnly)
              , (Tagged kid, tupleDef "kid" alEmpty ILUninterpolated)
              ]
          }
        dd = alSingleton (Root :/ kid) $ ConstChange Nothing []
        inDig = PnidTrpd $ (trpdEmpty fooN)
          { trpdData = dd
          }
        expectedOutDig = Ocud $ (frcudEmpty fooN)
          { frcudData = dd
          , frcudTypeAssignments =
              Map.singleton (Root :/ kid) (Tagged kid, ReadOnly)
          , frcudContOps = Map.singleton Root $
              Map.singleton kid (Nothing, SoAfter Nothing)
          }
        test = do
          sendFwd ((), inDig)
          waitThenRevOnly $ lift . (`shouldBe` expectedOutDig) . snd
      in runEffect $ test <<-> relay (Map.singleton fooN vsWithStuff)
    it "should respond sensibly to data changes" $
      let
        vsWithInt = unsafeValidateVs $ (baseValuespace (Tagged foo) ReadOnly)
          { vsTree = RtConstData bob [WireValue (3 :: Word32)]
          , vsTyDefs = Map.singleton (Tagged foo)
              (tupleDef "Thing" (alSingleton foo $ TtWord32 unbounded)
               ILUninterpolated)
          }
        dd = alSingleton Root $ ConstChange bob [WireValue (4 :: Word32)]
        test = do
            sendFwd ((), PnidTrpd $ (trpdEmpty fooN) {trpdData = dd})
            waitThenRevOnly $
              lift . (`shouldBe` (Ocud $ (frcudEmpty fooN) {frcudData = dd})) .
              snd
      in runEffect $ test <<-> relay (Map.singleton fooN vsWithInt)
    it "should not send empty digests to valid client requests" $
      let
        test = do
            sendFwd (1, PnidClientGet mempty)
            sendFwd (2, PnidClientGet $
              mempty {crDataRegs = Mos.singleton fooN [pathq|/whatevz|]})
            waitThenRevOnly $ lift . (`shouldSatisfy` (== (2 :: Int)) . fst)
      in runEffect $ test <<-> relay mempty
  where
    foo = [segq|foo|]
    fooN = Namespace foo
    bob = Just "bob"
