{-# LANGUAGE
    OverloadedStrings
#-}

module RelaySpec where

import Test.Hspec

import Control.Monad.Trans (lift)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Word
import Data.Tagged (Tagged(..))

import Clapi.TH
import Clapi.Protocol (waitThenRevOnly, sendFwd, runEffect, (<<->))
import Clapi.Relay (relay)
import Clapi.Tree (treeInsert, RoseTree(RtConstData, RtContainer))
import Clapi.Types.AssocList (alEmpty, alSingleton)
import Clapi.Types.Base (InterpolationLimit(..))
import Clapi.Types.Definitions (arrayDef, tupleDef, Liberty(..))
import Clapi.Types.Digests
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Types.Messages (SubErrorIndex(..))
import Clapi.Types.Path
  (pattern Root, tTypeName, pattern (:/), Namespace(..))
import Clapi.Types.Tree (TreeType(..), unbounded)
import Clapi.Types.Wire (WireValue(..))
import Clapi.Valuespace (baseValuespace, unsafeValidateVs, Valuespace(..))
import Clapi.NamespaceTracker
  ( PostNstInboundDigest(..), ClientGetDigest(..), cgdEmpty)

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
      in runEffect $ test <<-> relay baseValuespace
    it "should handle revoke" $
      let
        vsWithStuff = unsafeValidateVs $ baseValuespace
          { vsTree = treeInsert bob fooP (RtConstData bob []) $
              vsTree baseValuespace
          , vsTyDefs = Map.insert (Namespace foo)
              (Map.singleton (Tagged foo) $
                 tupleDef "Thing" alEmpty ILUninterpolated) $
              vsTyDefs baseValuespace
          }
        expectedOutDig1 = Ocrd $ FrcRootDigest $ Map.singleton foo SoAbsent
        expectedOutDig2 = Ocsed $
          Map.singleton (PathSubError $ Root :/ foo) ["Path not found"]
        test = do
          sendFwd ((), PnidTrprd $ TrprDigest fooN)
          sendFwd ((), PnidCgd $
            cgdEmpty { cgdDataGets = Set.singleton fooP })
          waitThenRevOnly $ lift . (`shouldBe` expectedOutDig1) . snd
          waitThenRevOnly $ lift . (`shouldBe` expectedOutDig2) . snd
      in runEffect $ test <<-> relay vsWithStuff
    it "should reject subscriptions to non-existant paths" $
      let
        p = [pathq|/madeup|]
        expectedOutDig = Ocsed $
          Map.singleton (PathSubError p) ["Path not found"]
        test = do
          sendFwd ((), PnidCgd $ cgdEmpty {cgdDataGets = Set.singleton p})
          waitThenRevOnly $ lift . (`shouldBe` expectedOutDig) . snd
      in runEffect $ test <<-> relay baseValuespace
    it "should have container ops for implicitly created children" $
      let
        kid = [segq|kid|]
        tyDefs = Map.fromList
          [ ( Tagged foo
            , arrayDef "arr" Nothing (tTypeName (Namespace foo) kid) Cannot)
          , (Tagged kid, tupleDef "kid" alEmpty ILUninterpolated)
          ]
        vsWithStuff = unsafeValidateVs $ baseValuespace
          { vsTree = treeInsert bob fooP (RtContainer alEmpty) $ vsTree baseValuespace
          , vsTyDefs = Map.insert fooN tyDefs $
               vsTyDefs baseValuespace
          }
        dd = alSingleton (Root :/ kid) $ ConstChange Nothing []
        inDig = PnidTrpd $ (trpdEmpty fooN)
          { trpdData = dd
          }
        expectedOutDig = Ocud $ (frcudEmpty fooN)
          { frcudData = dd
          , frcudTypeAssignments =
              Map.singleton (Root :/ kid) (tTypeName fooN kid, Cannot)
          }
        test = do
          sendFwd ((), inDig)
          waitThenRevOnly $ lift . (`shouldBe` expectedOutDig) . snd
      in runEffect $ test <<-> relay vsWithStuff
    it "should respond sensibly to data changes" $
      let
        vsWithInt = unsafeValidateVs $ baseValuespace
          { vsTree = treeInsert bob fooP
              (RtConstData bob [WireValue (3 :: Word32)]) $
              vsTree baseValuespace
          , vsTyDefs = Map.insert (Namespace foo)
              (Map.singleton (Tagged foo) $ tupleDef "Thing"
                (alSingleton foo $ TtWord32 unbounded) ILUninterpolated) $
              vsTyDefs baseValuespace
          }
        dd = alSingleton Root $ ConstChange bob [WireValue (4 :: Word32)]
        test = do
            sendFwd ((), PnidTrpd $ (trpdEmpty fooN) {trpdData = dd})
            waitThenRevOnly $
              lift . (`shouldBe` (Ocud $ (frcudEmpty fooN) {frcudData = dd})) .
              snd
      in runEffect $ test <<-> relay vsWithInt
    it "should not send empty digests to valid client requests" $
      let
        test = do
            sendFwd (1, PnidCgd $ cgdEmpty)
            sendFwd (2, PnidCgd $ cgdEmpty
              {cgdDataGets = Set.singleton [pathq|/whatevz|]})
            waitThenRevOnly $ lift . (`shouldSatisfy` (== (2 :: Int)) . fst)
      in runEffect $ test <<-> relay baseValuespace
  where
    foo = [segq|foo|]
    fooP = Root :/ foo
    fooN = Namespace foo
    bob = Just "bob"
    qualify ns = maybe (error "bad sneakers") id . alMapKeys (ns :</)
