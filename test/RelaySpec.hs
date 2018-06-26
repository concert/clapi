module RelaySpec where

import Test.Hspec

import Control.Monad.Trans (lift)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Word
import Data.Maybe (fromJust)
import Data.Tagged (Tagged(..))

import Clapi.TH
import Clapi.Protocol (waitThenRevOnly, sendFwd, runEffect, (<<->))
import Clapi.Relay (relay)
import Clapi.Tree (treeInsert, RoseTree(RtConstData, RtContainer))
import Clapi.Types.AssocList (alEmpty, alSingleton, alFromList, alMapKeys)
import Clapi.Types.Base (InterpolationLimit(..))
import Clapi.Types.Definitions
  (arrayDef, structDef, tupleDef, Liberty(..))
import Clapi.Types.Digests
  ( DefOp(..), DataChange(..), TrpDigest(..), trpDigest
  , InboundDigest(..), InboundClientDigest(..), OutboundDigest(..)
  , OutboundClientDigest(..), outboundClientDigest, TrprDigest(..))
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Types.Messages (ErrorIndex(..))
import Clapi.Types.Path
  (pattern Root, tTypeName, pattern (:/), pattern (:</), Namespace(..))
import Clapi.Types.Tree (TreeType(..), unbounded)
import Clapi.Types.Wire (WireValue(..))
import Clapi.Valuespace
  ( baseValuespace, unsafeValidateVs, apiNs, Valuespace(..)
  , rootTypeName, vsLookupDef)

spec :: Spec
spec = describe "the relay protocol" $ do
    it "should extend the root structure when a namespace is claimed" $
      let
        fooDef = tupleDef "Some Word32"
          (alSingleton [segq|value|] (TtWord32 unbounded)) ILUninterpolated
        dd = alSingleton Root $ ConstChange bob [WireValue (42 :: Word32)]
        inDig = Ipd $ (trpDigest $ Namespace foo)
          { trpdDefinitions = Map.singleton (Tagged foo) $ OpDefine fooDef
          , trpdData = dd
          }
        extendedRootDef = structDef "root def doc" $ alFromList
          [ (unNamespace apiNs, (tTypeName apiNs (unNamespace apiNs), Cannot))
          , (foo, (tTypeName (Namespace foo) foo, Cannot))
          ]
        expectedOutDig = Ocd $ outboundClientDigest
          { ocdData = qualify foo dd
          , ocdDefinitions = Map.fromList
            [ (tTypeName (Namespace foo) foo, OpDefine fooDef)
            , (tTypeName apiNs [segq|root|], OpDefine extendedRootDef)
            ]
          , ocdTypeAssignments = Map.insert
            [pathq|/foo|] (tTypeName (Namespace foo) foo, Cannot) mempty
          , ocdContainerOps = Map.singleton Root $
              Map.singleton foo
              (Nothing, SoPresentAfter (Just $ unNamespace apiNs))
          }
        test = do
          sendFwd ((), inDig)
          waitThenRevOnly $ lift . (`shouldBe` expectedOutDig) . snd
      in runEffect $ test <<-> relay baseValuespace
    it "should send root info on revoke" $
      let
        vsWithStuff = unsafeValidateVs $ baseValuespace
          { vsTree = treeInsert bob fooP (RtConstData bob []) $
              vsTree baseValuespace
          , vsTyDefs = Map.insert (Namespace foo)
              (Map.singleton (Tagged foo) $
                 tupleDef "Thing" alEmpty ILUninterpolated) $
              vsTyDefs baseValuespace
          }
        expectedOutDig = Ocd $ outboundClientDigest
          { ocdDefinitions = Map.fromList
            [ (rootTypeName, OpDefine $ fromJust $
                vsLookupDef rootTypeName baseValuespace)
            , (tTypeName (Namespace foo) foo, OpUndefine)
            ]
          , ocdContainerOps = Map.singleton Root $
              Map.singleton foo (Nothing, SoAbsent)
          }
        test = do
          sendFwd ((), Iprd $ TrprDigest $ Namespace foo)
          waitThenRevOnly $ lift . (`shouldBe` expectedOutDig) . snd
      in runEffect $ test <<-> relay vsWithStuff
    it "should reject subscriptions to non-existant paths" $
      let
        p = [pathq|/madeup|]
        expectedOutDig = Ocid $ outboundClientDigest
          { ocdErrors = Map.singleton (PathError p) ["Path not found"]
          }
        test = do
          sendFwd ((), Icd $
            InboundClientDigest (Set.singleton p) mempty mempty mempty alEmpty)
          waitThenRevOnly $ lift . (`shouldBe` expectedOutDig) . snd
      in runEffect $ test <<-> relay baseValuespace
    it "should have container ops for implicitly created children" $
      let
        kid = [segq|kid|]
        tyDefs = Map.fromList
          [ (Tagged foo, arrayDef "arr" (tTypeName (Namespace foo) kid) Cannot)
          , (Tagged kid, tupleDef "kid" alEmpty ILUninterpolated)
          ]
        vsWithStuff = unsafeValidateVs $ baseValuespace
          { vsTree = treeInsert bob fooP (RtContainer alEmpty) $ vsTree baseValuespace
          , vsTyDefs = Map.insert (Namespace foo) tyDefs $
               vsTyDefs baseValuespace
          }
        dd = alSingleton (Root :/ kid) $ ConstChange Nothing []
        inDig = Ipd $ (trpDigest $ Namespace foo)
          { trpdData = dd
          }
        qKid = fooP :/ kid
        expectedOutDig = Ocd $ outboundClientDigest
          { ocdData = qualify foo dd
          , ocdTypeAssignments = Map.singleton qKid
              (tTypeName (Namespace foo) kid, Cannot)
          , ocdContainerOps = Map.singleton fooP $
            Map.singleton kid (Nothing, SoPresentAfter Nothing)
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
            sendFwd ((), Ipd $ (trpDigest $ Namespace foo) {trpdData = dd})
            waitThenRevOnly $
              lift . (`shouldBe` Ocd (outboundClientDigest {ocdData = qualify foo dd})) .
              snd
      in runEffect $ test <<-> relay vsWithInt
    it "should not send empty ocids/opds to client requests" $
      let
        test = do
            sendFwd (1, Icd $ InboundClientDigest mempty mempty mempty mempty alEmpty)
            sendFwd (2, Icd $ InboundClientDigest (Set.singleton [pathq|/whatevz|]) mempty mempty mempty alEmpty)
            waitThenRevOnly $ lift . (`shouldSatisfy` (== (2 :: Int)) . fst)
      in runEffect $ test <<-> relay baseValuespace
  where
    foo = [segq|foo|]
    fooP = Root :/ foo
    bob = Just "bob"
    qualify ns = maybe (error "bad sneakers") id . alMapKeys (ns :</)
