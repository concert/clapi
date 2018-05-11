{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module RelaySpec where

import Test.Hspec

import Control.Monad.Trans (lift)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Word
import Data.Maybe (fromJust)

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
import Clapi.Types.Path (pattern Root, TypeName(..), pattern (:/), pattern (:</))
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
        inDig = Ipd $ (trpDigest foo)
          { trpdDefinitions = Map.singleton foo $ OpDefine fooDef
          , trpdData = dd
          }
        extendedRootDef = structDef "root def doc" $ alFromList
          [ (apiNs, (TypeName apiNs apiNs, Cannot))
          , (foo, (TypeName foo foo, Cannot))
          ]
        expectedOutDig = Ocd $ outboundClientDigest
          { ocdData = qualify foo dd
          , ocdDefinitions = Map.fromList
            [ (TypeName foo foo, OpDefine fooDef)
            , (TypeName apiNs [segq|root|], OpDefine extendedRootDef)
            ]
          , ocdTypeAssignments = Map.insert
            [pathq|/foo|] (TypeName foo foo, Cannot) mempty
          , ocdContainerOps = Map.singleton Root $
              Map.singleton foo (Nothing, SoPresentAfter (Just apiNs))
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
          , vsTyDefs = Map.insert foo
              (Map.singleton foo $ tupleDef "Thing" alEmpty ILUninterpolated) $
              vsTyDefs baseValuespace
          }
        expectedOutDig = Ocd $ outboundClientDigest
          { ocdDefinitions = Map.fromList
            [ (rootTypeName, OpDefine $ fromJust $
                vsLookupDef rootTypeName baseValuespace)
            , (TypeName foo foo, OpUndefine)
            ]
          , ocdContainerOps = Map.singleton Root $
              Map.singleton foo (Nothing, SoAbsent)
          }
        test = do
          sendFwd ((), Iprd $ TrprDigest foo)
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
            InboundClientDigest (Set.singleton p) mempty mempty alEmpty)
          waitThenRevOnly $ lift . (`shouldBe` expectedOutDig) . snd
      in runEffect $ test <<-> relay baseValuespace
    it "should have container ops for implicitly created children" $
      let
        kid = [segq|kid|]
        tyDefs = Map.fromList
          [ (foo, arrayDef "arr" (TypeName foo kid) Cannot)
          , (kid, tupleDef "kid" alEmpty ILUninterpolated)
          ]
        vsWithStuff = unsafeValidateVs $ baseValuespace
          { vsTree = treeInsert bob fooP (RtContainer alEmpty) $ vsTree baseValuespace
          , vsTyDefs = Map.insert foo tyDefs $ vsTyDefs baseValuespace
          }
        dd = alSingleton (Root :/ kid) $ ConstChange Nothing []
        inDig = Ipd $ (trpDigest foo)
          { trpdData = dd
          }
        qKid = fooP :/ kid
        expectedOutDig = Ocd $ outboundClientDigest
          { ocdData = qualify foo dd
          , ocdTypeAssignments = Map.singleton qKid (TypeName foo kid, Cannot)
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
          , vsTyDefs = Map.insert foo
              (Map.singleton foo $ tupleDef "Thing"
                (alSingleton foo $ TtWord32 unbounded) ILUninterpolated) $
              vsTyDefs baseValuespace
          }
        dd = alSingleton Root $ ConstChange bob [WireValue (4 :: Word32)]
        test = do
            sendFwd ((), Ipd $ (trpDigest foo) {trpdData = dd})
            waitThenRevOnly $
              lift . (`shouldBe` Ocd (outboundClientDigest {ocdData = qualify foo dd})) .
              snd
      in runEffect $ test <<-> relay vsWithInt
    it "should not send empty ocids/opds to client requests" $
      let
        test = do
            sendFwd (1, Icd $ InboundClientDigest mempty mempty mempty alEmpty)
            sendFwd (2, Icd $ InboundClientDigest (Set.singleton [pathq|/whatevz|]) mempty mempty alEmpty)
            waitThenRevOnly $ lift . (`shouldSatisfy` (== (2 :: Int)) . fst)
      in runEffect $ test <<-> relay baseValuespace
  where
    foo = [segq|foo|]
    fooP = Root :/ foo
    bob = Just "bob"
    qualify ns = maybe (error "bad sneakers") id . alMapKeys (ns :</)
