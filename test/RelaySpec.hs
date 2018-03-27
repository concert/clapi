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
import Clapi.Tree (treeInsert, RoseTree(RtConstData))
import Clapi.Types.AssocList (alEmpty, alSingleton, alFromList)
import Clapi.Types.Base (InterpolationLimit(..))
import Clapi.Types.Definitions
  (structDef, tupleDef, Liberty(..))
import Clapi.Types.Digests
  ( DefOp(..), DataChange(..), TrpDigest(..), trpDigest
  , InboundDigest(..), InboundClientDigest(..), OutboundDigest(..)
  , OutboundClientDigest(..), outboundClientDigest, TrprDigest(..))
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Types.Messages (ErrorIndex(..))
import Clapi.Types.Path (pattern Root, TypeName(..), pattern (:/))
import Clapi.Types.Tree (ttWord32, unbounded)
import Clapi.Types.Wire (WireValue(..))
import Clapi.Valuespace
  ( baseValuespace, unsafeValidateVs, apiNs, Valuespace(..)
  , rootTypeName, vsLookupDef)

spec :: Spec
spec = describe "the relay protocol" $ do
    it "should extend the root structure when a namespace is claimed" $
      let
        fooDef = tupleDef "Some Word32"
          (alSingleton [segq|value|] (ttWord32 unbounded)) ILUninterpolated
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
          { ocdData = dd
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
    it "should respond sensibly to data changes" $
      let
        vsWithInt = unsafeValidateVs $ baseValuespace
          { vsTree = treeInsert bob fooP
              (RtConstData bob [WireValue (3 :: Word32)]) $
              vsTree baseValuespace
          , vsTyDefs = Map.insert foo
              (Map.singleton foo $ tupleDef "Thing"
                (alSingleton foo $ ttWord32 unbounded) ILUninterpolated) $
              vsTyDefs baseValuespace
          }
        dd = alSingleton Root $ ConstChange bob [WireValue (4 :: Word32)]
        test = do
            sendFwd ((), Ipd $ (trpDigest foo) {trpdData = dd})
            waitThenRevOnly $
              lift . (`shouldBe` Ocd (outboundClientDigest {ocdData = dd})) .
              snd
      in runEffect $ test <<-> relay vsWithInt

  where
    foo = [segq|foo|]
    fooP = Root :/ foo
    bob = Just "bob"
