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

import qualified Data.Map.Mos as Mos

import Clapi.TH
import Clapi.Protocol (waitThenRevOnly, sendFwd, runEffect, (<<->))
import Clapi.Relay (relay)
import Clapi.Tree (treeInsert, RoseTree(RtConstData))
import Clapi.Types.AssocList (alEmpty, alSingleton, alFromList)
import Clapi.Types.Base (InterpolationLimit(..))
import Clapi.Types.Definitions
  (Definition(..), structDef, tupleDef, Liberty(..))
import Clapi.Types.Digests
  ( DefOp(..), DataChange(..), TrpDigest(..), trpDigest, FrpErrorDigest(..)
  , InboundDigest(..), InboundClientDigest(..), OutboundDigest(..), OutboundClientDigest(..)
  , outboundClientDigest, TrprDigest(..))
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Types.Messages (ErrorIndex(..))
import Clapi.Types.Path (pattern Root, TypeName(..), pattern (:/))
import Clapi.Types.Tree (ttWord32, unbounded)
import Clapi.Types.UniqList (ulEmpty, ulSingle)
import Clapi.Types.Wire (WireValue(..))
import Clapi.Valuespace (baseValuespace, apiNs, vsTyAssns, Valuespace(..), rootTypeName, lookupDef)

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
          -- FIXME: Should only get changed type assignments
          , ocdTypeAssignments = Map.insert
            [pathq|/foo|] (TypeName foo foo, Cannot)
            (fmap (,Cannot) $ fst $ vsTyAssns baseValuespace)
          , ocdContainerOps = Map.singleton Root $ Map.singleton foo (Nothing, SoPresentAfter (Just apiNs))
          }
        test = do
          sendFwd ((), inDig)
          waitThenRevOnly $ lift . (`shouldBe` expectedOutDig) . snd
      in runEffect $ test <<-> relay baseValuespace
    it "should send root info on revoke" $
      let
        fooP = Root :/ foo
        vsWithStuff = Valuespace
          (treeInsert bob fooP (RtConstData bob []) $ vsTree baseValuespace)
          -- FIXME: This should probably have updated the root def to include the foo NS
          (Map.insert foo (Map.singleton foo $ tupleDef "Thing" alEmpty ILUninterpolated) $ vsTyDefs baseValuespace)
          (Mos.setDependency fooP (TypeName foo foo) $ vsTyAssns baseValuespace)
        expectedOutDig = Ocd $ outboundClientDigest
          { ocdDefinitions = Map.fromList
            [ (rootTypeName, OpDefine $ fromJust $ lookupDef rootTypeName $ vsTyDefs baseValuespace)
            , (TypeName foo foo, OpUndefine)
            ]
          , ocdContainerOps = Map.singleton Root $ Map.singleton foo (Nothing, SoAbsent)
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
          sendFwd ((), Icd $ InboundClientDigest (Set.singleton p) mempty mempty alEmpty)
          waitThenRevOnly $ lift . (`shouldBe` expectedOutDig) . snd
      in runEffect $ test <<-> relay baseValuespace
  where
    foo = [segq|foo|]
    bob = Just "bob"
