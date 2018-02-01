{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module RelaySpec where

import Test.Hspec

import Control.Monad.Trans (lift)
import qualified Data.Map as Map
import Data.Word

import Clapi.TH
import Clapi.Protocol (waitThenRevOnly, sendFwd, runEffect, (<<->))
import Clapi.Relay
  ( InboundDigest(..), OutboundDigest(..), relay
  , OutboundClientDigest(..), outboundClientDigest)
import Clapi.Types.AssocList (alEmpty, alSingleton)
import Clapi.Types.Base (InterpolationLimit(..))
import Clapi.Types.Definitions
  (Definition(..), StructDefinition(..), TupleDefinition(..), Liberty(..))
import Clapi.Types.Digests
  (DefOp(..), DataChange(..), TrpDigest(..), trpDigest, FrpErrorDigest(..))
import Clapi.Types.Messages (ErrorIndex(..))
import Clapi.Types.Path (pattern Root, TypeName(..))
import Clapi.Types.Tree (ttWord32, unbounded)
import Clapi.Types.UniqList (ulEmpty, ulSingle)
import Clapi.Types.Wire (WireValue(..))
import Clapi.Valuespace (baseValuespace)

spec :: Spec
spec = describe "the relay protocol" $ do
    it "should extend the root structure when a namespace is claimed" $
      let
        fooDef = StructDefinition "Foo API root" $
          alEmpty
          -- alSingleton bar (barTn, Cannot)
        barDef = TupleDefinition  "Some Word32"
          (alSingleton [segq|value|] (ttWord32 unbounded)) ILUninterpolated
        inDig = Ipd $ (trpDigest foo)
          { trpdDefinitions = Map.fromList
              [ (foo, OpDefine $ StructDef fooDef)
              -- , (bar, OpDefine $ TupleDef barDef)
              ]
          --   -- FIXME: shouldn't the path be namespaced in foo??
          -- , trpdData = Map.singleton [pathq|/foo/bar|] $
          --     ConstChange Nothing [WireValue @Word32 42]
          , trpdChildAssignments = Map.singleton [pathq|/foo|] (Nothing, ulEmpty)
          }
        expectedOutDig = Ocd $ outboundClientDigest
          { ocdChildAssignments = Map.singleton Root $ (Nothing, ulSingle foo)
          , ocdDefinitions = Map.singleton (TypeName foo foo) (StructDef fooDef)
          , ocdTypeAssignments = Map.singleton Root (TypeName foo foo, Cannot)
          }
        test = do
          sendFwd ((), inDig)
          waitThenRevOnly $ lift . (`shouldBe` expectedOutDig) . snd
      in
        runEffect $ test <<-> relay baseValuespace
    it "should reject a namespace claim that contains no root definition" $
      let
        inDig = Ipd $ trpDigest foo
        outDigCheck (_, Ope (FrpErrorDigest ns m)) =
          ns == foo && Map.keys m == [GlobalError]
        outDigCheck _ = False
        test = do
          sendFwd ((), inDig)
          waitThenRevOnly $ lift . (`shouldSatisfy` outDigCheck)
      in
        runEffect $ test <<-> relay baseValuespace
  where
    foo = [segq|foo|]
    bar = [segq|bar|]
    barTn = TypeName foo bar
