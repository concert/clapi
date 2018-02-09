{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
module ValuespaceSpec where

import Test.Hspec
import Test.QuickCheck (
    Arbitrary(..), Gen, Property, arbitrary, oneof, elements, listOf, listOf1,
    arbitraryBoundedEnum, vector, vectorOf, property)
import Test.QuickCheck.Instances ()

import Data.Maybe (fromJust)
import Data.Either (either, isRight)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Data.Int
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Lens (view)
import Control.Monad (replicateM)
import Control.Monad.Fail (MonadFail)

import Clapi.TH
import Clapi.Types.AssocList (alFromMap, AssocList, mkAssocList, alSingleton, alEmpty, alFromList, alInsert)
import Clapi.Types
  ( InterpolationLimit(ILUninterpolated), Interpolation(..), WireValue(..)
  , TreeType(..) , TreeConcreteType(..), OfMetaType, Liberty(..)
  , tupleDef, structDef, arrayDef, ErrorIndex(..)
  , toWireValues, valuesToDef, defDispatch, metaType, Definition(..), StructDefinition(strDefTypes)
  , TrpDigest(..), DefOp(..), DataChange(..), TypeName(..))
import qualified Clapi.Types.Path as Path
import Clapi.Types.Path (Path(..), pattern (:/), pattern Root, Seg)
import Clapi.Valuespace (Valuespace(..), validateTree, baseValuespace, processToRelayProviderDigest, apiNs)
import Clapi.Tree (treePaths)
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Tree (RoseTree(RtEmpty))

vsProviderErrorsOn :: Valuespace -> TrpDigest -> [Path] -> Expectation
vsProviderErrorsOn vs d ps = case (processToRelayProviderDigest d vs) of
    Left errMap -> errMap `shouldSatisfy` (\em -> Set.fromList (PathError <$> ps) == Map.keysSet em)
    Right _ -> fail "Did not get expected errors"

validVersionTypeChange :: Valuespace -> TrpDigest
validVersionTypeChange vs =
  let
    svd = tupleDef
      "Stringy" (alSingleton [segq|vstr|] $ TtConc $ TcString "pear")
      ILUninterpolated
    rootDef = redefApiRoot (alInsert [segq|version|] $ TypeName apiNs [segq|stringVersion|]) vs
  in TrpDigest
    apiNs
    (Map.fromList
      [ ([segq|stringVersion|], OpDefine svd)
      , (apiNs, OpDefine rootDef)
      ])
    (alSingleton [pathq|/version|]
      $ ConstChange Nothing [WireValue ("pear" :: Text)])
    mempty
    mempty

vsAppliesCleanly :: MonadFail m => TrpDigest -> Valuespace -> m Valuespace
vsAppliesCleanly d vs = case (processToRelayProviderDigest d vs) of
    Left errs -> fail $ show errs
    Right vs' -> return vs'

redefApiRoot :: (AssocList Seg TypeName -> AssocList Seg TypeName) -> Valuespace -> Definition
redefApiRoot f vs = structDef "Frigged by test" $ (, Cannot) <$> f currentKids
  where
    currentKids = fst <$> (grabDefTypes $ grabApi $ grabApi $ vsTyDefs vs)
    grabApi = fromJust . Map.lookup apiNs
    grabDefTypes (StructDef sd) = strDefTypes sd
    grabDefTypes _ = error "API ns root type not a struct!"

extendedVs :: MonadFail m => Definition -> Seg -> DataChange -> m Valuespace
extendedVs def s dc =
  let
    rootDef = redefApiRoot (alInsert s $ TypeName apiNs s) baseValuespace
    d = TrpDigest
      apiNs
      (Map.fromList
        [ (s, OpDefine def)
        , (apiNs, OpDefine rootDef)])
      (alSingleton (Root :/ s) dc)
      mempty
      mempty
  in vsAppliesCleanly d baseValuespace

vsWithXRef :: MonadFail m => m Valuespace
vsWithXRef =
  let
    newNodeDef = tupleDef
      "for test"
      (alSingleton [segq|daRef|] $ TtConc $ TcRef $ TypeName [segq|api|] [segq|version|])
      ILUninterpolated
    newVal = ConstChange Nothing [WireValue $ Path.toText [pathq|/api/version|]]
  in extendedVs newNodeDef refSeg newVal

refSeg :: Seg
refSeg = [segq|ref|]

spec :: Spec
spec = do
  describe "Validation" $ do
    it "baseValuespace valid" $ validateTree baseValuespace
      `shouldBe` Right baseValuespace
    it "rechecks on type def changes" $
      -- Make sure changing (api, version) goes and checks things defined
      -- to have that type:
      let
          newDef = tupleDef
            "for test"
            (alSingleton [segq|versionString|] $ TtConc $ TcString "apple")
            ILUninterpolated
          d = TrpDigest
            apiNs (Map.singleton [segq|version|] $ OpDefine newDef) alEmpty mempty
            mempty
      in vsProviderErrorsOn baseValuespace d [[pathq|/api/version|]]
    it "can change the version type" $
      (
        vsAppliesCleanly (validVersionTypeChange baseValuespace) baseValuespace
        :: Either String Valuespace)
      `shouldSatisfy` isRight
    it "xref" $ do
      -- Change the type of the instance referenced in a cross reference
      vs <- vsWithXRef
      vsProviderErrorsOn vs (validVersionTypeChange vs) [Root :/ apiNs :/ refSeg]
    it "Array" $
      let
        ars = [segq|arr|]
        vaDef = arrayDef "for test" (TypeName apiNs [segq|version|]) Cannot
        rootDef = redefApiRoot (alInsert ars $ TypeName apiNs ars) baseValuespace
        emptyArrayD = TrpDigest
          apiNs
          (Map.fromList [(ars, OpDefine vaDef), (apiNs, OpDefine rootDef)])
          alEmpty
          (Map.singleton Root $ Map.singleton ars $ (Nothing, SoPresentAfter Nothing))
          mempty
        badChild = TrpDigest
          apiNs
          mempty
          (alSingleton [pathq|/arr/bad|] $ ConstChange Nothing [WireValue ("boo" :: Text)])
          mempty
          mempty
      in do
        vs <- vsAppliesCleanly emptyArrayD baseValuespace
        vsProviderErrorsOn vs badChild [[pathq|/api/arr/bad|]]
