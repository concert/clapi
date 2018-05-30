{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
module ValuespaceSpec where

import Test.Hspec
import Test.QuickCheck (
    Arbitrary(..), Gen, Property, arbitrary, oneof, elements, listOf, listOf1,
    arbitraryBoundedEnum, vector, vectorOf, property)

import Data.Maybe (fromJust)
import Data.Either (either, isRight)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Data.Int
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad (replicateM)
import Control.Monad.Fail (MonadFail)

import qualified Data.Map.Mos as Mos
import Clapi.TH
import Clapi.Types.AssocList
  ( alFromMap, AssocList, mkAssocList, alSingleton, alEmpty, alFromList
  , alInsert)
import Clapi.Types
  ( InterpolationLimit(ILUninterpolated), Interpolation(..), WireValue(..)
  , TreeType(..) , OfMetaType, Liberty(..)
  , tupleDef, structDef, arrayDef, ErrorIndex(..)
  , defDispatch, metaType, Definition(..)
  , StructDefinition(strDefTypes)
  , TrpDigest(..), DefOp(..), DataChange(..), TypeName(..))
import qualified Clapi.Types.Path as Path
import Clapi.Types.Path (Path(..), pattern (:/), pattern Root, Seg)
import Clapi.Valuespace
  ( Valuespace(..), validateVs, baseValuespace, processToRelayProviderDigest
  , processToRelayClientDigest, apiNs, vsRelinquish, ValidationErr(..))
import Clapi.Tree (treePaths, updateTreeWithDigest)
import Clapi.Types.SequenceOps (SequenceOp(..))
import Clapi.Tree (RoseTree(RtEmpty), RoseTreeNodeType(..))

deriving instance Eq RoseTreeNodeType
deriving instance Eq ValidationErr

vsProviderErrorsOn :: Valuespace -> TrpDigest -> [Path] -> Expectation
vsProviderErrorsOn vs d ps = case (processToRelayProviderDigest d vs) of
    Left errMap -> errMap `shouldSatisfy`
      (\em -> Set.fromList (PathError <$> ps) == Map.keysSet em)
    Right _ -> fail "Did not get expected errors"

validVersionTypeChange :: Valuespace -> TrpDigest
validVersionTypeChange vs =
  let
    svd = tupleDef
      "Stringy" (alSingleton [segq|vstr|] $ TtString "pear")
      ILUninterpolated
    rootDef = redefApiRoot
      (alInsert [segq|version|] $ TypeName apiNs [segq|stringVersion|]) vs
  in TrpDigest
    apiNs
    mempty
    (Map.fromList
      [ ([segq|stringVersion|], OpDefine svd)
      , (apiNs, OpDefine rootDef)
      ])
    (alSingleton [pathq|/version|]
      $ ConstChange Nothing [WireValue ("pear" :: Text)])
    mempty
    mempty

vsAppliesCleanly :: MonadFail m => TrpDigest -> Valuespace -> m Valuespace
vsAppliesCleanly d vs = either (fail . show) (return . snd) $
  processToRelayProviderDigest d vs

redefApiRoot
  :: (AssocList Seg TypeName -> AssocList Seg TypeName) -> Valuespace
  -> Definition
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
      mempty
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
      (alSingleton [segq|daRef|] $ TtRef $
        TypeName [segq|api|] [segq|version|])
      ILUninterpolated
    newVal = ConstChange Nothing [WireValue $ Path.toText [pathq|/api/version|]]
  in extendedVs newNodeDef refSeg newVal

refSeg :: Seg
refSeg = [segq|ref|]

emptyArrayD :: Seg -> Valuespace -> TrpDigest
emptyArrayD s vs = TrpDigest
    apiNs
    mempty
    (Map.fromList [(s, OpDefine vaDef), (apiNs, OpDefine rootDef)])
    alEmpty
    mempty
    mempty
  where
    vaDef = arrayDef "for test" (TypeName apiNs [segq|version|]) May
    rootDef = redefApiRoot (alInsert s $ TypeName apiNs s)
      baseValuespace

spec :: Spec
spec = do
  describe "Validation" $ do
    it "baseValuespace valid" $
      let
        apiTn = TypeName [segq|api|]
        allTainted = Map.fromList $ fmap (,Nothing) $ treePaths Root $
          vsTree baseValuespace
        validated = either (error . show) snd $
          validateVs allTainted baseValuespace
      in do
        validated `shouldBe` baseValuespace
    it "rechecks on data changes" $
      let
        d = TrpDigest apiNs mempty mempty
          (alSingleton [pathq|/version|] $
           ConstChange Nothing [WireValue @Text "wrong"])
          mempty mempty
      in vsProviderErrorsOn baseValuespace d [[pathq|/api/version|]]
    it "rechecks on type def changes" $
      -- Make sure changing (api, version) goes and checks things defined
      -- to have that type:
      let
          newDef = tupleDef
            "for test"
            (alSingleton [segq|versionString|] $ TtString "apple")
            ILUninterpolated
          d = TrpDigest
            apiNs mempty (Map.singleton [segq|version|] $ OpDefine newDef)
            alEmpty mempty mempty
      in vsProviderErrorsOn baseValuespace d [[pathq|/api/version|]]
    it "rechecks on container ops" $
      let
        d = TrpDigest
            apiNs
            mempty
            mempty
            alEmpty
            (Map.singleton Root $ Map.singleton [segq|version|] (Nothing, SoAbsent))
            mempty
      in vsProviderErrorsOn baseValuespace d [[pathq|/api|]]
    it "should only re-validate data that has been marked as invalid" $
      let
        p = [pathq|/api/version|]
        badVs = baseValuespace {
          vsTree = snd $ updateTreeWithDigest mempty
            (alSingleton p $ ConstChange Nothing []) $
            vsTree baseValuespace}
        invalidatedPaths = Map.singleton p Nothing
      in do
        -- Validation without specifying the change should miss the bad data:
        either (error . show) snd (validateVs mempty badVs) `shouldBe` badVs
        -- Validation explicitly asking to revalidate the change should fail:
        either id (error . show) (validateVs invalidatedPaths badVs)
          `shouldSatisfy` (not . null)
    it "can change the version type" $
      (
        vsAppliesCleanly (validVersionTypeChange baseValuespace) baseValuespace
        :: Either String Valuespace)
      `shouldSatisfy` isRight
    it "xref referee type change errors" $ do
      -- Change the type of the instance referenced in a cross reference
      vs <- vsWithXRef
      vsProviderErrorsOn vs (validVersionTypeChange vs) [Root :/ apiNs :/ refSeg]
    it "xref old references do not error" $
      let
        v2s = [segq|v2|]
        v2Val = alSingleton (Root :/ v2s) $ ConstChange Nothing
          [WireValue @Word32 1, WireValue @Word32 2, WireValue @Int32 3]
      in do
        vs <- vsWithXRef
        -- Add another version node:
        let v2ApiDef = redefApiRoot
              (alInsert v2s $ TypeName apiNs [segq|version|]) vs
        vs' <- vsAppliesCleanly
          (TrpDigest apiNs mempty (Map.singleton apiNs $ OpDefine v2ApiDef)
            v2Val mempty mempty)
          vs
        -- Update the ref to point at new version:
        vs'' <- vsAppliesCleanly
          (TrpDigest apiNs mempty mempty
            (alSingleton (Root :/ refSeg)
             $ ConstChange Nothing [WireValue $ Path.toText [pathq|/api/v2|]])
            mempty mempty)
          vs'
        (vsAppliesCleanly (validVersionTypeChange vs'') vs''
          :: Either String Valuespace) `shouldSatisfy` isRight
    it "Array" $
      let
        ars = [segq|arr|]
        badChild = TrpDigest
          apiNs
          mempty
          mempty
          (alSingleton [pathq|/arr/bad|] $
            ConstChange Nothing [WireValue ("boo" :: Text)])
          mempty
          mempty
        goodChild = TrpDigest
          apiNs
          mempty
          mempty
          (alSingleton [pathq|/arr/mehearties|] $
            ConstChange Nothing [WireValue @Word32 3, WireValue @Word32 4, WireValue @Int32 3])
          mempty
          mempty
        removeGoodChild = TrpDigest
          apiNs
          mempty
          mempty
          alEmpty
          (Map.singleton [pathq|/arr|] $ Map.singleton [segq|mehearties|] (Nothing, SoAbsent))
          mempty
      in do
        vs <- vsAppliesCleanly (emptyArrayD ars baseValuespace) baseValuespace
        vsProviderErrorsOn vs badChild [[pathq|/api/arr/bad|]]
        vs' <- vsAppliesCleanly goodChild vs
        vs'' <- vsAppliesCleanly removeGoodChild vs'
        vs'' `shouldBe` vs
    it "Errors on struct with missing child" $
      let
        rootDef = redefApiRoot (alInsert [segq|unfilled|] $ TypeName apiNs [segq|version|]) baseValuespace
        missingChild = TrpDigest
          apiNs
          mempty
          (Map.singleton apiNs $ OpDefine rootDef)
          alEmpty
          mempty
          mempty
      in vsProviderErrorsOn baseValuespace missingChild [[pathq|/api|]]
    it "Relinquish" $
      let
        fs = [segq|foo|]
        fooRootDef = arrayDef "frd" (TypeName apiNs [segq|version|]) Cannot
        claimFoo = TrpDigest
          fs
          mempty
          (Map.singleton fs $ OpDefine fooRootDef)
          alEmpty
          mempty
          mempty
      in do
        vs <- vsAppliesCleanly claimFoo baseValuespace
        vsRelinquish fs vs `shouldBe` baseValuespace
    describe "Client" $
        it "Can create new array entries" $
          let
            dd = alSingleton [pathq|/api/arr/a|] $ ConstChange Nothing
                [WireValue @Word32 1, WireValue @Word32 2, WireValue @Int32 3]
          in do
            vs <- vsAppliesCleanly (emptyArrayD [segq|arr|] baseValuespace) baseValuespace
            processToRelayClientDigest mempty dd vs `shouldBe` mempty
