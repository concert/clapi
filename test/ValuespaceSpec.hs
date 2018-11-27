{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
module ValuespaceSpec where

import Test.Hspec

import Data.Maybe (fromJust)
import Data.Either (either, isRight, isLeft)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Data.Word
import Data.Int
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad (void)
import Control.Monad.Fail (MonadFail)

import qualified Data.Map.Mol as Mol

import Clapi.TH
import Clapi.Types.AssocList
  ( AssocList, alSingleton, alEmpty, alInsert, alFromList)
import Clapi.Types
  ( InterpolationLimit(ILUninterpolated), WireValue(..)
  , TreeType(..), unbounded, Editable(..)
  , tupleDef, structDef, arrayDef, DataErrorIndex(..)
  , Definition(..)
  , StructDefinition(strDefTypes)
  , TrpDigest(..), DefOp(..), DataChange(..)
  , TrcUpdateDigest(..), trcudEmpty)
import qualified Clapi.Types.Path as Path
import Clapi.Types.Path
  ( Path, pattern (:/), pattern Root, Seg, Namespace(..))
import Clapi.Valuespace
  ( Valuespace(..), validateVs, baseValuespace, processToRelayProviderDigest
  , processTrcUpdateDigest, ValidationErr(..))
import Clapi.Tree
  ( RoseTree(..), RoseTreeNodeType(..), treePaths, updateTreeWithDigest)
import Clapi.Types.SequenceOps (SequenceOp(..))

deriving instance Eq RoseTreeNodeType
deriving instance Eq ValidationErr

-- | Fully revalidates the given Valuespace and throws an error if there are any
--   validation issues.
unsafeValidateVs :: Valuespace -> Valuespace
unsafeValidateVs vs = either (error . show) snd $ validateVs allTainted vs
  where
    allTainted = Map.fromList $ fmap (,Nothing) $ treePaths Root $ vsTree vs


testS :: Seg
testS = [segq|test|]
testNs :: Namespace
testNs = Namespace testS

versionS :: Seg
versionS = [segq|version|]

testValuespace :: Valuespace
testValuespace = unsafeValidateVs $ (baseValuespace (Tagged testS) Editable)
  { vsTyDefs = Map.fromList
      [ (Tagged testS, structDef "test root" $ alFromList
          [ (versionS, (Tagged versionS, ReadOnly))
          ])
      , (Tagged versionS, tupleDef
          "versioney" (alSingleton versionS $ TtInt32 unbounded) ILUninterpolated)
      ]
  , vsTree = RtContainer $ alSingleton versionS
      (Nothing, RtConstData Nothing [WireValue @Int32 3])
  }

vsProviderErrorsOn :: Valuespace -> TrpDigest -> [Path] -> Expectation
vsProviderErrorsOn vs d ps = case (processToRelayProviderDigest d vs) of
    Left errMap -> Mol.keysSet errMap `shouldBe` Set.fromList (PathError <$> ps)
    Right _ -> fail "Did not get expected errors"

vsClientErrorsOn :: Valuespace -> TrcUpdateDigest -> [Path] -> Expectation
vsClientErrorsOn vs d ps = let (errMap, _) = processTrcUpdateDigest vs d in
  if (null errMap)
    then fail "Did not get expected errors"
    else Mol.keysSet errMap `shouldBe` Set.fromList (PathError <$> ps)

validVersionTypeChange :: Valuespace -> TrpDigest
validVersionTypeChange vs =
  let
    svd = tupleDef
      "Stringy" (alSingleton [segq|vstr|] $ TtString "pear")
      ILUninterpolated
    rootDef = redefTestRoot
      (alInsert versionS $ Tagged [segq|stringVersion|]) vs
  in TrpDigest
    testNs
    mempty
    (Map.fromList
      [ (Tagged [segq|stringVersion|], OpDefine svd)
      , (Tagged $ unNamespace testNs, OpDefine rootDef)
      ])
    (alSingleton [pathq|/version|]
      $ ConstChange Nothing [WireValue ("pear" :: Text)])
    mempty
    mempty

vsAppliesCleanly :: MonadFail m => TrpDigest -> Valuespace -> m Valuespace
vsAppliesCleanly d vs = either (fail . show) (return . snd) $
  processToRelayProviderDigest d vs

redefTestRoot
  :: (AssocList Seg (Tagged Definition Seg)
      -> AssocList Seg (Tagged Definition Seg))
  -> Valuespace -> Definition
redefTestRoot f vs =
    structDef "Frigged by test" $ (, ReadOnly) <$> f currentKids
  where
    currentKids = fmap fst $ grabDefTypes $ fromJust $
      Map.lookup (Tagged $ unNamespace testNs) $ vsTyDefs vs
    grabDefTypes (StructDef sd) = strDefTypes sd
    grabDefTypes _ = error "Test vs root type not a struct!"

extendedVs :: MonadFail m => Definition -> Seg -> DataChange -> m Valuespace
extendedVs def s dc =
  let
    rootDef = redefTestRoot (alInsert s $ Tagged s) testValuespace
    d = TrpDigest
      testNs
      mempty
      (Map.fromList
        [ (Tagged s, OpDefine def)
        , (Tagged $ unNamespace testNs, OpDefine rootDef)])
      (alSingleton (Root :/ s) dc)
      mempty
      mempty
  in vsAppliesCleanly d testValuespace

vsWithXRef :: MonadFail m => m Valuespace
vsWithXRef =
  let
    newNodeDef = tupleDef
      "for test"
      -- FIXME: Should the ref seg be tagged?:
      (alSingleton [segq|daRef|] $ TtRef versionS)
      ILUninterpolated
    newVal = ConstChange Nothing
      [WireValue $ Path.toText Path.unSeg [pathq|/version|]]
  in extendedVs newNodeDef refSeg newVal

refSeg :: Seg
refSeg = [segq|ref|]

emptyArrayD :: Seg -> Valuespace -> TrpDigest
emptyArrayD s vs = TrpDigest
    testNs
    mempty
    (Map.fromList
     [ (Tagged s, OpDefine vaDef)
     , (Tagged $ unNamespace testNs, OpDefine rootDef)])
    alEmpty
    mempty
    mempty
  where
    vaDef = arrayDef "for test" Nothing (Tagged [segq|version|]) Editable
    -- FIXME: is vs always testValuespace?
    rootDef = redefTestRoot (alInsert s $ Tagged s) vs

spec :: Spec
spec = do
  return ()
  describe "Validation" $ do
    it "raw baseValuespace invalid" $
      let
        rawValuespace = baseValuespace (Tagged testS) Editable
        allTainted = Map.fromList $ fmap (,Nothing) $ treePaths Root $
          vsTree rawValuespace
      in validateVs allTainted rawValuespace `shouldSatisfy` isLeft
    it "rechecks on data changes" $
      let
        d = TrpDigest testNs mempty mempty
          (alSingleton [pathq|/version|] $
           ConstChange Nothing [WireValue @Text "wrong"])
          mempty mempty
      in vsProviderErrorsOn testValuespace d [[pathq|/version|]]
    it "rechecks on type def changes" $
      -- Make sure changing (api, version) goes and checks things defined
      -- to have that type:
      let
          newDef = tupleDef
            "for test"
            (alSingleton [segq|versionString|] $ TtString "apple")
            ILUninterpolated
          d = TrpDigest
            testNs mempty
            (Map.singleton (Tagged versionS) $ OpDefine newDef)
            alEmpty mempty mempty
      in vsProviderErrorsOn testValuespace d [[pathq|/version|]]
    it "rechecks on container ops" $
      let
        d = TrpDigest
            testNs
            mempty
            mempty
            alEmpty
            (Map.singleton Root $ Map.singleton [segq|version|] (Nothing, SoAbsent))
            mempty
      in vsProviderErrorsOn testValuespace d [Root]
    it "should only re-validate data that has been marked as invalid" $
      let
        p = [pathq|/api/version|]
        badVs = testValuespace {
          vsTree = snd $ updateTreeWithDigest mempty
            (alSingleton p $ ConstChange Nothing []) $
            vsTree testValuespace}
        invalidatedPaths = Map.singleton p Nothing
      in do
        -- Validation without specifying the change should miss the bad data:
        either (error . show) snd (validateVs mempty badVs) `shouldBe` badVs
        -- Validation explicitly asking to revalidate the change should fail:
        either id (error . show) (validateVs invalidatedPaths badVs)
          `shouldSatisfy` (not . null)
    it "can change the version type" $
      (
        vsAppliesCleanly (validVersionTypeChange testValuespace) testValuespace
        :: Either String Valuespace)
      `shouldSatisfy` isRight
    it "xref referee type change errors" $ do
      -- Change the type of the instance referenced in a cross reference
      vs <- vsWithXRef
      vsProviderErrorsOn vs (validVersionTypeChange vs)
        [Root :/ refSeg]
    it "xref old references do not error" $
      let
        v2s = [segq|v2|]
        v2Val = alSingleton (Root :/ v2s) $ ConstChange Nothing
          [WireValue @Int32 123]
      in do
        vs <- vsWithXRef
        -- Add another version node:
        let v2ApiDef = redefTestRoot
              (alInsert v2s $ Tagged [segq|version|]) vs
        vs' <- vsAppliesCleanly
          (TrpDigest testNs mempty
            (Map.singleton (Tagged $ unNamespace testNs) $ OpDefine v2ApiDef)
            v2Val mempty mempty)
          vs
        -- Update the ref to point at new version:
        vs'' <- vsAppliesCleanly
          (TrpDigest testNs mempty mempty
            (alSingleton (Root :/ refSeg)
             $ ConstChange Nothing
             [WireValue $ Path.toText Path.unSeg [pathq|/v2|]])
            mempty mempty)
          vs'
        (vsAppliesCleanly (validVersionTypeChange vs'') vs''
          :: Either String Valuespace) `shouldSatisfy` isRight
    it "Copes with contOps and set in same bundle" $
      let
        xS = [segq|cross|]
        aS = [segq|a|]
        vs = baseValuespace (Tagged xS) Editable
        d = TrpDigest
            (Namespace xS)
            mempty
            (Map.fromList
              [ (Tagged xS, OpDefine $ arrayDef "kriss" Nothing (Tagged aS) ReadOnly)
              , (Tagged aS, OpDefine $ tupleDef "ref a" (alSingleton aS $ TtInt32 unbounded) ILUninterpolated)
              ])
            (alSingleton [pathq|/ard|] $ ConstChange Nothing [WireValue @Int32 3])
            (Map.singleton [pathq|/|] $ Map.singleton [segq|ard|] (Nothing, SoAbsent))
            mempty
      in void $ vsAppliesCleanly d vs :: IO ()
    it "Array" $
      let
        ars = [segq|arr|]
        badChild = TrpDigest
          testNs
          mempty
          mempty
          (alSingleton [pathq|/arr/bad|] $
            ConstChange Nothing [WireValue @Text "boo"])
          mempty
          mempty
        goodChild = TrpDigest
          testNs
          mempty
          mempty
          (alSingleton [pathq|/arr/mehearties|] $
            ConstChange Nothing [WireValue @Int32 3])
          mempty
          mempty
        removeGoodChild = TrpDigest
          testNs
          mempty
          mempty
          alEmpty
          (Map.singleton [pathq|/arr|] $ Map.singleton [segq|mehearties|] (Nothing, SoAbsent))
          mempty
      in do
        vs <- vsAppliesCleanly (emptyArrayD ars testValuespace) testValuespace
        vsProviderErrorsOn vs badChild [[pathq|/arr/bad|]]
        vs' <- vsAppliesCleanly goodChild vs
        vs'' <- vsAppliesCleanly removeGoodChild vs'
        vs'' `shouldBe` vs
    it "Errors on struct with missing child" $
      let
        rootDef = redefTestRoot
          (alInsert [segq|unfilled|] $ Tagged [segq|version|])
          testValuespace
        missingChild = TrpDigest
          testNs
          mempty
          (Map.singleton (Tagged $ unNamespace testNs) $ OpDefine rootDef)
          alEmpty
          mempty
          mempty
      in vsProviderErrorsOn testValuespace missingChild [Root]
    it "Allows nested empty containers" $
      let
        emptyS = [segq|empty|]
        arrS = [segq|arr|]
        emptyNest = TrpDigest
            (Namespace emptyS)
            mempty
            (Map.fromList
              [ (Tagged emptyS, OpDefine $ structDef "oaea" $ alSingleton arrS (Tagged arrS, ReadOnly))
              , (Tagged arrS, OpDefine $ arrayDef "ea" Nothing (Tagged arrS) ReadOnly)
              ])
            alEmpty
            mempty
            mempty
        addToNestedStruct = TrpDigest
            (Namespace emptyS)
            mempty
            mempty
            alEmpty
            (Map.singleton [pathq|/arr|] $ Map.singleton emptyS (Nothing, SoAfter Nothing))
            mempty
      in do
        vs <- vsAppliesCleanly emptyNest $ baseValuespace (Tagged emptyS) Editable
        void $ vsAppliesCleanly addToNestedStruct vs :: IO ()
    it "Allows contops in array declaring digest" $
      let
        codS = [segq|cod|]
        codb = TrpDigest
            (Namespace codS)
            mempty
            (Map.singleton (Tagged codS) $ OpDefine $ arrayDef "fishy" Nothing (Tagged codS) ReadOnly)
            alEmpty
            (Map.singleton Root $ Map.singleton codS (Nothing, SoAfter Nothing))
            mempty
      in void $ vsAppliesCleanly codb $ baseValuespace (Tagged codS) Editable :: IO ()
    it "Rejects recursive struct" $
      let
        rS = [segq|r|]
        rDef = TrpDigest
            (Namespace rS)
            mempty
            (Map.singleton (Tagged rS) $ OpDefine $ structDef "r4eva" $ alSingleton rS (Tagged rS, ReadOnly))
            alEmpty
            mempty
            mempty
      in vsProviderErrorsOn (baseValuespace (Tagged rS) ReadOnly) rDef [Root]
    describe "Client" $
        it "Cannot itself create new array entries" $
          let
            dd = alSingleton [pathq|/arr/a|] $ ConstChange Nothing
                [WireValue @Word32 1, WireValue @Word32 2, WireValue @Int32 3]
            trcud = (trcudEmpty testNs) {trcudData = dd}
          in do
            vs <- vsAppliesCleanly (emptyArrayD [segq|arr|] testValuespace) testValuespace
            vsClientErrorsOn vs trcud [[pathq|/arr/a|]]
