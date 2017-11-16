{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module TestValuespace where

import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (
    Arbitrary(..), Gen, Property, arbitrary, oneof, elements, listOf, listOf1,
    arbitraryBoundedEnum, vector, vectorOf)

import Data.Coerce (coerce)
import Data.Either (either)
import qualified Data.Text as T
import Data.Word (Word16)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Lens (view)
import Control.Monad (replicateM)
import Control.Monad.Fail (MonadFail)

import Helpers (assertFailed)

import Clapi.Path (Path(..))
import Clapi.PathQ
import Clapi.Types (InterpolationType, Interpolation(..), ClapiValue(..))
import Clapi.Validator (validate)
import Clapi.Valuespace
    -- Definition(..), Liberty(..), tupleDef, structDef, arrayDef, validators,
    -- metaType, defToValues, valuesToDef, vsValidate, baseValuespace,
    -- vsAssignType, vsSet, anon, tconst, globalSite)

tests = [
    testCase "tupleDef" testTupleDef,
    testCase "test base valuespace" testBaseValuespace,
    testCase "test child type validation" testChildTypeValidation,
    testCase "test type change invalidates" testTypeChangeInvalidation,
    testCase "xref validation" testXRefValidation,
    testCase "xref revalidation" testXRefRevalidation,
    testProperty "Definition <-> ClapiValue round trip" propDefRoundTrip
    ]

testTupleDef =
  do
    assertFailed "duplicate names" $
        tupleDef "docs" ["hi", "hi"] ["string", "int32"] mempty
    assertFailed "mismatched names/types" $
        tupleDef "docs" ["a", "b"] ["int32"] mempty
    def <- tupleDef "docs" ["a"] ["int32"] mempty
    assertEqual "tuple validation" (return []) $
        validate undefined (view validators def) [ClInt32 42]


testBaseValuespace = assertEqual "correct base valuespace" mempty $
    fst $ vsValidate $ coerce baseValuespace

assertValidationErrors errPaths =
    assertEqual "did not find errors" (Set.fromList errPaths) . Map.keysSet .
    fst . vsValidate

testChildTypeValidation =
  do
    -- Set /api/self/version to have the wrong type, but perfectly valid
    -- data for that wrong type:
    badVs <- vsSet anon IConstant [ClString "banana"] [pathq|/api/self/version|]
          globalSite tconst baseValuespace >>=
        return . vsAssignType [pathq|/api/self/version|]
            [pathq|/api/types/self/build|]
    assertValidationErrors [[pathq|/api/self|]] badVs


testTypeChangeInvalidation =
  do
    -- Make sure changing /api/types/self/version goes and checks things defined
    -- to have that type:
    newDef <- tupleDef "for test" ["versionString"] ["string[apple]"]
        mempty
    badVs <- vsSet anon IConstant (defToValues newDef)
        [pathq|/api/types/self/version|] globalSite tconst baseValuespace
    assertValidationErrors [[pathq|/api/self/version|]] badVs


vsWithXRef :: (MonadFail m) => m (Valuespace Unvalidated)
vsWithXRef =
  do
    newCDef <- structDef "updated for test"
        ["base", "self", "containers", "test_type", "test_value"] [
          [pathq|/api/types/containers/base|],
          [pathq|/api/types/containers/types_self|],
          [pathq|/api/types/containers/containers|],
          (metaTypePath Tuple),
          [pathq|/api/types/test_type|]]
        [Cannot, Cannot, Cannot, Cannot, Cannot]
    newNodeDef <- tupleDef "for test" ["daRef"]
        ["ref[/api/types/self/version]"] mempty
    vsAdd anon IConstant (defToValues newNodeDef)
          [pathq|/api/types/test_type|] globalSite tconst
          baseValuespace >>=
        vsSet anon IConstant (defToValues newCDef)
          [pathq|/api/types/containers/types|] globalSite tconst >>=
        -- FIXME: should infer this from the container
        return . vsAssignType [pathq|/api/types/test_type|]
          (metaTypePath Tuple) >>=
        vsAdd anon IConstant [ClString "/api/self/version"]
          [pathq|/api/types/test_value|] globalSite tconst >>=
        -- FIXME: should infer this from the container
        return . vsAssignType [pathq|/api/types/test_value|]
          [pathq|/api/types/test_type|]


testXRefValidation =
  do
    -- Change the type of the instance referenced in a cross reference
    vs <- vsWithXRef
    assertValidationErrors [] vs
    badVs <- vsSet anon IConstant [ClString "/api/self/build"]
        [pathq|/api/types/test_value|] globalSite tconst vs
    assertValidationErrors [[pathq|/api/types/test_value|]] badVs


testXRefRevalidation =
  do
    newDef <- structDef "for test" ["build", "version"]
        [[pathq|/api/types/self/build|], [pathq|/api/types/self/build|]]
        [Cannot, Cannot]
    badVs <- vsWithXRef >>=
        vsSet anon IConstant (defToValues newDef)
            [pathq|/api/types/containers/self|] globalSite tconst >>=
        return . vsAssignType [pathq|/api/self/version|]
            [pathq|/api/types/self/build|] >>=
        vsSet anon IConstant [ClString "banana"] [pathq|/api/self/version|]
            globalSite tconst
    assertValidationErrors [[pathq|/api/types/test_value|]] badVs


arbitraryPath :: Gen Path
arbitraryPath = fmap Path $ listOf $ fmap T.pack $ listOf1 $ elements ['a'..'z']

instance Arbitrary Liberty where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary InterpolationType where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

-- http://stackoverflow.com/questions/9542313/
infiniteStrings :: Int -> [String]
infiniteStrings minLen = [minLen..] >>= (`replicateM` ['a'..'z'])

instance Arbitrary Definition where
    arbitrary =
      do
        n <- arbitrary
        fDef <- oneof [
            tupleDef <$> arbitrary <*>
                (pure $ take n $ T.pack <$> infiniteStrings 4) <*>
                pure (replicate n "int32") <*> arbitrary,
            structDef <$> arbitrary <*> vector n <*>
                vectorOf n arbitraryPath <*> vector n,
            arrayDef <$> arbitrary <*> arbitraryPath <*> arbitrary]
        either error return fDef


propDefRoundTrip :: Definition -> Bool
propDefRoundTrip d = (valuesToDef (metaType d) . defToValues) d == Just d
