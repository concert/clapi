{-# LANGUAGE OverloadedStrings #-}
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

import Clapi.Path (Path)
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
        tupleDef Cannot "docs" ["hi", "hi"] ["bool", "int32"] mempty
    assertFailed "mismatched names/types" $
        tupleDef Cannot "docs" ["a", "b"] ["bool"] mempty
    def <- tupleDef Cannot "docs" ["a"] ["bool"] mempty
    assertEqual "tuple validation" (return []) $
        validate undefined (view validators def) [CBool True]


testBaseValuespace = assertEqual "correct base valuespace" mempty $
    fst $ vsValidate $ coerce baseValuespace

assertValidationErrors errPaths =
    assertEqual "did not find errors" (Set.fromList errPaths) . Map.keysSet .
    fst . vsValidate

testChildTypeValidation =
  do
    -- Set /api/self/version to have the wrong type, but perfectly valid
    -- data for that wrong type:
    badVs <- vsSet anon IConstant [CString "banana"] ["api", "self", "version"]
          globalSite tconst baseValuespace >>=
        return . vsAssignType ["api", "self", "version"]
          ["api", "types", "self", "build"]
    assertValidationErrors [["api", "self"]] badVs


testTypeChangeInvalidation =
  do
    -- Make sure changing /api/types/self/version goes and checks things defined
    -- to have that type:
    newDef <- tupleDef Cannot "for test" ["versionString"] ["string[apple]"]
        mempty
    badVs <- vsSet anon IConstant (defToValues newDef)
        ["api", "types", "self", "version"] globalSite tconst baseValuespace
    assertValidationErrors [["api", "self", "version"]] badVs


vsWithXRef :: (MonadFail m) => m (Valuespace Unvalidated)
vsWithXRef =
  do
    newCDef <- structDef Cannot "updated for test"
        ["base", "self", "containers", "test_type", "test_value"] [
          ["api", "types", "containers", "base"],
          ["api", "types", "containers", "types_self"],
          ["api", "types", "containers", "containers"],
          (metaTypePath Tuple),
          ["api", "types", "test_type"]]
        [Cannot, Cannot, Cannot, Cannot, Cannot]
    newNodeDef <- tupleDef Cannot "for test" ["daRef"]
        ["ref[/api/types/self/version]"] mempty
    vsAdd anon IConstant (defToValues newNodeDef)
          ["api", "types", "test_type"] globalSite tconst
          baseValuespace >>=
        vsSet anon IConstant (defToValues newCDef)
          ["api", "types", "containers", "types"] globalSite tconst >>=
        -- FIXME: should infer this from the container
        return . vsAssignType ["api", "types", "test_type"]
          (metaTypePath Tuple) >>=
        vsAdd anon IConstant [CString "/api/self/version"]
          ["api", "types", "test_value"] globalSite tconst >>=
        -- FIXME: should infer this from the container
        return . vsAssignType ["api", "types", "test_value"]
          ["api", "types", "test_type"]


testXRefValidation =
  do
    -- Change the type of the instance referenced in a cross reference
    vs <- vsWithXRef
    assertValidationErrors [] vs
    badVs <- vsSet anon IConstant [CString "/api/self/build"]
        ["api", "types", "test_value"] globalSite tconst vs
    assertValidationErrors [["api", "types", "test_value"]] badVs


testXRefRevalidation =
  do
    newDef <- structDef Cannot "for test" ["build", "version"]
        [["api", "types", "self", "build"], ["api", "types", "self", "build"]]
        [Cannot, Cannot]
    badVs <- vsWithXRef >>=
        vsSet anon IConstant (defToValues newDef)
            ["api", "types", "containers", "self"] globalSite tconst >>=
        return . vsAssignType ["api", "self", "version"]
            ["api", "types", "self", "build"] >>=
        vsSet anon IConstant [CString "banana"] ["api", "self", "version"]
            globalSite tconst
    assertValidationErrors [["api", "types", "test_value"]] badVs


arbitraryPath :: Gen Path
arbitraryPath = listOf $ listOf1 $ elements ['a'..'z']

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
            tupleDef <$> arbitrary <*> arbitrary <*>
                (pure $ take n $ infiniteStrings 4) <*>
                pure (replicate n "bool") <*> arbitrary,
            structDef <$> arbitrary <*> arbitrary <*> vector n <*>
                vectorOf n arbitraryPath <*> vector n,
            arrayDef <$> arbitrary <*> arbitrary <*> arbitraryPath <*>
                arbitrary]
        either error return fDef


propDefRoundTrip :: Definition -> Bool
propDefRoundTrip d = (valuesToDef (metaType d) . defToValues) d == Just d
