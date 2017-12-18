{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module TestValuespace where

import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (
    Arbitrary(..), Gen, Property, arbitrary, oneof, elements, listOf, listOf1,
    arbitraryBoundedEnum, vector, vectorOf)

import Data.Either (either)
import qualified Data.Text as T
import Data.Word (Word16)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Lens (view)
import Control.Monad (replicateM)
import Control.Monad.Fail (MonadFail)

import Helpers (assertFailed)

import Clapi.Path (Path(..), (+|))
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
    testCase "array handling" testArray,
    testCase "cyclic inference" testCyclicInference,
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
    fst $ vsValidate $ ownerUnlock baseValuespace

assertValidationErrors :: [Path] -> Valuespace OwnerUnvalidated -> IO ()
assertValidationErrors errPaths vs = let (errs, _vvs) = vsValidate vs in
    assertEqual
        (show errs ++ " did not contain expected paths")
        (Set.fromList errPaths) (Map.keysSet errs)

testChildTypeValidation =
  do
    -- Set /api/self/version to have the wrong type, but perfectly valid
    -- data for that wrong type:
    badVs <- vsSet
          anon IConstant
          [ClString "doccy", ClList [], ClList [], ClList [ClEnum 0]]
          [pathq|/api/self/version|]
          globalSite tconst (ownerUnlock baseValuespace) >>=
        return . vsAssignType [pathq|/api/self/version|]
            [pathq|/api/types/base/tuple|]
    assertValidationErrors [[pathq|/api/self|]] badVs


testTypeChangeInvalidation =
  do
    -- Make sure changing /api/types/self/version goes and checks things defined
    -- to have that type:
    newDef <- tupleDef "for test" ["versionString"] ["string[apple]"]
        mempty
    badVs <- vsSet anon IConstant (defToValues newDef)
        [pathq|/api/types/self/version|] globalSite tconst
        (ownerUnlock baseValuespace)
    assertValidationErrors [[pathq|/api/self/version|]] badVs


extendedVs :: (MonadFail m) => Definition -> m (Valuespace OwnerUnvalidated, Path)
extendedVs def = do
    newCDef <- structDef "updated for test"
        ["base", "self", "containers", "test_type", "test_value"] [
          [pathq|/api/types/containers/base|],
          [pathq|/api/types/containers/types_self|],
          [pathq|/api/types/containers/containers|],
          (metaTypePath $ metaType def),
          [pathq|/api/types/test_type|]]
        [Cannot, Cannot, Cannot, Cannot, Cannot]
    vs <- vsAdd anon IConstant (defToValues def) [pathq|/api/types/test_type|]
          globalSite tconst (ownerUnlock baseValuespace) >>=
        vsSet anon IConstant (defToValues newCDef)
          [pathq|/api/types/containers/types|] globalSite tconst
    return (vs, [pathq|/api/types/test_value|])


vsWithXRef :: (MonadFail m) => m (Valuespace OwnerUnvalidated)
vsWithXRef =
  do
    newNodeDef <- tupleDef "for test" ["daRef"]
        ["ref[/api/types/self/version]"] mempty
    (vs, testPath) <- extendedVs newNodeDef
    vsAdd anon IConstant [ClString "/api/self/version"] testPath globalSite tconst vs


testXRefValidation =
  do
    -- Change the type of the instance referenced in a cross reference
    vs <- vsWithXRef
    assertValidationErrors [] vs
    badVs <- vsSet anon IConstant [ClString "/api/types/base/tuple"]
        [pathq|/api/types/test_value|] globalSite tconst vs
    assertValidationErrors [[pathq|/api/types/test_value|]] badVs


testXRefRevalidation =
  do
    newDef <- structDef "for test" ["version"]
        [[pathq|/api/types/base/tuple|]]
        [Cannot]
    badVs <- vsWithXRef >>=
        vsSet anon IConstant (defToValues newDef)
            [pathq|/api/types/containers/self|] globalSite tconst >>=
        vsSet anon IConstant [ClString "banana", ClList [], ClList [], ClList [ClEnum 0]] [pathq|/api/self/version|]
            globalSite tconst
    assertValidationErrors [[pathq|/api/types/test_value|]] badVs


testArray = do
    newDef <- arrayDef "for test" [pathq|/api/types/self/version|] Cannot
    (evs, testPath) <- extendedVs newDef
    assertValidationErrors [[pathq|/api/types|]] evs
    emptyArrayVs <- vsSetChildren testPath [] evs
    assertValidationErrors [] emptyArrayVs
    missingEntryVs <- vsSetChildren testPath ["a"] emptyArrayVs
    assertValidationErrors [testPath] missingEntryVs
    filledEntryVs <- vsAdd anon IConstant [ClWord32 0, ClInt32 12]
        (testPath +| "a") globalSite tconst missingEntryVs
    assertValidationErrors [] filledEntryVs


addDef ::
    MonadFail m => Path -> Definition -> Valuespace OwnerUnvalidated ->
    m (Valuespace OwnerUnvalidated)
addDef p d = vsAdd anon IConstant (defToValues d) p globalSite tconst


testCyclicInference = do
    outerType <- structDef "outer" ["defs"] [[pathq|/api/cyclic/defs/cyclic|]] [Cannot]
    innerType <- structDef "inner" ["cyclic", "defs"] [[pathq|/api/types/base/struct|], [pathq|/api/types/base/struct|]] [Cannot, Cannot]
    newCDef <- structDef "updated for test"
        ["self", "types", "cyclic"] [
          [pathq|/api/types/containers/self|],
          [pathq|/api/types/containers/types|],
          [pathq|/api/cyclic/defs/cyclic|]]
        [Cannot, Cannot, Cannot]
    badVs <- vsSet anon IConstant (defToValues newCDef)
          [pathq|/api/types/containers/api|] globalSite tconst
          (ownerUnlock baseValuespace) >>=
        addDef [pathq|/api/cyclic/defs/cyclic|] outerType >>=
        addDef [pathq|/api/cyclic/defs/defs|] innerType -- >>=
    assertValidationErrors [
        [pathq|/api/cyclic|], [pathq|/api/cyclic/defs|],
        [pathq|/api/cyclic/defs/cyclic|], [pathq|/api/cyclic/defs/defs|],
        [pathq|/api/types/containers/api|]]
        badVs

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
