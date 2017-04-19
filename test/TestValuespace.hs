{-# LANGUAGE OverloadedStrings #-}
module TestValuespace where

import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (
    Arbitrary(..), Gen, Property, arbitrary, oneof, elements, listOf, listOf1,
    arbitraryBoundedEnum, vector, vectorOf)

import Data.Either (either)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T
import Data.Word (Word16)
import qualified Data.Map.Strict as Map
import Control.Lens (view)
import Control.Monad (replicateM)

import Helpers (assertFailed)

import Path (Path)
import Types (InterpolationType, ClapiValue(..))
import Validator (validate)
import Valuespace (
    --getBaseValuespace, getTree, Liberty, Definition(..), metaType, tupleDef,
    Liberty(..), tupleDef,
    structDef, arrayDef, definitionValidators, defToValues, valuesToDef)
import Tree (treeOrphansAndMissing)

tests = [
    testCase "tupleDef" testTupleDef
    -- testCase "test base valuespace" testBaseValuespace,
    -- testProperty "Definition <-> ClapiValue round trip" propDefRoundTrip
    ]

testTupleDef =
  do
    assertFailed "duplicate names" $
        tupleDef Cannot "docs" ["hi", "hi"] ["bool", "int32"] mempty
    assertFailed "mismatched names/types" $
        tupleDef Cannot "docs" ["a", "b"] ["bool"] mempty
    let def = fromJust $ tupleDef Cannot "docs" ["a"] ["bool"] mempty
    assertEqual "tuple validation" (Right ()) $
        validate undefined (definitionValidators def) [CBool True]


-- testBaseValuespace = assertEqual "clean base valuespace" (mempty, mempty) $
--     treeOrphansAndMissing baseVsTree
--     -- FIXME: this misses latent errors in the values because of laziness
--   where
--     baseVsTree = view getTree getBaseValuespace


-- arbitraryPath :: Gen Path
-- arbitraryPath = listOf $ listOf1 $ elements ['a'..'z']

-- instance Arbitrary Liberty where
--     arbitrary = arbitraryBoundedEnum

-- instance Arbitrary InterpolationType where
--     arbitrary = arbitraryBoundedEnum

-- instance Arbitrary T.Text where
--     arbitrary = T.pack <$> arbitrary

-- instance Arbitrary Definition where
--     arbitrary =
--       do
--         n <- arbitrary
--         fDef <- oneof [
--             tupleDef <$> arbitrary <*> arbitrary <*> vector n <*>
--                 return (replicate n "bool") <*> arbitrary,
--             structDef <$> arbitrary <*> arbitrary <*> vector n <*>
--                 vectorOf n arbitraryPath <*> vector n,
--             arrayDef <$> arbitrary <*> arbitrary <*> arbitraryPath <*>
--                 arbitrary]
--         either error return fDef


-- propDefRoundTrip :: Definition -> Bool
-- propDefRoundTrip d = (valuesToDef (metaType d) . defToValues) d == Just d
