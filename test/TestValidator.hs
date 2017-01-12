{-# LANGUAGE OverloadedStrings #-}
module TestValidator where

import Util (assertFailed)
import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad (join)
import qualified Data.Set as Set

import Types (ClapiValue(..))
import Tree (treeInitNode)
import Validator (success, fromText, validatorValidator, duplicates)

tests = [
    testCase "ref validator" testRefValidator,
    testCase "validator validator" testValidatorValidator,
    testCase "duplicates" testDuplicates
    ]

testRefValidator =
  do
    assertEqual "success case" success $ result (CString "/test/good_value/")
    assertFailed "failure case" $ result (CString "/test/bad_value")
  where
    tree =
        treeInitNode ["test", "good_value"] ["test", "target_type"] $
        treeInitNode ["test", "bad_value"] ["test", "wrong_type"]
        mempty
    fv = fromText "ref[/test/target_type]"
    result cv = join (fv <*> pure tree <*> pure cv)


testValidatorValidator =
  do
    assertFailed "bad description" $ fromText badValue
    assertFailed "bad description" $ fromText badValue'
    assertFailed "bad value" $ validate (CString badValue)
    assertEqual "success" success $ validate (CString "bool")
  where
    validate = validatorValidator mempty
    badValue = "validator[]"
    badValue' = "validator[foo]"


testDuplicates =
  do
    assertEqual "empty" mempty $ duplicates ([] :: [Char])
    assertEqual "unique" mempty $ duplicates ['a', 'b', 'c', 'd']
    assertEqual "duplicates" dups $ duplicates as
  where
    as = ['a', 'b', 'a', 'c', 'd', 'd', 'e', 'd']
    dups = Set.fromList ['a', 'd']
