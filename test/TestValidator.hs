{-# LANGUAGE OverloadedStrings #-}
module TestValidator where

import Util (assertFailed)
import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import Types (ClapiValue(..))
import Tree (treeInitNode)
import Validator (success, getRefValidator)

tests = [
    testCase "ref validator" testRefValidator
    ]

testRefValidator =
  do
    assertEqual "success case" success $ v tree (CString "/test/good_value/")
    assertFailed "failure case" $ v tree (CString "/test/bad_value")
  where
    tree =
        treeInitNode ["test", "good_value"] ["test", "target_type"] $
        treeInitNode ["test", "bad_value"] ["test", "wrong_type"]
        mempty
    v = getRefValidator ["test", "target_type"]
