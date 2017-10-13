{-# LANGUAGE OverloadedStrings #-}
module TestValidator where

import Helpers (assertFailed)
import Test.HUnit (assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad (join)
import qualified Data.Set as Set

import Clapi.Types (ClapiValue(..))
import Clapi.Tree (treeInitNode)
import Clapi.Validator (success, fromText, validatorValidator, goValidate)

tests = [
    -- testCase "ref validator" testRefValidator,
    testCase "validator validator" testValidatorValidator
    ]

-- testRefValidator =
--   do
--     assertEqual "success case" success $ result (CString "/test/good_value/")
--     assertFailed "failure case" $ result (CString "/test/bad_value")
--   where
--     tree =
--         treeInitNode ["test", "good_value"] ["test", "target_type"] $
--         treeInitNode ["test", "bad_value"] ["test", "wrong_type"]
--         mempty
--     fv = fromText "ref[/test/target_type]"
--     result cv = join (fv <*> pure tree <*> pure cv)


testValidatorValidator =
  do
    assertFailed "bad description" $ fromText badValue
    assertFailed "bad description" $ fromText badValue'
    assertFailed "bad value" $ validate (CString badValue)
    assertEqual "success" success $ validate (CString "bool")
  where
    validate = goValidate (validatorValidator "desk") undefined
    badValue = "validator[]"
    badValue' = "validator[foo]"
