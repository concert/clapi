{-# LANGUAGE OverloadedStrings #-}
module ValidatorSpec where

import Test.Hspec

import Control.Monad (join)
import Data.Either (isRight, isLeft)
import qualified Data.Set as Set

import Clapi.Types (ClapiValue(..))
import Clapi.Tree (treeInitNode)
import Clapi.Validator (success, fromText, validatorValidator, goValidate)

-- testRefValidator =
--   do
--     assertEqual "success case" success $ result (ClString "/test/good_value/")
--     assertFailed "failure case" $ result (ClString "/test/bad_value")
--   where
--     tree =
--         treeInitNode ["test", "good_value"] ["test", "target_type"] $
--         treeInitNode ["test", "bad_value"] ["test", "wrong_type"]
--         mempty
--     fv = fromText "ref[/test/target_type]"
--     result cv = join (fv <*> pure tree <*> pure cv)


spec = describe "Validator" $ do
    it "Fails on no description" $ fromText badValue `shouldSatisfy` isLeft
    it "Fails on foo" $ fromText badValue' `shouldSatisfy` isLeft
    it "Fails from wrapper" $ validate (ClString badValue) `shouldSatisfy` isLeft
    it "Works in success case" $ validate (ClString "int32") `shouldSatisfy` isRight
  where
    validate = goValidate (validatorValidator "desk") undefined
    badValue = "validator[]"
    badValue' = "validator[foo]"
