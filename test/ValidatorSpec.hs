{-# LANGUAGE OverloadedStrings #-}
module ValidatorSpec where

import Test.Hspec

import Control.Monad (join)
import Data.Either (isRight, isLeft)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Int

import Clapi.Types
  ( Time(..), WireValue(..), TreeType(..), TreeConcreteType(..)
  , TreeContainerType(..))
import Clapi.Tree (treeInitNode)
import Clapi.Validator (validate)

spec = describe "Validator" $ do
  describe "time" $ do
    it "should verify a valid value" $
      validate (TtConc TcTime) (WireValue $ Time 2 3) `shouldBe` Just ()
    it "should reject an invalid value" $
      validate (TtConc TcTime) (WireValue (3 :: Int32)) `shouldBe` Nothing
  -- describe "enum" $ do
  --   it "should verify a valid value" $  return ()
  --   it "should reject an invalid value" $  return ()
  -- describe "number" $ do
  --   it "should verify a valid value" $  return ()
  --   it "should reject an invalid value" $  return ()
  -- describe "string" $ do
  --   it "should verify a valid value" $  return ()
  --   it "should reject an invalid value" $  return ()
  -- describe "ref" $ do
  --   it "should verify a valid value" $  return ()
  --   it "should reject an invalid value" $  return ()
  -- describe "validator description" $ do
  --   it "should verify a valid value" $  return ()
  --   it "should reject an invalid value" $  return ()
  -- describe "list" $ do
  --   it "should verify a valid value" $  return ()
  --   it "should reject an invalid value" $  return ()
  -- describe "set" $ do
  --   it "should verify a valid value" $  return ()
  --   it "should reject an invalid value" $  return ()
  -- describe "ordered set" $ do
  --   it "should verify a valid value" $  return ()
  --   it "should reject an invalid value" $  return ()
