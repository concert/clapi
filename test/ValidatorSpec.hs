{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module ValidatorSpec where

import Test.Hspec

import Control.Monad (void)
import Data.Int
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word

import Clapi.TextSerialisation (ttToText)
import Clapi.TH
import Clapi.Types
  ( Time(..), WireValue(..), TreeType(..), ttEnum, bounds, unbounded)
import Clapi.Validator (validate)

spec :: Spec
spec = describe "validation" $ do
  describeTreeType TtTime $ do
    successCase (WireValue $ Time 2 3)
    failureCase (WireValue @Int32 3)
  describeTreeType (ttEnum $ Proxy @TestEnum) $ do
    successCase (WireValue @Word8 2)
    failureCase (WireValue @Word8 3)
  describe "example bounded numbers" $ do
    bs0 <- either fail return $ bounds Nothing (Just 12)
    describeTreeType (TtInt32 bs0) $ do
      successCase (WireValue @Int32 minBound)
      successCase (WireValue @Int32 12)
      failureCase (WireValue @Int32 13)
    bs1 <- either fail return $ bounds (Just 12) Nothing
    describeTreeType (TtWord32 bs1) $ do
      successCase (WireValue @Word32 maxBound)
      successCase (WireValue @Word32 12)
      failureCase (WireValue @Word32 11)
    bs2 <- either fail return $ bounds (Just 12) (Just 13)
    describeTreeType (TtDouble bs2) $ do
      successCase (WireValue @Double 12)
      successCase (WireValue @Double 12.5)
      successCase (WireValue @Double 13)
      -- NB: these two numbers are empirically tested to the limit of our
      -- precision:
      failureCase (WireValue @Double 13.000000000000001)
      failureCase (WireValue @Double 11.999999999999999)
  describeTreeType (TtString "") $
    successCase (WireValue @Text "banana")
  describeTreeType (TtString "b[an]*") $ do
    successCase (WireValue @Text "banana")
    failureCase (WireValue @Text "apple")
  describeTreeType (TtRef [segq|a|]) $ do
    successCase (WireValue @Text "/x/y")
    failureCase (WireValue @Text "not a path")
  describeTreeType (TtList $ TtString "hello") $ do
    successCase (WireValue @[Text] ["hello", "hello"])
    failureCase (WireValue @[Text] ["hello", "hello", "what's all this then?"])
  describeTreeType (TtSet $ TtFloat unbounded) $ do
    successCase (WireValue @[Float] [41.0, 42.0, 43.0])
    failureCase (WireValue @[Float] [41.0, 42.0, 43.0, 42.0])
  describeTreeType (TtOrdSet $ TtWord32 unbounded) $ do
    successCase (WireValue @[Word32] [41, 42, 43])
    failureCase (WireValue @[Word32] [41, 42, 43, 42])
  describeTreeType (TtMaybe $ TtString "banana") $ do
    successCase (WireValue @(Maybe Text) Nothing)
    successCase (WireValue @(Maybe Text) $ Just "banana")
    failureCase (WireValue @(Maybe Text) $ Just "apple")

describeTreeType :: TreeType -> SpecWith TreeType -> Spec
describeTreeType ty = around (\s -> void $ s ty) .
  describe ("validate of: " ++ (Text.unpack $ ttToText ty))

successCase :: WireValue -> SpecWith TreeType
successCase wv = it ("should accept the valid value: " ++ show wv) $
  \ty -> validate ty wv `shouldBe` Just ()

failureCase :: WireValue -> SpecWith TreeType
failureCase wv = it ("should reject the invalid value: " ++ show wv) $
  \ty -> validate ty wv `shouldBe` Nothing

data TestEnum = One | Two | Three deriving (Show, Enum, Bounded)
