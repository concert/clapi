{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}

module ValidatorSpec where

import Test.Hspec

import Control.Monad (void)
import Data.Constraint (Dict(..))
import Data.Either (isLeft, isRight)
import qualified Data.Text as Text

import Clapi.TextSerialisation (ttToText)
import Clapi.TH
import Clapi.Types
  ( Time(..), WireType(..), SomeWireValue(..), someWv, someWireable
  , TreeType(..), bounds, unbounded, getTtShow)
import qualified Clapi.Types.SymbolList as SL
import Clapi.Validator (validate)

spec :: Spec
spec = describe "validation" $ do
  describeTreeType TtTime $ do
    successCase (someWireable $ Time 2 3)
    failureCase (someWv WtInt32 3)
  describeTreeType (TtEnum $ SL.fromType @('[ "One", "Two", "Three" ])) $ do
    successCase (someWv WtWord32 2)
    failureCase (someWv WtWord32 3)
  describe "example bounded numbers" $ do
    bs0 <- either fail return $ bounds Nothing (Just 12)
    describeTreeType (TtInt32 bs0) $ do
      successCase (someWv WtInt32 minBound)
      successCase (someWv WtInt32 12)
      failureCase (someWv WtInt32 13)
    bs1 <- either fail return $ bounds (Just 12) Nothing
    describeTreeType (TtWord32 bs1) $ do
      successCase (someWv WtWord32 maxBound)
      successCase (someWv WtWord32 12)
      failureCase (someWv WtWord32 11)
    bs2 <- either fail return $ bounds (Just 12) (Just 13)
    describeTreeType (TtDouble bs2) $ do
      successCase (someWv WtDouble 12)
      successCase (someWv WtDouble 12.5)
      successCase (someWv WtDouble 13)
      -- NB: these two numbers are empirically tested to the limit of our
      -- precision:
      failureCase (someWv WtDouble 13.000000000000001)
      failureCase (someWv WtDouble 11.999999999999999)
  describeTreeType (TtString "") $
    successCase (someWv WtString "banana")
  describeTreeType (TtString "b[an]*") $ do
    successCase (someWv WtString "banana")
    failureCase (someWv WtString "apple")
  describeTreeType (TtRef [n|a|]) $ do
    successCase (someWv WtString "/x/y")
    failureCase (someWv WtString "not a path")
  describeTreeType (TtList $ TtString "hello") $ do
    successCase (someWv (WtList WtString) ["hello", "hello"])
    failureCase (someWv (WtList WtString) ["hello", "hello", "what's all this then?"])
  describeTreeType (TtSet $ TtFloat unbounded) $ do
    successCase (someWv (WtList WtFloat) [41.0, 42.0, 43.0])
    failureCase (someWv (WtList WtFloat) [41.0, 42.0, 43.0, 42.0])
  describeTreeType (TtOrdSet $ TtWord32 unbounded) $ do
    successCase (someWv (WtList WtWord32) [41, 42, 43])
    failureCase (someWv (WtList WtWord32) [41, 42, 43, 42])
  describeTreeType (TtMaybe $ TtString "banana") $ do
    successCase (someWv (WtMaybe WtString) Nothing)
    successCase (someWv (WtMaybe WtString) $ Just "banana")
    failureCase (someWv (WtMaybe WtString) $ Just "apple")

describeTreeType :: TreeType a -> SpecWith (TreeType a) -> Spec
describeTreeType ty = around (\s -> void $ s ty) .
  describe ("validate of: " ++ (Text.unpack $ ttToText ty))

successCase :: SomeWireValue -> SpecWith (TreeType a)
successCase (SomeWireValue wv) =
  it ("should accept the valid value: " ++ show wv) $
    \tt -> case getTtShow tt of
      Dict -> validate @(Either String) tt wv `shouldSatisfy` isRight

failureCase :: SomeWireValue -> SpecWith (TreeType a)
failureCase (SomeWireValue wv) =
  it ("should reject the invalid value: " ++ show wv) $
    \tt -> case getTtShow tt of
      Dict -> validate @(Either String) tt wv `shouldSatisfy` isLeft
